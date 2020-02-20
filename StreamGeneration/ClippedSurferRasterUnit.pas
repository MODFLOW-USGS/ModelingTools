unit ClippedSurferRasterUnit;

interface

uses
  SubPolygonUnit, SurferGridFileReaderUnit, ShapefileUnit, System.IOUtils,
  System.SysUtils, System.Classes;

type
  EClipError = class(Exception);

  // @name can use Shapefiles to define the polygons to be
  TExcludeOutline = class(TOutline)
  public
    procedure ExcludeShapeFiles(FileNames: TStrings);
  end;

  IClipRaster = interface
    procedure SetIncludeShapeFileName(const Value: string);
    function GetIncludeShapeFileName: string;
    function GetExcludeShapeFileNames: TStrings;
    procedure SetExcludeShapeFileNames(FileNames: TStrings);
    property IncludeShapeFileName: string read GetIncludeShapeFileName
      write SetIncludeShapeFileName;
    property ExcludeShapeFileNames: TStrings read GetExcludeShapeFileNames
      write SetExcludeShapeFileNames;
  end;

  // @name overrides @link(GetIgnore) so that points will be considered as
  // not having data if they are outside the polygons defined in
  // @link(IncludeShapeFileName) or are inside any polygons in
  // @link(ExcludeShapeFiles).
  TClippedSurferRaster = class(TSurferRaster7File2, IClipRaster)
  private
    FIncludeShapeFileName: string;
    FIncludeClipper: TSubPolygon;
    FExcludeOutline: TExcludeOutline;
    FExcludeShapeFileNames: TStrings;
    procedure SetIncludeShapeFileName(const Value: string);
    function GetIncludeShapeFileName: string;
    function GetExcludeShapeFileNames: TStrings;
    procedure SetExcludeShapeFileNames(FileNames: TStrings);
  protected
    function GetIgnore(XIndex, YIndex: integer): Boolean; override;
  public
    // FileName is the name of an existing binary Surfer version 7 raster file.
    constructor Create(FileName: string);
    destructor Destroy; override;
    // @name is used to define the outer limit of the area in @classname that
    // will be considered to have values.
    property IncludeShapeFileName: string read GetIncludeShapeFileName
      write SetIncludeShapeFileName;
    // @name is used to define inactive areas within what would otherwise be
    // considered active areas. The FileNames must refer to Shapefiles.
    property ExcludeShapeFileNames: TStrings read GetExcludeShapeFileNames
      write SetExcludeShapeFileNames;
  end;

  TClippedSurferRasterBlock = class(TSurferRaster7FileBlock, IClipRaster)
  private
    FIncludeShapeFileName: string;
    FIncludeClipper: TSubPolygon;
    FExcludeOutline: TExcludeOutline;
    FExcludeShapeFileNames: TStrings;
    procedure SetIncludeShapeFileName(const Value: string);
    function GetIncludeShapeFileName: string;
    function GetExcludeShapeFileNames: TStrings;
    procedure SetExcludeShapeFileNames(FileNames: TStrings);
  protected
    function GetIgnore(XIndex, YIndex: integer): Boolean; override;
  public
    // FileName is the name of an existing binary Surfer version 7 raster file.
    constructor Create(RasterStream: TStream; XStart, YStart, EdgeCount: Integer);
    destructor Destroy; override;
    // @name is used to define the outer limit of the area in @classname that
    // will be considered to have values.
    property IncludeShapeFileName: string read GetIncludeShapeFileName
      write SetIncludeShapeFileName;
    // @name is used to define inactive areas within what would otherwise be
    // considered active areas. The FileNames must refer to Shapefiles.
    property ExcludeShapeFileNames: TStrings read GetExcludeShapeFileNames
      write SetExcludeShapeFileNames;
  end;


  TClippedSurferRasterMemory = class(TSurferRaster7, IClipRaster)
  private
    FIncludeShapeFileName: string;
    FIncludeClipper: TSubPolygon;
    FExcludeOutline: TExcludeOutline;
    FExcludeShapeFileNames: TStrings;
    procedure SetIncludeShapeFileName(const Value: string);
    function GetIncludeShapeFileName: string;
    function GetExcludeShapeFileNames: TStrings;
  protected
    function GetIgnore(XIndex, YIndex: integer): Boolean; override;
    // @name is used to define inactive areas within what would otherwise be
    // considered active areas. The FileNames must refer to Shapefiles.
    procedure SetExcludeShapeFileNames(FileNames: TStrings);
  public
    // FileName is the name of an existing binary Surfer version 7 raster file.
    constructor Create; overload;
    constructor Create(FileName: string); overload;
    destructor Destroy; override;
    // @name is used to define the outer limit of the area in @classname that
    // will be considered to have values.
    property IncludeShapeFileName: string read GetIncludeShapeFileName
      write SetIncludeShapeFileName;
    property ExcludeShapeFileNames: TStrings read GetExcludeShapeFileNames
      write SetExcludeShapeFileNames;
  end;

implementation

uses
  FastGEO;

resourcestring
  StrTheShapeInTheSha = 'The Shape in the Shapefile must have only one part.';
  StrTheShapefileMustC = 'The Shapefile must contain exactly one shape.';
  StrEither0sOr1s = 'Either %0:s or %1:s does not exist';
  StrTheShapeFileMust = 'The shape file must contain polygons';

{ TClippedSurferRaster }

constructor TClippedSurferRaster.Create(FileName: string);
begin
  inherited;
  FExcludeOutline := TExcludeOutline.Create;
  FExcludeShapeFileNames:= TStringList.Create;
end;

destructor TClippedSurferRaster.Destroy;
begin
  inherited;
  FExcludeShapeFileNames.Free;
  FExcludeOutline.Free;
  FIncludeClipper.Free;
end;

procedure TClippedSurferRaster.SetExcludeShapeFileNames(FileNames: TStrings);
begin
  FExcludeShapeFileNames.Assign(FileNames);
  FExcludeOutline.ExcludeShapeFiles(FileNames);
end;

function TClippedSurferRaster.GetExcludeShapeFileNames: TStrings;
begin
  result := FExcludeShapeFileNames;
end;

function TClippedSurferRaster.GetIgnore(XIndex, YIndex: integer): Boolean;
var
  APoint: TPoint2D;
begin
  Result := inherited GetIgnore(XIndex, YIndex);
  if result then
  begin
    Exit;
  end;
  APoint := LowerLeft;
  APoint.x := APoint.x + XSpacing * (XIndex + 0.5);
  APoint.y := APoint.y + YSpacing * (YIndex + 0.5);
  if (FIncludeClipper <> nil) then
  begin
    result := not FIncludeClipper.IsPointInside(APoint.x, APoint.y);
  end;
  if not result then
  begin
    result := FExcludeOutline.PointInside(APoint);
  end;
end;

function TClippedSurferRaster.GetIncludeShapeFileName: string;
begin
  result := FIncludeShapeFileName;
end;

procedure TClippedSurferRaster.SetIncludeShapeFileName(const Value: string);
var
  ShapeIndexFileName: string;
  ShapeReader: TShapefileGeometryReader;
  AShape: TShapeObject;
  Points: TRealPointArray;
  APoint: TShapePoint;
  PointIndex: Integer;
begin
  if FIncludeShapeFileName <> Value then
  begin
    FIncludeShapeFileName := Value;
    FreeAndNil(FIncludeClipper);
    ShapeIndexFileName := ChangeFileExt(Value, '.shx');
    if TFile.Exists(Value) and TFile.Exists(ShapeIndexFileName) then
    begin
      ShapeReader := TShapefileGeometryReader.Create;
      try
        ShapeReader.ReadFromFile(Value, ShapeIndexFileName);
        if (ShapeReader.FileHeader.ShapeType in [stPolygon, stPolygonZ, stPolygonM])
          and (ShapeReader.Count = 1) then
        begin
          AShape := ShapeReader[0];
          if AShape.FNumParts = 1 then
          begin
            SetLength(Points, AShape.FNumPoints+1);
            for PointIndex := 0 to AShape.FNumPoints - 1 do
            begin
              APoint := AShape.FPoints[PointIndex];
              Points[PointIndex].x := APoint.X;
              Points[PointIndex].y := APoint.Y;
            end;
            Points[AShape.FNumPoints] := Points[0];
            FIncludeClipper := TSubPolygon.Create(Points, AShape.FNumPoints+1, 0, 0);
          end
          else
          begin

            raise EClipError.Create(StrTheShapeInTheSha);
          end;
        end
        else
        begin
          if ShapeReader.Count <> 1 then
          begin
            raise EClipError.Create(StrTheShapefileMustC);
          end
          else
          begin
            raise EClipError.Create(StrTheShapeFileMust);
          end;
        end;
      finally
        ShapeReader.Free;
      end;
    end
    else if Value = '' then
    begin
      // do nothing.
    end
    else
    begin
      raise EClipError.Create(Format(StrEither0sOr1s,
        [Value,ShapeIndexFileName]));
    end;
  end;
end;

{ TExcludeOutline }

procedure TExcludeOutline.ExcludeShapeFiles(FileNames: TStrings);
var
  FileIndex: Integer;
  ShapeFileName: string;
  ShapefileReader: TShapefileGeometryReader;
  ShapeIndex: Integer;
  ShapeIndexFileName: string;
  AShape: TShapeObject;
  PartIndex: Integer;
  Points: TRealPointArray;
  StartIndex: Integer;
  EndIndex: Integer;
  PointIndex: Integer;
  APoint: TShapePoint;
//  PriorPoly: TGpcPolygonClass;
//  Poly: TGpcPolygonClass;
//  UnionPoly: TGpcPolygonClass;
//  AVertex: TPoint2D;
//  PolyIndex: Integer;
begin
//  PriorPoly := nil;
  try
    FPolygons.Clear;
    for FileIndex := 0 to FileNames.Count - 1 do
    begin
      ShapeFileName := FileNames[FileIndex];
      ShapeIndexFileName := ChangeFileExt(ShapeFileName, '.shx');
      if TFile.Exists(ShapeFileName) and TFile.Exists(ShapeIndexFileName) then
      begin
        ShapefileReader := TShapefileGeometryReader.Create;
        try
          ShapefileReader.ReadFromFile(ShapeFileName, ShapeIndexFileName);
          if (ShapefileReader.FileHeader.ShapeType in [stPolygon, stPolygonZ, stPolygonM]) then
          begin
            for ShapeIndex := 0 to ShapefileReader.Count - 1 do
            begin
              AShape := ShapefileReader[ShapeIndex];
              if AShape.FNumPoints > 0 then
              begin
//                Poly := TGpcPolygonClass.Create;
//                Poly.NumberOfContours := AShape.FNumParts;
                for PartIndex := 0 to AShape.FNumParts - 1 do
                begin
                  StartIndex := AShape.FParts[PartIndex];
                  if PartIndex = AShape.FNumParts - 1 then
                  begin
                    EndIndex := AShape.FNumPoints
                  end
                  else
                  begin
                    EndIndex := AShape.FParts[PartIndex+1];
                  end;
//                  Poly.VertexCount[PartIndex] := EndIndex-StartIndex;
                  SetLength(Points, (EndIndex-StartIndex)+1);
                  for PointIndex := StartIndex to EndIndex - 1 do
                  begin
                    APoint := AShape.FPoints[PointIndex];
//                    AVertex.X := APoint.X;
//                    AVertex.Y := APoint.Y;
//                    Poly.Vertices[PartIndex, PointIndex-StartIndex] := AVertex;
                    Points[PointIndex-StartIndex].x := APoint.X;
                    Points[PointIndex-StartIndex].y := APoint.Y;
                  end;
                  Points[Length(Points)-1] := Points[0];
                  FPolygons.Add(TSubPolygon.Create(Points, Length(Points), 0, 0));
                end;
//                if PriorPoly = nil then
//                begin
//                  PriorPoly := Poly
//                end
//                else
//                begin
//                  UnionPoly := TGpcPolygonClass.CreateFromOperation(GPC_UNION, PriorPoly, Poly);
//                  Poly.Free;
//                  PriorPoly.Free;
//                  PriorPoly := UnionPoly;
//                end;
              end;
            end;
          end
          else
          begin
            raise EClipError.Create(StrTheShapeFileMust);
          end;
        finally
          ShapefileReader.Free;
        end;
      end
      else
      begin
        raise EClipError.Create(Format(StrEither0sOr1s,
          [ShapeFileName, ShapeIndexFileName]));
      end;
    end;
//    FPolygons.Capacity := PriorPoly.NumberOfContours;
//    for PolyIndex := 0 to PriorPoly.NumberOfContours - 1 do
//    begin
//      SetLength(Points, PriorPoly.VertexCount[PolyIndex]+1);
//      for PointIndex := 0 to PriorPoly.VertexCount[PolyIndex] - 1 do
//      begin
//        Points[PointIndex] := PriorPoly.Vertices[PolyIndex, PointIndex];
//      end;
//      Points[Length(Points)-1] := Points[0];
//      FPolygons.Add(TSubPolygon.Create(Points, Length(Points), 0, 0));
//    end;
  finally
//    PriorPoly.Free;
  end;
end;

{ TClippedSurferRasterMemory }

constructor TClippedSurferRasterMemory.Create;
begin
  inherited;
  FExcludeOutline := TExcludeOutline.Create;
  FExcludeShapeFileNames:= TStringList.Create;
end;

constructor TClippedSurferRasterMemory.Create(FileName: string);
begin
  Create;
  ReadSurfer7GrdFile(FileName, self);
end;

destructor TClippedSurferRasterMemory.Destroy;
begin
  FExcludeShapeFileNames.Free;
  FExcludeOutline.Free;
  FIncludeClipper.Free;
  inherited;
end;

procedure TClippedSurferRasterMemory.SetExcludeShapeFileNames(FileNames: TStrings);
begin
  FExcludeShapeFileNames.Assign(FileNames);
  FExcludeOutline.ExcludeShapeFiles(FileNames);
end;

function TClippedSurferRasterMemory.GetExcludeShapeFileNames: TStrings;
begin
  result := FExcludeShapeFileNames;
end;

function TClippedSurferRasterMemory.GetIgnore(XIndex, YIndex: integer): Boolean;
var
  APoint: TPoint2D;
begin
  Result := inherited GetIgnore(XIndex, YIndex);
  if result then
  begin
    Exit;
  end;
  APoint := LowerLeft;
  APoint.x := APoint.x + XSpacing * (XIndex + 0.5);
  APoint.y := APoint.y + YSpacing * (YIndex + 0.5);
  if (FIncludeClipper <> nil) then
  begin
    result := not FIncludeClipper.IsPointInside(APoint.x, APoint.y);
  end;
  if not result then
  begin
    result := FExcludeOutline.PointInside(APoint);
  end;
end;

function TClippedSurferRasterMemory.GetIncludeShapeFileName: string;
begin
  Result := FIncludeShapeFileName;
end;

procedure TClippedSurferRasterMemory.SetIncludeShapeFileName(const Value: string);
var
  ShapeIndexFileName: string;
  ShapeReader: TShapefileGeometryReader;
  AShape: TShapeObject;
  Points: TRealPointArray;
  APoint: TShapePoint;
  PointIndex: Integer;
begin
  if FIncludeShapeFileName <> Value then
  begin
    FIncludeShapeFileName := Value;
    FreeAndNil(FIncludeClipper);
    ShapeIndexFileName := ChangeFileExt(Value, '.shx');
    if TFile.Exists(Value) and TFile.Exists(ShapeIndexFileName) then
    begin
      ShapeReader := TShapefileGeometryReader.Create;
      try
        ShapeReader.ReadFromFile(Value, ShapeIndexFileName);
        if (ShapeReader.FileHeader.ShapeType in [stPolygon, stPolygonZ, stPolygonM])
          and (ShapeReader.Count = 1) then
        begin
          AShape := ShapeReader[0];
          if AShape.FNumParts = 1 then
          begin
            SetLength(Points, AShape.FNumPoints+1);
            for PointIndex := 0 to AShape.FNumPoints - 1 do
            begin
              APoint := AShape.FPoints[PointIndex];
              Points[PointIndex].x := APoint.X;
              Points[PointIndex].y := APoint.Y;
            end;
            Points[AShape.FNumPoints] := Points[0];
            FIncludeClipper := TSubPolygon.Create(Points, AShape.FNumPoints+1, 0, 0);
          end
          else
          begin

            raise EClipError.Create(StrTheShapeInTheSha);
          end;
        end
        else
        begin
          if ShapeReader.Count <> 1 then
          begin
            raise EClipError.Create(StrTheShapefileMustC);
          end
          else
          begin
            raise EClipError.Create(StrTheShapeFileMust);
          end;
        end;
      finally
        ShapeReader.Free;
      end;
    end
    else if Value = '' then
    begin
      // do nothing.
    end
    else
    begin
      raise EClipError.Create(Format(StrEither0sOr1s,
        [Value,ShapeIndexFileName]));
    end;
  end;
end;

{ TClippedSurferRasterBlock }

constructor TClippedSurferRasterBlock.Create(RasterStream: TStream; XStart,
  YStart, EdgeCount: Integer);
begin
  inherited;
  FExcludeOutline := TExcludeOutline.Create;
  FExcludeShapeFileNames:= TStringList.Create;
end;

destructor TClippedSurferRasterBlock.Destroy;
begin
  inherited;
  FExcludeShapeFileNames.Free;
  FExcludeOutline.Free;
  FIncludeClipper.Free;
end;

function TClippedSurferRasterBlock.GetExcludeShapeFileNames: TStrings;
begin
  result := FExcludeShapeFileNames;
end;

function TClippedSurferRasterBlock.GetIgnore(XIndex, YIndex: integer): Boolean;
var
  APoint: TPoint2D;
begin
  Result := inherited GetIgnore(XIndex, YIndex);
  if result then
  begin
    Exit;
  end;
  APoint := LowerLeft;
  APoint.x := APoint.x + XSpacing * (XIndex + 0.5);
  APoint.y := APoint.y + YSpacing * (YIndex + 0.5);
  if (FIncludeClipper <> nil) then
  begin
    result := not FIncludeClipper.IsPointInside(APoint.x, APoint.y);
  end;
  if not result then
  begin
    result := FExcludeOutline.PointInside(APoint);
  end;
end;

function TClippedSurferRasterBlock.GetIncludeShapeFileName: string;
begin
  result := FIncludeShapeFileName;
end;

procedure TClippedSurferRasterBlock.SetExcludeShapeFileNames(
  FileNames: TStrings);
begin
  FExcludeShapeFileNames.Assign(FileNames);
  FExcludeOutline.ExcludeShapeFiles(FileNames);
end;

procedure TClippedSurferRasterBlock.SetIncludeShapeFileName(
  const Value: string);
var
  ShapeIndexFileName: string;
  ShapeReader: TShapefileGeometryReader;
  AShape: TShapeObject;
  Points: TRealPointArray;
  APoint: TShapePoint;
  PointIndex: Integer;
begin
  if FIncludeShapeFileName <> Value then
  begin
    FIncludeShapeFileName := Value;
    FreeAndNil(FIncludeClipper);
    ShapeIndexFileName := ChangeFileExt(Value, '.shx');
    if TFile.Exists(Value) and TFile.Exists(ShapeIndexFileName) then
    begin
      ShapeReader := TShapefileGeometryReader.Create;
      try
        ShapeReader.ReadFromFile(Value, ShapeIndexFileName);
        if (ShapeReader.FileHeader.ShapeType in [stPolygon, stPolygonZ, stPolygonM])
          and (ShapeReader.Count = 1) then
        begin
          AShape := ShapeReader[0];
          if AShape.FNumParts = 1 then
          begin
            SetLength(Points, AShape.FNumPoints+1);
            for PointIndex := 0 to AShape.FNumPoints - 1 do
            begin
              APoint := AShape.FPoints[PointIndex];
              Points[PointIndex].x := APoint.X;
              Points[PointIndex].y := APoint.Y;
            end;
            Points[AShape.FNumPoints] := Points[0];
            FIncludeClipper := TSubPolygon.Create(Points, AShape.FNumPoints+1, 0, 0);
          end
          else
          begin

            raise EClipError.Create(StrTheShapeInTheSha);
          end;
        end
        else
        begin
          if ShapeReader.Count <> 1 then
          begin
            raise EClipError.Create(StrTheShapefileMustC);
          end
          else
          begin
            raise EClipError.Create(StrTheShapeFileMust);
          end;
        end;
      finally
        ShapeReader.Free;
      end;
    end
    else if Value = '' then
    begin
      // do nothing.
    end
    else
    begin
      raise EClipError.Create(Format(StrEither0sOr1s,
        [Value,ShapeIndexFileName]));
    end;
  end;
end;

end.
