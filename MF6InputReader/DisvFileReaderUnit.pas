unit DisvFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections, AtsFileReaderUnit, DisFileReaderUnit;

type
  TDisvOptions = class(TCustomDisOptions);

  TDisvDimensions = class(TCustomMf6Persistent)
  private
    FDimensions: TDimensions;
    NVERT: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetDimensions: TDimensions;
  protected
    procedure Initialize; override;
    property Dimensions: TDimensions read GetDimensions;
  end;

  TDisvGridData = class(TCustomMf6Persistent)
  private
    FDimensions: TDimensions;
    TOP: TDArray2D;
    BOTM: TDArray3D;
    IDOMAIN: TIArray3D;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  end;

  TVertex = record
    iv: Integer;
    xv: Extended;
    yv: Extended;
  end;

  TVertexList = TList<TVertex>;

  TCustomVertices = class(TCustomMf6Persistent)
  private
    FNVERT: Integer;
    FVerticies: TVertexList;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NVERT: Integer);
  end;

  TDisvVertices = class(TCustomVertices);

  TDisvCell = record
    icell2d: Integer;
    xc: Extended;
    yc: Extended;
    ncvert: Integer;
    icvert: TIArray1D;
  end;

  TDisvCellList = TList<TDisvCell>;

  TCustomCells = class(TCustomMf6Persistent)
  private
    FCells: TDisvCellList;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TDisvCells = class(TCustomCells);

  TDisv = class(TPackageReader)
  private
    FOptions: TDisvOptions;
    FDimensions: TDisvDimensions;
    FGridData: TDisvGridData;
    FVerticies: TDisvVertices;
    FCells: TDisvCells;
    function GetDimensions: TDimensions;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    property Dimensions: TDimensions read GetDimensions;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedDISOp = 'Unrecognized DISV option in the following line.';
  StrUnrecognizedDISDI = 'Unrecognized DISV DIMENSION option in the followi' +
  'ng line.';
  StrInTheDISVFileNVE = 'In the DISV file NVERT (%d) is not equal to the num' +
  'ber of vertices read (%d).';
  StrUnrecognizedVERTICE = 'Unrecognized VERTICES in the following line';
  StrUnrecognizedCELL2D = 'Unrecognized CELL2D in the following line';

{ TDisvDimensions }

function TDisvDimensions.GetDimensions: TDimensions;
begin
  result := FDimensions;
end;

procedure TDisvDimensions.Initialize;
begin
  inherited;
  FDimensions.Initialize;
  NVERT := 0;
end;

procedure TDisvDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;
    if ReadEndOfSection(ALine, ErrorLine, 'DIMENSIONS', Unhandled) then
    begin
      Exit
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'NLAY' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NLAY) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NCPL' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NCOL) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NVERT' then
      begin
        if not TryStrToInt(FSplitter[1], NVERT) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedDISDI);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedDISDI);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDisvGridData }

procedure TDisvGridData.Initialize;
begin
  inherited;
  SetLength(TOP,0);
  SetLength(BOTM,0);
  SetLength(IDOMAIN,0);
end;

procedure TDisvGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Layered: Boolean;
  TwoDReader: TDouble2DArrayReader;
  ThreeDReader: TDouble3DArrayReader;
  ThreeIReader: TInteger3DArrayReader;
begin
  FDimensions := Dimensions;
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;

    SectionName := 'GRIDDATA';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'TOP' then
    begin
      TwoDReader := TDouble2DArrayReader.Create(FDimensions, FPackageType);
      try
        TwoDReader.Read(Stream, Unhandled);
        TOP := TwoDReader.FData;
      finally
        TwoDReader.Free;
      end;
    end
    else if FSplitter[0] = 'BOTM' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      ThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        ThreeDReader.Read(Stream, Unhandled);
        BOTM := ThreeDReader.FData;
      finally
        ThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'IDOMAIN' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      ThreeIReader := TInteger3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        ThreeIReader.Read(Stream, Unhandled);
        IDOMAIN := ThreeIReader.FData;
      finally
        ThreeIReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized DISV GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDisvVerticies }

constructor TCustomVertices.Create(PackageType: string);
begin
  FVerticies := TVertexList.Create;
  inherited;
end;

destructor TCustomVertices.Destroy;
begin
  FVerticies.Free;
  inherited;
end;

procedure TCustomVertices.Initialize;
begin
  inherited;
  FVerticies.Clear;
end;

procedure TCustomVertices.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  NVERT: Integer);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Vertex: TVertex;
begin
  FNVERT := NVERT;
  Initialize;
  FVerticies.Capacity := FNVERT;
  try
    while not Stream.EndOfStream do
    begin
      ALine := Stream.ReadLine;
      ErrorLine := ALine;
      ALine := StripFollowingComments(ALine);
      if ALine = '' then
      begin
        Continue;
      end;

      SectionName := 'VERTICES';
      if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
      begin
        Exit;
      end;

  //    ALine := UpperCase(ALine);
      FSplitter.DelimitedText := ALine;
      if not TryStrToInt(FSplitter[0], Vertex.iv) then
      begin
        Unhandled.WriteLine(StrUnrecognizedVERTICE);
        Unhandled.WriteLine(ErrorLine);
        Continue;
      end;

      if not TryFortranStrToFloat(FSplitter[1], Vertex.xv) then
      begin
        Unhandled.WriteLine(StrUnrecognizedVERTICE);
        Unhandled.WriteLine(ErrorLine);
        Continue;
      end;

      if not TryFortranStrToFloat(FSplitter[2], Vertex.yv) then
      begin
        Unhandled.WriteLine(StrUnrecognizedVERTICE);
        Unhandled.WriteLine(ErrorLine);
        Continue;
      end;

      FVerticies.Add(Vertex);
    end;
  finally
    if FVerticies.Count <> FNVERT then
    begin
      Unhandled.WriteLine(Format(StrInTheDISVFileNVE, [FNVERT, FVerticies.Count]));
    end;
  end;
end;

{ TDisvCells }

constructor TCustomCells.Create(PackageType: string);
begin
  FCells := TDisvCellList.Create;
  inherited;
end;

destructor TCustomCells.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TCustomCells.Initialize;
begin
  inherited;
  FCells.Clear;
end;

procedure TCustomCells.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Cell: TDisvCell;
  VertIndex: Integer;
  ErrorFound: Boolean;
begin
  Initialize;
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;

    SectionName := 'CELL2D';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

//    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;

    if not TryStrToInt(FSplitter[0], Cell.icell2d) then
    begin
      Unhandled.WriteLine(StrUnrecognizedCELL2D);
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;

    if not TryFortranStrToFloat(FSplitter[1], Cell.xc) then
    begin
      Unhandled.WriteLine(StrUnrecognizedCELL2D);
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;

    if not TryFortranStrToFloat(FSplitter[2], Cell.yc) then
    begin
      Unhandled.WriteLine(StrUnrecognizedCELL2D);
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;

    if not TryStrToInt(FSplitter[3], Cell.ncvert) then
    begin
      Unhandled.WriteLine(StrUnrecognizedCELL2D);
      Unhandled.WriteLine(ErrorLine);
      Continue;
    end;
    SetLength(Cell.icvert, Cell.ncvert);

    ErrorFound := False;
    for VertIndex := 0 to Cell.ncvert - 1 do
    begin
      if not TryStrToInt(FSplitter[4 + VertIndex], Cell.icvert[VertIndex]) then
      begin
        Unhandled.WriteLine(StrUnrecognizedCELL2D);
        Unhandled.WriteLine(ErrorLine);
        ErrorFound := True;
        break;
      end;
    end;
    if ErrorFound then
    begin
      Continue;;
    end;

    FCells.Add(Cell);
  end;
end;

{ TDisv }

constructor TDisv.Create(PackageType: string);
begin
  inherited;
  FOptions := TDisvOptions.Create(PackageType);
  FDimensions := TDisvDimensions.Create(PackageType);
  FGridData := TDisvGridData.Create(PackageType);
  FVerticies := TDisvVertices.Create(PackageType);
  FCells := TDisvCells.Create(PackageType);
end;

destructor TDisv.Destroy;
begin
  FOptions.Free;
  FDimensions.Free;
  FGridData.Free;
  FVerticies.Free;
  FCells.Free;
  inherited;
end;

function TDisv.GetDimensions: TDimensions;
begin
  result := FDimensions.FDimensions;
end;

procedure TDisv.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  while not Stream.EndOfStream do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;

    ALine := UpperCase(ALine);
    if Pos('BEGIN', ALine) = 1 then
    begin
      if Trim(Copy(ALine,Length('BEGIN')+1,1)) <> '' then
      begin
        Unhandled.WriteLine(StrUnrecognizedDISOp);
        Unhandled.WriteLine(ErrorLine);
        Continue;
      end;
      ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
      if Pos('OPTIONS', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('OPTIONS')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FOptions.Read(Stream, Unhandled)
      end
      else if Pos('DIMENSIONS', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('DIMENSIONS')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FDimensions.Read(Stream, Unhandled)
      end
      else if Pos('GRIDDATA', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('GRIDDATA')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FGridData.Read(Stream, Unhandled, FDimensions.FDimensions);
      end
      else if Pos('VERTICES', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('VERTICES')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FVerticies.Read(Stream, Unhandled, FDimensions.NVERT);
      end
      else if Pos('CELL2D', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('CELL2D')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FCells.Read(Stream, Unhandled);
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedDISOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
