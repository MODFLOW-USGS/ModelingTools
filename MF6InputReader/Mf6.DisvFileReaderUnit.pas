unit Mf6.DisvFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.AtsFileReaderUnit, Mf6.DisFileReaderUnit,
  System.Generics.Defaults;

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
    FTOP: TDArray2D;
    FBOTM: TDArray3D;
    FIDOMAIN: TIArray3D;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    property TOP: TDArray2D read FTOP;
    property BOTM: TDArray3D read FBOTM;
    property IDOMAIN: TIArray3D read FIDOMAIN;
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
    function GetCount: Integer;
    function GetVertex(Index: Integer): TVertex;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NVERT: Integer);
    property Count: Integer read GetCount;
    property Vertices[Index: Integer]: TVertex read GetVertex; default;
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
    function GetCell(Index: Integer): TDisvCell;
    function GetCount: Integer;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TDisvCell read GetCell; default;
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
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Dimensions: TDimensions read GetDimensions;
    property Options: TDisvOptions read FOptions;
    property GridData: TDisvGridData read FGridData;
    property Verticies: TDisvVertices read FVerticies;
    property Cells: TDisvCells read FCells;
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
    RestoreStream(Stream);
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

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'DIMENSIONS') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 2 then
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
  SetLength(FTOP,0);
  SetLength(FBOTM,0);
  SetLength(FIDOMAIN,0);
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
    RestoreStream(Stream);
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

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'TOP' then
    begin
      TwoDReader := TDouble2DArrayReader.Create(FDimensions, FPackageType);
      try
        TwoDReader.Read(Stream, Unhandled);
        FTOP := TwoDReader.FData;
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
        FBOTM := ThreeDReader.FData;
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
        FIDOMAIN := ThreeIReader.FData;
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

function TCustomVertices.GetCount: Integer;
begin
  result := FVerticies.Count;
end;

function TCustomVertices.GetVertex(Index: Integer): TVertex;
begin
  result := FVerticies[Index];
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
      RestoreStream(Stream);
      ErrorLine := ALine;
      ALine := StripFollowingComments(ALine);
      if ALine = '' then
      begin
        Continue;
      end;

      SectionName := 'VERTICES';
      if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
      begin
        FVerticies.Sort(
          TComparer<TVertex>.Construct(
            function(const Left, Right: TVertex): Integer
            begin
              Result := Left.iv - Right.iv;
            end
          ));
        Exit;
      end;

      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
      begin
        // do nothing
      end
      else
      begin
        if not TryStrToInt(FSplitter[0], Vertex.iv)
          or not TryFortranStrToFloat(FSplitter[1], Vertex.xv)
          or not TryFortranStrToFloat(FSplitter[2], Vertex.yv)
          then
        begin
          Unhandled.WriteLine(StrUnrecognizedVERTICE);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;

        FVerticies.Add(Vertex);
      end;
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

function TCustomCells.GetCell(Index: Integer): TDisvCell;
begin
  result := FCells[Index];
end;

function TCustomCells.GetCount: Integer;
begin
  result := FCells.Count;
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
    RestoreStream(Stream);
    ErrorLine := ALine;
    ALine := StripFollowingComments(ALine);
    if ALine = '' then
    begin
      Continue;
    end;

    SectionName := 'CELL2D';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      FCells.Sort(
        TComparer<TDisvCell>.Construct(
          function(const Left, Right: TDisvCell): Integer
          begin
            Result := Left.icell2d - Right.icell2d;
          end
        ));
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else
    begin

      if not TryStrToInt(FSplitter[0], Cell.icell2d)
        or not TryFortranStrToFloat(FSplitter[1], Cell.xc)
        or not TryFortranStrToFloat(FSplitter[2], Cell.yc)
        or not TryStrToInt(FSplitter[3], Cell.ncvert)
        then
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

procedure TDisv.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading DISV package');
  end;
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
