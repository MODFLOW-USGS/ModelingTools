unit Mf6.DisuFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.AtsFileReaderUnit, Mf6.DisFileReaderUnit,
  Mf6.DisvFileReaderUnit;

type
  TDisuOptions = class(TCustomDisOptions)
  private
    VERTICAL_OFFSET_TOLERANCE: Extended;
  protected
    procedure HandleOption(ErrorLine: string; Unhandled: TStreamWriter);  override;
    procedure Initialize; override;
  end;

  TDisuDimensions = class(TCustomMf6Persistent)
  private
    NODES: Integer;
    NJA: Integer;
    NVERT: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetDimensions: TDimensions;
  protected
    procedure Initialize; override;
    property Dimensions: TDimensions read GetDimensions;
  end;

  TDisuGridData = class(TCustomMf6Persistent)
  private
    TOP: TDArray1D;
    BOT: TDArray1D;
    AREA: TDArray1D;
    IDOMAIN: TIArray1D;
    FNODES: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NODES: Integer);
  protected
    procedure Initialize; override;
  end;

  TDisuConnectionData = class(TCustomMf6Persistent)
  private
    IAC: TIArray1D;
    JA: TIArray1D;
    IHC: TIArray1D;
    CL12: TDArray1D;
    HWVA: TDArray1D;
    ANGLDEGX: TDArray1D;
    FNODES: Integer;
    FNJA: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NODES, NJA: Integer);
  protected
    procedure Initialize; override;
  end;

  TDisuVertices = class(TCustomVertices);

  TDisuCells = class(TCustomCells);

  TDisu = class(TPackageReader)
  private
    FOptions: TDisuOptions;
    FDimensions: TDisuDimensions;
    FGridData: TDisuGridData;
    FConnections: TDisuConnectionData;
    FVertices: TDisuVertices;
    FCells: TDisuCells;
    function GetDimensions: TDimensions;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Dimensions: TDimensions read GetDimensions;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedDISUDI = 'Unrecognized DISU DIMENSION option in the followi' +
  'ng line.';

{ TDisuOptions }

procedure TDisuOptions.HandleOption(ErrorLine: string;
  Unhandled: TStreamWriter);
begin
  if FSplitter.Count >= 2 then
  begin
    if FSplitter[0] = 'VERTICAL_OFFSET_TOLERANCE' then
    begin
      if not TryFortranStrToFloat(FSplitter[1], VERTICAL_OFFSET_TOLERANCE) then
      begin
        Unhandled.WriteLine(StrUnrecognizedDISOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      inherited;
    end;
  end
  else
  begin
    inherited;
  end;
end;

procedure TDisuOptions.Initialize;
begin
  inherited;
  VERTICAL_OFFSET_TOLERANCE := 0;
end;

{ TDisuDimensions }

function TDisuDimensions.GetDimensions: TDimensions;
begin
  result.Initialize;
  result.NCOL := NODES;
end;

procedure TDisuDimensions.Initialize;
begin
  inherited;
  NODES := 0;
  NJA := 0;
  NVERT := 0;
end;

procedure TDisuDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
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
    SectionName := 'DIMENSIONS';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'NODES' then
      begin
        if not TryStrToInt(FSplitter[1], NODES) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISUDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NJA' then
      begin
        if not TryStrToInt(FSplitter[1], NJA) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISUDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NVERT' then
      begin
        if not TryStrToInt(FSplitter[1], NVERT) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISUDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedDISUDI);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedDISUDI);
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDisuGridData }

procedure TDisuGridData.Initialize;
begin
  inherited;
  SetLength(TOP, 0);
  SetLength(BOT, 0);
  SetLength(AREA, 0);
  SetLength(IDOMAIN, 0);
end;

procedure TDisuGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  NODES: Integer);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  OneDReader: TDouble1DArrayReader;
  OneIReader: TInteger1DArrayReader;
begin
  FNODES := NODES;
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
      OneDReader := TDouble1DArrayReader.Create(FNodes, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        TOP := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'BOT' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FNodes, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        BOT := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'AREA' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FNodes, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        AREA := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'IDOMAIN' then
    begin
      OneIReader := TInteger1DArrayReader.Create(FNodes, FPackageType);
      try
        OneIReader.Read(Stream, Unhandled);
        IDOMAIN := OneIReader.FData;
      finally
        OneIReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized DISU GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDisuConnectionData }

procedure TDisuConnectionData.Initialize;
begin
  inherited;
  SetLength(IAC, 0);
  SetLength(JA, 0);
  SetLength(IHC, 0);
  SetLength(CL12, 0);
  SetLength(HWVA, 0);
  SetLength(ANGLDEGX, 0);
end;

procedure TDisuConnectionData.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter; NODES, NJA: Integer);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  OneDReader: TDouble1DArrayReader;
  OneIReader: TInteger1DArrayReader;
begin
  FNODES := NODES;
  FNJA := NJA;
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

    SectionName := 'CONNECTIONDATA';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'IAC' then
    begin
      OneIReader := TInteger1DArrayReader.Create(FNodes, FPackageType);
      try
        OneIReader.Read(Stream, Unhandled);
        IAC := OneIReader.FData;
      finally
        OneIReader.Free;
      end;
    end
    else if FSplitter[0] = 'JA' then
    begin
      OneIReader := TInteger1DArrayReader.Create(FNJA, FPackageType);
      try
        OneIReader.Read(Stream, Unhandled);
        JA := OneIReader.FData;
      finally
        OneIReader.Free;
      end;
    end
    else if FSplitter[0] = 'IHC' then
    begin
      OneIReader := TInteger1DArrayReader.Create(FNJA, FPackageType);
      try
        OneIReader.Read(Stream, Unhandled);
        IHC := OneIReader.FData;
      finally
        OneIReader.Free;
      end;
    end
    else if FSplitter[0] = 'CL12' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FNJA, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        CL12 := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'HWVA' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FNJA, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        HWVA := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLDEGX' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FNJA, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        ANGLDEGX := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized CONNECTIONDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDisu }

constructor TDisu.Create(PackageType: string);
begin
  inherited;
  FOptions := TDisuOptions.Create(PackageType);
  FDimensions := TDisuDimensions.Create(PackageType);
  FGridData := TDisuGridData.Create(PackageType);
  FConnections := TDisuConnectionData.Create(PackageType);
  FVertices := TDisuVertices.Create(PackageType);
  FCells := TDisuCells.Create(PackageType);
end;

destructor TDisu.Destroy;
begin
  FOptions.Free;
  FDimensions.Free;
  FGridData.Free;
  FConnections.Free;
  FVertices.Free;
  FCells.Free;
  inherited;
end;

function TDisu.GetDimensions: TDimensions;
begin
  result := FDimensions.Dimensions;
end;

procedure TDisu.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading DISU package');
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
        FGridData.Read(Stream, Unhandled, FDimensions.NODES);
      end
      else if Pos('CONNECTIONDATA', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('CONNECTIONDATA')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FConnections.Read(Stream, Unhandled, FDimensions.NODES, FDimensions.NJA);
      end
      else if Pos('VERTICES', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('VERTICES')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISOp);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FVertices.Read(Stream, Unhandled, FDimensions.NVERT);
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
