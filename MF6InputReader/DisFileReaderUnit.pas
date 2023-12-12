unit DisFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections, AtsFileReaderUnit;

type

  TCustomDisOptions = class(TCustomMf6Persistent)
  private
    LENGTH_UNITS: string;
    NOGRB: Boolean;
    XORIGIN: Extended;
    YORIGIN: Extended;
    ANGROT: Extended;
  protected
    procedure Initialize; override;
    procedure HandleOption(ErrorLine: string; Unhandled: TStreamWriter);  virtual;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TDisOptions = class(TCustomDisOptions);

  TDisDimensions = class(TCustomMf6Persistent)
  private
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TDisGridData = class(TCustomMf6Persistent)
  private
    FDimensions: TDimensions;
    DELR: TDArray1D;
    DELC: TDArray1D;
    TOP: TDArray2D;
    BOTM: TDArray3D;
    IDOMAIN: TIArray3D;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  end;

  TDis = class(TPackageReader)
  private
    FOptions: TDisOptions;
    FDimensions: TDisDimensions;
    FGridData: TDisGridData;
    function GetDimensions: TDimensions;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    property Dimensions: TDimensions read GetDimensions;
  end;

resourcestring
  StrUnrecognizedDISOp = 'Unrecognized Discretization option in the following line.';

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedDISDI = 'Unrecognized DIS DIMENSION option in the followi' +
  'ng line.';

{ TDisOptions }

procedure TCustomDisOptions.Initialize;
begin
  LENGTH_UNITS := 'UNKNOWN';
  NOGRB := False;
  XORIGIN := 0;
  YORIGIN := 0;
  ANGROT := 0;
  inherited;
end;

procedure TCustomDisOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  ValidUnits: TStringList;
begin
  Initialize;
  ValidUnits := TStringList.Create;
  try
    ValidUnits.Add('UNKNOWN');
    ValidUnits.Add('FEET');
    ValidUnits.Add('METERS');
    ValidUnits.Add('CENTIMETERS');

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
        if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
        begin
          Exit
        end;

        ALine := UpperCase(ALine);
        FSplitter.DelimitedText := ALine;
        Assert(FSplitter.Count > 0);
        if FSplitter[0] = 'NOGRB' then
        begin
          NOGRB := True;
        end
        else if FSplitter.Count >= 2 then
        begin
          if FSplitter[0] = 'LENGTH_UNITS' then
          begin
            LENGTH_UNITS := FSplitter[1];
          end
          else if FSplitter[0] = 'XORIGIN' then
          begin
            if not TryFortranStrToFloat(FSplitter[1], XORIGIN) then
            begin
              Unhandled.WriteLine(StrUnrecognizedDISOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else if FSplitter[0] = 'YORIGIN' then
          begin
            if not TryFortranStrToFloat(FSplitter[1], YORIGIN) then
            begin
              Unhandled.WriteLine(StrUnrecognizedDISOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else if FSplitter[0] = 'ANGROT' then
          begin
            if not TryFortranStrToFloat(FSplitter[1], ANGROT) then
            begin
              HandleOption(ErrorLine, Unhandled);
            end;
          end
          else
          begin
            HandleOption(ErrorLine, Unhandled);
          end;
        end
        else
        begin
          HandleOption(ErrorLine, Unhandled);
        end;
      end;
    finally
      if ValidUnits.IndexOf(LENGTH_UNITS) < 0 then
      begin
        Unhandled.WriteLine(Format('Unrecognized length unit "%s".', [LENGTH_UNITS]));
      end;
    end;
  finally
    ValidUnits.Free;
  end;
end;

procedure TCustomDisOptions.HandleOption(ErrorLine: string; Unhandled: TStreamWriter);
begin
  Unhandled.WriteLine(StrUnrecognizedDISOp);
  Unhandled.WriteLine(ErrorLine);
end;

{ TDisDimensions }

procedure TDisDimensions.Initialize;
begin
  inherited;
  FDimensions.Initialize;
end;

procedure TDisDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
      else if FSplitter[0] = 'NROW' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NROW) then
        begin
          Unhandled.WriteLine(StrUnrecognizedDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NCOL' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NCOL) then
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

{ TDisGridData }

procedure TDisGridData.Initialize;
begin
  SetLength(DELR, 0);
  SetLength(DELC, 0);
  SetLength(TOP, 0);
  SetLength(BOTM, 0);
  SetLength(IDOMAIN, 0);
  inherited;
end;

procedure TDisGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Layered: Boolean;
  OneDReader: TDouble1DArrayReader;
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
    if FSplitter[0] = 'DELR' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FDimensions.NCol, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        DELR := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DELC' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FDimensions.NRow, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        DELC := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'TOP' then
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
      Unhandled.WriteLine('Unrecognized DIS GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDis }

constructor TDis.Create(PackageType: string);
begin
  FOptions := TDisOptions.Create(PackageType);
  FDimensions := TDisDimensions.Create(PackageType);
  FGridData := TDisGridData.Create(PackageType);
  inherited;
end;

destructor TDis.Destroy;
begin
  FOptions.Free;
  FDimensions.Free;
  FGridData.Free;
  inherited;
end;

function TDis.GetDimensions: TDimensions;
begin
  result := FDimensions.FDimensions;
end;

procedure TDis.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedDISOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
