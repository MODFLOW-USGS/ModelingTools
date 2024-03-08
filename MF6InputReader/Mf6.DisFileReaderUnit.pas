unit Mf6.DisFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.AtsFileReaderUnit;

type

  TCustomDisOptions = class(TCustomMf6Persistent)
  private
    FLENGTH_UNITS: string;
    FNOGRB: Boolean;
    FXORIGIN: Extended;
    FYORIGIN: Extended;
    FANGROT: Extended;
    ValidUnits: TStringList;
    function GetLengthUnit: Integer;
  protected
    procedure Initialize; override;
    procedure HandleOption(ErrorLine: string; Unhandled: TStreamWriter);
      virtual;
  public
    constructor Create(PackageType: string); override;
    Destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    property LENGTH_UNITS: Integer read GetLengthUnit;
    property NOGRB: Boolean read FNOGRB;
    property XORIGIN: Extended read FXORIGIN;
    property YORIGIN: Extended read FYORIGIN;
    property ANGROT: Extended read FANGROT;
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
    FDELR: TDArray1D;
    FDELC: TDArray1D;
    FTOP: TDArray2D;
    FBOTM: TDArray3D;
    FIDOMAIN: TIArray3D;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    property DELR: TDArray1D read FDELR;
    property DELC: TDArray1D read FDELC;
    property TOP: TDArray2D read FTOP;
    property BOTM: TDArray3D read FBOTM;
    property IDOMAIN: TIArray3D read FIDOMAIN;
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
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Dimensions: TDimensions read GetDimensions;
    property Options: TDisOptions read FOptions;
    property GridData: TDisGridData read FGridData;
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
  FLENGTH_UNITS := 'UNKNOWN';
  FNOGRB := False;
  FXORIGIN := 0;
  FYORIGIN := 0;
  FANGROT := 0;
  inherited;
end;

procedure TCustomDisOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
  Initialize;

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
      if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
      begin
        Exit
      end;

      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
      begin
        // do nothing
      end
      else if FSplitter[0] = 'NOGRB' then
      begin
        FNOGRB := True;
      end
      else if FSplitter.Count >= 2 then
      begin
        if FSplitter[0] = 'LENGTH_UNITS' then
        begin
          FLENGTH_UNITS := FSplitter[1];
        end
        else if FSplitter[0] = 'XORIGIN' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], FXORIGIN) then
          begin
            Unhandled.WriteLine(StrUnrecognizedDISOp);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else if FSplitter[0] = 'YORIGIN' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], FYORIGIN) then
          begin
            Unhandled.WriteLine(StrUnrecognizedDISOp);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else if FSplitter[0] = 'ANGROT' then
        begin
          if not TryFortranStrToFloat(FSplitter[1], FANGROT) then
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
    if ValidUnits.IndexOf(FLENGTH_UNITS) < 0 then
    begin
      Unhandled.WriteLine(Format('Unrecognized length unit "%s".', [FLENGTH_UNITS]));
    end;
  end;
end;

constructor TCustomDisOptions.Create(PackageType: string);
begin
  inherited;
  ValidUnits := TStringList.Create;
  ValidUnits.Add('UNKNOWN');
  ValidUnits.Add('FEET');
  ValidUnits.Add('METERS');
  ValidUnits.Add('CENTIMETERS');
end;

destructor TCustomDisOptions.Destroy;
begin
  ValidUnits.Free;
  inherited;
end;

function TCustomDisOptions.GetLengthUnit: Integer;
begin
  result := ValidUnits.IndexOf(FLENGTH_UNITS);
  if result < 0 then
  begin
    result := 0;
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
  SetLength(FDELR, 0);
  SetLength(FDELC, 0);
  SetLength(FTOP, 0);
  SetLength(FBOTM, 0);
  SetLength(FIDOMAIN, 0);
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
  SectionName := 'GRIDDATA';
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

    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, SectionName) then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'DELR' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FDimensions.NCol, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        FDELR := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DELC' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FDimensions.NRow, FPackageType);
      try
        OneDReader.Read(Stream, Unhandled);
        FDELC := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
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

procedure TDis.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading DIS package');
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
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedDISOp);
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
