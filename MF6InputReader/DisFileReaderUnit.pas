unit DisFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections, AtsFileReaderUnit;

type
  TDisOptions = class(TCustomMf6Persistent)
  private
    LENGTH_UNITS: string;
    NOGRB: Boolean;
    XORIGIN: Extended;
    YORIGIN: Extended;
    ANGROT: Extended;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

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
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TDis = class(TCustomMf6Persistent)
  private
    FOptions: TDisOptions;
    FDimensions: TDisDimensions;
    FGridData: TDisGridData;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedTDISOp = 'Unrecognized DIS option in the following line.';
  StrUnrecognizedTDISDI = 'Unrecognized DIS DIMENSION option in the followi' +
  'ng line.';

{ TDisOptions }

procedure TDisOptions.Initialize;
begin
  LENGTH_UNITS := '';
  NOGRB := False;
  XORIGIN := 0;
  YORIGIN := 0;
  ANGROT := 0;
  inherited;
end;

procedure TDisOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
        ALine := UpperCase(ALine);
        if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
        begin
          Exit
        end;

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
              Unhandled.WriteLine(StrUnrecognizedTDISOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else if FSplitter[0] = 'YORIGIN' then
          begin
            if not TryFortranStrToFloat(FSplitter[1], YORIGIN) then
            begin
              Unhandled.WriteLine(StrUnrecognizedTDISOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else if FSplitter[0] = 'ANGROT' then
          begin
            if not TryFortranStrToFloat(FSplitter[1], ANGROT) then
            begin
              Unhandled.WriteLine(StrUnrecognizedTDISOp);
              Unhandled.WriteLine(ErrorLine);
            end;
          end
          else
          begin
            Unhandled.WriteLine(StrUnrecognizedTDISOp);
            Unhandled.WriteLine(ErrorLine);
          end;
        end
        else
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISOp);
          Unhandled.WriteLine(ErrorLine);
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
    ALine := UpperCase(ALine);
    if ReadEndOfSection(ALine, ErrorLine, 'DIMENSIONS', Unhandled) then
    begin
      Exit
    end;

    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter.Count >= 2 then
    begin
      if FSplitter[0] = 'NLAY' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NLAY) then
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NROW' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NROW) then
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else if FSplitter[0] = 'NCOL' then
      begin
        if not TryStrToInt(FSplitter[1], FDimensions.NCOL) then
        begin
          Unhandled.WriteLine(StrUnrecognizedTDISDI);
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedTDISDI);
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(StrUnrecognizedTDISDI);
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

procedure TDisGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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

    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'DELR' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FDimensions.NCol);
      try
        OneDReader.Read(Stream, Unhandled);
        DELR := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'DELC' then
    begin
      OneDReader := TDouble1DArrayReader.Create(FDimensions.NRow);
      try
        OneDReader.Read(Stream, Unhandled);
        DELC := OneDReader.FData;
      finally
        OneDReader.Free;
      end;
    end
    else if FSplitter[0] = 'TOP' then
    begin
      TwoDReader := TDouble2DArrayReader.Create(FDimensions);
      try
        TwoDReader.Read(Stream, Unhandled);
        TOP := TwoDReader.FData;
      finally
        TwoDReader.Free;
      end;
    end
    else if FSplitter[0] = 'BOTM' then
    begin
      Layered := (FSplitter.Count <= 2) and (FSplitter[1] = 'LAYERED');
      ThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered);
      try
        ThreeDReader.Read(Stream, Unhandled);
        BOTM := ThreeDReader.FData;
      finally
        ThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'IDOMAIN' then
    begin
      Layered := (FSplitter.Count <= 2) and (FSplitter[1] = 'LAYERED');
      ThreeIReader := TInteger3DArrayReader.Create(FDimensions, Layered);
      try
        ThreeIReader.Read(Stream, Unhandled);
        IDOMAIN := ThreeIReader.FData;
      finally
        ThreeIReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDis }

constructor TDis.Create;
begin
  FOptions := TDisOptions.Create;
  FDimensions := TDisDimensions.Create;
  FGridData := TDisGridData.Create;
  inherited;
end;

destructor TDis.Destroy;
begin
  FOptions.Free;
  FDimensions.Free;
  FGridData.Free;
  inherited;
end;

procedure TDis.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
begin

end;

end.
