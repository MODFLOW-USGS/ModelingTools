unit Mf6.DspFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TDspOptions = class(TCustomMf6Persistent)
  private
    FXT3D_OFF: Boolean;
    FXT3D_RHS: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property XT3D_OFF: Boolean read FXT3D_OFF;
    property XT3D_RHS: Boolean read FXT3D_RHS;
  end;

  TDspGridData = class(TCustomMf6Persistent)
  private
    FDIFFC: TDArray3D;
    FALH: TDArray3D;
    FALV: TDArray3D;
    FATH1: TDArray3D;
    FATH2: TDArray3D;
    FATV: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property DIFFC: TDArray3D read FDIFFC;
    property ALH: TDArray3D read FALH;
    property ALV: TDArray3D read FALV;
    property ATH1: TDArray3D read FATH1;
    property ATH2: TDArray3D read FATH2;
    property ATV: TDArray3D read FATV;
  end;

  TDsp = class(TDimensionedPackageReader)
  private
    FOptions: TDspOptions;
    FGridData: TDspGridData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TDspOptions read FOptions;
    property GridData: TDspGridData read FGridData;
  end;



implementation

{ TDspOptions }

procedure TDspOptions.Initialize;
begin
  inherited;
  FXT3D_OFF := False;
  FXT3D_RHS := False;
end;

procedure TDspOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'XT3D_OFF' then
    begin
      FXT3D_OFF := True;
    end
    else if FSplitter[0] = 'XT3D_RHS' then
    begin
      FXT3D_RHS := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TDspGridData }

constructor TDspGridData.Create(PackageType: string);
begin
  inherited;
  FDimensions.Initialize;
end;

procedure TDspGridData.Initialize;
begin
  inherited;
  SetLength(FDIFFC, 0);
  SetLength(FALH, 0);
  SetLength(FALV, 0);
  SetLength(FATH1, 0);
  SetLength(FATH2, 0);
  SetLength(FATV, 0);

end;

procedure TDspGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Layered: Boolean;
  DoubleThreeDReader: TDouble3DArrayReader;
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

    if ReadEndOfSection(ALine, ErrorLine, 'GRIDDATA', Unhandled) then
    begin
      Exit;
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'GRIDDATA') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'DIFFC' then
    begin
      SetLength(FDIFFC, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FDIFFC := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ALH' then
    begin
      SetLength(FALH, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FALH := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ALV' then
    begin
      SetLength(FALV, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FALV := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ATH1' then
    begin
      SetLength(FATH1, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FATH1 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ATH2' then
    begin
      SetLength(FATH2, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FATH2 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ATV' then
    begin
      SetLength(FATV, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FATV := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSGRID, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TDsp }

constructor TDsp.Create(PackageType: string);
begin
  inherited;
  FOptions := TDspOptions.Create(PackageType);
  FGridData := TDspGridData.Create(PackageType);
end;

destructor TDsp.Destroy;
begin
  FOptions.Free;
  FGridData.Free;
  inherited;
end;

procedure TDsp.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading DSP package');
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
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'BEGIN' then
    begin
      if FSplitter[1] ='OPTIONS' then
      begin
        FOptions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='GRIDDATA' then
      begin
        FGridData.Read(Stream, Unhandled, FDimensions);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSSect, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSSect, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

end.
