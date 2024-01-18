unit Mf6.NpfFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.TvkFileReaderUnit;

type
  TRewet = record
    WETFCT: Extended;
    IWETIT: Integer;
    IHDWET: Integer;
    Used: Boolean;
    procedure Initialize;
  end;

  TNpfOptions = class(TCustomMf6Persistent)
  private
    FSAVE_FLOWS: Boolean;
    FPRINT_FLOWS: Boolean;
    FALTERNATIVE_CELL_AVERAGING: string;
    FTHICKSTRT: Boolean;
    FVARIABLECV: Boolean;
    FDEWATERED: Boolean;
    FPERCHED: Boolean;
    FREWET: TRewet;
    FXT3D: Boolean;
    FRHS: Boolean;
    FSAVE_SPECIFIC_DISCHARGE: Boolean;
    FSAVE_SATURATION: Boolean;
    FK22OVERK: Boolean;
    FK33OVERK: Boolean;
    TVK6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property SAVE_FLOWS: Boolean read FSAVE_FLOWS;
    property PRINT_FLOWS: Boolean read FPRINT_FLOWS;
    property ALTERNATIVE_CELL_AVERAGING: string read FALTERNATIVE_CELL_AVERAGING;
    property THICKSTRT: Boolean read FTHICKSTRT;
    property VARIABLECV: Boolean read FVARIABLECV;
    property DEWATERED: Boolean read FDEWATERED;
    property PERCHED: Boolean read FPERCHED;
    property REWET: TRewet read FREWET;
    property XT3D: Boolean read FXT3D;
    property RHS: Boolean read FRHS;
    property SAVE_SPECIFIC_DISCHARGE: Boolean read FSAVE_SPECIFIC_DISCHARGE;
    property SAVE_SATURATION: Boolean read FSAVE_SATURATION;
    property K22OVERK: Boolean read FK22OVERK;
    property K33OVERK: Boolean read FK33OVERK;
  end;

  TNpfGridData = class(TCustomMf6Persistent)
  private
    FICELLTYPE: TIArray3D;
    FK: TDArray3D;
    FK22: TDArray3D;
    FK33: TDArray3D;
    FANGLE1: TDArray3D;
    FANGLE2: TDArray3D;
    FANGLE3: TDArray3D;
    FWETDRY: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property ICELLTYPE: TIArray3D read FICELLTYPE;
    property K: TDArray3D read FK;
    property K22: TDArray3D read FK22;
    property K33: TDArray3D read FK33;
    property ANGLE1: TDArray3D read FANGLE1;
    property ANGLE2: TDArray3D read FANGLE2;
    property ANGLE3: TDArray3D read FANGLE3;
    property WETDRY: TDArray3D read FWETDRY;
  end;

  TNpf = class(TDimensionedPackageReader)
  private
    FOptions: TNpfOptions;
    FGridData: TNpfGridData;
    FTvkPackages: TPackageList;
    function GetCount: Integer;
    function GetTvk(Index: Integer): TPackage;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TNpfOptions read FOptions;
    property GridData: TNpfGridData read FGridData;
    property Count: Integer read GetCount;
    property TvkPackages[Index: Integer]: TPackage read GetTvk; default;
  end;

implementation

uses
  ModelMuseUtilities;

{ TRewet }

procedure TRewet.Initialize;
begin
  WETFCT := 0;
  IWETIT := -1;
  IHDWET := -1;
  Used := False;
end;

{ TNpfOptions }

constructor TNpfOptions.Create(PackageType: string);
begin
  TVK6_FileNames := TStringList.Create;
  inherited;

end;

destructor TNpfOptions.Destroy;
begin
  TVK6_FileNames.Free;
  inherited;
end;

procedure TNpfOptions.Initialize;
begin
  inherited;
  FSAVE_FLOWS := False;
  FPRINT_FLOWS := False;
  FALTERNATIVE_CELL_AVERAGING := '';
  FTHICKSTRT := False;
  FVARIABLECV := False;
  FDEWATERED := False;
  FPERCHED := False;
  FREWET.Initialize;
  FXT3D := False;
  FRHS := False;
  FSAVE_SPECIFIC_DISCHARGE := False;
  FSAVE_SATURATION := False;
  FK22OVERK := False;
  FK33OVERK := False;
  TVK6_FileNames.Clear;

end;

procedure TNpfOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TVK6_FileName: string;
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

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'OPTIONS') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      FSAVE_FLOWS := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      FPRINT_FLOWS := True;
    end
    else if (FSplitter[0] = 'ALTERNATIVE_CELL_AVERAGING') and (FSplitter.Count >= 2) then
    begin
      FALTERNATIVE_CELL_AVERAGING := FSplitter[1];
    end
    else if (FSplitter[0] = 'THICKSTRT') then
    begin
      FTHICKSTRT := True;
    end
    else if (FSplitter[0] = 'VARIABLECV') then
    begin
      FVARIABLECV := True;
      if (FSplitter.Count >= 2) then
      begin
        FDEWATERED := (FSplitter[1] = 'DEWATERED')
      end;
    end
    else if (FSplitter[0] = 'PERCHED') then
    begin
      FPERCHED := True;
    end
    else if (FSplitter[0] = 'REWET')
      and (FSplitter.Count >= 7)
      and (FSplitter[1] >= 'WETFCT')
      and (FSplitter[3] >= 'IWETIT')
      and (FSplitter[5] >= 'IHDWET')
      and TryFortranStrToFloat(FSplitter[2], FREWET.WETFCT)
      and TryStrToInt(FSplitter[4], FREWET.IWETIT)
      and TryStrToInt(FSplitter[6], FREWET.IHDWET)
      then
    begin
      FREWET.Used := True;
    end
    else if (FSplitter[0] = 'XT3D') then
    begin
      FXT3D := True;
      if (FSplitter.Count >= 2) then
      begin
        FRHS := (FSplitter[1] = 'RHS')
      end;
    end
    else if (FSplitter[0] = 'SAVE_SPECIFIC_DISCHARGE') then
    begin
      FSAVE_SPECIFIC_DISCHARGE := True;
    end
    else if (FSplitter[0] = 'SAVE_SATURATION') then
    begin
      FSAVE_SATURATION := True;
    end
    else if (FSplitter[0] = 'K22OVERK') then
    begin
      FK22OVERK := True;
    end
    else if (FSplitter[0] = 'K33OVERK') then
    begin
      FK33OVERK := True;
    end
    else if (FSplitter[0] = 'TVK6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TVK6_FileName := FSplitter[2];
      TVK6_FileNames.Add(TVK6_FileName);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;



{ TNpfGridData }

constructor TNpfGridData.Create(PackageType: string);
begin
  FDimensions.Initialize;
  inherited;

end;

procedure TNpfGridData.Initialize;
begin
  SetLength(FICELLTYPE, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FK, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FK22, 0);
  SetLength(FK33, 0);
  SetLength(FANGLE1, 0);
  SetLength(FANGLE2, 0);
  SetLength(FANGLE3, 0);
  SetLength(FWETDRY, 0);
  inherited;
end;

procedure TNpfGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Layered: Boolean;
  DoubleThreeDReader: TDouble3DArrayReader;
  IntThreeDReader: TInteger3DArrayReader;
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
    else if FSplitter[0] = 'ICELLTYPE' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      IntThreeDReader := TInteger3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        IntThreeDReader.Read(Stream, Unhandled);
        FICELLTYPE := IntThreeDReader.FData;
      finally
        IntThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'K' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FK := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'K22' then
    begin
      SetLength(FK22, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FK22 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'K33' then
    begin
      SetLength(FK33, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FK33 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLE1' then
    begin
      SetLength(FANGLE1, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FANGLE1 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLE2' then
    begin
      SetLength(FANGLE2, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FANGLE2 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLE3' then
    begin
      SetLength(FANGLE3, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FANGLE3 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'WETDRY' then
    begin
      SetLength(FWETDRY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FWETDRY := DoubleThreeDReader.FData;
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

{ TNpf }

constructor TNpf.Create(PackageType: string);
begin
  inherited;
  FOptions := TNpfOptions.Create(PackageType);
  FGridData := TNpfGridData.Create(PackageType);
  FTvkPackages := TPackageList.Create;
end;

destructor TNpf.Destroy;
begin
  FTvkPackages.Free;
  FOptions.Free;
  FGridData.Free;
  inherited;
end;

function TNpf.GetCount: Integer;
begin
  result := FTvkPackages.Count;
end;

function TNpf.GetTvk(Index: Integer): TPackage;
begin
  result := FTvkPackages[Index];
end;

procedure TNpf.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  TvkReader: TTvk;
  PackageIndex: Integer;
  TvkPackage: TPackage;
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

  for PackageIndex := 0 to FOptions.TVK6_FileNames.Count - 1 do
  begin
    TvkPackage := TPackage.Create;
    FTvkPackages.Add(TvkPackage);
    TvkPackage.FileType := 'TVK6';
    TvkPackage.FileName := FOptions.TVK6_FileNames[PackageIndex];
    TvkPackage.PackageName := '';

    TvkReader := TTvk.Create(TvkPackage.FileType);
    TvkReader.Dimensions := FDimensions;
    TvkPackage.Package := TvkReader;
    TvkPackage.ReadPackage(Unhandled, NPER);
  end;

end;

end.
