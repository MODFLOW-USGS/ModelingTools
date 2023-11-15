unit NpfFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections, TvkFileReaderUnit;

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
    SAVE_FLOWS: Boolean;
    PRINT_FLOWS: Boolean;
    ALTERNATIVE_CELL_AVERAGING: string;
    THICKSTRT: Boolean;
    VARIABLECV: Boolean;
    DEWATERED: Boolean;
    PERCHED: Boolean;
    REWET: TRewet;
    XT3D: Boolean;
    RHS: Boolean;
    SAVE_SPECIFIC_DISCHARGE: Boolean;
    SAVE_SATURATION: Boolean;
    K22OVERK: Boolean;
    K33OVERK: Boolean;
    TVK6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TNpfGridData = class(TCustomMf6Persistent)
  private
    ICELLTYPE: TIArray3D;
    K: TDArray3D;
    K22: TDArray3D;
    K33: TDArray3D;
    ANGLE1: TDArray3D;
    ANGLE2: TDArray3D;
    ANGLE3: TDArray3D;
    WETDRY: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
  end;

  TNpf = class(TDimensionedPackageReader)
  private
    FOptions: TNpfOptions;
    FGridData: TNpfGridData;
    TvkPackages: TPackageList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
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
  SAVE_FLOWS := False;
  PRINT_FLOWS := False;
  ALTERNATIVE_CELL_AVERAGING := '';
  THICKSTRT := False;
  VARIABLECV := False;
  DEWATERED := False;
  PERCHED := False;
  REWET.Initialize;
  XT3D := False;
  RHS := False;
  SAVE_SPECIFIC_DISCHARGE := False;
  SAVE_SATURATION := False;
  K22OVERK := False;
  K33OVERK := False;
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
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if (FSplitter[0] = 'ALTERNATIVE_CELL_AVERAGING') and (FSplitter.Count >= 2) then
    begin
      ALTERNATIVE_CELL_AVERAGING := FSplitter[1];
    end
    else if (FSplitter[0] = 'THICKSTRT') then
    begin
      THICKSTRT := True;
    end
    else if (FSplitter[0] = 'VARIABLECV') then
    begin
      VARIABLECV := True;
      if (FSplitter.Count >= 2) then
      begin
        DEWATERED := (FSplitter[1] = 'DEWATERED')
      end;
    end
    else if (FSplitter[0] = 'PERCHED') then
    begin
      PERCHED := True;
    end
    else if (FSplitter[0] = 'REWET')
      and (FSplitter.Count >= 7)
      and (FSplitter[1] >= 'WETFCT')
      and (FSplitter[3] >= 'IWETIT')
      and (FSplitter[5] >= 'IHDWET')
      and TryFortranStrToFloat(FSplitter[2], REWET.WETFCT)
      and TryStrToInt(FSplitter[4], REWET.IWETIT)
      and TryStrToInt(FSplitter[6], REWET.IHDWET)
      then
    begin
      REWET.Used := True;
    end
    else if (FSplitter[0] = 'XT3D') then
    begin
      XT3D := True;
      if (FSplitter.Count >= 2) then
      begin
        RHS := (FSplitter[1] = 'RHS')
      end;
    end
    else if (FSplitter[0] = 'SAVE_SPECIFIC_DISCHARGE') then
    begin
      SAVE_SPECIFIC_DISCHARGE := True;
    end
    else if (FSplitter[0] = 'SAVE_SATURATION') then
    begin
      SAVE_SATURATION := True;
    end
    else if (FSplitter[0] = 'K22OVERK') then
    begin
      K22OVERK := True;
    end
    else if (FSplitter[0] = 'K33OVERK') then
    begin
      K33OVERK := True;
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
      Unhandled.WriteLine('Unrecognized NPF option in the following line.');
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
  SetLength(ICELLTYPE, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(K, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(K22, 0);
  SetLength(K33, 0);
  SetLength(ANGLE1, 0);
  SetLength(ANGLE2, 0);
  SetLength(ANGLE3, 0);
  SetLength(WETDRY, 0);
  inherited;
end;

procedure TNpfGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  SectionName: string;
  Layered: Boolean;
  DoubleThreeDReader: TDouble3DArrayReader;
  IntThreeDReader: TInteger3DArrayReader;
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

    if ReadEndOfSection(ALine, ErrorLine, 'GRIDDATA', Unhandled) then
    begin
      Exit;
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'ICELLTYPE' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      IntThreeDReader := TInteger3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        IntThreeDReader.Read(Stream, Unhandled);
        ICELLTYPE := IntThreeDReader.FData;
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
        K := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'K22' then
    begin
      SetLength(K22, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        K22 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'K33' then
    begin
      SetLength(K33, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        K33 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLE1' then
    begin
      SetLength(ANGLE1, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        ANGLE1 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLE2' then
    begin
      SetLength(ANGLE2, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        ANGLE2 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'ANGLE3' then
    begin
      SetLength(ANGLE3, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        ANGLE3 := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'WETDRY' then
    begin
      SetLength(WETDRY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        WETDRY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized NPF GRIDDATA in the following line');
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
  TvkPackages := TPackageList.Create;
end;

destructor TNpf.Destroy;
begin
  TvkPackages.Free;
  FOptions.Free;
  FGridData.Free;
  inherited;
end;

procedure TNpf.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
        Unhandled.WriteLine('Unrecognized NPF section on the following Line');
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;

  for PackageIndex := 0 to FOptions.TVK6_FileNames.Count - 1 do
  begin
    TvkPackage := TPackage.Create;
    TvkPackages.Add(TvkPackage);
    TvkPackage.FileType := 'TVK6';
    TvkPackage.FileName := FOptions.TVK6_FileNames[PackageIndex];
    TvkPackage.PackageName := '';

    TvkReader := TTvk.Create(TvkPackage.FileType);
    TvkReader.Dimensions := FDimensions;
    TvkPackage.Package := TvkReader;
    TvkPackage.ReadPackage(Unhandled);
  end;

end;

end.
