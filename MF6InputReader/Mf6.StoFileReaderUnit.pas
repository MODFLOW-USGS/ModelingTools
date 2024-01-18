unit Mf6.StoFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TStoOptions = class(TCustomMf6Persistent)
  private
    FSAVE_FLOWS: Boolean;
    FSTORAGECOEFFICIENT: Boolean;
    FSS_CONFINED_ONLY: Boolean;
    FTVS6_FileNames: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property SAVE_FLOWS: Boolean read FSAVE_FLOWS;
    property STORAGECOEFFICIENT: Boolean read FSTORAGECOEFFICIENT;
  end;

  TStoGridData = class(TCustomMf6Persistent)
  private
    FICONVERT: TIArray3D;
    FSS: TDArray3D;
    FSY: TDArray3D;
    FDimensions: TDimensions;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    property ICONVERT: TIArray3D read FICONVERT;
    property SS: TDArray3D read FSS;
    property SY: TDArray3D read FSY;
  end;

  TStoStressPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FTRANSIENT: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property Period: Integer read IPer;
    property TRANSIENT: Boolean read FTRANSIENT;
  end;

  TStoStressPeriodList = TObjectList<TStoStressPeriod>;

  TSto = class(TDimensionedPackageReader)
  private
    FOptions: TStoOptions;
    FGridData: TStoGridData;
    FStressPeriods: TStoStressPeriodList;
    FTvsPackages: TPackageList;
    function GetCount: Integer;
    function GetTvsCount: Integer;
    function GetTvsPackage(Index: Integer): TPackage;
    function GetStressPeriod(Index: Integer): TStoStressPeriod;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TStoOptions read FOptions;
    property GridData: TStoGridData read FGridData;
    property Count: Integer read GetCount;
    property StressPeriods[Index: Integer]: TStoStressPeriod read GetStressPeriod; default;
    property TvsCount: Integer read GetTvsCount;
    property TvsPackages[Index: Integer]: TPackage read GetTvsPackage;
  end;

implementation

uses
  Mf6.TvsFileReaderUnit;

{ TStoOptions }

constructor TStoOptions.Create(PackageType: string);
begin
  FTVS6_FileNames := TStringList.Create;
  inherited;

end;

destructor TStoOptions.Destroy;
begin
  FTVS6_FileNames.Free;
  inherited;
end;

procedure TStoOptions.Initialize;
begin
  inherited;
  FTVS6_FileNames.Clear;
end;

procedure TStoOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  TVS6_FileName: string;
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
    else if FSplitter[0] = 'STORAGECOEFFICIENT' then
    begin
      FSTORAGECOEFFICIENT := True;
    end
    else if FSplitter[0] = 'SS_CONFINED_ONLY' then
    begin
      FSS_CONFINED_ONLY := True;
    end
    else if (FSplitter[0] = 'TVS6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      TVS6_FileName := FSplitter[2];
      FTVS6_FileNames.Add(TVS6_FileName);
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized STO option in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TStoGridData }

constructor TStoGridData.Create(PackageType: string);
begin
  FDimensions.Initialize;
  inherited;

end;

procedure TStoGridData.Initialize;
begin
  SetLength(FICONVERT, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FSS, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  SetLength(FSY, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
  inherited;
end;

procedure TStoGridData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
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
    else if FSplitter[0] = 'ICONVERT' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      IntThreeDReader := TInteger3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        IntThreeDReader.Read(Stream, Unhandled);
        FICONVERT := IntThreeDReader.FData;
      finally
        IntThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SS' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FSS := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else if FSplitter[0] = 'SY' then
    begin
      Layered := (FSplitter.Count >= 2) and (FSplitter[1] = 'LAYERED');
      DoubleThreeDReader := TDouble3DArrayReader.Create(FDimensions, Layered, FPackageType);
      try
        DoubleThreeDReader.Read(Stream, Unhandled);
        FSY := DoubleThreeDReader.FData;
      finally
        DoubleThreeDReader.Free;
      end;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized STO GRIDDATA in the following line');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TSto }

constructor TSto.Create(PackageType: string);
begin
  inherited;
  FOptions := TStoOptions.Create(PackageType);
  FGridData := TStoGridData.Create(PackageType);
  FStressPeriods := TStoStressPeriodList.Create;
  FTvsPackages := TPackageList.Create;
end;

destructor TSto.Destroy;
begin
  FOptions.Free;
  FGridData.Free;
  FStressPeriods.Free;
  FTvsPackages.Free;
  inherited;
end;

function TSto.GetCount: Integer;
begin
  result := FStressPeriods.Count;
end;

function TSto.GetStressPeriod(Index: Integer): TStoStressPeriod;
begin
  result := FStressPeriods[Index]
end;

function TSto.GetTvsCount: Integer;
begin
  result := FTvsPackages.Count;
end;

function TSto.GetTvsPackage(Index: Integer): TPackage;
begin
  result := FTvsPackages[Index];
end;

procedure TSto.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  TvsReader: TTvs;
  PackageIndex: Integer;
  TvsPackage: TPackage;
  IPER: Integer;
  APeriod: TStoStressPeriod;
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
      else if FSplitter[1] ='PERIOD' then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TStoStressPeriod.Create(FPackageType);
          FStressPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled);
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized STO data in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized STO section on the following Line');
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;

  for PackageIndex := 0 to FOptions.FTVS6_FileNames.Count - 1 do
  begin
    TvsPackage := TPackage.Create;
    FTvsPackages.Add(TvsPackage);
    TvsPackage.FileType := 'TVS6';
    TvsPackage.FileName := FOptions.FTVS6_FileNames[PackageIndex];
    TvsPackage.PackageName := '';

    TvsReader := TTvs.Create(TvsPackage.FileType);
    TvsReader.Dimensions := FDimensions;
    TvsPackage.Package := TvsReader;
    TvsPackage.ReadPackage(Unhandled, NPER);
  end;

end;

{ TStoStressPeriod }

procedure TStoStressPeriod.Initialize;
begin
  FTRANSIENT := False;
  inherited;

end;

procedure TStoStressPeriod.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
begin
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
    if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
    begin
      Exit
    end;

    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter[0] = 'STEADY-STATE' then
    begin
      FTRANSIENT := False;
    end
    else if FSplitter[0] = 'TRANSIENT' then
    begin
      FTRANSIENT := True;
    end
    else
    begin
      Unhandled.WriteLine('Unrecognized STO PERIOD data in the following line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

end.
