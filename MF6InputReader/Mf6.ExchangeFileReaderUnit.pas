unit Mf6.ExchangeFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TCustomExchangeOptions = class(TCustomMf6Persistent)
  private
    AUXILIARY: TStringList;
    BOUNDNAMES: Boolean;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    OBS6: TStringList;
    procedure HandleAdditionalOptions(ErrorLine: string; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TGwfExchangeOptions = class(TCustomExchangeOptions)
  private
    CELL_AVERAGING: string;
    VARIABLECV: Boolean;
    DEWATERED: Boolean;
    NEWTON: Boolean;
    XT3D: Boolean;
    GNC6: TStringList;
    MVR6: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TGwtExchangeOptions = class(TCustomExchangeOptions)
  private
    GWFMODELNAME1: string;
    GWFMODELNAME2: string;
    ADV_SCHEME: string;
    DSP_XT3D_OFF: Boolean;
    DSP_XT3D_RHS: Boolean;
    MVT6: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TExcDimensions = class(TCustomMf6Persistent)
  private
    NEXG: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  ExcItem = class(TObject)
    cellidm1: TCellId;
    cellidm2: TCellId;
    ihc: integer;
    cl1: Extended;
    cl2: Extended;
    hwva: Extended;
    aux: TList<Extended>;
    boundname: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  ExcItemList = TObjectList<ExcItem>;

  TExcData = class(TCustomMf6Persistent)
  private
    FItems: ExcItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      Dimensions1, Dimensions2: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TGwfGwf = class(TDimensionedPackageReader)
  private
    FOptions: TGwfExchangeOptions;
    FExcDimensions: TExcDimensions;
    FData: TExcData;
    FObservationsPackages: TPackageList;
    FGncPackages: TPackageList;
    FMvrPackages: TPackageList;
  public
    FDimensions2: TDimensions;
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
  end;

  TGwtGwt = class(TDimensionedPackageReader)
  private
    FOptions: TGwtExchangeOptions;
    FExcDimensions: TExcDimensions;
    FData: TExcData;
    FObservationsPackages: TPackageList;
    FMvtPackages: TPackageList;
  public
    FDimensions2: TDimensions;
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
  end;

implementation

uses
  ModelMuseUtilities, Mf6.ObsFileReaderUnit, Mf6.GncFileReaderUnit, Mf6.MvrFileReaderUnit,
  Mf6.MvtFileReaderUnit;

resourcestring
  StrUnrecognizedSEXCH = 'Unrecognized %s EXCHANGEDATA data in the following' +
  ' line.';

{ TCustomExchangeOptions }

constructor TCustomExchangeOptions.Create(PackageType: string);
begin
  AUXILIARY := TStringList.Create;
  AUXILIARY.CaseSensitive := False;
  OBS6 := TStringList.Create;
  inherited;

end;

destructor TCustomExchangeOptions.Destroy;
begin
  AUXILIARY.Free;
  OBS6.Free;
  inherited;
end;

procedure TCustomExchangeOptions.Initialize;
begin
  inherited;
  AUXILIARY.Clear;
  OBS6.Clear;
  BOUNDNAMES := False;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
end;

procedure TCustomExchangeOptions.HandleAdditionalOptions(ErrorLine: string;
  Unhandled: TStreamWriter);
var
  ALine: string;
  CaseSensitiveLine: string;
  AuxIndex: Integer;
  AUXILIARY_Name: string;
  Obs_FileName: string;
begin
  ALine := StripFollowingComments(ErrorLine);
  Assert(ALine <> '');

  CaseSensitiveLine := ALine;
  ALine := UpperCase(ALine);
  FSplitter.DelimitedText := ALine;
  Assert(FSplitter.Count > 0);
  if FSplitter[0] = 'BOUNDNAMES' then
  begin
    BOUNDNAMES := True;
  end
  else if FSplitter[0] = 'PRINT_INPUT' then
  begin
    PRINT_INPUT := True;
  end
  else if FSplitter[0] = 'PRINT_FLOWS' then
  begin
    PRINT_FLOWS := True;
  end
  else if FSplitter[0] = 'SAVE_FLOWS' then
  begin
    SAVE_FLOWS := True;
  end
  else if (FSplitter[0] = 'AUXILIARY')
    and (FSplitter.Count >= 2) then
  begin
    FSplitter.DelimitedText := CaseSensitiveLine;
    for AuxIndex := 1 to FSplitter.Count - 1 do
    begin
      AUXILIARY_Name := FSplitter[AuxIndex];
      AUXILIARY.Add(AUXILIARY_Name);
    end;
  end
  else if (FSplitter[0] = 'OBS6')
    and (FSplitter.Count >= 3)
    and (FSplitter[1] = 'FILEIN') then
  begin
    FSplitter.DelimitedText := CaseSensitiveLine;
    Obs_FileName := FSplitter[2];
    OBS6.Add(Obs_FileName);
  end
  else
  begin
    Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
    Unhandled.WriteLine(ErrorLine);
  end;

end;

{ TGwfExchangeOptions }

constructor TGwfExchangeOptions.Create(PackageType: string);
begin
  GNC6 := TStringList.Create;
  MVR6 := TStringList.Create;
  inherited;

end;

destructor TGwfExchangeOptions.Destroy;
begin
  GNC6.Free;
  MVR6.Free;
  inherited;
end;

procedure TGwfExchangeOptions.Initialize;
begin
  inherited;
  GNC6.Clear;
  MVR6.Clear;
end;

procedure TGwfExchangeOptions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  GNC6_Name: string;
  MVR6_FileName: string;
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
    else if (FSplitter[0] = 'CELL_AVERAGING') and (FSplitter.Count >= 2) then
    begin
      CELL_AVERAGING := FSplitter[1];
    end
    else if FSplitter[0] = 'VARIABLECV' then
    begin
      VARIABLECV := True;
      if (FSplitter.Count >= 2) and (FSplitter[1] = 'DEWATERED') then
      begin
        DEWATERED := True;
      end;
    end
    else if FSplitter[0] = 'NEWTON' then
    begin
      NEWTON := True;
    end
    else if FSplitter[0] = 'XT3D' then
    begin
      XT3D := True;
    end
    else if (FSplitter[0] = 'GNC6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN')
      then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      GNC6_Name := FSplitter[2];
      GNC6.Add(GNC6_Name);
    end
    else if (FSplitter[0] = 'MVR6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN') then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      MVR6_FileName := FSplitter[2];
      MVR6.Add(MVR6_FileName);
    end
    else
    begin
      HandleAdditionalOptions(ErrorLine, Unhandled);
    end;
  end;
end;

{ TGwtExchangeOptions }

constructor TGwtExchangeOptions.Create(PackageType: string);
begin
  MVT6 := TStringList.Create;
  inherited;

end;

destructor TGwtExchangeOptions.Destroy;
begin
  MVT6.Free;
  inherited;
end;

procedure TGwtExchangeOptions.Initialize;
begin
  inherited;
  MVT6.Clear;
  GWFMODELNAME1 := '';
  GWFMODELNAME2 := '';
  ADV_SCHEME := '';
  DSP_XT3D_OFF := False;
  DSP_XT3D_RHS := False;
end;

procedure TGwtExchangeOptions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  MVT6_Name: string;
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
    else if (FSplitter[0] = 'GWFMODELNAME1') and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      GWFMODELNAME1 := FSplitter[1];
    end
    else if (FSplitter[0] = 'GWFMODELNAME2') and (FSplitter.Count >= 2) then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      GWFMODELNAME2 := FSplitter[1];
    end
    else if (FSplitter[0] = 'ADV_SCHEME') and (FSplitter.Count >= 2) then
    begin
      ADV_SCHEME := FSplitter[1];
    end
    else if FSplitter[0] = 'DSP_XT3D_OFF' then
    begin
      DSP_XT3D_OFF := True;
    end
    else if FSplitter[0] = 'DSP_XT3D_RHS' then
    begin
      DSP_XT3D_RHS := True;
    end
    else if (FSplitter[0] = 'MVT6')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEIN')
      then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      MVT6_Name := FSplitter[2];
      MVT6.Add(MVT6_Name);
    end
    else
    begin
      HandleAdditionalOptions(ErrorLine, Unhandled);
    end;
  end;
end;

{ TExcDimensions }

procedure TExcDimensions.Initialize;
begin
  inherited;
  NEXG := 0;
end;

procedure TExcDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NEXG') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NEXG) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ ExcItem }

constructor ExcItem.Create;
begin
  aux := TList<Extended>.Create;
  cellidm1.Initialize;
  cellidm2.Initialize;
  ihc := 0;
  cl1 := 0;
  cl2 := 0;
  hwva := 0;
end;

destructor ExcItem.Destroy;
begin
  aux.Free;
  inherited;
end;

{ TExcData }

constructor TExcData.Create(PackageType: string);
begin
  FItems := ExcItemList.Create;
  inherited;

end;

destructor TExcData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TExcData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TExcData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions1, Dimensions2: TDimensions; naux: Integer; BOUNDNAMES: Boolean);
var
  DimensionCount1: Integer;
  Exchange: ExcItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Aux: Extended;
  StartIndex: Integer;
  AuxIndex: Integer;
  NumberOfColumns: Integer;
  DimensionCount2: Integer;
  SumDimensions: Integer;
begin
  DimensionCount1 := Dimensions1.DimensionCount;
  DimensionCount2 := Dimensions2.DimensionCount;
  SumDimensions := DimensionCount1+DimensionCount1;
  NumberOfColumns := SumDimensions + 4 + naux;
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

    if ReadEndOfSection(ALine, ErrorLine, 'EXCHANGEDATA', Unhandled) then
    begin
      Exit;
    end;

    Exchange := ExcItem.Create;;
    try
      CaseSensitiveLine := ALine;
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'EXCHANGEDATA') then
      begin
        // do nothing
      end
      else if FSplitter.Count >= NumberOfColumns then
      begin
        if ReadCellID(Exchange.cellidm1, 0, DimensionCount1)
          and ReadCellID(Exchange.cellidm2, DimensionCount1, DimensionCount2)
          and TryStrToInt(FSplitter[SumDimensions], Exchange.ihc)
          and TryFortranStrToFloat(FSplitter[SumDimensions+1], Exchange.cl1)
          and TryFortranStrToFloat(FSplitter[SumDimensions+2], Exchange.cl2)
          and TryFortranStrToFloat(FSplitter[SumDimensions+3], Exchange.hwva)
          then
        begin
          StartIndex := SumDimensions + 4;
          for AuxIndex := 0 to naux - 1 do
          begin
            if TryFortranStrToFloat(FSplitter[StartIndex], Aux) then
            begin
              Exchange.aux.Add(Aux);
            end
            else
            begin
              Exchange.Free;
              Exchange := nil;
              Unhandled.WriteLine(Format(StrUnrecognizedSEXCH, [FPackageType]));
              Unhandled.WriteLine(ErrorLine);
              break;
            end;
            Inc(StartIndex);
          end;
          if (Exchange <> nil) and BOUNDNAMES and (FSplitter.Count >= NumberOfColumns+1) then
          begin
            FSplitter.DelimitedText := CaseSensitiveLine;
            Exchange.boundname := FSplitter[StartIndex];
          end;
          FItems.Add(Exchange);
          Exchange:= nil;
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSEXCH, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSEXCH, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    finally
      Exchange.Free;
    end;
  end;

end;

{ TGwfGwf }

constructor TGwfGwf.Create(PackageType: string);
begin
  inherited;
  FOptions := TGwfExchangeOptions.Create(PackageType);
  FExcDimensions := TExcDimensions.Create(PackageType);
  FData := TExcData.Create(PackageType);
  FObservationsPackages := TPackageList.Create;
  FGncPackages := TPackageList.Create;
  FMvrPackages := TPackageList.Create;
  FDimensions2.Initialize;
end;

destructor TGwfGwf.Destroy;
begin
  FOptions.Free;
  FExcDimensions.Free;
  FData.Free;
  FObservationsPackages.Free;
  FGncPackages.Free;
  FMvrPackages.Free;
  inherited;
end;

procedure TGwfGwf.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  PackageIndex: Integer;
  ObsReader: TObs;
  ObsPackage: TPackage;
  GncPackage: TPackage;
  GncReader: TGnc;
  MvrPackage: TPackage;
  MvrReader: TMvr;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading gwf-gwf exchange package');
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
      else if FSplitter[1] ='DIMENSIONS' then
      begin
        FExcDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='EXCHANGEDATA' then
      begin
        FData.Read(Stream, Unhandled, self.FDimensions, FDimensions2,
          FOptions.AUXILIARY.Count, FOptions.BOUNDNAMES);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
  for PackageIndex := 0 to FOptions.Obs6.Count - 1 do
  begin
    ObsPackage := TPackage.Create;
    FObservationsPackages.Add(ObsPackage);
    ObsPackage.FileType := FPackageType;
    ObsPackage.FileName := FOptions.Obs6[PackageIndex];
    ObsPackage.PackageName := '';

    ObsReader := TObs.Create(FPackageType);
    ObsReader.Dimensions := FDimensions;
    ObsPackage.Package := ObsReader;
    ObsPackage.ReadPackage(Unhandled, NPER);
  end;

  for PackageIndex := 0 to FOptions.GNC6.Count - 1 do
  begin
    GncPackage := TPackage.Create;
    FGncPackages.Add(GncPackage);
    GncPackage.FileType := 'GNC6';
    GncPackage.FileName := FOptions.GNC6[PackageIndex];
    GncPackage.PackageName := '';

    GncReader := TGnc.Create(FPackageType);
    GncReader.Dimensions := FDimensions;
    GncPackage.Package := GncReader;
    GncPackage.ReadPackage(Unhandled, NPER);
  end;

  for PackageIndex := 0 to FOptions.MVR6.Count - 1 do
  begin
    MvrPackage := TPackage.Create;
    FMvrPackages.Add(MvrPackage);
    MvrPackage.FileType := 'MVR6';
    MvrPackage.FileName := FOptions.MVR6[PackageIndex];
    MvrPackage.PackageName := '';

    MvrReader := TMvr.Create(FPackageType);
    MvrPackage.Package := MvrReader;
    MvrPackage.ReadPackage(Unhandled, NPER);
  end;

end;

{ TGwtGwt }

constructor TGwtGwt.Create(PackageType: string);
begin
  inherited;
  FOptions := TGwtExchangeOptions.Create(PackageType);
  FExcDimensions := TExcDimensions.Create(PackageType);
  FData := TExcData.Create(PackageType);
  FObservationsPackages := TPackageList.Create;
  FMvtPackages := TPackageList.Create;
  FDimensions2.Initialize;
end;

destructor TGwtGwt.Destroy;
begin
  FOptions.Free;
  FExcDimensions.Free;
  FData.Free;
  FObservationsPackages.Free;
  FMvtPackages.Free;
  inherited;
end;

procedure TGwtGwt.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  PackageIndex: Integer;
  ObsReader: TObs;
  ObsPackage: TPackage;
  MvtPackage: TPackage;
  MvtReader: TMvt;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading gwt-gwt exchange package');
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
      else if FSplitter[1] ='DIMENSIONS' then
      begin
        FExcDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='EXCHANGEDATA' then
      begin
        FData.Read(Stream, Unhandled, self.FDimensions, FDimensions2,
          FOptions.AUXILIARY.Count, FOptions.BOUNDNAMES);
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
  for PackageIndex := 0 to FOptions.Obs6.Count - 1 do
  begin
    ObsPackage := TPackage.Create;
    FObservationsPackages.Add(ObsPackage);
    ObsPackage.FileType := FPackageType;
    ObsPackage.FileName := FOptions.Obs6[PackageIndex];
    ObsPackage.PackageName := '';

    ObsReader := TObs.Create(FPackageType);
    ObsReader.Dimensions := FDimensions;
    ObsPackage.Package := ObsReader;
    ObsPackage.ReadPackage(Unhandled, NPER);
  end;

  for PackageIndex := 0 to FOptions.MVT6.Count - 1 do
  begin
    MvtPackage := TPackage.Create;
    FMvtPackages.Add(MvtPackage);
    MvtPackage.FileType := 'MVT6';
    MvtPackage.FileName := FOptions.MVT6[PackageIndex];
    MvtPackage.PackageName := '';

    MvtReader := TMvt.Create(FPackageType);
    MvtPackage.Package := MvtReader;
    MvtPackage.ReadPackage(Unhandled, NPER);
  end;

end;

end.
