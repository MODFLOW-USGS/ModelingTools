unit NameFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections, AtsFileReaderUnit;

type
  TCustomNameFileOptions = class(TCustomMf6Persistent)
  private
    ListingFileName: string;
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    procedure HandleAdditionalSingleOptions(ErrorLine: string;
      Unhandled: TStreamWriter); virtual;
    procedure HandleAdditionalDoubleOptions(ErrorLine: string;
      Unhandled: TStreamWriter); virtual;
  protected
    procedure Initialize; override;
  end;

  TFlowNameFileOptions = class(TCustomNameFileOptions)
  private
    NEWTON: Boolean;
    UNDER_RELAXATION: Boolean;
    procedure HandleAdditionalSingleOptions(ErrorLine: string;
      Unhandled: TStreamWriter); override;
    procedure HandleAdditionalDoubleOptions(ErrorLine: string;
      Unhandled: TStreamWriter); override;
  protected
    procedure Initialize; override;
  end;

  TTransportNameFileOptions = class(TCustomNameFileOptions)
  end;

  TPackage = class(TObject)
  private
    FileType: string;
    FileName: string;
    PackageName: string;
    FPackage: TPackageReader;
    procedure ReadPackage(Unhandled: TStreamWriter);
  public
    destructor Destroy; override;
  end;

  TPackageList = TObjectList<TPackage>;

  TCustomPackages = class(TCustomMf6Persistent)
  private
    FPackages: TPackageList;
    FValidPackageTypes: TStringList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Initialize; override;
  end;

  TFlowPackages =class(TCustomPackages)
    procedure Initialize; override;
  end;

  TTransportPackages =class(TCustomPackages)
    procedure Initialize; override;
  end;

  TCustomNameFile = class(TCustomMf6Persistent)
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); virtual; abstract;
    procedure ReadInput(Unhandled: TStreamWriter); virtual; abstract;
  end;

  TNameFile<Options: TCustomNameFileOptions; Packages: TCustomPackages> = class(TCustomNameFile)
  private
    const
    StrUnrecognizedNameOption = 'Unrecognized Name file option in the following line.';
    var
    FOptions: Options;
    FPackages: Packages;
    FDimensions: TDimensions;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    procedure ReadInput(Unhandled: TStreamWriter); override;
  end;

  TFlowNameFile = TNameFile<TFlowNameFileOptions, TFlowPackages>;
  TTransportNameFile = TNameFile<TTransportNameFileOptions, TTransportPackages>;

implementation

uses
  DisFileReaderUnit, DisvFileReaderUnit, DisuFileReaderUnit;

{ TCustomNameFileOptions }

procedure TCustomNameFileOptions.Initialize;
begin
  inherited;
  ListingFileName := '';
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
end;

procedure TCustomNameFileOptions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  AValue: string;
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

//    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;

    if FSplitter.Count >= 1 then
    begin
      ALine := UpperCase(ALine);
      FSplitter.DelimitedText := ALine;
      AValue := FSplitter[0];
      if AValue = 'PRINT_INPUT' then
      begin
        PRINT_INPUT := True;
      end
      else if AValue = 'PRINT_FLOWS' then
      begin
        PRINT_FLOWS := True;
      end
      else if AValue = 'SAVE_FLOWS' then
      begin
        SAVE_FLOWS := True;
      end
      else if FSplitter.Count >= 2 then
      begin
        if UpperCase(FSplitter[0]) = 'LIST' then
        begin
          ListingFileName := FSplitter[1]
        end
        else
        begin
          ALine := UpperCase(ALine);
          FSplitter.DelimitedText := ALine;
          HandleAdditionalDoubleOptions(ErrorLine, Unhandled);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized name file option in the following line.');
        HandleAdditionalSingleOptions(ErrorLine, Unhandled);
      end;
    end
    else
    begin
        Unhandled.WriteLine('Unrecognized name file option in the following line.');
        Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

procedure TCustomNameFileOptions.HandleAdditionalSingleOptions(ErrorLine: string;
  Unhandled: TStreamWriter);
begin
  Unhandled.WriteLine('Unrecognized name file option in the following line.');
  Unhandled.WriteLine(ErrorLine);
end;

procedure TCustomNameFileOptions.HandleAdditionalDoubleOptions(ErrorLine: string; Unhandled: TStreamWriter);
begin
  Unhandled.WriteLine('Unrecognized name file option in the following line.');
  Unhandled.WriteLine(ErrorLine);
end;

{ TFlowNameFileOptions }

procedure TFlowNameFileOptions.HandleAdditionalDoubleOptions(ErrorLine: string;
  Unhandled: TStreamWriter);
var
  AValue: string;
begin
  inherited;
  AValue := FSplitter[0];
  if AValue = 'NEWTON' then
  begin
    NEWTON := True;
    if FSplitter[1] = 'UNDER_RELAXATION' then
    begin
      UNDER_RELAXATION := True;
    end
    else
    begin
      inherited
    end;
  end
  else
  begin
    inherited
  end;
end;

procedure TFlowNameFileOptions.HandleAdditionalSingleOptions(ErrorLine: string;
  Unhandled: TStreamWriter);
var
  AValue: string;
begin
  AValue := FSplitter[0];
  if AValue = 'NEWTON' then
  begin
    NEWTON := True;
  end
  else
  begin
    inherited
  end;

end;

procedure TFlowNameFileOptions.Initialize;
begin
  inherited;
  NEWTON := False;
  UNDER_RELAXATION := False;
end;

{ TCustomPackages }

constructor TCustomPackages.Create;
begin
  FPackages := TPackageList.Create;
  FValidPackageTypes := TStringList.Create;
  inherited;

end;

destructor TCustomPackages.Destroy;
begin
  FValidPackageTypes.Free;
  FPackages.Free;
  inherited;
end;

procedure TCustomPackages.Initialize;
begin
  inherited;
  FPackages.Clear;
  FValidPackageTypes.Clear;
end;

procedure TCustomPackages.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  APackage: TPackage;
  SectionName: string;
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

    SectionName := 'PACKAGES';
    if ReadEndOfSection(ALine, ErrorLine, SectionName, Unhandled) then
    begin
      Exit;
    end;

    FSplitter.DelimitedText := ALine;

    if FSplitter.Count >= 2 then
    begin
      APackage := TPackage.Create;
      FPackages.Add(APackage);
      APackage.FileType := UpperCase(FSplitter[0]);
      APackage.FileName := FSplitter[1];
      if FSplitter.Count >= 3 then
      begin
        APackage.PackageName := FSplitter[2];
      end;

      if FValidPackageTypes.IndexOf(APackage.FileType) < 0 then
      begin
        Unhandled.WriteLine('Unrecognized package type in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine('Error reading the following model line.');
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TFlowPackages }

procedure TFlowPackages.Initialize;
begin
  inherited;
  FValidPackageTypes.Add('DIS6');
  FValidPackageTypes.Add('DISV6');
  FValidPackageTypes.Add('DISU6');
  FValidPackageTypes.Add('IC6');
  FValidPackageTypes.Add('OC6');
  FValidPackageTypes.Add('NPF6');
  FValidPackageTypes.Add('STO6');
  FValidPackageTypes.Add('CSUB6');
  FValidPackageTypes.Add('BUY6');
  FValidPackageTypes.Add('VSC6');
  FValidPackageTypes.Add('HFB6');
  FValidPackageTypes.Add('CHD6');
  FValidPackageTypes.Add('WEL6');
  FValidPackageTypes.Add('DRN6');
  FValidPackageTypes.Add('RIV6');
  FValidPackageTypes.Add('GHB6');
  FValidPackageTypes.Add('RCH6');
  FValidPackageTypes.Add('EVT6');
  FValidPackageTypes.Add('MAW6');
  FValidPackageTypes.Add('SFR6');
  FValidPackageTypes.Add('LAK6');
  FValidPackageTypes.Add('UZF6');
  FValidPackageTypes.Add('MVR6');
  FValidPackageTypes.Add('GNC6');
  FValidPackageTypes.Add('OBS6');
end;

{ TTransportPackages }

procedure TTransportPackages.Initialize;
begin
  inherited;
  FValidPackageTypes.Add('DIS6');
  FValidPackageTypes.Add('DISV6');
  FValidPackageTypes.Add('DISU6');
  FValidPackageTypes.Add('FMI6');
  FValidPackageTypes.Add('IC6');
  FValidPackageTypes.Add('OC6');
  FValidPackageTypes.Add('ADV6');
  FValidPackageTypes.Add('DSP6');
  FValidPackageTypes.Add('SSM6');
  FValidPackageTypes.Add('MST6');
  FValidPackageTypes.Add('IST6');
  FValidPackageTypes.Add('CNC6');
  FValidPackageTypes.Add('SRC6');
  FValidPackageTypes.Add('LKT6');
  FValidPackageTypes.Add('SFT6');
  FValidPackageTypes.Add('MWT6');
  FValidPackageTypes.Add('UZT6');
  FValidPackageTypes.Add('MVT6');
  FValidPackageTypes.Add('OBS6');
end;

{ TNameFile<Options, Packages> }

constructor TNameFile<Options, Packages>.Create;
begin
  inherited;
  FOptions := Options.Create;
  FPackages := Packages.Create;
end;

destructor TNameFile<Options, Packages>.Destroy;
begin
  FPackages.Free;
  FOptions.Free;
  inherited;
end;

procedure TNameFile<Options, Packages>.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
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
        Unhandled.WriteLine(StrUnrecognizedNameOption);
        Unhandled.WriteLine(ErrorLine);
        Continue;
      end;
      ALine := Trim(Copy(ALine, Length('BEGIN')+1, MaxInt)) ;
      if Pos('OPTIONS', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('OPTIONS')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedNameOption);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FOptions.Read(Stream, Unhandled)
      end
      else if Pos('PACKAGES', ALine) = 1 then
      begin
        if Trim(Copy(ALine,Length('PACKAGES')+1,1)) <> '' then
        begin
          Unhandled.WriteLine(StrUnrecognizedNameOption);
          Unhandled.WriteLine(ErrorLine);
          Continue;
        end;
        FPackages.Read(Stream, Unhandled)
      end
      else
      begin
        Unhandled.WriteLine(StrUnrecognizedNameOption);
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

procedure TNameFile<Options, Packages>.ReadInput(Unhandled: TStreamWriter);
var
  PackageIndex: Integer;
  APackage: TPackage;
  DisReader: TDis;
  DisvReader: TDisv;
  DisuReader: TDisu;
begin
  // First read discretization
  for PackageIndex := 0 to FPackages.FPackages.Count - 1 do
  begin
    APackage := FPackages.FPackages[PackageIndex];
    if APackage.FileType = 'DIS6' then
    begin
      DisReader := TDis.Create;
      APackage.FPackage := DisReader;
      APackage.ReadPackage(Unhandled);
      FDimensions := DisReader.Dimensions;
    end
    else if APackage.FileType = 'DISV6' then
    begin
      DisvReader := TDisv.Create;
      APackage.FPackage := DisvReader;
      APackage.ReadPackage(Unhandled);
      FDimensions := DisvReader.Dimensions;
    end
    else if APackage.FileType = 'DISU6' then
    begin
      DisuReader := TDisu.Create;
      APackage.FPackage := DisuReader;
      APackage.ReadPackage(Unhandled);
      FDimensions := DisuReader.Dimensions;
    end;
  end;
end;

{ TPackage }

destructor TPackage.Destroy;
begin
  FPackage.Free;
  inherited;
end;

procedure TPackage.ReadPackage(Unhandled: TStreamWriter);
var
  PackageFile: TStreamReader;
begin
  if FileName <> '' then
  begin
    if TFile.Exists(FileName) then
    begin
      try
        PackageFile := TFile.OpenText(FileName);
        try
          try
            FPackage.Read(PackageFile, Unhandled);
          except on E: Exception do
            begin
              Unhandled.WriteLine('ERROR');
              Unhandled.WriteLine(E.Message);
            end;
          end;
        finally
          PackageFile.Free;
        end;
      except on E: Exception do
        begin
          Unhandled.WriteLine('ERROR');
          Unhandled.WriteLine(E.Message);
        end;
      end
    end
    else
    begin
      Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
        [FileName]));
    end;
  end;
end;

end.
