unit VscFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TVscOptions = class(TCustomMf6Persistent)
  private
    VISCREF: TRealOption;
    VISCOSITY: TBooleanOption;
    TEMPERATURE_SPECIES_NAME: TStringOption;
    THERMAL_FORMULATION: TStringOption;
    THERMAL_A2: TRealOption;
    THERMAL_A3: TRealOption;
    THERMAL_A4: TRealOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
  end;

  TVscDimensions = class(TCustomMf6Persistent)
  private
    NVISCSPECIES: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TVscItem = record
  private
    iviscspec: Integer;
    dviscdc: Extended;
    cviscref: Extended;
    modelname: string;
    auxspeciesname: string;
    procedure Initialize;
  end;

  TVscItemList = TList<TVscItem>;

  TVscPackageData = class(TCustomMf6Persistent)
  private
    FItems: TVscItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TVsc = class(TDimensionedPackageReader)
  private
    FOptions: TVscOptions;
    FVscDimensions: TVscDimensions;
    FPackageData: TVscPackageData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

uses
  ModelMuseUtilities;

{ TVscOptions }

procedure TVscOptions.Initialize;
begin
  VISCREF.Initialize;
  VISCOSITY.Initialize;
  inherited;
end;

procedure TVscOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
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
    if FSplitter[0] = 'VISCOSITY' then
    begin
      VISCOSITY.Value := True;
      VISCOSITY.Used := True;
    end
    else if (FSplitter.Count >= 2) then
    begin
      if (FSplitter[0] = 'VISCREF')
        and TryFortranStrToFloat(FSplitter[1], VISCREF.Value) then
      begin
        VISCREF.Used := True;
      end
      else if (FSplitter[0] = 'TEMPERATURE_SPECIES_NAME') then
      begin
        TEMPERATURE_SPECIES_NAME.Value := FSplitter[1];
        TEMPERATURE_SPECIES_NAME.Used := True;
      end
      else if (FSplitter[0] = 'THERMAL_FORMULATION') then
      begin
        THERMAL_FORMULATION.Value := FSplitter[1];
        THERMAL_FORMULATION.Used := True;
      end
      else if (FSplitter[0] = 'THERMAL_A2')
        and TryFortranStrToFloat(FSplitter[1], THERMAL_A2.Value) then
      begin
        THERMAL_A2.Used := True;
      end
      else if (FSplitter[0] = 'THERMAL_A3')
        and TryFortranStrToFloat(FSplitter[1], THERMAL_A3.Value) then
      begin
        THERMAL_A3.Used := True;
      end
      else if (FSplitter[0] = 'THERMAL_A4')
        and TryFortranStrToFloat(FSplitter[1], THERMAL_A4.Value) then
      begin
        THERMAL_A4.Used := True;
      end
      else
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedOCOpti, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOCOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TVscDimensions }

procedure TVscDimensions.Initialize;
begin
  inherited;
  NVISCSPECIES := 0;
end;

procedure TVscDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'NVISCSPECIES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NVISCSPECIES) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOCOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TVscItem }

procedure TVscItem.Initialize;
begin
  iviscspec := 0;
  dviscdc := 0;
  cviscref := 0;
  modelname := '';
  auxspeciesname := '';
end;

{ TVscPackageData }

constructor TVscPackageData.Create(PackageType: string);
begin
  FItems := TVscItemList.Create;
  inherited;

end;

destructor TVscPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TVscPackageData.Initialize;
begin
  FItems.Clear;
  inherited;
end;

procedure TVscPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TVscItem;
  CaseSensitiveLine: string;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PACKAGEDATA', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;

    CaseSensitiveLine := ALine;
    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if (FSplitter.Count >= 5)
      and TryStrToInt(FSplitter[0],Item.iviscspec)
      and TryFortranStrToFloat(FSplitter[1], Item.dviscdc)
      and TryFortranStrToFloat(FSplitter[2], Item.cviscref)
      then
    begin
      FSplitter.DelimitedText := CaseSensitiveLine;
      Item.modelname := FSplitter[3];
      Item.auxspeciesname := FSplitter[4];
      FItems.Add(Item)
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPACK,  [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TVsc }

constructor TVsc.Create(PackageType: string);
begin
  FOptions := TVscOptions.Create(PackageType);
  FVscDimensions := TVscDimensions.Create(PackageType);
  FPackageData := TVscPackageData.Create(PackageType);
  inherited;
end;

destructor TVsc.Destroy;
begin
  FOptions.Free;
  FVscDimensions.Free;
  FPackageData.Free;
  inherited;
end;

procedure TVsc.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    FSplitter.DelimitedText := ALine;
    if FSplitter[0] = 'BEGIN' then
    begin
      if FSplitter[1] ='OPTIONS' then
      begin
        FOptions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='DIMENSIONS' then
      begin
        FVscDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='PACKAGEDATA' then
      begin
        FPackageData.Read(Stream, Unhandled);
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
end;

end.
