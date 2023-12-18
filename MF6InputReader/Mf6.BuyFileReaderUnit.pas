unit Mf6.BuyFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TBuyOptions = class(TCustomMf6Persistent)
  private
    HHFORMULATION_RHS: TBooleanOption;
    DENSEREF: TRealOption;
    DENSITY: TBooleanOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
  end;

  TBuyDimensions = class(TCustomMf6Persistent)
  private
    nrhospecies: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TBuyItem = record
  private
    irhospec: Integer;
    drhodc: Extended;
    crhoref: Extended;
    modelname: string;
    auxspeciesname: string;
    procedure Initialize;
  end;

  TBuyItemList = TList<TBuyItem>;

  TBuyPackageData = class(TCustomMf6Persistent)
  private
    FItems: TBuyItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TBuy = class(TDimensionedPackageReader)
  private
    FOptions: TBuyOptions;
    FBuyDimensions: TBuyDimensions;
    FPackageData: TBuyPackageData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

uses
  ModelMuseUtilities;

{ TBuyOptions }

procedure TBuyOptions.Initialize;
begin
  HHFORMULATION_RHS.Initialize;
  DENSEREF.Initialize;
  DENSITY.Initialize;
  inherited;
end;

procedure TBuyOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if FSplitter[0] = 'HHFORMULATION_RHS' then
    begin
      HHFORMULATION_RHS.Value := True;
      HHFORMULATION_RHS.Used := True;
    end
    else if FSplitter[0] = 'DENSITY' then
    begin
      DENSITY.Value := True;
      DENSITY.Used := True;
    end
    else if (FSplitter[0] = 'DENSEREF') and (FSplitter.Count >= 2)
      and TryFortranStrToFloat(FSplitter[1], DENSEREF.Value) then
    begin
      DENSEREF.Used := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TBuyDimensions }

procedure TBuyDimensions.Initialize;
begin
  inherited;
  nrhospecies := 0;
end;

procedure TBuyDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'NRHOSPECIES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NRHOSPECIES) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TBuyItem }

procedure TBuyItem.Initialize;
begin
  irhospec := 0;
  drhodc := 0;
  crhoref := 0;
  modelname := '';
  auxspeciesname := '';
end;

{ TBuyPackageData }

constructor TBuyPackageData.Create(PackageType: string);
begin
  FItems := TBuyItemList.Create;
  inherited;

end;

destructor TBuyPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TBuyPackageData.Initialize;
begin
  FItems.Clear;
  inherited;
end;

procedure TBuyPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TBuyItem;
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
      and TryStrToInt(FSplitter[0],Item.irhospec)
      and TryFortranStrToFloat(FSplitter[1], Item.drhodc)
      and TryFortranStrToFloat(FSplitter[2], Item.crhoref)
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

{ TBuy }

constructor TBuy.Create(PackageType: string);
begin
  FOptions := TBuyOptions.Create(PackageType);
  FBuyDimensions := TBuyDimensions.Create(PackageType);
  FPackageData := TBuyPackageData.Create(PackageType);
  inherited;
end;

destructor TBuy.Destroy;
begin
  FOptions.Free;
  FBuyDimensions.Free;
  FPackageData.Free;
  inherited;
end;

procedure TBuy.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
        FBuyDimensions.Read(Stream, Unhandled);
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
