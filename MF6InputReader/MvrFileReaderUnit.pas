unit MvrFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TMvrOptions = class(TCustomMf6Persistent)
  private
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    MODELNAMES: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TMvrDimensions = class(TCustomMf6Persistent)
  private
    MAXMVR: Integer;
    MAXPACKAGES: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TMvrPackageItem = record
    mname: string;
    pname: string;
    procedure Initialize;
  end;

  TMvrPackageItemList = TList<TMvrPackageItem>;

  TMvrPackages = class(TCustomMf6Persistent)
  private
    FItems: TMvrPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      MODELNAMES: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TMvrPeriodItem = record
    mname1: string;
    pname1: string;
    id1: Integer;
    mname2: string;
    pname2: string;
    id2: Integer;
    mvrtype: string;
    value: Extended;
    procedure Initialize;
  end;

  TMvrPeriodItemList = TList<TMvrPeriodItem>;

  TMvrPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FItems: TMvrPeriodItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      MODELNAMES: Boolean);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TMvrPeriodList = TObjectList<TMvrPeriod>;

  TMvr = class(TPackageReader)
  private
    FOptions: TMvrOptions;
    FMvrDimensions: TMvrDimensions;
    FPackages: TMvrPackages;
    FPeriods: TMvrPeriodList;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
  end;

implementation

uses
  ModelMuseUtilities;

{ TMvrOptions }

procedure TMvrOptions.Initialize;
begin
  inherited;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  MODELNAMES := False;
  BUDGET := False;
  BUDGETCSV := False;
end;

procedure TMvrOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if ReadEndOfSection(ALine, ErrorLine, 'OPTIONS', Unhandled) then
    begin
      Exit
    end;

    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    Assert(FSplitter.Count > 0);
    if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'MODELNAMES' then
    begin
      MODELNAMES := True;
    end
    else if (FSplitter[0] = 'BUDGET')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      BUDGET := True;
    end
    else if (FSplitter[0] = 'BUDGETCSV')
      and (FSplitter.Count >= 3)
      and (FSplitter[1] = 'FILEOUT') then
    begin
      BUDGETCSV := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TMvrDimensions }

procedure TMvrDimensions.Initialize;
begin
  inherited;
  MAXMVR := 0;
  MAXPACKAGES := 0;
end;

procedure TMvrDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    if (FSplitter[0] = 'MAXMVR') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXMVR) then
    begin
    end
    else if (FSplitter[0] = 'MAXPACKAGES') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], MAXPACKAGES) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TMvrPackageItem }

procedure TMvrPackageItem.Initialize;
begin
  mname := '';
  pname := '';
end;

{ TMvrPackages }

constructor TMvrPackages.Create(PackageType: string);
begin
  FItems := TMvrPackageItemList.Create;
  inherited;

end;

destructor TMvrPackages.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TMvrPackages.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TMvrPackages.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter; MODELNAMES: Boolean);
var
  ALine: string;
  ErrorLine: string;
  Item: TMvrPackageItem;
  NumberOfItems: Integer;
begin
  if MODELNAMES then
  begin
    NumberOfItems := 2;
  end
  else
  begin
    NumberOfItems := 1;
  end;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PACKAGES', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;
    FSplitter.DelimitedText := ALine;
    if (FSplitter.Count >= NumberOfItems) then
    begin
      if MODELNAMES then
      begin
        Item.mname := FSplitter[0];
        Item.pname := FSplitter[1];
      end
      else
      begin
        Item.pname := FSplitter[0];
      end;
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TMvrPeriodItem }

procedure TMvrPeriodItem.Initialize;
begin
  mname1 := '';
  pname1 := '';
  id1 := 0;
  mname2 := '';
  pname2 := '';
  id2 := 0;
  mvrtype := '';
  value := 0;
end;

{ TMvrPeriod }

constructor TMvrPeriod.Create(PackageType: string);
begin
  FItems := TMvrPeriodItemList.Create;
  inherited;

end;

destructor TMvrPeriod.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TMvrPeriod.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TMvrPeriod.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  MODELNAMES: Boolean);
var
  MovItem: TMvrPeriodItem;
  ALine: string;
  ErrorLine: string;
  NumberOfColumns: Integer;
begin
  if MODELNAMES then
  begin
    NumberOfColumns := 8;
  end
  else
  begin
    NumberOfColumns := 6;
  end;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PERIOD', Unhandled) then
    begin
      Exit;
    end;

    MovItem.Initialize;
    FSplitter.DelimitedText := ALine;
    if FSplitter.Count >= NumberOfColumns then
    begin
      if MODELNAMES then
      begin
        if TryStrToInt(FSplitter[2], MovItem.id1)
          and TryStrToInt(FSplitter[5], MovItem.id2)
          and TryFortranStrToFloat(FSplitter[7], MovItem.value)
          then
        begin
          MovItem.mname1 := FSplitter[0];
          MovItem.pname1 := FSplitter[1];
          MovItem.mname2 := FSplitter[3];
          MovItem.pname2 := FSplitter[4];
          MovItem.mvrtype := UpperCase(FSplitter[6]);
          FItems.Add(MovItem);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        if TryStrToInt(FSplitter[1], MovItem.id1)
          and TryStrToInt(FSplitter[3], MovItem.id2)
          and TryFortranStrToFloat(FSplitter[5], MovItem.value)
          then
        begin
          MovItem.pname1 := FSplitter[0];
          MovItem.pname2 := FSplitter[2];
          MovItem.mvrtype := UpperCase(FSplitter[4]);
          FItems.Add(MovItem);
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPERI, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ TMvr }

constructor TMvr.Create(PackageType: string);
begin
  inherited;
  FOptions := TMvrOptions.Create(PackageType);
  FMvrDimensions := TMvrDimensions.Create(PackageType);
  FPackages := TMvrPackages.Create(PackageType);
  FPeriods := TMvrPeriodList.Create;
end;

destructor TMvr.Destroy;
begin
  FOptions.Free;
  FMvrDimensions.Free;
  FPackages.Free;
  FPeriods.Free;
  inherited;
end;

procedure TMvr.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  IPER: Integer;
  APeriod: TMvrPeriod;
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
        FMvrDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='PACKAGES' then
      begin
        FPackages.Read(Stream, Unhandled, FOptions.MODELNAMES);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          APeriod := TMvrPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FOptions.MODELNAMES);
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
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSData, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

end.
