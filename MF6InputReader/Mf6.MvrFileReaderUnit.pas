unit Mf6.MvrFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TMvrOptions = class(TCustomMf6Persistent)
  private
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    FMODELNAMES: Boolean;
    BUDGET: Boolean;
    BUDGETCSV: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
    property MODELNAMES: Boolean read FMODELNAMES;
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
  private
    Fmname: string;
    Fpname: string;
    procedure Initialize;
  public
    property mname: string read Fmname;
    property pname: string read Fpname;
  end;

  TMvrPackageItemList = TList<TMvrPackageItem>;

  TMvrPackages = class(TCustomMf6Persistent)
  private
    FItems: TMvrPackageItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      MODELNAMES: Boolean);
    function GetCount: Integer;
    function GetItem(Index: Integer): TMvrPackageItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMvrPackageItem read GetItem; default;
  end;

  TMvrPeriodItem = record
  private
    Fmname1: string;
    Fpname1: string;
    Fid1: Integer;
    Fmname2: string;
    Fpname2: string;
    Fid2: Integer;
    Fmvrtype: string;
    Fvalue: Extended;
    procedure Initialize;
  public
    property mname1: string read Fmname1;
    property pname1: string read Fpname1;
    property id1: Integer read Fid1;
    property mname2: string read Fmname2;
    property pname2: string read Fpname2;
    property id2: Integer read Fid2;
    property mvrtype: string read Fmvrtype;
    property value: Extended read Fvalue;
    function SourceMatch(PackageName: string; ID: Integer): Boolean;
    function ReceiverMatch(PackageName: string; ID: Integer): Boolean;
  end;

  TMvrPeriodItemList = TList<TMvrPeriodItem>;

  TMvrPeriod = class(TCustomMf6Persistent)
  private
    IPer: Integer;
    FItems: TMvrPeriodItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      MODELNAMES: Boolean);
    function GetCount: Integer;
    function GetItem(Index: Integer): TMvrPeriodItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Period: Integer read IPer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TMvrPeriodItem read GetItem; default;
    function HasSource(PackageName: string; ID: Integer): Boolean;
    function HasReceiver(PackageName: string; ID: Integer): Boolean;
  end;

  TMvrPeriodList = TObjectList<TMvrPeriod>;

  TMvr = class(TPackageReader)
  private
    FOptions: TMvrOptions;
    FMvrDimensions: TMvrDimensions;
    FPackages: TMvrPackages;
    FPeriods: TMvrPeriodList;
    function GetPeriod(Index: Integer): TMvrPeriod;
    function GetPeriodCount: Integer;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Options: TMvrOptions read FOptions;
    property Packages: TMvrPackages read FPackages;
    property PeriodCount: Integer read GetPeriodCount;
    property Periods[Index: Integer]: TMvrPeriod read GetPeriod; default;
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
  FMODELNAMES := False;
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
    else if FSplitter[0] = 'PRINT_INPUT' then
    begin
      PRINT_INPUT := True;
    end
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'MODELNAMES' then
    begin
      FMODELNAMES := True;
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
    else if (FSplitter[0] = 'MAXMVR') and (FSplitter.Count >= 2)
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
  Fmname := '';
  Fpname := '';
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

function TMvrPackages.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TMvrPackages.GetItem(Index: Integer): TMvrPackageItem;
begin
  result := FItems[Index];
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
    RestoreStream(Stream);
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
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGES') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= NumberOfItems) then
    begin
      if MODELNAMES then
      begin
        Item.Fmname := FSplitter[0];
        Item.Fpname := FSplitter[1];
      end
      else
      begin
        Item.Fpname := FSplitter[0];
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
  Fmname1 := '';
  Fpname1 := '';
  Fid1 := 0;
  Fmname2 := '';
  Fpname2 := '';
  Fid2 := 0;
  Fmvrtype := '';
  Fvalue := 0;
end;

function TMvrPeriodItem.ReceiverMatch(PackageName: string;
  ID: Integer): Boolean;
begin
  Result := (ID = ID1) and AnsiSameText(PackageName, pname1)
end;

function TMvrPeriodItem.SourceMatch(PackageName: string; ID: Integer): Boolean;
begin
  Result := (ID = ID2) and AnsiSameText(PackageName, pname2)
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

function TMvrPeriod.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TMvrPeriod.GetItem(Index: Integer): TMvrPeriodItem;
begin
    Result := FItems[Index];
end;

function TMvrPeriod.HasReceiver(PackageName: string; ID: Integer): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Count - 1 do
  begin
    Result := Items[Index].SourceMatch(PackageName, ID);
    if result then
    begin
      Exit;
    end;
  end;
end;

function TMvrPeriod.HasSource(PackageName: string; ID: Integer): Boolean;
var
  Index: Integer;
begin
  Result := False;
  for Index := 0 to Count - 1 do
  begin
    Result := Items[Index].ReceiverMatch(PackageName, ID);
    if result then
    begin
      Exit;
    end;
  end;

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
    RestoreStream(Stream);
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
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PERIOD') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= NumberOfColumns then
    begin
      if MODELNAMES then
      begin
        if TryStrToInt(FSplitter[2], MovItem.Fid1)
          and TryStrToInt(FSplitter[5], MovItem.Fid2)
          and TryFortranStrToFloat(FSplitter[7], MovItem.Fvalue)
          then
        begin
          MovItem.Fmname1 := FSplitter[0];
          MovItem.Fpname1 := FSplitter[1];
          MovItem.Fmname2 := FSplitter[3];
          MovItem.Fpname2 := FSplitter[4];
          MovItem.Fmvrtype := UpperCase(FSplitter[6]);
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
        if TryStrToInt(FSplitter[1], MovItem.Fid1)
          and TryStrToInt(FSplitter[3], MovItem.Fid2)
          and TryFortranStrToFloat(FSplitter[5], MovItem.Fvalue)
          then
        begin
          MovItem.Fpname1 := FSplitter[0];
          MovItem.Fpname2 := FSplitter[2];
          MovItem.Fmvrtype := UpperCase(FSplitter[4]);
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

function TMvr.GetPeriod(Index: Integer): TMvrPeriod;
begin
  Result := FPeriods[Index];
end;

function TMvr.GetPeriodCount: Integer;
begin
  Result := FPeriods.Count;
end;

procedure TMvr.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
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
        FPackages.Read(Stream, Unhandled, FOptions.FMODELNAMES);
      end
      else if (FSplitter[1] ='PERIOD') and (FSplitter.Count >= 3) then
      begin
        if TryStrToInt(FSplitter[2], IPER) then
        begin
          if IPER > NPER then
          begin
            break;
          end;
          APeriod := TMvrPeriod.Create(FPackageType);
          FPeriods.Add(APeriod);
          APeriod.IPer := IPER;
          APeriod.Read(Stream, Unhandled, FOptions.FMODELNAMES);
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
