unit Mf6.SsmFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TSsmOptions = class(TCustomMf6Persistent)
  private
    PRINT_FLOWS: Boolean;
    SAVE_FLOWS: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TSsmSourcesItem = record
    pname: string;
    srctype: string;
    auxname: string;
    procedure Initialize;
  end;

  TSsmSourcesItemList = TList<TSsmSourcesItem>;

  TSsmSources = class(TCustomMf6Persistent)
  private
    FItems: TSsmSourcesItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetCount: Integer;
    function GetItem(Index: Integer): TSsmSourcesItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSsmSourcesItem read GetItem; default;
  end;

  TSsmFileInputItem = record
    pname: string;
    spc6_filename: string;
    MIXED: Boolean;
    procedure Initialize;
  end;

  TSsmFileInputItemList = TList<TSsmFileInputItem>;

  TSsmFileInput = class(TCustomMf6Persistent)
  private
    FItems: TSsmFileInputItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetCount: Integer;
    function GetItem(Index: Integer): TSsmFileInputItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TSsmFileInputItem read GetItem; default;
  end;

  TSsm = class(TDimensionedPackageReader)
  private
    FOptions: TSsmOptions;
    FSources: TSsmSources;
    FFileInput: TSsmFileInput;
    FSpcPackages: TPackageList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPackage;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    property Sources: TSsmSources read FSources;
    property FileInput: TSsmFileInput read FFileInput;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPackage read GetItem; default;
  end;

implementation

uses
  Mf6.SpcFileReaderUnit;


{ TSsmOptions }

procedure TSsmOptions.Initialize;
begin
  inherited;
  PRINT_FLOWS := False;
  SAVE_FLOWS := False;
end;

procedure TSsmOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if FSplitter[0] = 'PRINT_FLOWS' then
    begin
      PRINT_FLOWS := True;
    end
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TSsmSourcesItem }

procedure TSsmSourcesItem.Initialize;
begin
  pname := '';
  srctype := '';
  auxname := '';
end;

{ TSsmSources }

constructor TSsmSources.Create(PackageType: string);
begin
  FItems := TSsmSourcesItemList.Create;
  inherited;

end;

destructor TSsmSources.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TSsmSources.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSsmSources.GetItem(Index: Integer): TSsmSourcesItem;
begin
  Result := FItems[Index];
end;

procedure TSsmSources.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSsmSources.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  SmsItem: TSsmSourcesItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
begin
  NumberOfItems := 3;
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

    if ReadEndOfSection(ALine, ErrorLine, 'SOURCES', Unhandled) then
    begin
      Exit;
    end;

    SmsItem.Initialize;
    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'SOURCES') then
    begin
      // do nothing
    end
    else if FSplitter.Count >= NumberOfItems then
    begin
      SmsItem.pname := FSplitter[0];
      SmsItem.srctype := FSplitter[1];
      FSplitter.DelimitedText := CaseSensitiveLine;
      SmsItem.auxname := FSplitter[2];
      FItems.Add(SmsItem);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSSOUR, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ TSsmFileInputItem }

procedure TSsmFileInputItem.Initialize;
begin
  pname := '';
  spc6_filename := '';
  MIXED := False;
end;

{ TSsmFileInput }

constructor TSsmFileInput.Create(PackageType: string);
begin
  FItems := TSsmFileInputItemList.Create;
  inherited;

end;

destructor TSsmFileInput.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TSsmFileInput.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TSsmFileInput.GetItem(Index: Integer): TSsmFileInputItem;
begin
  Result := FItems[Index];
end;

procedure TSsmFileInput.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TSsmFileInput.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  SmsFileInputItem: TSsmFileInputItem;
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  NumberOfItems: Integer;
begin
  NumberOfItems := 4;
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

    if ReadEndOfSection(ALine, ErrorLine, 'FILEINPUT', Unhandled) then
    begin
      Exit;
    end;

    SmsFileInputItem.Initialize;
    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'FILEINPUT') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= NumberOfItems)
      and (FSplitter[1] = 'SPC6')
      and (FSplitter[2] = 'FILEIN')
      then
    begin
      SmsFileInputItem.pname := FSplitter[0];
      if (FSplitter.Count >= NumberOfItems + 1)
        and (FSplitter[4] = 'MIXED')
        then
      begin
        SmsFileInputItem.MIXED := True;
      end;
      FSplitter.DelimitedText := CaseSensitiveLine;
      SmsFileInputItem.spc6_filename := FSplitter[3];
      FItems.Add(SmsFileInputItem);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSFILE, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;

end;

{ TSsm }

constructor TSsm.Create(PackageType: string);
begin
  inherited;
  FOptions := TSsmOptions.Create(PackageType);
  FSources := TSsmSources.Create(PackageType);
  FFileInput := TSsmFileInput.Create(PackageType);
  FSpcPackages := TPackageList.Create;

end;

destructor TSsm.Destroy;
begin
  FOptions.Free;
  FSources.Free;
  FFileInput.Free;
  FSpcPackages.Free;
  inherited;
end;

function TSsm.GetCount: Integer;
begin
  Result := FSpcPackages.Count;
end;

function TSsm.GetItem(Index: Integer): TPackage;
begin
  Result := FSpcPackages[Index];
end;

procedure TSsm.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  SpcPackage: TPackage;
  PackageIndex: Integer;
  SpcReader: TSpc;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading SSM package');
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
      else if FSplitter[1] ='SOURCES' then
      begin
        FSources.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='FILEINPUT' then
      begin
        FFileInput.Read(Stream, Unhandled);
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
  for PackageIndex := 0 to FFileInput.FItems.Count - 1 do
  begin
    SpcPackage := TPackage.Create;
    FSpcPackages.Add(SpcPackage);
    SpcPackage.FileType := FPackageType;
    SpcPackage.FileName := FFileInput.FItems[PackageIndex].pname;
    SpcPackage.PackageName := '';

    SpcReader := TSpc.Create(FPackageType);
    SpcReader.Dimensions := FDimensions;
    SpcPackage.Package := SpcReader;
    SpcPackage.ReadPackage(Unhandled, NPER);
  end;
end;

end.
