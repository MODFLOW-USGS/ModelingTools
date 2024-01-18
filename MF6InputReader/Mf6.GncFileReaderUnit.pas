unit Mf6.GncFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TGncOptions = class(TCustomMf6Persistent)
  private
    PRINT_INPUT: Boolean;
    PRINT_FLOWS: Boolean;
    EXPLICIT: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TGncDimensions = class(TCustomMf6Persistent)
  private
    NUMGNC: Integer;
    NUMALPHAJ: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TGncDataItem = class(TObject)
  private
    cellidn: TCellId;
    cellidm: TCellId;
    cellidsj: TCellIdList;
    alphasj: TDoubleList;
  public
    Constructor Create;
    Destructor Destroy; override;
  end;

  TGncDataItemList = TObjectList<TGncDataItem>;

  TGncData = class(TCustomMf6Persistent)
  private
    FItems: TGncDataItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      NUMALPHAJ: Integer; Dimensions: TDimensions);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TGnc = class(TDimensionedPackageReader)
  private
    FOptions: TGncOptions;
    FGncDimensions: TGncDimensions;
    FData: TGncData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
  end;

implementation

uses
  ModelMuseUtilities;

{ TGncOptions }

procedure TGncOptions.Initialize;
begin
  inherited;
  PRINT_INPUT := False;
  PRINT_FLOWS := False;
  EXPLICIT := False;
end;

procedure TGncOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if FSplitter[0] = 'EXPLICIT' then
    begin
      EXPLICIT := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TGncDimensions }

procedure TGncDimensions.Initialize;
begin
  inherited;
  NUMGNC := 0;
  NUMALPHAJ := 0;
end;

procedure TGncDimensions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NUMGNC') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NUMGNC) then
    begin
    end
    else if (FSplitter[0] = 'NUMALPHAJ') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NUMALPHAJ) then
    begin
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TGncDataItem }

constructor TGncDataItem.Create;
begin
  cellidsj := TCellIdList.Create;
  alphasj := TDoubleList.Create;

end;

destructor TGncDataItem.Destroy;
begin
  cellidsj.Free;
  alphasj.Free;

  inherited;
end;

{ TGncData }

constructor TGncData.Create(PackageType: string);
begin
  FItems := TGncDataItemList.Create;
  inherited;

end;

destructor TGncData.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TGncData.Initialize;
begin
  FItems.Clear;
  inherited;

end;

procedure TGncData.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  NUMALPHAJ: Integer; Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  Item: TGncDataItem;
  NumberOfItems: Integer;
  DimensionCount: Integer;
  StartIndex: Integer;
  CellId: TCellId;
  ErrorFound: Boolean;
  AValue: Extended;
  CellIndex: Integer;
begin
  DimensionCount := Dimensions.DimensionCount;
  NumberOfItems := (2+NUMALPHAJ)*DimensionCount + NUMALPHAJ;
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

    if ReadEndOfSection(ALine, ErrorLine, 'GNCDATA', Unhandled) then
    begin
      Exit;
    end;

    Item := TGncDataItem.Create;
    try
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'GNCDATA') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= NumberOfItems) then
    begin
      if ReadCellID(Item.cellidn, 0, DimensionCount)
        and ReadCellID(Item.cellidm, DimensionCount, DimensionCount) then
      begin
        ErrorFound := False;
        StartIndex := DimensionCount*2;
        Item.cellidsj.Capacity := NUMALPHAJ;
        for CellIndex := 0 to NUMALPHAJ - 1 do
        begin
          CellId.Initialize;
          if ReadCellID(CellId, StartIndex, DimensionCount) then
          begin
            Item.cellidsj.Add(CellId);
          end
          else
          begin
            Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
            Unhandled.WriteLine(ErrorLine);
            ErrorFound := True;
          end;
          Inc(StartIndex,DimensionCount);
        end;
        if not ErrorFound then
        begin
          Item.alphasj.Capacity := NUMALPHAJ;
          for CellIndex := 0 to NUMALPHAJ - 1 do
          begin
            if TryFortranStrToFloat(FSplitter[StartIndex], AValue) then
            begin
              Item.alphasj.Add(AValue);
            end
            else
            begin
              Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
              Unhandled.WriteLine(ErrorLine);
              ErrorFound := True;
            end;
            Inc(StartIndex);
          end;
        end;
        if not ErrorFound then
        begin
          FItems.Add(Item);
          Item := nil;
        end;
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
    finally
      Item.Free;
    end;
  end;
end;

{ TGnc }

constructor TGnc.Create(PackageType: string);
begin
  inherited;
  FOptions := TGncOptions.Create(PackageType);
  FGncDimensions := TGncDimensions.Create(PackageType);
  FData := TGncData.Create(PackageType);

end;

destructor TGnc.Destroy;
begin
  FOptions.Free;
  FGncDimensions.Free;
  FData.Free;
  inherited;
end;

procedure TGnc.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
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
        FGncDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='GNCDATA' then
      begin
        FData.Read(Stream, Unhandled, FGncDimensions.NUMALPHAJ, FDimensions);
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
