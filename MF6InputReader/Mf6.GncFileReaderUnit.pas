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
    FEXPLICIT: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property EXPLICIT: Boolean read FEXPLICIT;
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
    Fcellidn: TMfCellId;
    Fcellidm: TMfCellId;
    Fcellidsj: TCellIdList;
    Falphasj: TDoubleList;
    function GetAlpha(Index: Integer): Double;
    function GetCell(Index: Integer): TMfCellId;
    function GetCount: Integer;
  public
    Constructor Create;
    Destructor Destroy; override;
    property cellidn: TMfCellId read Fcellidn;
    property cellidm: TMfCellId read Fcellidm;
    property Count: Integer read GetCount;
    property Cells[Index: Integer]: TMfCellId read GetCell; default;
    property Alpha[Index: Integer]: Double read GetAlpha;
  end;

  TGncDataItemList = TObjectList<TGncDataItem>;

  TGncData = class(TCustomMf6Persistent)
  private
    FItems: TGncDataItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      NUMALPHAJ: Integer; Dimensions: TDimensions);
    function GetCount: Integer;
    function GetItem(Index: Integer): TGncDataItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TGncDataItem read GetItem; default;
  end;

  TGnc = class(TDimensionedPackageReader)
  private
    FOptions: TGncOptions;
    FGncDimensions: TGncDimensions;
    FData: TGncData;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property Options: TGncOptions read FOptions;
    property Data: TGncData read FData;
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
  FEXPLICIT := False;
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
      FEXPLICIT := True;
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
  Fcellidsj := TCellIdList.Create;
  Falphasj := TDoubleList.Create;

end;

destructor TGncDataItem.Destroy;
begin
  Fcellidsj.Free;
  Falphasj.Free;

  inherited;
end;

function TGncDataItem.GetAlpha(Index: Integer): Double;
begin
  result := Falphasj[Index];
end;

function TGncDataItem.GetCell(Index: Integer): TMfCellId;
begin
  result := Fcellidsj[Index];
end;

function TGncDataItem.GetCount: Integer;
begin
  result := Fcellidsj.Count;
  Assert(result = Falphasj.Count);
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

function TGncData.GetCount: Integer;
begin
  result := FItems.Count
end;

function TGncData.GetItem(Index: Integer): TGncDataItem;
begin
  result := FItems[Index];
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
  CellId: TMfCellId;
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
      if ReadCellID(Item.Fcellidn, 0, DimensionCount)
        and ReadCellID(Item.Fcellidm, DimensionCount, DimensionCount) then
      begin
        ErrorFound := False;
        StartIndex := DimensionCount*2;
        Item.Fcellidsj.Capacity := NUMALPHAJ;
        for CellIndex := 0 to NUMALPHAJ - 1 do
        begin
          CellId.Initialize;
          if ReadCellID(CellId, StartIndex, DimensionCount) then
          begin
            Item.Fcellidsj.Add(CellId);
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
          Item.Falphasj.Capacity := NUMALPHAJ;
          for CellIndex := 0 to NUMALPHAJ - 1 do
          begin
            if TryFortranStrToFloat(FSplitter[StartIndex], AValue) then
            begin
              Item.Falphasj.Add(AValue);
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
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading GNC package');
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
