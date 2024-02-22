unit Mf6.LakeTableFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TLakeTableDimensions = class(TCustomMf6Persistent)
  private
    FNROW: Integer;
    FNCOL: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
    property NROW: Integer read FNROW;
    property NCOL: Integer read FNCOL;
  end;

  TLakeTableItem = record
  private
    Fstage: Extended;
    Fvolume: Extended;
    Fsarea: Extended;
    Fbarea: TRealOption;
    procedure Initialize;
  public
    property stage: Extended read Fstage;
    property volume: Extended read Fvolume;
    property sarea: Extended read Fsarea;
    property barea: TRealOption read Fbarea;
  end;

  TLakeTableItemList = TList<TLakeTableItem>;

  TLakeTableValues = class(TCustomMf6Persistent)
  private
    FItems: TLakeTableItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NCOL: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): TLakeTableItem;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TLakeTableItem read GetItem; default;
  end;

  TLakeTable = class(TPackageReader)
  private
    FLakeTableDimensions: TLakeTableDimensions;
    FTable: TLakeTableValues;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter;
      const NPER: Integer); override;
    property LakeTableDimensions: TLakeTableDimensions read FLakeTableDimensions;
    property Table: TLakeTableValues read FTable;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedSLakeTableD = 'Unrecognized %s Lake Table dimension in the fol' +
  'lowing line';
  StrUnrecognizedSLakeTableRow = 'Unrecognized %s Lake Table row in the ' +
  'following line';

{ TLakeTableDimensions }

procedure TLakeTableDimensions.Initialize;
begin
  FNROW := 0;
  FNCOL := 0;
end;

procedure TLakeTableDimensions.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
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
    else if (FSplitter[0] = 'NROW') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNROW) then
    begin
    end
    else if (FSplitter[0] = 'NCOL') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], FNCOL) then
    begin
      if not (FNCOL in [3,4]) then
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSLakeTableD, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSLakeTableD, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TLakeTableItem }

procedure TLakeTableItem.Initialize;
begin
  Fstage := 0;
  Fvolume := 0;
  Fsarea := 0;
  Fbarea.Initialize
end;

{ TLakeTableValues }

constructor TLakeTableValues.Create(PackageType: string);
begin
  FItems := TLakeTableItemList.Create;
  inherited;

end;

destructor TLakeTableValues.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TLakeTableValues.GetCount: Integer;
begin
  result := FItems.Count;
end;

function TLakeTableValues.GetItem(Index: Integer): TLakeTableItem;
begin
  result := FItems[Index];
end;

procedure TLakeTableValues.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TLakeTableValues.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter; NCOL: Integer);
var
  ALine: string;
  ErrorLine: string;
  TableItem: TLakeTableItem;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading LAKE TABLE file');
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
    if ReadEndOfSection(ALine, ErrorLine, 'TABLE', Unhandled) then
    begin
      Exit
    end;

    TableItem.Initialize;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'TABLE') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= NCOL)
      and TryFortranStrToFloat(FSplitter[0], TableItem.Fstage)
      and TryFortranStrToFloat(FSplitter[1], TableItem.Fvolume)
      and TryFortranStrToFloat(FSplitter[1], TableItem.Fsarea)
      then
    begin
      if (NCOL = 4) then
      begin
        if TryFortranStrToFloat(FSplitter[2], TableItem.Fbarea.Value) then
        begin
          TableItem.Fbarea.Used := True;
          FItems.Add(TableItem)
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSLakeTableRow, [FPackageType]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        FItems.Add(TableItem)
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSLakeTableRow, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TLakeTable }

constructor TLakeTable.Create(PackageType: string);
begin
  inherited;
  FLakeTableDimensions := TLakeTableDimensions.Create(PackageType);
  FTable := TLakeTableValues.Create(PackageType);

end;

destructor TLakeTable.Destroy;
begin
  FLakeTableDimensions.Free;
  FTable.Free;
  inherited;
end;

procedure TLakeTable.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
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
      if FSplitter[1] ='DIMENSIONS' then
      begin
        FLakeTableDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='TABLE' then
      begin
        FTable.Read(Stream, Unhandled, FLakeTableDimensions.FNCOL);
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
