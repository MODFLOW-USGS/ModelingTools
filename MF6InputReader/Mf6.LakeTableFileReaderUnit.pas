unit Mf6.LakeTableFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TLakeTableDimensions = class(TCustomMf6Persistent)
  private
    NROW: Integer;
    NCOL: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TLakeTableItem = record
    stage: Extended;
    volume: Extended;
    sarea: Extended;
    barea: TRealOption;
    procedure Initialize;
  end;

  TLakeTableItemList = TList<TLakeTableItem>;

  TLakeTableValues = class(TCustomMf6Persistent)
  private
    FItems: TLakeTableItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NCOL: Integer);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TLakeTable = class(TPackageReader)
  private
    FLakeTableDimensions: TLakeTableDimensions;
    FTable: TLakeTableValues;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
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
  NROW := 0;
  NCOL := 0;
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
    if (FSplitter[0] = 'NROW') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NROW) then
    begin
    end
    else if (FSplitter[0] = 'NCOL') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NCOL) then
    begin
      if not (NCOL in [3,4]) then
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
  stage := 0;
  volume := 0;
  sarea := 0;
  barea.Initialize
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
    if ReadEndOfSection(ALine, ErrorLine, 'TABLE', Unhandled) then
    begin
      Exit
    end;

    TableItem.Initialize;
//    ALine := UpperCase(ALine);
    FSplitter.DelimitedText := ALine;
    if (FSplitter.Count >= NCOL)
      and TryFortranStrToFloat(FSplitter[0], TableItem.stage)
      and TryFortranStrToFloat(FSplitter[1], TableItem.volume)
      and TryFortranStrToFloat(FSplitter[1], TableItem.sarea)
      then
    begin
      if (NCOL = 4) then
      begin
        if TryFortranStrToFloat(FSplitter[2], TableItem.barea.Value) then
        begin
          TableItem.barea.Used := True;
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

procedure TLakeTable.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
        FTable.Read(Stream, Unhandled, FLakeTableDimensions.NCOL);
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
