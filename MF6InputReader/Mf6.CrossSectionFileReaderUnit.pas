unit Mf6.CrossSectionFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections;

type
  TCrossSectionDimensions = class(TCustomMf6Persistent)
  private
    NROW: Integer;
    NCOL: Integer;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TCrossSectionTableItem = record
    xfraction: Extended;
    height: Extended;
    manfraction: TRealOption;
    procedure Initialize;
  end;

  TCrossSectionTableItemList = TList<TCrossSectionTableItem>;

  TCrossSectionTable = class(TCustomMf6Persistent)
  private
    FItems: TCrossSectionTableItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; NCOL: Integer);
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
  end;

  TCrossSection = class(TPackageReader)
  private
    FCrossSectionDimensions: TCrossSectionDimensions;
    FTable: TCrossSectionTable;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
  end;

implementation

uses
  ModelMuseUtilities;

resourcestring
  StrUnrecognizedSCros = 'Unrecognized %s Cross section dimension in the fol' +
  'lowing line';
  StrUnrecognizedSCrosRow = 'Unrecognized %s Cross section table row in the ' +
  'following line';

{ TCrossSectionDimensions }

procedure TCrossSectionDimensions.Initialize;
begin
  NROW := 0;
  NCOL := 0;
end;

procedure TCrossSectionDimensions.Read(Stream: TStreamReader;
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
      and TryStrToInt(FSplitter[1], NROW) then
    begin
    end
    else if (FSplitter[0] = 'NCOL') and (FSplitter.Count >= 2)
      and TryStrToInt(FSplitter[1], NCOL) then
    begin
      if not (NCOL in [2,3]) then
      begin
        Unhandled.WriteLine(Format(StrUnrecognizedSCros, [FPackageType]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSCros, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TCrossSectionTableItem }

procedure TCrossSectionTableItem.Initialize;
begin
  xfraction := 0;
  height := 0;
  manfraction.Initialize
end;

{ TCrossSectionTable }

constructor TCrossSectionTable.Create(PackageType: string);
begin
  FItems := TCrossSectionTableItemList.Create;
  inherited;

end;

destructor TCrossSectionTable.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TCrossSectionTable.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TCrossSectionTable.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter; NCOL: Integer);
var
  ALine: string;
  ErrorLine: string;
  TableItem: TCrossSectionTableItem;
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
      and TryFortranStrToFloat(FSplitter[0], TableItem.xfraction)
      and TryFortranStrToFloat(FSplitter[1], TableItem.height)
      then
    begin
      if (NCOL = 3) then
      begin
        if TryFortranStrToFloat(FSplitter[2], TableItem.manfraction.Value) then
        begin
          TableItem.manfraction.Used := True;
          FItems.Add(TableItem)
        end
        else
        begin
          Unhandled.WriteLine(Format(StrUnrecognizedSCrosRow, [FPackageType]));
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
      Unhandled.WriteLine(Format(StrUnrecognizedSCrosRow, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TCrossSection }

constructor TCrossSection.Create(PackageType: string);
begin
  inherited;
  FCrossSectionDimensions := TCrossSectionDimensions.Create(PackageType);
  FTable := TCrossSectionTable.Create(PackageType);

end;

destructor TCrossSection.Destroy;
begin
  FCrossSectionDimensions.Free;
  FTable.Free;
  inherited;
end;

procedure TCrossSection.Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
begin
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading stream cross sections package');
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
      if FSplitter[1] ='DIMENSIONS' then
      begin
        FCrossSectionDimensions.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='TABLE' then
      begin
        FTable.Read(Stream, Unhandled, FCrossSectionDimensions.NCOL);
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
