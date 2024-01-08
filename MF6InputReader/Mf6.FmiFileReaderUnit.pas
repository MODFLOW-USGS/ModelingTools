unit Mf6.FmiFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.TvkFileReaderUnit;

type
  TFmiOptions = class(TCustomMf6Persistent)
  private
    SAVE_FLOWS: Boolean;
    FLOW_IMBALANCE_CORRECTION: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  TFmiItem = record
    // If the flowtype is GWFBUDGET or GWFHEAD, the file name here can be
    // matched with output file names in the OC package of the flow model and
    // thus identify the flow model for this transport model.
    flowtype: string;
    fname: string;
    FFullFileName: string;
    procedure Initialize;
  end;

  TFmiItemList = TList<TFmiItem>;

  TFmiPackageData = class(TCustomMf6Persistent)
  private
    FItems: TFmiItemList;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
    function GetFullBudgetFileName: string;
  protected
    procedure Initialize; override;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property FullBudgetFileName: string read GetFullBudgetFileName;
  end;

  TFmi = class(TPackageReader)
  private
    FOptions: TFmiOptions;
    FPackageData: TFmiPackageData;
    function GetFullBudgetFileName: string;
  public
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter); override;
    property FullBudgetFileName: string read GetFullBudgetFileName;
  end;


implementation

{ TFmiOptions }

procedure TFmiOptions.Initialize;
begin
  inherited;
  SAVE_FLOWS := False;
  FLOW_IMBALANCE_CORRECTION := False;
end;

procedure TFmiOptions.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
    else if FSplitter[0] = 'SAVE_FLOWS' then
    begin
      SAVE_FLOWS := True;
    end
    else if FSplitter[0] = 'FLOW_IMBALANCE_CORRECTION' then
    begin
      FLOW_IMBALANCE_CORRECTION := True;
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedOpti, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
end;

{ TFmiPackageData }

constructor TFmiPackageData.Create(PackageType: string);
begin
  FItems := TFmiItemList.Create;
  inherited;

end;

destructor TFmiPackageData.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TFmiPackageData.GetFullBudgetFileName: string;
var
  Index: Integer;
begin
  result := '';
  for Index := 0 to FItems.Count - 1 do
  begin
    if FItems[Index].flowtype = 'GWFBUDGET' then
    begin
      result := FItems[Index].FFullFileName;
      Exit;
    end;
  end;
end;

procedure TFmiPackageData.Initialize;
begin
  inherited;
  FItems.Clear;
end;

procedure TFmiPackageData.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Item: TFmiItem;
  CaseSensitiveLine: string;
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

    if ReadEndOfSection(ALine, ErrorLine, 'PACKAGEDATA', Unhandled) then
    begin
      Exit;
    end;

    Item.Initialize;

    CaseSensitiveLine := ALine;
    if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'PACKAGEDATA') then
    begin
      // do nothing
    end
    else if (FSplitter.Count >= 3) and (FSplitter[1] = 'FILEIN') then
    begin
      Item.flowtype := FSplitter[0];
      FSplitter.DelimitedText := CaseSensitiveLine;
      Item.fname := FSplitter[2];
      Item.FFullFileName := ExpandFileName(Item.fname);
      FItems.Add(Item);
    end
    else
    begin
      Unhandled.WriteLine(Format(StrUnrecognizedSPACK, [FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

{ TFmiItem }

procedure TFmiItem.Initialize;
begin
  flowtype := '';
  fname := '';
  FFullFileName := '';
end;

{ TFmi }

constructor TFmi.Create(PackageType: string);
begin
  inherited;
  FOptions := TFmiOptions.Create(PackageType);
  FPackageData := TFmiPackageData.Create(PackageType);
end;

destructor TFmi.Destroy;
begin
  FOptions.Free;
  FPackageData.Free;
  inherited;
end;

function TFmi.GetFullBudgetFileName: string;
begin
  result := FPackageData.FullBudgetFileName;
end;

procedure TFmi.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
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
