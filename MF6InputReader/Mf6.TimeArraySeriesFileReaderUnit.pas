unit Mf6.TimeArraySeriesFileReaderUnit;

interface

uses
  System.Classes, System.IOUtils, System.SysUtils, Mf6.CustomMf6PersistentUnit,
  System.Generics.Collections, Mf6.TimeSeriesFileReaderUnit;

type
  TTasAttributes = class(TCustomMf6Persistent)
  private
    FName: string;
    FMethod: TTsMethod;
    FSFAC: TRealOption;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  public
    property Name: string read FName;
    property Method: TTsMethod read FMethod;
    property SFAC: TRealOption read FSFAC;
  end;

  TTasTime = class(TCustomMf6Persistent)
  private
    FTime: Extended;
    FValues: TDArray2D;
    FConstant: Boolean;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; Dimensions: TDimensions);
  protected
    property Constant: Boolean read FConstant;
    procedure Initialize; override;
  public
    property Time: Extended read FTime;
    property Values: TDArray2D read FValues;
  end;

  TTasTimeList = TObjectList<TTasTime>;

  TTimeArraySeries = class(TDimensionedPackageReader)
  private
    FAttributes: TTasAttributes;
    FTimes: TTasTimeList;
    FConstant: Boolean;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); override;
    constructor Create(PackageType: string); override;
    destructor Destroy; override;
    property Constant: Boolean read FConstant;
    property Attributes: TTasAttributes read FAttributes;
    property Times: TTasTimeList read FTimes;
  end;




implementation

uses
  ModelMuseUtilities;

{ TTasAttributes }

procedure TTasAttributes.Initialize;
begin
  FName := '';
  FMethod := tsUndefined;
  FSFAC.Initialize;
  inherited;

end;

procedure TTasAttributes.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  CaseSensitiveLine: string;
  Tag: string;
  ValidMethods: TStringList;
  ItemIndex: Integer;
begin
  ValidMethods := TStringList.Create;
  try
    ValidMethods.Capacity := 3;
    ValidMethods.Add('STEPWISE');
    ValidMethods.Add('LINEAR');
    ValidMethods.Add('LINEAREND');
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
      if ReadEndOfSection(ALine, ErrorLine, 'ATTRIBUTES', Unhandled) then
      begin
        Exit
      end;

      CaseSensitiveLine := ALine;
      if SwitchToAnotherFile(Stream, ErrorLine, Unhandled, ALine, 'ATTRIBUTES') then
      begin
        Continue;
      end;
      Tag := UpperCase(FSplitter[0]);
      if (Tag = 'NAME') then
      begin
        FSplitter.DelimitedText := CaseSensitiveLine;
        FName := FSplitter[1];
      end
      else if (Tag = 'METHOD') then
      begin
        ItemIndex := ValidMethods.IndexOf(FSplitter[1]);
        if ItemIndex >= 0 then
        begin
          FMethod := TTsMethod(ItemIndex);
        end
        else
        begin
          FMethod := tsUndefined;
        end;
      end
      else if (Tag = 'SFAC') then
      begin
        if TryFortranStrToFloat(FSplitter[1], FSFAC.Value) then
        begin
          FSFAC.Used := True;
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized Time Series Array attribue in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end
  finally
    ValidMethods.Free;
  end;
end;

{ TTasTime }

procedure TTasTime.Initialize;
begin
  FTime := 01E30;
  FValues := nil;
  inherited;

end;

procedure TTasTime.Read(Stream: TStreamReader; Unhandled: TStreamWriter;
  Dimensions: TDimensions);
var
  ALine: string;
  ErrorLine: string;
  StreamPosition: Int64;
  Double2DDReader: TDouble2DArrayReader;
begin
  Initialize;
  Dimensions.NLay := 1;
  while not Stream.EndOfStream do
  begin
    Assert(FValues = nil);

    Double2DDReader := TDouble2DArrayReader.Create(Dimensions, FPackageType);
    try
      Double2DDReader.Read(Stream, Unhandled);
      FValues := Double2DDReader.FData;
      FConstant := Double2DDReader.Constant;
    finally
      Double2DDReader.Free;
    end;

    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    if ALine = '' then
    begin
      Continue;
    end;
    if ReadEndOfSection(ALine, ErrorLine, 'TIME', Unhandled) then
    begin
      Exit
    end
    else
    begin
      Assert(False);
    end;

  end
end;

{ TTimeArraySeries }

constructor TTimeArraySeries.Create(PackageType: string);
begin
  inherited;
  FAttributes := TTasAttributes.Create(PackageType);
  FTimes := TTasTimeList.Create;

end;

destructor TTimeArraySeries.Destroy;
begin
  FAttributes.Free;
  FTimes.Free;
  inherited;
end;

procedure TTimeArraySeries.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter; const NPER: Integer);
var
  ALine: string;
  ErrorLine: string;
  ATime: Extended;
  TasTime: TTasTime;
  FirstTime: Boolean;
begin
  FirstTime := True;
  if Assigned(OnUpdataStatusBar) then
  begin
    OnUpdataStatusBar(self, 'reading time-array-file');
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
      if FSplitter[1] ='ATTRIBUTES' then
      begin
        FAttributes.Read(Stream, Unhandled);
      end
      else if FSplitter[1] ='TIME' then
      begin
        if (FSplitter.Count >= 3) and TryFortranStrToFloat(FSplitter[2], ATime) then
        begin
          TasTime := TTasTime.Create(FPackageType);
          TasTime.FTime := ATime;
          FTimes.Add(TasTime);
          TasTime.Read(Stream, Unhandled, FDimensions);
          if FirstTime then
          begin
            FConstant := TasTime.Constant;
            FirstTime := False;
          end
          else
          begin
            FConstant := FConstant and TasTime.Constant;
          end;
        end
        else
        begin
          Unhandled.WriteLine('Unrecognized Time Array series data in the following line.');
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine('Unrecognized Time Array series data in the following line.');
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end;
end;

end.
