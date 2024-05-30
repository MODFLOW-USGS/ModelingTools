unit Mf6.CustomMf6PersistentUnit;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  System.Generics.Collections, System.Generics.Defaults;

type
  TOnUpdataStatusBar = procedure(AnObject: TObject; Text: string) of object;

   TRealOption = record
     Value: Extended;
     Used: Boolean;
     procedure Initialize;
   end;

   TIntegerOption = record
     Value: Integer;
     Used: Boolean;
     procedure Initialize;
   end;

   TStringOption = record
     Value: string;
     Used: Boolean;
     procedure Initialize;
   end;

   TBooleanOption= record
     Value: Boolean;
     Used: Boolean;
     procedure Initialize;
   end;


  TPrintFormat = record
    Columns: Integer;
    Width: Integer;
    Digits: Integer;
    Format: string;
    Used: Boolean;
    procedure Initialize;
  end;

  TMfCellId = record
    Layer: Integer;
    Row: Integer;
    Column: Integer;
    procedure Initialize;
    function SameLocation(OtherCellID: TMfCellId): Boolean;
  end;

  TCellIdList = class(TList<TMfCellId>)
    procedure Sort;
  end;

  TValueType = (vtNumeric, vtString);

  TTimeVariableCell = record
    ValueType: TValueType;
    CellId: TMfCellId;
    VariableName: string;
    StringValue: string;
    NumericValue: Extended;
    procedure Initialize;
  end;

  TTimeVariableCellList = TList<TTimeVariableCell>;

  TMf6BoundaryValue = record
    ValueType: TValueType;
    StringValue: string;
    NumericValue: Extended;
    AuxName: string;
    procedure Initialize;
  end;

  TBoundaryValueList = TList<TMf6BoundaryValue>;
  TMf6BoundaryValueArray = TArray<TMf6BoundaryValue>;
  TMf6BoundaryValueArrays = TArray<TMf6BoundaryValueArray>;

  TDoubleList = TList<Double>;


  TCustomMf6Persistent = class(TPersistent)
  private
    FOnUpdataStatusBar: TOnUpdataStatusBar;
  protected
    FSplitter: TStringList;
    FPackageType: string;
    FOriginalStream: TStreamReader;
    procedure Initialize; virtual;
    function StripFollowingComments(AValue: string): string;
    function ReadEndOfSection(ALine: string; const ErrorLine: string;
      const SectionName: string; Unhandled: TStreamWriter): Boolean;
    procedure ReadPrintFormat(ErrorLine: string; Unhandled: TStreamWriter;
      PackageName: string; var PrintFormat: TPrintFormat);
    function ReadCellID(var Cell: TMfCellId;
      StartIndex, DimensionCount: Integer): Boolean;
    function SwitchToAnotherFile(var Stream: TStreamReader; ErrorLine: string;
      Unhandled: TStreamWriter; var ALine: string; Block: string): Boolean;
    procedure RestoreStream(var Stream: TStreamReader);
    procedure SetOnUpdataStatusBar(const Value: TOnUpdataStatusBar); virtual;
  public
    constructor Create(PackageType: string); virtual;
    destructor Destroy; override;
    property OnUpdataStatusBar: TOnUpdataStatusBar read FOnUpdataStatusBar
      write SetOnUpdataStatusBar;
  end;

  TDataType = (dtReal, dtInteger);
  TDimensions = record
    NLay: Integer;
    NRow: Integer;
    NCol: Integer;
    DisVNRow: Integer;
    procedure Initialize;
    function DimensionCount: Integer;
  end;

  TPackageReader = class(TCustomMf6Persistent)
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter; const NPER: Integer); virtual; abstract;
  end;

  TDimensionedPackageReader = class(TPackageReader)
  private
    procedure SetDimensions(const Value: TDimensions);
  protected
    FDimensions: TDimensions;
  public
    property Dimensions: TDimensions read FDimensions write SetDimensions;
  end;

  TArrayType = (atConstant, atInternal, atExternal, atUndefined);

  TCustomArrayReader<DataType: record> = class(TCustomMf6Persistent)
  private
  const
    StrErrorReadingArray = 'Error reading array control line in the following ' +
    'line.';
  var
    ArrayType: TArrayType;
    FFactor: DataType;
    IPRN: Integer;
    FExternalFileName: string;
    FBinary: Boolean;
    FConstantValue: DataType;
    function StrToDataType(AValue: string; var DataValue: DataType): Boolean; virtual; abstract;
    procedure ReadControlLine(Stream: TStreamReader; Unhandled: TStreamWriter);
  protected
    procedure Initialize; override;
  end;

  T1DArrayReader<DataType: record> = class(TCustomArrayReader<DataType>)
  private
    FDimension: Integer;
  public
    FData: TArray<DataType>;
    constructor Create(Dimension: Integer; PackageType: string); reintroduce;
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TDArray1D = TArray<double>;
  TDArray2D = TArray<TDArray1D>;
  TDArray3D = TArray<TDArray2D>;

  TIArray1D = TArray<longint>;
  TIArray2D = TArray<TIArray1D>;
  TIArray3D = TArray<TIArray2D>;

  TBArray1D = TArray<Boolean>;
  TBArray2D = TArray<TBArray1D>;

  TArrayItem = record
    value: TDArray2D;
    TimeArraySeries: string;
    procedure Initialize;
    procedure Assign(Source: TArrayItem);
  end;

  TArrayItemList = TList<TArrayItem>;

  TDouble1DArrayReader = class(T1DArrayReader<Double>)
  private
    procedure ReadDataFromTextFile(Stream: TStreamReader);
    function StrToDataType(AValue: string; var DataValue: Double): Boolean; override;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TInteger1DArrayReader = class(T1DArrayReader<Longint >)
  private
    procedure ReadDataFromTextFile(Stream: TStreamReader);
    function StrToDataType(AValue: string; var DataValue: Longint): Boolean; override;
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  T2DArrayReader<DataType: record> = class(TCustomArrayReader<DataType>)
  private
    FDimensions: TDimensions;
  public
    FData: TArray<TArray<DataType>>;
    constructor Create(Dimensions: TDimensions; PackageType: string); reintroduce;
  end;

  TInteger2DArrayReader = class(T2DArrayReader<Longint>)
    function StrToDataType(AValue: string; var DataValue: Longint): Boolean; override;
    procedure Read2DArrayFromTextFile(Stream: TStreamReader);
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TDouble2DArrayReader = class(T2DArrayReader<Double>)
  private
    function StrToDataType(AValue: string; var DataValue: Double): Boolean; override;
    procedure Read2DArrayFromTextFile(Stream: TStreamReader);
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  T3DArrayReader<DataType: record> = class(TCustomArrayReader<DataType>)
  private
    FDimensions: TDimensions;
    FLayered: Boolean;
  public
    FData: TArray<TArray<TArray<DataType>>>;
    constructor Create(Dimensions: TDimensions;
      Layered: Boolean; PackageType: string); reintroduce;
  end;

  TDouble3DArrayReader = class(T3DArrayReader<Double>)
  private
    function StrToDataType(AValue: string; var DataValue: Double): Boolean; override;
    procedure Read2DArrayFromTextFile(Stream: TStreamReader; LayerIndex: Integer);
    procedure Read3DArrayFromTextFile(Stream: TStreamReader);
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TInteger3DArrayReader = class(T3DArrayReader<Longint >)
  private
    function StrToDataType(AValue: string; var DataValue: Longint ): Boolean; override;
    procedure Read2DArrayFromTextFile(Stream: TStreamReader; LayerIndex: Longint );
    procedure Read3DArrayFromTextFile(Stream: TStreamReader);
  public
    procedure Read(Stream: TStreamReader; Unhandled: TStreamWriter);
  end;

  TCellIdType = (citCell, citName);

  TNamedCellId = record
    IdType: TCellIdType;
    Layer: Integer;
    Row: Integer;
    Column: Integer;
    Name: string;
    procedure Initialize;
  end;

  TNumberedItem = record
    IdNumber: Integer;
    Name: string;
    AuxName: string;
    IntValue: Integer;
    FloatValue: Extended;
    StringValue: string;
    FloatValues: array of Extended;
    procedure Initialize;
  end;

  TNumberedItemList = class(TList<TNumberedItem>)
    procedure Sort;
  end;

  TNumberedItemLists = class(TObjectList<TNumberedItemList>)
    function HasData: boolean;
  end;

  TExtendedList = TList<Extended>;

  TPackage = class(TObject)
  private
    FFileType: string;
    FFileName: string;
    FPackageName: string;
    FPackage: TPackageReader;
  public
    destructor Destroy; override;
    property FileType: string read FFileType write FFileType;
    property FileName: string read FFileName write FFileName;
    property PackageName: string read FPackageName write FPackageName;
    property Package: TPackageReader read FPackage write FPackage;
    procedure ReadPackage(Unhandled: TStreamWriter; const NPER: Integer);
  end;

  TPackageList = TObjectList<TPackage>;

resourcestring
  StrUnrecognizedOpti = 'Unrecognized %S option in the following line.';
  StrUnrecognizedSPACK = 'Unrecognized %s PACKAGEDATA in the following line';
  StrUnrecognizedSData = 'Unrecognized %s data in the following line.';
  StrUnrecognizedSPERI = 'Unrecognized %s PERIOD data in the following line.';
  StrUnrecognizedSCONN = 'Unrecognized %s CONNECTIONDATA in the following li' +
  'ne';
  StrUnrecognizedSGRID = 'Unrecognized %s GRIDDATA in the following line';
  StrUnrecognizedSSect = 'Unrecognized %s section on the following Line';
  StrUnrecognizedSSOUR = 'Unrecognized %s SOURCES data in the following line' +
  '.';
  StrUnrecognizedSFILE = 'Unrecognized %s FILEINPUT data in the following li' +
  'ne.';

implementation

uses
  ModelMuseUtilities, ReadModflowArrayUnit;

resourcestring
  StrUnrecognizedTDISOp = 'Unrecognized TDIS option in the following line.';

constructor TCustomMf6Persistent.Create(PackageType: string);
begin
  FPackageType := PackageType;
  FSplitter := TStringList.Create;
  Initialize;
end;

procedure TCustomMf6Persistent.SetOnUpdataStatusBar(
  const Value: TOnUpdataStatusBar);
begin
  FOnUpdataStatusBar := Value;
end;

function TCustomMf6Persistent.StripFollowingComments(AValue: string): string;
var
  CommentPosition: Integer;
  SingleQuotePostion: Integer;
  CharIndex: Integer;
  QuoteCount: Integer;
begin
  SingleQuotePostion := Pos('''', AValue);
  if SingleQuotePostion = 0 then
  begin
    CommentPosition := Pos('#', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Copy(AValue, 1, CommentPosition - 1);
    end;
    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Copy(AValue, 1, CommentPosition - 1);
    end;
    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      AValue := Copy(AValue, 1, CommentPosition - 1);
    end;
  end
  else
  begin
    CommentPosition := Pos('#', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if AValue[CharIndex] = '#' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex - 1);
            Break;
          end;
        end;
      end;
    end;
    CommentPosition := Pos('!', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if AValue[CharIndex] = '!' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex - 1);
            Break;
          end;
        end;
      end;
    end;
    CommentPosition := Pos('//', AValue);
    if CommentPosition > 0 then
    begin
      QuoteCount := 0;
      for CharIndex := 1 to Length(AValue) do
      begin
        if AValue[CharIndex] = '''' then
        begin
          Inc(QuoteCount);
        end;
        if Copy(AValue, CharIndex, 2) = '//' then
        begin
          if not Odd(QuoteCount) then
          begin
            AValue := Copy(AValue, 1, CharIndex - 1);
            Break;
          end;
        end;
      end;
    end;
  end;
  result := Trim(AValue);
end;

destructor TCustomMf6Persistent.Destroy;
begin
  FSplitter.Free;
  inherited;
end;

procedure TCustomMf6Persistent.Initialize;
begin
  FSplitter.QuoteChar := '''';
  FSplitter.Delimiter := ',';
  FOriginalStream := nil;
end;

function TCustomMf6Persistent.ReadEndOfSection(ALine: string;
  const ErrorLine: string; const SectionName: string;
  Unhandled: TStreamWriter): boolean;
begin
  result := False;
  FSplitter.DelimitedText := StripFollowingComments(UpperCase(ALine));
  if FSplitter.Count > 0 then
  begin
    if FSplitter[0] = 'END' then
    begin
      if FSplitter.Count > 1 then
      begin
        if FSplitter[1] = SectionName then
        begin
          result := True;
        end
        else
        begin
          Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
        Unhandled.WriteLine(ErrorLine);
      end;
    end;
  end
  else
  begin
    Unhandled.WriteLine(Format('Error reading the following %s line.', [SectionName]));
    Unhandled.WriteLine(ErrorLine);
  end;
end;

{ TArrayReader }

constructor T3DArrayReader<DataType>.Create(Dimensions: TDimensions;
  Layered: Boolean; PackageType: string);
begin
  inherited Create(PackageType);
  FLayered := Layered;
  FDimensions := Dimensions;
  if FDimensions.NRow = 0 then
  begin
    FDimensions.NRow := 1;
  end;
  if FDimensions.NLay = 0 then
  begin
    FDimensions.NLay := 1;
  end;
  SetLength(FData, FDimensions.NLay, FDimensions.NRow, FDimensions.NCol);
end;


{ T1DArrayReader }

constructor T1DArrayReader<DataType>.Create(Dimension: Integer; PackageType: string);
begin
  inherited Create(PackageType);
  FDimension := Dimension;
  SetLength(FData, Dimension);
end;

procedure T1DArrayReader<DataType>.Read(Stream: TStreamReader; Unhandled: TStreamWriter);
begin
  ReadControlLine(Stream, Unhandled);
end;

{ TCustomArrayReader }

procedure TCustomArrayReader<DataType>.Initialize;
begin
  inherited;
  ArrayType := atUndefined;
  StrToDataType('1', FFactor);
  IPRN := -1;
  FExternalFileName := '';
  FBinary := False;
  StrToDataType('0', FConstantValue);
end;

procedure TCustomArrayReader<DataType>.ReadControlLine(Stream: TStreamReader; Unhandled: TStreamWriter);
var
  ALine: string;
  ErrorLine: string;
  Option: string;
  OptionIndex: Integer;
  Factor: string;
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

    if FSplitter[0] = 'CONSTANT' then
    begin
      ArrayType := atConstant;
    end
    else if FSplitter[0] = 'INTERNAL' then
    begin
      ArrayType := atInternal;
    end
    else if FSplitter[0] = 'OPEN/CLOSE' then
    begin
      ArrayType := atExternal;
    end
    else
    begin
      Unhandled.WriteLine(StrErrorReadingArray);
      Unhandled.WriteLine(ErrorLine);
      Exit;
    end;

    case ArrayType of
      atConstant:
        begin
          Assert(StrToDataType(FSplitter[1], FConstantValue));
        end;
      atInternal:
        begin
          if FSplitter.Count >= 3 then
          begin
            Option := FSplitter[1];
            if Option = 'FACTOR' then
            begin
              Assert(StrToDataType(FSplitter[2], FFactor));
            end
            else if Option = 'IPRN' then
            begin
              if not TryStrToInt(FSplitter[2], IPRN) then
              begin
                Unhandled.WriteLine('Error reading IPRN in an array control line in the following line.');
                Unhandled.WriteLine(ErrorLine);
              end;
            end
            else
            begin
              Exit;
            end;
            if FSplitter.Count >= 5 then
            begin
              Option := FSplitter[3];
              if Option = 'FACTOR' then
              begin
                Assert(StrToDataType(FSplitter[4], FFactor));
              end
              else if Option = 'IPRN' then
              begin
                if not TryStrToInt(FSplitter[4], IPRN) then
                begin
                  Unhandled.WriteLine('Error reading IPRN in an array control line in the following line.');
                  Unhandled.WriteLine(ErrorLine);
                end;
              end
              else
              begin
                Exit;
              end;
            end;
          end;
        end;
      atExternal:
        begin
          FExternalFileName := FSplitter[1];
          OptionIndex := 2;
          while OptionIndex < FSplitter.Count do
          begin
            Option := FSplitter[OptionIndex];
            if Option = 'FACTOR' then
            begin
              Inc(OptionIndex);
              Assert(StrToDataType(FSplitter[OptionIndex], FFactor));
              Inc(OptionIndex);
            end
            else if Option = 'IPRN' then
            begin
              Inc(OptionIndex);
              if not TryStrToInt(FSplitter[OptionIndex], IPRN) then
              begin
                Unhandled.WriteLine('Error reading IPRN in an array control line in the following line.');
                Unhandled.WriteLine(ErrorLine);
              end;
              Inc(OptionIndex);
            end
            else if Option = '(BINARY)' then
            begin
              FBinary := True;
              Inc(OptionIndex);
            end
            else
            begin
              Exit;
            end;
          end;
        end;
    end;

    Exit;
  end;

end;

procedure TDouble1DArrayReader.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  Index: Integer;
  ExternalFileStream: TStreamReader;
begin
  inherited;
  case ArrayType of
    atConstant:
      begin
        for Index := 0 to FDimension - 1 do
        begin
          FData[Index] := FConstantValue
        end;
      end;
    atInternal:
      begin
        ReadDataFromTextFile(Stream);
      end;
    atExternal:
      begin
        if FBinary then
        begin
          Assert(False);
        end
        else
        begin
          if TFile.Exists(FExternalFileName) then
          begin
            try
              ExternalFileStream := TFile.OpenText(FExternalFileName);
              try
                ReadDataFromTextFile(ExternalFileStream)
              finally
                ExternalFileStream.Free;
              end;
            except
              on E: EEncodingError do
              begin
                raise;
              end;
              on E: Exception do
              begin
                Unhandled.WriteLine('ERROR');
                Unhandled.WriteLine(E.Message);
              end;
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
              [FExternalFileName]));
          end;
        end;
      end;
    else
      begin
        Unhandled.WriteLine('Error reading array control line.');
      end;
  end;

end;

procedure TDouble1DArrayReader.ReadDataFromTextFile(Stream: TStreamReader);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  Index: Integer;
  DataValue: Double;
begin
  Index := 0;
  while Index < FDimension do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[Index] := DataValue * FFactor;
        Inc(Index);
      end;
    end;
  end;
end;

function TDouble1DArrayReader.StrToDataType(AValue: string; var DataValue: Double): Boolean;
var
  TestValue: Extended;
begin
  result := TryFortranStrToFloat(AValue, TestValue);
  DataValue := TestValue;
end;


{ TDouble3DArrayReader }

procedure TDouble3DArrayReader.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ExternalFileStream: TStreamReader;
  ExternalBinaryFileStream: TFileStream;
//  KSTP: Integer;
//  KPER: Integer;
//  PERTIM: TModflowDouble;
//  TOTIM: TModflowDouble;
//  DESC: TModflowDesc;
//  NCOL: Integer;
//  NROW: Integer;
//  ILAY: Integer;
//  AnArray: TModflowDoubleArray;
begin
  inherited;
  if FLayered then
  begin
    for LayerIndex := 0 to FDimensions.NLay - 1 do
    begin
      ReadControlLine(Stream, Unhandled);
      case ArrayType of
        atConstant:
          begin
            for RowIndex := 0 to FDimensions.NRow - 1 do
            begin
              for ColIndex := 0 to FDimensions.NCol - 1 do
              begin
                FData[LayerIndex,RowIndex, ColIndex] := FConstantValue;
              end;
            end;
          end;
        atInternal:
          begin
            Read2DArrayFromTextFile(Stream, LayerIndex);
          end;
        atExternal:
          begin
          if FBinary then
          begin
            if TFile.Exists(FExternalFileName) then
            begin
              try
                ExternalBinaryFileStream := TFile.Create(FExternalFileName,
                  fmOpenRead or fmShareDenyWrite);
                try
//                  ReadDoublePrecisionModflowBinaryRealArray(ExternalBinaryFileStream,
//                  KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, True);
                  for RowIndex := 0 to FDimensions.NRow - 1 do
                  begin
                    for ColIndex := 0 to FDimensions.NCol - 1 do
                    begin
                      ExternalBinaryFileStream.Read(FData[LayerIndex,RowIndex, ColIndex], SizeOf(Double));
//                      FData[LayerIndex,RowIndex, ColIndex] :=
//                        AnArray[RowIndex, ColIndex] * FFactor;
                    end;
                  end;
                finally
                  ExternalBinaryFileStream.Free;
                end;
              except
                on E: EEncodingError do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  Unhandled.WriteLine('ERROR');
                  Unhandled.WriteLine(E.Message);
                end;
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
                [FExternalFileName]));
            end;
          end
          else
          begin
            if TFile.Exists(FExternalFileName) then
            begin
              try
                ExternalFileStream := TFile.OpenText(FExternalFileName);
                try
                  Read2DArrayFromTextFile(ExternalFileStream, LayerIndex);
                finally
                  ExternalFileStream.Free;
                end;
              except
                on E: EEncodingError do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  Unhandled.WriteLine('ERROR');
                  Unhandled.WriteLine(E.Message);
                end;
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
                [FExternalFileName]));
            end;
          end;
      end;
        else
          begin
            Assert(False);
          end;
      end;
    end;
  end
  else
  begin
    ReadControlLine(Stream, Unhandled);
    case ArrayType of
      atConstant:
        begin
          for LayerIndex := 0 to FDimensions.NLay - 1 do
          begin
            for RowIndex := 0 to FDimensions.NRow - 1 do
            begin
              for ColIndex := 0 to FDimensions.NCol - 1 do
              begin
                FData[LayerIndex,RowIndex, ColIndex] := FConstantValue;
              end;
            end;
          end;
        end;
      atInternal:
        begin
          Read3DArrayFromTextFile(Stream);
        end;
      atExternal:
        begin
          if FBinary then
          begin
            if TFile.Exists(FExternalFileName) then
            begin
              try
                ExternalBinaryFileStream := TFile.Create(FExternalFileName,
                  fmOpenRead or fmShareDenyWrite);
                try
                  for LayerIndex := 0 to FDimensions.NLay - 1 do
                  begin
//                    ReadDoublePrecisionModflowBinaryRealArray(ExternalBinaryFileStream,
//                    KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, True);
                    for RowIndex := 0 to FDimensions.NRow - 1 do
                    begin
                      for ColIndex := 0 to FDimensions.NCol - 1 do
                      begin
                        ExternalBinaryFileStream.Read(FData[LayerIndex,RowIndex, ColIndex], SizeOf(Double));
//                        FData[LayerIndex,RowIndex, ColIndex] :=
//                          AnArray[RowIndex, ColIndex] * FFactor;
                      end;
                    end;
                  end;
                finally
                  ExternalBinaryFileStream.Free;
                end;
              except
                on E: EEncodingError do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  Unhandled.WriteLine('ERROR');
                  Unhandled.WriteLine(E.Message);
                end;
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
                [FExternalFileName]));
            end;
          end
          else
          begin
            if TFile.Exists(FExternalFileName) then
            begin
              try
                ExternalFileStream := TFile.OpenText(FExternalFileName);
                try
                  Read3DArrayFromTextFile(ExternalFileStream);
                finally
                  ExternalFileStream.Free;
                end;
              except
                on E: EEncodingError do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  Unhandled.WriteLine('ERROR');
                  Unhandled.WriteLine(E.Message);
                end;
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
                [FExternalFileName]));
            end;
          end;
        end;
      else
        begin
          Unhandled.WriteLine('Error reading array control line.');
        end;
    end;
  end;

end;

procedure TDouble3DArrayReader.Read2DArrayFromTextFile(Stream: TStreamReader; LayerIndex: Integer);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Double;
begin
  RowIndex := 0;
  ColIndex := 0;
  while (RowIndex < FDimensions.NRow) and (ColIndex < FDimensions.NCol) do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[LayerIndex, RowIndex, ColIndex] := DataValue * FFactor;
        Inc(ColIndex);
        if ColIndex = FDimensions.NCol then
        begin
          ColIndex := 0;
          Inc(RowIndex);
        end;
      end;
    end;
  end;
end;

procedure TDouble3DArrayReader.Read3DArrayFromTextFile(Stream: TStreamReader);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Double;
begin
  LayerIndex := 0;
  RowIndex := 0;
  ColIndex := 0;
  while (LayerIndex < FDimensions.NLay) and (RowIndex < FDimensions.NRow)
    and (ColIndex < FDimensions.NCol) do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[LayerIndex, RowIndex, ColIndex] :=
          DataValue * FFactor;
        Inc(ColIndex);
        if ColIndex = FDimensions.NCol then
        begin
          ColIndex := 0;
          Inc(RowIndex);
          if RowIndex = FDimensions.NRow then
          begin
            RowIndex := 0;
            Inc(LayerIndex);
          end;
        end;
      end;
    end;
  end;
end;

function TDouble3DArrayReader.StrToDataType(AValue: string; var DataValue: Double): Boolean;
var
  TestValue: Extended;
begin
  result := TryFortranStrToFloat(AValue, TestValue);
  DataValue := TestValue;
end;

{ TCellId }

procedure TMfCellId.Initialize;
begin
  Layer := 0;
  Row := 0;
  Column := 0;
end;

function TMfCellId.SameLocation(OtherCellID: TMfCellId): Boolean;
begin
  result := (Layer = OtherCellID.Layer)
    and (Row = OtherCellID.Row)
    and (Column = OtherCellID.Column)
end;

{ TDimensions }

function TDimensions.DimensionCount: Integer;
begin
  result := 0;
  if NLay >= 1 then
  begin
    Inc(Result);
  end;
  if DisVNRow < 0 then
  begin
    if NRow >= 1 then
    begin
      Inc(Result);
    end;
  end
  else
  begin
    if DisVNRow >= 1 then
    begin
      Inc(Result);
    end;
  end;
  if NCol >= 1 then
  begin
    Inc(Result);
  end;
  Assert(Result in [1..3]);
end;

procedure TDimensions.Initialize;
begin
  NLay := 0;
  NRow := 0;
  NCol := 0;
  DisVNRow := -1;
end;

{ T2DArrayReader<DataType> }

constructor T2DArrayReader<DataType>.Create(Dimensions: TDimensions; PackageType: string);
begin
  inherited Create(PackageType);
  FDimensions := Dimensions;
  if FDimensions.NRow = 0 then
  begin
    FDimensions.NRow := 1;
  end;
  SetLength(FData, FDimensions.NRow, FDimensions.NCol);
end;

{ TDouble2DArrayReader }

procedure TDouble2DArrayReader.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  RowIndex: Integer;
  ColIndex: Integer;
  ExternalFileStream: TStreamReader;
  ExternalBinaryFileStream: TFileStream;
begin
  ReadControlLine(Stream, Unhandled);
  case ArrayType of
    atConstant:
      begin
        for RowIndex := 0 to FDimensions.NRow - 1 do
        begin
          for ColIndex := 0 to FDimensions.NCol - 1 do
          begin
            FData[RowIndex, ColIndex] := FConstantValue;
          end;
        end;
      end;
    atInternal:
      begin
        Read2DArrayFromTextFile(Stream);
      end;
    atExternal:
      begin
        if FBinary then
        begin
          if TFile.Exists(FExternalFileName) then
          begin
            try
              ExternalBinaryFileStream := TFile.Create(FExternalFileName,
                fmOpenRead or fmShareDenyWrite);
              try
//                ReadDoublePrecisionModflowBinaryRealArray(ExternalBinaryFileStream,
//                KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, True);
                for RowIndex := 0 to FDimensions.NRow - 1 do
                begin
                  for ColIndex := 0 to FDimensions.NCol - 1 do
                  begin
                    ExternalBinaryFileStream.Read(FData[RowIndex, ColIndex], SizeOf(Double));
//                    FData[RowIndex, ColIndex] :=
//                      AnArray[RowIndex, ColIndex] * FFactor;
                  end;
                end;
              finally
                ExternalBinaryFileStream.Free;
              end;
            except
              on E: EEncodingError do
              begin
                raise;
              end;
              on E: Exception do
              begin
                Unhandled.WriteLine('ERROR');
                Unhandled.WriteLine(E.Message);
              end;
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
              [FExternalFileName]));
          end;
        end
        else
        begin
          if TFile.Exists(FExternalFileName) then
          begin
            try
              ExternalFileStream := TFile.OpenText(FExternalFileName);
              try
                Read2DArrayFromTextFile(ExternalFileStream);
              finally
                ExternalFileStream.Free;
              end;
            except
              on E: EEncodingError do
              begin
                raise;
              end;
              on E: Exception do
              begin
                Unhandled.WriteLine('ERROR');
                Unhandled.WriteLine(E.Message);
              end;
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
              [FExternalFileName]));
          end;
        end;
      end;
    else
      begin
        Unhandled.WriteLine('Error reading array control line.');
      end;
  end;
end;

procedure TDouble2DArrayReader.Read2DArrayFromTextFile(Stream: TStreamReader);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Double;
begin
  RowIndex := 0;
  ColIndex := 0;
  while (RowIndex < FDimensions.NRow) and (ColIndex < FDimensions.NCol) do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[RowIndex, ColIndex] := DataValue * FFactor;
        Inc(ColIndex);
        if ColIndex = FDimensions.NCol then
        begin
          ColIndex := 0;
          Inc(RowIndex);
        end;
      end;
    end;
  end;
end;

function TDouble2DArrayReader.StrToDataType(AValue: string; var DataValue: Double): Boolean;
var
  TestValue: Extended;
begin
  result := TryFortranStrToFloat(AValue, TestValue);
  DataValue := TestValue;
end;

{ TInteger3DArrayReader }

procedure TInteger3DArrayReader.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ExternalFileStream: TStreamReader;
begin
  inherited;
  if FLayered then
  begin
    for LayerIndex := 0 to FDimensions.NLay - 1 do
    begin
      ReadControlLine(Stream, Unhandled);
      case ArrayType of
        atConstant:
          begin
            for RowIndex := 0 to FDimensions.NRow - 1 do
            begin
              for ColIndex := 0 to FDimensions.NCol - 1 do
              begin
                FData[LayerIndex,RowIndex, ColIndex] := FConstantValue;
              end;
            end;
          end;
        atInternal:
          begin
            Read2DArrayFromTextFile(Stream, LayerIndex);
          end;
        atExternal:
          begin
          if FBinary then
          begin
            Assert(False);
          end
          else
          begin
            if TFile.Exists(FExternalFileName) then
            begin
              try
                ExternalFileStream := TFile.OpenText(FExternalFileName);
                try
                  Read2DArrayFromTextFile(ExternalFileStream, LayerIndex);
                finally
                  ExternalFileStream.Free;
                end;
              except
                on E: EEncodingError do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  Unhandled.WriteLine('ERROR');
                  Unhandled.WriteLine(E.Message);
                end;
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
                [FExternalFileName]));
            end;
          end;
      end;
        else
          begin
            Assert(False);
          end;
      end;
    end;
  end
  else
  begin
    ReadControlLine(Stream, Unhandled);
    case ArrayType of
      atConstant:
        begin
          for LayerIndex := 0 to FDimensions.NLay - 1 do
          begin
            for RowIndex := 0 to FDimensions.NRow - 1 do
            begin
              for ColIndex := 0 to FDimensions.NCol - 1 do
              begin
                FData[LayerIndex,RowIndex, ColIndex] := FConstantValue;
              end;
            end;
          end;
        end;
      atInternal:
        begin
          Read3DArrayFromTextFile(Stream);
        end;
      atExternal:
        begin
          if FBinary then
          begin
            Assert(False);
//            if TFile.Exists(FExternalFileName) then
//            begin
//              try
//                ExternalBinaryFileStream := TFile.Create(FExternalFileName,
//                  fmOpenRead or fmShareDenyWrite);
//                try
//                  for LayerIndex := 0 to FDimensions.NLay - 1 do
//                  begin
//                    ReadDoublePrecisionModflowBinaryRealArray(ExternalBinaryFileStream,
//                    KSTP, KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray, True);
//                    for RowIndex := 0 to FDimensions.NRow - 1 do
//                    begin
//                      for ColIndex := 0 to FDimensions.NCol - 1 do
//                      begin
//                        FData[LayerIndex,RowIndex, ColIndex] :=
//                          AnArray[RowIndex, ColIndex] * FFactor;
//                      end;
//                    end;
//                  end;
//                finally
//                  ExternalBinaryFileStream.Free;
//                end;
//              except on E: Exception do
//                begin
//                  Unhandled.WriteLine('ERROR');
//                  Unhandled.WriteLine(E.Message);
//                end;
//              end;
//            end
//            else
//            begin
//              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
//                [FExternalFileName]));
//            end;
          end
          else
          begin
            if TFile.Exists(FExternalFileName) then
            begin
              try
                ExternalFileStream := TFile.OpenText(FExternalFileName);
                try
                  Read3DArrayFromTextFile(ExternalFileStream);
                finally
                  ExternalFileStream.Free;
                end;
              except
                on E: EEncodingError do
                begin
                  raise;
                end;
                on E: Exception do
                begin
                  Unhandled.WriteLine('ERROR');
                  Unhandled.WriteLine(E.Message);
                end;
              end;
            end
            else
            begin
              Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
                [FExternalFileName]));
            end;
          end;
        end;
      else
        begin
          Unhandled.WriteLine('Error reading array control line.');
        end;
    end;
  end;

end;

procedure TInteger3DArrayReader.Read2DArrayFromTextFile(Stream: TStreamReader;
  LayerIndex: Longint );
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Integer;
begin
  RowIndex := 0;
  ColIndex := 0;
  while (RowIndex < FDimensions.NRow) and (ColIndex < FDimensions.NCol) do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[LayerIndex, RowIndex, ColIndex] := DataValue * FFactor;
        Inc(ColIndex);
        if ColIndex = FDimensions.NCol then
        begin
          ColIndex := 0;
          Inc(RowIndex);
        end;
      end;
    end;
  end;
end;

procedure TInteger3DArrayReader.Read3DArrayFromTextFile(Stream: TStreamReader);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Integer;
begin
  LayerIndex := 0;
  RowIndex := 0;
  ColIndex := 0;
  while (LayerIndex < FDimensions.NLay) and (RowIndex < FDimensions.NRow)
    and (ColIndex < FDimensions.NCol) do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[LayerIndex, RowIndex, ColIndex] :=
          DataValue * FFactor;
        Inc(ColIndex);
        if ColIndex = FDimensions.NCol then
        begin
          ColIndex := 0;
          Inc(RowIndex);
          if RowIndex = FDimensions.NRow then
          begin
            RowIndex := 0;
            Inc(LayerIndex);
          end;
        end;
      end;
    end;
  end;
end;

function TInteger3DArrayReader.StrToDataType(AValue: string; var DataValue: Longint ): Boolean;
begin
  result := TryStrToInt(AValue, DataValue);
end;

{ TInteger1DArrayReader }

procedure TInteger1DArrayReader.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  Index: Integer;
  ExternalFileStream: TStreamReader;
begin
  inherited;
  case ArrayType of
    atConstant:
      begin
        for Index := 0 to FDimension - 1 do
        begin
          FData[Index] := FConstantValue
        end;
      end;
    atInternal:
      begin
        ReadDataFromTextFile(Stream);
      end;
    atExternal:
      begin
        if FBinary then
        begin
          Assert(False);
        end
        else
        begin
          if TFile.Exists(FExternalFileName) then
          begin
            try
              ExternalFileStream := TFile.OpenText(FExternalFileName);
              try
                ReadDataFromTextFile(ExternalFileStream)
              finally
                ExternalFileStream.Free;
              end;
            except
              on E: EEncodingError do
              begin
                raise;
              end;
              on E: Exception do
              begin
                Unhandled.WriteLine('ERROR');
                Unhandled.WriteLine(E.Message);
              end;
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
              [FExternalFileName]));
          end;
        end;
      end;
    else
      begin
        Unhandled.WriteLine('Error reading array control line.');
      end;
  end;

end;

procedure TInteger1DArrayReader.ReadDataFromTextFile(Stream: TStreamReader);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  Index: Integer;
  DataValue: Integer;
begin
  Index := 0;
  while Index < FDimension do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[Index] := DataValue * FFactor;
      end;
      Inc(Index);
    end;
  end;
end;

function TInteger1DArrayReader.StrToDataType(AValue: string; var DataValue: Longint): Boolean;
begin
  result := TryStrToInt(AValue, DataValue);
end;

{ TPrintFormat }

procedure TPrintFormat.Initialize;
begin
  Used := False;
end;

procedure TCustomMf6Persistent.ReadPrintFormat(ErrorLine: string;
  Unhandled: TStreamWriter; PackageName: string; var PrintFormat: TPrintFormat);
begin
  if FSplitter.Count >= 8 then
  begin
    if (FSplitter[2] = 'COLUMNS') and (FSplitter[4] = 'WIDTH') and
      (FSplitter[6] = 'DIGITS') then
    begin
      PrintFormat.Used := True;
      PrintFormat.Columns := StrToInt(FSplitter[3]);
      PrintFormat.Width := StrToInt(FSplitter[5]);
      PrintFormat.Digits := StrToInt(FSplitter[7]);
      if FSplitter.Count >= 9 then
      begin
        PrintFormat.Format := FSplitter[8];
        if (PrintFormat.Format <> 'EXPONENTIAL') and
          (PrintFormat.Format <> 'FIXED') and (PrintFormat.Format <> 'GENERAL')
          and (PrintFormat.Format <> 'SCIENTIFIC') then
        begin
          Unhandled.WriteLine
            (Format('Unrecognized format option in %s option "PRINT_FORMAT" in the following line.',
            [PackageName]));
          Unhandled.WriteLine(ErrorLine);
        end;
      end
      else
      begin
        PrintFormat.Format := 'GENERAL';
      end;
    end
    else
    begin
      Unhandled.WriteLine(Format( StrUnrecognizedOpti, [PackageName]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end
  else
  begin
    Unhandled.WriteLine(Format( StrUnrecognizedOpti, [PackageName]));
    Unhandled.WriteLine(ErrorLine);
  end;
end;

{ TNamedCellId }

procedure TNamedCellId.Initialize;
begin
  IdType := citCell;
  Layer := 0;
  Row := 0;
  Column := 0;
  Name := '';
end;

function TCustomMf6Persistent.ReadCellID(var Cell: TMfCellId;
  StartIndex, DimensionCount: Integer): Boolean;
begin
  result := FSplitter.Count >= StartIndex + DimensionCount;
  if result then
  begin
    case DimensionCount of
      1:
        begin
          if not TryStrToInt(FSplitter[StartIndex], Cell.Column) then
          begin
            result := False;
          end;
        end;
      2:
        begin
          if not TryStrToInt(FSplitter[StartIndex], Cell.Layer) or
            not TryStrToInt(FSplitter[StartIndex + 1], Cell.Column) then
          begin
            result := False;
          end;
        end;
      3:
        begin
          if not TryStrToInt(FSplitter[StartIndex], Cell.Layer) or
            not TryStrToInt(FSplitter[StartIndex + 1], Cell.Row) or
            not TryStrToInt(FSplitter[StartIndex + 2], Cell.Column) then
          begin
            result := False;
          end;
        end;
    end
  end;
end;

{ TPackage }

destructor TPackage.Destroy;
begin
  FPackage.Free;
  inherited;
end;

procedure TPackage.ReadPackage(Unhandled: TStreamWriter; const NPER: Integer);
var
  PackageFile: TStreamReader;
begin
  if FFileName <> '' then
  begin
    if TFile.Exists(FFileName) then
    begin
      try
        PackageFile := TFile.OpenText(FFileName);
        try
          try
            FPackage.Read(PackageFile, Unhandled, NPER);
          except
            on E: EEncodingError do
            begin
              raise;
            end;
            on E: Exception do
            begin
              Unhandled.WriteLine('ERROR');
              Unhandled.WriteLine(E.Message);
            end;
          end;
        finally
          PackageFile.Free;
        end;
      except
        on E: EEncodingError do
        begin
          raise;
        end;
        on E: Exception do
        begin
          Unhandled.WriteLine('ERROR');
          Unhandled.WriteLine(E.Message);
        end;
      end
    end
    else
    begin
      Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
        [FFileName]));
    end;
  end;
end;

{ TTimeVariableCell }

procedure TTimeVariableCell.Initialize;
begin
  ValueType := vtNumeric;
  CellId.Initialize;
  VariableName := '';
  StringValue := '';
  NumericValue := 0;
end;

{ TRealOption }

procedure TRealOption.Initialize;
begin
  Value := 0;
  Used := False;
end;

{ TIntegerOption }

procedure TIntegerOption.Initialize;
begin
  Value := 0;
  Used := False;
end;

{ TStringOption }

procedure TStringOption.Initialize;
begin
  Value := '';
  Used := False;
end;

{ TBooleanOption }

procedure TBooleanOption.Initialize;
begin
  Value := False;
  Used := False;
end;

{ TBoundaryValue }

procedure TMf6BoundaryValue.Initialize;
begin
  ValueType := vtNumeric;
  StringValue := '';
  NumericValue := 0;
  AuxName := '';
end;

{ TArrayItem }

procedure TArrayItem.Assign(Source: TArrayItem);
begin
  Value := Source.value;
  TimeArraySeries := Source.TimeArraySeries;
  if Value <> nil then
  begin
    SetLength(Value, Length(Value), Length(Value[0]));
  end;
end;

procedure TArrayItem.Initialize;
begin
  value := nil;
  TimeArraySeries := '';
end;

{ TNumberedItem }

procedure TNumberedItem.Initialize;
begin
  IdNumber := 0;
  FloatValue := 0;
  IntValue := 0;
  StringValue := '';
  FloatValues := nil;
  Name := '';
  AuxName := '';
end;

function TCustomMf6Persistent.SwitchToAnotherFile(var Stream: TStreamReader;
  ErrorLine: string; Unhandled: TStreamWriter; var ALine: string;
  Block: string): Boolean;
var
  FileName: string;
  CaseSensitiveLine: string;
begin
  CaseSensitiveLine := ALine;
  ALine := UpperCase(ALine);
  FSplitter.DelimitedText := ALine;
  result := False;
  if (FSplitter.Count >= 2) and (FSplitter[0] = 'OPEN/CLOSE') then
  begin
    Assert(FOriginalStream = nil);
    FOriginalStream := Stream;
    FSplitter.DelimitedText := CaseSensitiveLine;
    FileName := FSplitter[1];
    if TFile.Exists(FileName) then
    begin
      Stream := TFile.OpenText(FileName);
      result := True;
    end
    else
    begin
      Unhandled.WriteLine(Format('Nonexistent file for %s in %s',
        [Block, FPackageType]));
      Unhandled.WriteLine(ErrorLine);
    end;
  end;
end;

procedure TCustomMf6Persistent.RestoreStream(var Stream: TStreamReader);
begin
  if Stream.EndOfStream and (FOriginalStream <> nil) then
  begin
    Stream.Free;
    Stream := FOriginalStream;
    FOriginalStream := nil;
  end;
end;

{ TNumberedItemList }

procedure TNumberedItemList.Sort;
begin
  inherited Sort(
    TComparer<TNumberedItem>.Construct(
      function(const Left, Right: TNumberedItem): Integer
      begin
        result := Left.IdNumber - Right.IdNumber;
        if result = 0 then
        begin
          Result := AnsiCompareText(Left.Name, Right.Name);
        end;
      end
    ));
end;

{ TNumberedItemLists }

function TNumberedItemLists.HasData: boolean;
begin
  result := False;
  for var Index := 0 to Count - 1 do
  begin
    result := Items[Index].Count > 0;
    if Result then
    begin
      Exit;
    end;
  end;
end;

{ TInteger2DArrayReader }

procedure TInteger2DArrayReader.Read(Stream: TStreamReader;
  Unhandled: TStreamWriter);
var
  RowIndex: Integer;
  ColIndex: Integer;
  ExternalFileStream: TStreamReader;
  ExternalBinaryFileStream: TFileStream;
begin
  ReadControlLine(Stream, Unhandled);
  case ArrayType of
    atConstant:
      begin
        for RowIndex := 0 to FDimensions.NRow - 1 do
        begin
          for ColIndex := 0 to FDimensions.NCol - 1 do
          begin
            FData[RowIndex, ColIndex] := FConstantValue;
          end;
        end;
      end;
    atInternal:
      begin
        Read2DArrayFromTextFile(Stream);
      end;
    atExternal:
      begin
        if FBinary then
        begin
          if TFile.Exists(FExternalFileName) then
          begin
            try
              ExternalBinaryFileStream := TFile.Create(FExternalFileName,
                fmOpenRead or fmShareDenyWrite);
              try
                for RowIndex := 0 to FDimensions.NRow - 1 do
                begin
                  for ColIndex := 0 to FDimensions.NCol - 1 do
                  begin
                    ExternalBinaryFileStream.Read(FData[RowIndex, ColIndex], SizeOf(Integer));
                  end;
                end;
              finally
                ExternalBinaryFileStream.Free;
              end;
            except
              on E: EEncodingError do
              begin
                raise;
              end;
              on E: Exception do
              begin
                Unhandled.WriteLine('ERROR');
                Unhandled.WriteLine(E.Message);
              end;
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
              [FExternalFileName]));
          end;
        end
        else
        begin
          if TFile.Exists(FExternalFileName) then
          begin
            try
              ExternalFileStream := TFile.OpenText(FExternalFileName);
              try
                Read2DArrayFromTextFile(ExternalFileStream);
              finally
                ExternalFileStream.Free;
              end;
            except
              on E: EEncodingError do
              begin
                raise;
              end;
              on E: Exception do
              begin
                Unhandled.WriteLine('ERROR');
                Unhandled.WriteLine(E.Message);
              end;
            end;
          end
          else
          begin
            Unhandled.WriteLine(Format('Unable to open %s because it does not exist.',
              [FExternalFileName]));
          end;
        end;
      end;
    else
      begin
        Unhandled.WriteLine('Error reading array control line.');
      end;
  end;
end;

procedure TInteger2DArrayReader.Read2DArrayFromTextFile(Stream: TStreamReader);
var
  ALine: string;
  ErrorLine: string;
  ItemIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  DataValue: Integer;
begin
  RowIndex := 0;
  ColIndex := 0;
  while (RowIndex < FDimensions.NRow) and (ColIndex < FDimensions.NCol) do
  begin
    ALine := Stream.ReadLine;
    ErrorLine := ALine;
    FSplitter.DelimitedText := ALine;
    for ItemIndex := 0 to FSplitter.Count - 1 do
    begin
      if StrToDataType(FSplitter[ItemIndex], DataValue) then
      begin
        FData[RowIndex, ColIndex] := DataValue * FFactor;
        Inc(ColIndex);
        if ColIndex = FDimensions.NCol then
        begin
          ColIndex := 0;
          Inc(RowIndex);
        end;
      end;
    end;
  end;
end;

function TInteger2DArrayReader.StrToDataType(AValue: string;
  var DataValue: Longint): Boolean;
begin
  result := TryStrToInt(AValue, DataValue);
end;

{ TDimensionedPackageReader }

procedure TDimensionedPackageReader.SetDimensions(const Value: TDimensions);
begin
  FDimensions := Value;
  FDimensions.DisVNRow := FDimensions.NRow;
  if FDimensions.NRow = 0 then
  begin
    FDimensions.NRow := 1;
  end;
end;

{ TCellIdList }

procedure TCellIdList.Sort;
begin
  inherited Sort(
    TComparer<TMfCellId>.Construct(
      function(const Left, Right: TMfCellId): Integer
      begin
        result := Left.Row - Right.Row;
        if Result = 0 then
        begin
          result := Left.column - Right.column;
          if result = 0 then
          begin
            result := Left.Layer - Right.Layer;
          end;
        end;
      end
    ));

end;

end.
