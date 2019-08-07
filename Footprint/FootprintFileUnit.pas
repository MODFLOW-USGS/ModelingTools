unit FootprintFileUnit;

interface

uses
  System.Classes, System.SysUtils, FootPrintUtilities, System.IOUtils, FastGEO,
  Xml.VerySimple;

type
  EFootPrintFileException = class(Exception);
  EFootPrintArrayError = class(EFootPrintFileException);
  EFootPrintColumnCountError = class(EFootPrintFileException);
  EFootPrintRowCountError = class(EFootPrintFileException);
  EFootPrintWrongVersion = class(EFootPrintFileException);
  EFootPrintConvertError = class(EFootPrintFileException);
  EFootPrintHeaderError = class(EFootPrintFileException);
  EFootPrintValueNotFoundError = class(EFootPrintFileException);
  EFootPrintOutlineCountNotFoundError = class(EFootPrintFileException);
  EFootPrintUnspecifiedMethod = class(EFootPrintFileException);

  TReadInputEvent = procedure (Sender: TObject; const Text: string) of object;

  TFootPrintFile = class(TObject)
  private
    const
    FileHeader = 'Well_Footprint_File';
    FileVersion = 1;
    FileFormatVersionTag = 'FileFormatVersion';
    NumberOfRowsTag = 'Number_Of_Rows';
    NumberOfColumnsTag = 'Number_Of_Columns';
    ClosureCriterionTag = 'Closure_Criterion';
    MaxIterationsTag = 'Max_Iterations';
    InitialDistributionTag = 'Initial_Distribution';
    RedistributionCriterionTag = 'Redistribution_Criterion';
    DepthRateIndexTag = 'Depth_Rate_Index';
    WithdrawalTag = 'Withdrawals';
    DirectTag = 'Direct';
    UniformTag = 'Uniform';
    TextFileTag = 'Text_File';
    BinaryFileTag = 'Binary_File';
    CellSizeTag = 'Cell_Size';
    GridAngleTag = 'Grid_Angle';
    OutlineTag = 'Outline';
    PointTag = 'Point';
    XTag = 'X';
    YTag = 'Y';
    RowTag = 'Row';
    ColumnTag = 'Column';
    ValueTag = 'Value';
    ListingFileTag = 'Listing_File';
    AsciiOutputFileTag = 'Text_Results_File';
    BinaryOutputFileTag = 'Binary_Results_File';
    ActiveTag = 'Active';
    Indentation = '  ';
    procedure ReadToStartOfArrayInTextResultsFile(Reader: TStreamReader;
      TextFileName, ArrayName: string; var NumberOfRows, NumberOfColumns: integer);
    procedure ReadAsciRealArray(var AnArray: TTwoDRealArray; Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
    procedure ReadAsciIntArray(var AnArray: TTwoDIntArray; Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
    procedure ReadRealArray(var AnArray: TTwoDRealArray; StartTag: string;
      ArrayName: string = '');
    procedure CheckArrayIndicies(Row, Col: Integer);
    function GetDepthRateIndex(Row, Col: Integer): double;
    function GetWithdrawals(Row, Col: Integer): double;
    procedure SetCapacity(Row, Col: Integer; const Value: double);
    procedure SetWithdrawals(Row, Col: Integer; const Value: double);
    procedure SetCellSize(const Value: double);
    function GetDepthRateIndexArray: TTwoDRealArray;
    function GetWithdrawalsArray: TTwoDRealArray;
    procedure SetGridAngle(const Value: Double);
    procedure SetOutline(const Value: TPolygon2D);
    function GetOutline: TPolygon2D;
    procedure WriteOutline;
    procedure SetAsciiFileName(const Value: string);
    procedure SetBinaryFileName(const Value: string);
    procedure SetListingFileName(const Value: string);
    procedure ReadAsciiFileName;
    procedure ReadBinaryFilename;
    function GetAsciiFileName: string;
    function GetBinaryFileName: string;
    function GetListingFileName: string;
    function GetActive(Row, Col: Integer): Boolean;
    procedure SetActive(Row, Col: Integer; const Value: Boolean);
    procedure ReadBooleanArray(var AnArray: TTwoDBooleanArray;
      StartTag: string);
    procedure ReadXmlBooleanArray(var AnArray: TTwoDBooleanArray;
      ParentNode: TXmlNode);
    procedure ReadBooleanArrayFromFile(var AnArray: TTwoDBooleanArray;
      Reader: TStreamReader);
    procedure WriteTaggedBooleanArray(Tag: string; Value: TTwoDBooleanArray);
    function GetActiveArray: TTwoDBooleanArray;
    procedure MoveToStartOfArrayInBinaryResultsFile(const ArrayName: string;
      BinaryFile: TFileStream; var NumberOfRows, NumberOfColumns: integer);
    function ReadInteger(ParentNode: TXmlNode; const Tag: string;
      out Value: integer): Boolean;
    function ReadFloat(ParentNode: TXmlNode; const Tag: string;
      out Value: Extended): Boolean;
    function ReadBoolean(ParentNode: TXmlNode; const Tag: string;
      out Value: Boolean): Boolean;
    function ReadString(ParentNode: TXmlNode; const Tag: string;
      out Value: String): Boolean;
    procedure ReadXmlRealArray(var AnArray: TTwoDRealArray;
      ParentNode: TXmlNode; NumberOfRows, NumberOfColumns: integer);
    var
    FSettingsFileWriter: TXmlVerySimple;
    FSettingsFileReader: TXmlVerySimple;
    // version number of the file being read.
    FVersion: Integer;
    FFileName: string;
    FNumberOfRows: Integer;
    FNumberOfColumns: Integer;
    FMaxIterations: integer;
    FClosureCriterion: double;
    FRedistributionCriterion: Integer;
    FInitialDistribution: boolean;
    FDepthRateIndex: TTwoDRealArray;
    FWithdrawals: TTwoDRealArray;
    FActive: TTwoDBooleanArray;
    FCellSize: double;
    FGridAngle: double;
    FOutline: TPolygon2D;
    FBinaryFileName: string;
    FListingFileName: string;
    FAsciiFileName: string;
    FOnReadInput: TReadInputEvent;
    FOnReadListingFileName: TNotifyEvent;
    FOnReadOutline: TNotifyEvent;
    procedure WriteTaggedInteger(Tag: string; Value: Integer);
    procedure ReadNumberOfRows;
    procedure ReadNumberOfColumns;
    procedure InitializeArrays;
    procedure SetNumberOfColumns(const Value: Integer);
    procedure SetNumberOfRows(const Value: Integer);
    procedure ReadHeader;
    procedure ReadVersion;
    procedure SetClosureCriterion(const Value: double);
    procedure SetInitialDistribution(const Value: boolean);
    procedure SetMaxIterations(const Value: integer);
    procedure SetRedistributionCriterion(const Value: Integer);
    procedure ReadClosureCriterion;
    procedure ReadMaximumNumberOfIterations;
    procedure ReadRedistributionCriterion;
    procedure ReadCellSize;
    procedure WriteTaggedReal(Tag: string; Value: double);
    procedure WriteTaggedBoolean(Tag: string; Value: Boolean);
    procedure WriteTaggedString(Tag: string; Value: string);
    procedure ReadInitialRedistribution;
    procedure WriteTaggedRealArray(Tag: string; Value: TTwoDRealArray);
    procedure ReadGridAngle;
    procedure ReadOutline;
    procedure ReadListingFile;
  public
    constructor Create;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    property NumberOfRows: Integer read FNumberOfRows write SetNumberOfRows;
    property NumberOfColumns: Integer read FNumberOfColumns
      write SetNumberOfColumns;
    property ClosureCriterion: double read FClosureCriterion
      write SetClosureCriterion;
    property CellSize: double read FCellSize write SetCellSize;
    property MaxIterations: integer read FMaxIterations write SetMaxIterations stored True;
    property InitialDistribution: boolean read FInitialDistribution
      write SetInitialDistribution;
    property RedistributionCriterion: Integer read FRedistributionCriterion
      write SetRedistributionCriterion;
    property DepthRateIndex[Row, Col: Integer]: double read GetDepthRateIndex
      write SetCapacity;
    property Withdrawals[Row, Col: Integer]: double read GetWithdrawals
      write SetWithdrawals;
    property Active[Row, Col: Integer]: Boolean read GetActive write SetActive;
    property DepthRateIndexArray: TTwoDRealArray read GetDepthRateIndexArray;
    property WithdrawalsArray: TTwoDRealArray read GetWithdrawalsArray;
    property ActiveArray: TTwoDBooleanArray read GetActiveArray;
    property Outline: TPolygon2D read GetOutline write SetOutline;
    // @name is measured counterclockwise in degrees.
    property GridAngle: Double read FGridAngle write SetGridAngle;
    property ListingFileName: string read GetListingFileName
      write SetListingFileName;
    property AsciiFileName: string read GetAsciiFileName write SetAsciiFileName;
    property BinaryFileName: string read GetBinaryFileName
      write SetBinaryFileName;
    function ListingFileFullPath: string;
    function AsciiFileFullPath: string;
    function BinaryFileFullPath: string;
    procedure ReadTextRealArray(TextFileName: string; var Reader: TStreamReader;
      var AnArray: TTwoDRealArray; ArrayName: string);
    procedure ReadBinaryRealArray(BinaryFileName:
      string; var BinaryFile: TFileStream; var AnArray: TTwoDRealArray;
      ArrayName: string);
    procedure ReadTextIntArray(TextFileName: string; var Reader: TStreamReader;
      var AnArray: TTwoDIntArray; ArrayName: string);
    procedure ReadBinaryIntArray(BinaryFileName: string;
      var BinaryFile: TFileStream; var AnArray: TTwoDIntArray; ArrayName: string);

    property OnReadListingFileName: TNotifyEvent read FOnReadListingFileName
      write FOnReadListingFileName;
    property OnReadInput: TReadInputEvent read FOnReadInput write FOnReadInput;
    property OnReadOutline: TNotifyEvent read FOnReadOutline write FOnReadOutline;
  end;

const
  StrDistributedWithdraw = 'Distributed_Withdrawals';
  StrFootprintCode = 'Footprint_Code';
  StrFootprintBinaryFile = 'Footprint_Binary_File';

implementation

uses
  System.Generics.Collections;

resourcestring
  StrSIsNotAFootprin = '%s is not a WellFootprint file';
  StrErrorSettingTheNu = 'Error setting the number of %0:s to %1:d. The numb' +
  'er of %0:s must be greater than 0.';
  StrErrorReadingTheNu = 'Error reading the number of %0:s from %1:s. The nu' +
  'mber of %0:s should equal %2:d but in the file, it is %3:d.';
  StrErrorReadingCount = 'Error reading the number of %0:s from %1:s';

{ TFootPrintFile }

function TFootPrintFile.ListingFileFullPath: string;
var
  CurrentDir: string;
begin
  if FListingFileName <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      result := TPath.GetFullPath(FListingFileName)
    finally
      SetCurrentDir(CurrentDir)
    end;
  end
  else
  begin
    result := ''
  end;
end;

procedure TFootPrintFile.LoadFromFile(FileName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  FFileName := FileName;

  FSettingsFileReader := TXmlVerySimple.Create;
  try
    FSettingsFileReader.LoadFromFile(FFileName);

    ReadHeader;
    ReadListingFile;
    ReadVersion;
    ReadNumberOfRows;
    ReadNumberOfColumns;
    ReadClosureCriterion;
    ReadMaximumNumberOfIterations;
    ReadInitialRedistribution;
    ReadRedistributionCriterion;
    ReadCellSize;
    ReadGridAngle;
    ReadOutline;
    ReadAsciiFileName;
    ReadBinaryFilename;
    ReadRealArray(FDepthRateIndex, DepthRateIndexTag);
    ReadRealArray(FWithdrawals, WithdrawalTag, StrDistributedWithdraw);
    ReadBooleanArray(FActive, ActiveTag);
  finally
    FSettingsFileReader.Free;
  end;

  for RowIndex := 0 to NumberOfRows - 1 do
  begin
    for ColIndex := 0 to NumberOfColumns - 1 do
    begin
      if not Active[RowIndex,ColIndex]  then
      begin
        Withdrawals[RowIndex,ColIndex] := 0;
      end;
    end;
  end;
end;

procedure TFootPrintFile.WriteOutline;
var
  LineIndex: Integer;
  OutlineNode: TXmlNode;
  PointNode: TXmlNode;
  XNode: TXmlNode;
  YNode: TXmlNode;
begin
  Assert(FSettingsFileWriter <> nil);
  Assert(FSettingsFileWriter.DocumentElement <> nil);
  OutlineNode := FSettingsFileWriter.DocumentElement.AddChild(OutlineTag);
  OutlineNode.NodeValue := IntToStr(Length(FOutline));

  for LineIndex := 0 to Length(FOutline) - 1 do
  begin
    PointNode := OutlineNode.AddChild(PointTag);
    PointNode.NodeValue := IntToStr(LineIndex);
    XNode := PointNode.AddChild(XTag);
    XNode.NodeValue := FortranFloatToStr(FOutline[LineIndex].x);
    YNode := PointNode.AddChild(YTag);
    YNode.NodeValue := FortranFloatToStr(FOutline[LineIndex].y);
  end;
end;

procedure TFootPrintFile.SaveToFile(FileName: string);
begin
  FFileName := FileName;
  FSettingsFileWriter := TXmlVerySimple.Create;
  try
    FSettingsFileWriter.AddChild(FileHeader);

    WriteTaggedInteger(FileFormatVersionTag, FileVersion);
    WriteTaggedInteger(NumberOfRowsTag, NumberOfRows);
    WriteTaggedInteger(NumberOfColumnsTag, NumberOfColumns);
    WriteTaggedReal(ClosureCriterionTag, ClosureCriterion);
    WriteTaggedReal(CellSizeTag, CellSize);
    WriteTaggedInteger(MaxIterationsTag, MaxIterations);
    WriteTaggedBoolean(InitialDistributionTag, InitialDistribution);
    WriteTaggedInteger(RedistributionCriterionTag, RedistributionCriterion);
    WriteTaggedReal(GridAngleTag, GridAngle);
    WriteOutline;
    WriteTaggedRealArray(DepthRateIndexTag, FDepthRateIndex);
    WriteTaggedRealArray(WithdrawalTag, FWithdrawals);
    WriteTaggedBooleanArray(ActiveTag, FActive);
    WriteTaggedString(ListingFileTag, FListingFileName);
    WriteTaggedString(AsciiOutputFileTag, FAsciiFileName);
    WriteTaggedString(BinaryOutputFileTag, FBinaryFileName);
    FSettingsFileWriter.SaveToFile(FFileName)
  finally
    FSettingsFileWriter.Free;
  end;
end;

procedure TFootPrintFile.ReadXmlBooleanArray(var AnArray: TTwoDBooleanArray;
  ParentNode: TXmlNode);
var
  RowNodes: TXmlNodeList;
  RowIndex: Integer;
  ARowNode: TXmlNode;
  RowNumber: Integer;
  Columns: TXmlNodeList;
  ColIndex: Integer;
  AColNode: TXmlNode;
  ColumnNumber: Integer;
  ValueNode: TXmlNode;
begin
  RowNodes := ParentNode.FindNodes(RowTag);
  try
    for RowIndex := 0 to RowNodes.Count - 1 do
    begin
      ARowNode := RowNodes[RowIndex];
      if TryStrToInt(ARowNode.NodeValue, RowNumber) then
      begin
        Assert(RowNumber >= 1);
        Assert(RowNumber <= NumberOfRows);
      end
      else
      begin
        raise EFootPrintConvertError.Create(Format(
          'Error converting %0:s to a row number in %1:s',
          [ARowNode.NodeValue, FFileName]));
      end;
      Columns := ARowNode.FindNodes(ColumnTag);
      try
        for ColIndex := 0 to Columns.Count - 1 do
        begin
          AColNode := Columns[ColIndex];
          if TryStrToInt(AColNode.NodeValue, ColumnNumber) then
          begin
            Assert(ColumnNumber >= 1);
            Assert(ColumnNumber <= NumberOfColumns);
          end
          else
          begin
            raise EFootPrintConvertError.Create(Format(
              'Error converting %0:s to a column number in %1:s',
              [AColNode.NodeValue, FFileName]));
          end;
          ValueNode := AColNode.Find(ValueTag);
          Assert(ValueNode <> nil, Format(
            'No value assigned for row %0:d; column %1:d in an array in %2:s',
            [RowNumber, ColumnNumber, FFileName]));
          if AnsiSameText(ValueNode.NodeValue, 'True') then
          begin
            AnArray[RowNumber-1, ColumnNumber-1] := True;
          end
          else if AnsiSameText(ValueNode.NodeValue, 'False') then
          begin
            AnArray[RowNumber-1, ColumnNumber-1] := False;
          end
          else
          begin
            raise EFootPrintConvertError.Create(Format(
              'Error converting %0:s to an array[%1:d, %2:d] value in %3:s',
              [ValueNode.NodeValue, RowNumber, ColumnNumber, FFileName]));
          end;
        end;
      finally
        Columns.Free;
      end;
    end;
  finally
    RowNodes.Free;
  end;
end;

procedure TFootPrintFile.ReadXmlRealArray(var AnArray: TTwoDRealArray;
  ParentNode: TXmlNode; NumberOfRows, NumberOfColumns: integer);
var
  RowNodes: TXmlNodeList;
  RowIndex: Integer;
  ARowNode: TXmlNode;
  RowNumber: Integer;
  Columns: TXmlNodeList;
  ColIndex: Integer;
  AColNode: TXmlNode;
  ColumnNumber: Integer;
  AValue: Extended;
  ValueNode: TXmlNode;
begin
  RowNodes := ParentNode.FindNodes(RowTag);
  try
//    Assert(RowNodes.Count = NumberOfRows);
    for RowIndex := 0 to RowNodes.Count - 1 do
    begin
      ARowNode := RowNodes[RowIndex];
      if TryStrToInt(ARowNode.NodeValue, RowNumber) then
      begin
        Assert(RowNumber >= 1);
        Assert(RowNumber <= NumberOfRows);
      end
      else
      begin
        raise EFootPrintConvertError.Create(Format(
          'Error converting %0:s to a row number in %1:s',
          [ARowNode.NodeValue, FFileName]));
      end;
      Columns := ARowNode.FindNodes(ColumnTag);
//      Assert(Columns.Count = NumberOfColumns);
      try
        for ColIndex := 0 to Columns.Count - 1 do
        begin
          AColNode := Columns[ColIndex];
          if TryStrToInt(AColNode.NodeValue, ColumnNumber) then
          begin
            Assert(ColumnNumber >= 1);
            Assert(ColumnNumber <= NumberOfColumns);
          end
          else
          begin
            raise EFootPrintConvertError.Create(Format(
              'Error converting %0:s to a column number in %1:s',
              [AColNode.NodeValue, FFileName]));
          end;
          ValueNode := AColNode.Find(ValueTag);
          Assert(ValueNode <> nil, Format(
            'No value assigned for row %0:d; column %1:d in an array in %2:s',
            [RowNumber, ColumnNumber, FFileName]));
          if TryFortranStrToFloat(ValueNode.NodeValue, AValue) then
          begin
            AnArray[RowNumber-1, ColumnNumber-1] := AValue;
          end
          else
          begin
            raise EFootPrintConvertError.Create(Format(
              'Error converting %0:s to an array[%1:d, %2:d] value in %3:s',
              [ValueNode.NodeValue, RowNumber, ColumnNumber, FFileName]));
          end;
        end;
      finally
        Columns.Free;
      end;
    end;
  finally
    RowNodes.Free;
  end;
end;


procedure TFootPrintFile.ReadAsciRealArray(var AnArray: TTwoDRealArray;
  Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
var
  RowIndex: Integer;
  index: Integer;
  Splitter: TStringList;
  ColIndex: Integer;
begin
  Splitter := TStringList.Create;
  try
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      index := 0;
      Splitter.CommaText := Reader.ReadLine;
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        if index >= Splitter.Count then
        begin
          Splitter.CommaText := Reader.ReadLine;
          index := 0;
        end;
        AnArray[RowIndex, ColIndex] := FortranStrToFloat(Splitter[index]);
        Inc(index);
        if ColIndex = NumberOfColumns - 1 then
        begin
          Assert(index = Splitter.Count);
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

function TFootPrintFile.ReadBoolean(ParentNode: TXmlNode; const Tag: string;
  out Value: Boolean): Boolean;
var
  ChildNode: TXmlNode;
begin
  result := False;
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  ChildNode := ParentNode.ChildNodes.FindNode(Tag);
  if ChildNode = nil then
  begin
    Exit;
  end;

  if AnsiSameText(Trim(ChildNode.NodeValue), 'True') then
  begin
    Result := True;
    Value := True;
  end
  else if AnsiSameText(Trim(ChildNode.NodeValue), 'False') then
  begin
    Result := True;
    Value := False;
  end
  else
  begin
    raise EFootPrintConvertError.Create(Format('Unable to convert "%s" to a Boolean.',
      [Trim(ChildNode.NodeValue)]));
  end;
end;

procedure TFootPrintFile.ReadBooleanArrayFromFile(var AnArray: TTwoDBooleanArray;
  Reader: TStreamReader);
var
  RowIndex: Integer;
  index: Integer;
  Splitter: TStringList;
  ColIndex: Integer;
  Value: string;
begin
  Splitter := TStringList.Create;
  try
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      index := 0;
      Splitter.CommaText := Reader.ReadLine;
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        if index >= Splitter.Count then
        begin
          Splitter.CommaText := Reader.ReadLine;
          index := 0;
        end;
        Value := Splitter[index];
        if AnsiSameText(Value, 'True') then
        begin
          AnArray[RowIndex, ColIndex] := True;
        end
        else if AnsiSameText(Value, 'False') then
        begin
          AnArray[RowIndex, ColIndex] := False;
        end
        else
        begin
          AnArray[RowIndex, ColIndex] := (StrToInt(Value) <> 0);
        end;
        Inc(index);
        if ColIndex = NumberOfColumns - 1 then
        begin
          Assert(index = Splitter.Count);
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TFootPrintFile.ReadBooleanArray(
  var AnArray: TTwoDBooleanArray; StartTag: string);
var
//  MethodTag: string;
  Reader: TStreamReader;
  TextFileName: string;
  CurrDir: string;
  BinaryFileName: string;
  RowIndex: Integer;
  ColIndex: Integer;
  BinaryFile: TFileStream;
  AnInt: longint;
  ArrayNode: TXmlNode;
  DirectNode: TXmlNode;
  TextFileNode: TXmlNode;
  BinaryNode: TXmlNode;
  UniformNode: TXmlNode;
  BooleanValue: Boolean;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  ArrayNode := FSettingsFileReader.DocumentElement.Find(StartTag);
  if ArrayNode <> nil then
  begin
    if Assigned(OnReadInput) then
    begin
      OnReadInput(Self, Format('Reading %s.', [StartTag]));
    end;
    DirectNode := ArrayNode.Find(DirectTag);
    if DirectNode <> nil then
    begin
      ReadXmlBooleanArray(AnArray, DirectNode);
      Exit;
    end;

    UniformNode := ArrayNode.Find(UniformTag);
    if UniformNode <> nil then
    begin
      if AnsiSameText(UniformNode.NodeValue, 'True') then
      begin
        BooleanValue := True;
      end
      else if AnsiSameText(UniformNode.NodeValue, 'False') then
      begin
        BooleanValue := False;
      end
      else
      begin
        raise EFootPrintConvertError.Create(Format(
          'Error converting %0:s to a uniform array value in %1:s',
          [UniformNode.NodeValue, FFileName]));
      end;
      
      for RowIndex := 0 to Length(AnArray) - 1 do
      begin
        for ColIndex := 0 to Length(AnArray[RowIndex]) - 1 do
        begin
          AnArray[RowIndex, ColIndex] := BooleanValue;
        end;
      end;
      Exit;
    end;

    TextFileNode := ArrayNode.Find(TextFileTag);
    if TextFileNode <> nil then
    begin
      TextFileName := Trim(TextFileNode.NodeValue);
      CurrDir := GetcurrentDir;
      try
        SetCurrentDir(ExtractFileDir(FFileName));

        try
          Reader := TFile.OpenText(TextFileName);
        except
        {$IFDEF MSWINDOWS}
          on E: ENotSupportedException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the file name is valid');
          end;
        {$ENDIF}
          on E: EArgumentException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the file name is valid');
          end;
          on E: EDirectoryNotFoundException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the directory exists');
          end;
          on E: EFileNotFoundException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the file exists');
          end;
        end;
        try
          ReadBooleanArrayFromFile(AnArray, Reader);

        finally
          Reader.Free;
        end;
      finally
        SetCurrentDir(CurrDir)
      end;
      Exit;
    end;

    BinaryNode := ArrayNode.Find(BinaryFileTag);
    if BinaryNode <> nil then
    begin
      BinaryFileName := Trim(BinaryNode.NodeValue);
      CurrDir := GetcurrentDir;
      try
        SetCurrentDir(ExtractFileDir(FFileName));

        try
          BinaryFile := TFile.Open(BinaryFileName, TFileMode.fmOpen);
        except
        {$IFDEF MSWINDOWS}
          on E: ENotSupportedException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the file name is valid');
          end;
        {$ENDIF}
          on E: EArgumentException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the file name is valid');
          end;
          on E: EDirectoryNotFoundException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the directory exists');
          end;
          on E: EFileNotFoundException do
          begin
            raise EFootPrintFileException.Create(
              'Error opening "' + TextFileName + '". Check that the file exists');
          end;
        end;
      try
        for RowIndex := 0 to NumberOfRows - 1 do
        begin
          for ColIndex := 0 to NumberOfColumns - 1 do
          begin
            AnInt := BinaryFile.Read(AnInt, SizeOf(AnInt));
            AnArray[RowIndex,ColIndex] := AnInt <> 0;
          end;
        end;

        finally
          BinaryFile.Free;
        end;
      finally
        SetCurrentDir(CurrDir)
      end;
      Exit;
    end;

    raise EFootPrintUnspecifiedMethod.Create(
      Format('No method has been specified for reading the array %0:s in %1:s',
      [StartTag, FFileName]));

  end;
end;

procedure TFootPrintFile.MoveToStartOfArrayInBinaryResultsFile(
  const ArrayName: string; BinaryFile: TFileStream; var NumberOfRows, NumberOfColumns: integer);
var
  FileID: string;
  CharArray: array of Char;
  SizeOfDouble: longint;
  ACount: longint;
  FileText: string;
begin
  if  ArrayName <> '' then
  begin
    FileID := StrFootprintBinaryFile;
    SetLength(CharArray, Length(FileID));
    BinaryFile.Read(CharArray[0], Length(FileID)*SizeOf(Char));
    FileText := CharArrayToString(CharArray);
    // case sensitive
    if AnsiCompareStr(FileText,FileID) = 0 then
    begin
      BinaryFile.Read(SizeOfDouble, SizeOf(SizeOfDouble));
      Assert(SizeOfDouble = SizeOf(double));

      repeat
        BinaryFile.Read(ACount, SizeOf(ACount));
        SetLength(CharArray, ACount);
        BinaryFile.Read(CharArray[0], ACount*SizeOf(Char));

        BinaryFile.Read(ACount, SizeOf(ACount));
        if NumberOfRows < 0 then
        begin
          NumberOfRows := ACount;
        end;
        Assert(ACount = NumberOfRows);
        BinaryFile.Read(ACount, SizeOf(ACount));
        if NumberOfColumns < 0 then
        begin
          NumberOfColumns := ACount;
        end;
        Assert(ACount = NumberOfColumns);


        FileText := CharArrayToString(CharArray);
        // not case sensitive
        if  AnsiCompareText(FileText,ArrayName) = 0 then
        begin
          break;
        end;
        BinaryFile.Position := BinaryFile.Position
          + NumberOfRows*NumberOfColumns*SizeOf(Double);
      until BinaryFile.Size = BinaryFile.Position;
    end
    else
    begin
      BinaryFile.Position := 0;
    end;
  end
end;

procedure TFootPrintFile.ReadRealArray(var AnArray: TTwoDRealArray;
  StartTag: string; ArrayName: string = '');
var
  Reader: TStreamReader;
  TextFileName: string;
  CurrDir: string;
  BinaryFileName: string;
  BinaryFile: TFileStream;
  ArrayNode: TXmlNode;
  DirectNode: TXmlNode;
  TextFileNode: TXmlNode;
  BinaryNode: TXmlNode;
  RowIndex: Integer;
  ColIndex: Integer;
  UniformNode: TXmlNode;
  NumericValue: Extended;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if Assigned(OnReadInput) then
  begin
    OnReadInput(Self, Format('Reading %s.', [StartTag]));
  end;

  ArrayNode := FSettingsFileReader.DocumentElement.Find(StartTag);
  if ArrayNode <> nil then
  begin
    DirectNode := ArrayNode.Find(DirectTag);
    if DirectNode <> nil then
    begin
      ReadXmlRealArray(AnArray, DirectNode, NumberOfRows, NumberOfColumns);
      Exit;
    end;

    UniformNode := ArrayNode.Find(UniformTag);
    if UniformNode <> nil then
    begin
      if TryFortranStrToFloat(UniformNode.NodeValue, NumericValue) then
      begin
        for RowIndex := 0 to Length(AnArray) - 1 do
        begin
          for ColIndex := 0 to Length(AnArray[RowIndex]) - 1 do
          begin
            AnArray[RowIndex, ColIndex] := NumericValue;
          end;
        end;
      end
      else
      begin
        raise EFootPrintConvertError.Create(Format(
          'Error converting %0:s to a uniform array value in %1:s',
          [UniformNode.NodeValue, FFileName]));
      end;
      Exit;
    end;
    
    TextFileNode := ArrayNode.Find(TextFileTag);
    if TextFileNode <> nil then
    begin
      TextFileName := Trim(TextFileNode.NodeValue);
      CurrDir := GetcurrentDir;
      try
        SetCurrentDir(ExtractFileDir(FFileName));
        Reader := nil;
        try
          ReadTextRealArray(TextFileName, Reader, AnArray, ArrayName);
        finally
          Reader.Free;
        end;
      finally
        SetCurrentDir(CurrDir)
      end;
      Exit;
    end;
    
    BinaryNode := ArrayNode.Find(BinaryFileTag);
    if BinaryNode <> nil then
    begin
      BinaryFileName := Trim(BinaryNode.NodeValue);
      CurrDir := GetcurrentDir;
      try
        SetCurrentDir(ExtractFileDir(FFileName));
        BinaryFile := nil;
        try
          ReadBinaryRealArray(BinaryFileName, BinaryFile, AnArray, ArrayName );
        finally
          BinaryFile.Free;
        end;
      finally
        SetCurrentDir(CurrDir)
      end;
      Exit;
    end;

    raise EFootPrintUnspecifiedMethod.Create(
      Format('No method has been specified for reading the array %0:s in %1:s',
      [StartTag, FFileName]));

  end;
end;

function TFootPrintFile.AsciiFileFullPath: string;
var
  CurrentDir: string;
begin
  if FAsciiFileName <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      result := TPath.GetFullPath(FAsciiFileName)
    finally
      SetCurrentDir(CurrentDir)
    end;
  end
  else
  begin
    Result := ''
  end;
end;

function TFootPrintFile.BinaryFileFullPath: string;
var
  CurrentDir: string;
begin
  if FBinaryFileName <> '' then
  begin
    CurrentDir := GetCurrentDir;
    try
      SetCurrentDir(ExtractFileDir(FFileName));
      result := TPath.GetFullPath(FBinaryFileName)
    finally
      SetCurrentDir(CurrentDir)
    end;
  end
  else
  begin
    Result := ''
  end;
end;

procedure TFootPrintFile.ReadTextIntArray(TextFileName: string;
  var Reader: TStreamReader; var AnArray: TTwoDIntArray; ArrayName: string);
var
  NumRow: Integer;
  NumCol: Integer;
begin
  try
    if Reader = nil then
    begin
      Reader := TFile.OpenText(TextFileName);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file exists');
    end;
  end;
  NumRow := NumberOfRows;
  NumCol := NumberOfColumns;
  ReadToStartOfArrayInTextResultsFile(Reader, TextFileName, ArrayName, NumRow, NumCol);
  SetLength(AnArray, NumRow, NumCol);
  ReadAsciIntArray(AnArray, Reader, NumRow, NumCol);
end;

procedure TFootPrintFile.ReadTextRealArray(TextFileName: string;
  var Reader: TStreamReader; var AnArray: TTwoDRealArray; ArrayName: string);
var
  NumRow: Integer;
  NumCol: Integer;
begin
  try
    if Reader = nil then
    begin
      Reader := TFile.OpenText(TextFileName);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + TextFileName + '". Check that the file exists');
    end;
  end;
  NumRow := NumberOfRows;
  NumCol := NumberOfColumns;
  ReadToStartOfArrayInTextResultsFile(Reader, TextFileName, ArrayName, NumRow, NumCol);
  SetLength(AnArray, NumRow, NumCol);
  ReadAsciRealArray(AnArray, Reader, NumRow, NumCol);
end;

procedure TFootPrintFile.ReadBinaryRealArray(BinaryFileName: string;
  var BinaryFile: TFileStream; var AnArray: TTwoDRealArray; ArrayName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
  NumRows: Integer;
  NumCols: Integer;
begin
  try
    if BinaryFile = nil then
    begin
      BinaryFile := TFile.Open(BinaryFileName, TFileMode.fmOpen);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file exists');
    end;
  end;
  NumRows := NumberOfRows;
  NumCols := NumberOfColumns;
  MoveToStartOfArrayInBinaryResultsFile(ArrayName, BinaryFile, NumRows, NumCols);
  SetLength(AnArray, NumRows, NumCols);
  for RowIndex := 0 to NumRows - 1 do
  begin
    for ColIndex := 0 to NumCols - 1 do
    begin
      BinaryFile.Read(AnArray[RowIndex, ColIndex],
        SizeOf(AnArray[RowIndex, ColIndex]));
    end;
  end;
end;

procedure TFootPrintFile.ReadToStartOfArrayInTextResultsFile(
  Reader: TStreamReader; TextFileName, ArrayName: string; var NumberOfRows, NumberOfColumns: integer);
var
  ACount: Integer;
  ALine: string;
begin
  if ArrayName <> '' then
  begin
    ALine := Reader.ReadLine;
    if AnsiCompareText(ALine, ArrayName) = 0 then
    begin
      ALine := Reader.ReadLine;
      if TryStrToInt(ALine, ACount) then
      begin
        if NumberOfRows < 0 then
        begin
          NumberOfRows := ACount;
        end;
        if ACount <> NumberOfRows then
        begin
          raise EFootPrintRowCountError.Create(
            Format(StrErrorReadingTheNu,
            ['rows', TextFileName, NumberOfRows, ACount]));
        end;
      end
      else
      begin
        raise EFootPrintRowCountError.Create(Format(
          StrErrorReadingCount, ['rows', TextFileName]));
      end;
      ALine := Reader.ReadLine;
      if TryStrToInt(ALine, ACount) then
      begin
        if NumberOfColumns < 0 then
        begin
          NumberOfColumns := ACount;
        end;
        if ACount <> NumberOfColumns then
        begin
          raise EFootPrintColumnCountError.Create(
            Format(StrErrorReadingTheNu,
            ['columns', TextFileName, NumberOfRows, ACount]));
        end;
      end
      else
      begin
        raise EFootPrintColumnCountError.Create(Format(
          StrErrorReadingCount, ['columns', TextFileName]));
      end;
    end
    else
    begin
      Reader.DiscardBufferedData;
      Reader.BaseStream.Position := 0;
    end;
  end;
end;

procedure TFootPrintFile.CheckArrayIndicies(Row, Col: Integer);
begin
  if (Row < 0) or (Col < 0)
    or (Row >= NumberOfRows) or (Col >= NumberOfColumns) then
  begin
    raise EFootPrintArrayError.Create('Array index out of bounds');
  end;
end;

procedure TFootPrintFile.ReadInitialRedistribution;
var
  Value: Boolean;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadBoolean(FSettingsFileReader.DocumentElement, InitialDistributionTag, Value) then
  begin
    InitialDistribution := Value;
  end;
  if Assigned(OnReadInput) then
  begin
    if InitialDistribution then
    begin
      OnReadInput(self, 'Redistribute withdrawals before the first iteration = True');
    end
    else
    begin
      OnReadInput(self, 'Redistribute withdrawals before the first iteration = False');
    end;
  end;
end;

procedure TFootPrintFile.ReadListingFile;
var
  Value: string;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  if ReadString(FSettingsFileReader.DocumentElement, ListingFileTag, Value) then
  begin
    ListingFileName := Trim(Value);
  end
  else
  begin
    ListingFileName := ChangeFileExt(TPath.GetFullPath(FFileName), '.fplst')
  end;
  Assert(not AnsiSameText(TPath.GetFullPath(ListingFileName), TPath.GetFullPath(FFileName)),
    'The input and output files must have different names.');
  if Assigned(OnReadInput) then
  begin
    OnReadInput(self, Format('Name of listing file = "%s"', [ListingFileName]));
  end;
end;

procedure TFootPrintFile.ReadAsciiFileName;
var
  Value: string;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  if ReadString(FSettingsFileReader.DocumentElement, AsciiOutputFileTag, Value) then
  begin
    AsciiFileName := Trim(Value);
  end;
  if Assigned(OnReadInput) then
  begin
    if AsciiFileName = '' then
    begin
      OnReadInput(self, 'No text file for output selected');
    end
    else
    begin
      OnReadInput(self, Format('Text file for output = %s', [AsciiFileFullPath]));
    end;
  end;
end;

procedure TFootPrintFile.ReadAsciIntArray(var AnArray: TTwoDIntArray;
  Reader: TStreamReader; NumberOfRows, NumberOfColumns: integer);
var
  RowIndex: Integer;
  index: Integer;
  Splitter: TStringList;
  ColIndex: Integer;
begin
  Splitter := TStringList.Create;
  try
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      index := 0;
      Splitter.CommaText := Reader.ReadLine;
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        if index >= Splitter.Count then
        begin
          Splitter.CommaText := Reader.ReadLine;
          index := 0;
        end;
        AnArray[RowIndex, ColIndex] := StrToInt(Splitter[index]);
        Inc(index);
        if ColIndex = NumberOfColumns - 1 then
        begin
          Assert(index = Splitter.Count);
        end;
      end;
    end;
  finally
    Splitter.Free;
  end;
end;

procedure TFootPrintFile.ReadBinaryFilename;
var
  Value: string;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  if ReadString(FSettingsFileReader.DocumentElement, BinaryOutputFileTag, Value) then
  begin
    BinaryFileName := Trim(Value);
  end;
  if Assigned(OnReadInput) then
  begin
    if BinaryFileName = '' then
    begin
      OnReadInput(self, 'No binary file for output selected.');
    end
    else
    begin
      OnReadInput(self, Format('Binary file for output = %s', [BinaryFileFullPath]));
    end;
  end;
end;


procedure TFootPrintFile.ReadBinaryIntArray(BinaryFileName: string;
  var BinaryFile: TFileStream; var AnArray: TTwoDIntArray;ArrayName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
  NumRows: Integer;
  NumCols: Integer;
begin
  try
    if BinaryFile = nil then
    begin
      BinaryFile := TFile.Open(BinaryFileName, TFileMode.fmOpen);
    end;
  except
    on E: ENotSupportedException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EArgumentException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file name is valid');
    end;
    on E: EDirectoryNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the directory exists');
    end;
    on E: EFileNotFoundException do
    begin
      raise EFootPrintFileException.Create(
        'Error opening "' + BinaryFileName + '". Check that the file exists');
    end;
  end;
  NumRows := NumberOfRows;
  NumCols := NumberOfColumns;
  MoveToStartOfArrayInBinaryResultsFile(ArrayName, BinaryFile, NumRows, NumCols);
  SetLength(AnArray, NumRows, NumCols);
  for RowIndex := 0 to NumRows - 1 do
  begin
    for ColIndex := 0 to NumCols - 1 do
    begin
      BinaryFile.Read(AnArray[RowIndex, ColIndex],
        SizeOf(AnArray[RowIndex, ColIndex]));
    end;
  end;
end;

procedure TFootPrintFile.ReadRedistributionCriterion;
var
  Value: Integer;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadInteger(FSettingsFileReader.DocumentElement, RedistributionCriterionTag, Value) then
  begin
    RedistributionCriterion := Value;
  end;
  if Assigned(OnReadInput) then
  begin
    OnReadInput(Self, '');
    OnReadInput(Self, 'Number of iterations in which no withdrawal codes change ');
    OnReadInput(self, Format('before withdrawals are redistributed to edge = %d.', [RedistributionCriterion]));
    OnReadInput(Self, '');
  end;
end;

function TFootPrintFile.ReadString(ParentNode: TXmlNode; const Tag: string;
  out Value: String): Boolean;
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  ChildNode := ParentNode.ChildNodes.FindNode(Tag);
  if ChildNode = nil then
  begin
    result := false;
    Exit;
  end;

  result := True;
  Value := Trim(ChildNode.NodeValue);
end;

procedure TFootPrintFile.ReadMaximumNumberOfIterations;
var
  Value: Integer;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadInteger(FSettingsFileReader.DocumentElement, MaxIterationsTag, Value) then
  begin
    MaxIterations := Value;
  end;
  if Assigned(OnReadInput) then
  begin
    OnReadInput(self, Format('Maximum number of iterations = %d.', [MaxIterations]));
  end;
end;

procedure TFootPrintFile.ReadCellSize;
var
  Value: Extended;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadFloat(FSettingsFileReader.DocumentElement, CellSizeTag, Value) then
  begin
    CellSize := Value;
  end;
  if Assigned(OnReadInput) then
  begin
    OnReadInput(self, Format('Cell size = %g.', [CellSize]));
  end;
end;

procedure TFootPrintFile.ReadClosureCriterion;
var
  Value: Extended;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadFloat(FSettingsFileReader.DocumentElement, ClosureCriterionTag, Value) then
  begin
    ClosureCriterion := Value;
  end;
  if Assigned(OnReadInput) then
  begin
    OnReadInput(self, Format('Closure criterion = %g.', [ClosureCriterion]));
  end;
end;

function TFootPrintFile.ReadFloat(ParentNode: TXmlNode; const Tag: string;
  out Value: Extended): Boolean;
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  ChildNode := ParentNode.ChildNodes.FindNode(Tag);
  if ChildNode = nil then
  begin
    result := false;
    Exit;
  end;
  result := True;
  try
    Value := FortranStrToFloat(Trim(ChildNode.NodeValue));
  except on E: EConvertError do
    begin
      raise EFootPrintConvertError.Create(E.Message);
    end;
  end;
end;

procedure TFootPrintFile.ReadGridAngle;
var
  Value: Extended;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadFloat(FSettingsFileReader.DocumentElement, GridAngleTag, Value) then
  begin
    GridAngle := Value;
  end;
  if Assigned(OnReadInput) then
  begin
    OnReadInput(self, Format('Grid angle = %g.', [GridAngle]));
  end;
end;

procedure TFootPrintFile.ReadNumberOfColumns;
var
  Value: Integer;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadInteger(FSettingsFileReader.DocumentElement, NumberOfColumnsTag, Value) then
  begin
    NumberOfColumns := Value;
    if Assigned(OnReadInput) then
    begin
      OnReadInput(self, Format('Number of columns = %d.', [Value]));
    end;
  end
  else
  begin
    raise EFootPrintValueNotFoundError.Create(
      Format('Number of columns not found in %s', [FFileName]));
  end;
end;

function TFootPrintFile.ReadInteger(ParentNode: TXmlNode; const Tag: string;
  out Value: integer): Boolean;
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  ChildNode := ParentNode.ChildNodes.FindNode(Tag);
  if ChildNode = nil then
  begin
    result := false;
    Exit;
  end;
  result := True;
  try
    Value := StrToInt(Trim(ChildNode.NodeValue));
  except on E: EConvertError do
    begin
      raise EFootPrintConvertError.Create(E.Message);
    end;
  end;
end;

procedure TFootPrintFile.ReadNumberOfRows;
var
  Value: Integer;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if ReadInteger(FSettingsFileReader.DocumentElement, NumberOfRowsTag, Value) then
  begin
    NumberOfRows := Value;
    if Assigned(OnReadInput) then
    begin
      OnReadInput(self, Format('Number of rows = %d.', [Value]));
    end;
  end
  else
  begin
    raise EFootPrintValueNotFoundError.Create(
      Format('Number of rows not found in %s', [FFileName]));
  end;

end;

procedure TFootPrintFile.ReadOutline;
var
  APoint2D: TPoint2D;
  OutlineNode: TXmlNode;
  OutlineCount: Integer;
  NodeIndex: Integer;
  ChildNode: TXmlNode;
  OutLineIndex: Integer;
  XValue: Extended;
  YValue: Extended;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);
  if Assigned(OnReadInput) then
  begin
    OnReadInput(self, 'Reading model outline');
  end;

  OutlineNode := FSettingsFileReader.DocumentElement.ChildNodes.FindNode(OutlineTag);
  if OutlineNode <> nil then
  begin
    if TryStrToInt(OutlineNode.NodeValue, OutlineCount) then
    begin
      SetLength(FOutline, OutlineCount);
    end
    else
    begin
      raise EFootPrintOutlineCountNotFoundError.Create(
        'The number of points in the outline was not specified.');
    end;

    for NodeIndex := 0 to OutlineNode.ChildNodes.Count - 1 do
    begin
      ChildNode := OutlineNode.ChildNodes[NodeIndex];
      if ChildNode.NodeName = PointTag then
      begin
        if not TryStrToInt(ChildNode.NodeValue, OutLineIndex) then
        begin
            raise EFootPrintConvertError.Create(Format(
              'Error reading the node number for vertex %0:d of the outline in %1:s',
              [NodeIndex+1, FFileName]));
        end;
        Assert(OutLineIndex < OutlineCount, 'Node numbers in the outline must be less than the number of nodes in the outline');
        Assert(OutLineIndex >= 0, 'Node numbers in the outline must be greater or equal to zero.');
        if ReadFloat(ChildNode, XTag, XValue) then
        begin
          APoint2D.x := XValue;
        end
        else
        begin
          raise EFootPrintConvertError.Create(Format(
            'Error reading the X coordinate for vertex %0:d of the outline in %1:s',
            [NodeIndex+1, FFileName]));
        end;
        if ReadFloat(ChildNode, YTag, YValue) then
        begin
          APoint2D.Y := YValue;
        end
        else
        begin
          raise EFootPrintConvertError.Create(Format(
            'Error reading the Y coordinate for vertex %0:d of the outline in %1:s',
            [NodeIndex+1, FFileName]));
        end;
        FOutline[OutLineIndex] := APoint2D;
      end;
    end;
  end;
  if Assigned(OnReadOutline) then
  begin
    OnReadOutline(self);
  end;
end;

procedure TFootPrintFile.ReadVersion;
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  if Assigned(FOnReadInput) then
  begin
    FOnReadInput(self, 'Reading version number');
  end;

  ChildNode := FSettingsFileReader.DocumentElement.ChildNodes.FindNode(FileFormatVersionTag);
  if ChildNode = nil then
  begin
    raise EFootPrintWrongVersion.Create(Format('File version not found in %s', [FFileName]));
  end;

  if TryStrToInt(Trim(ChildNode.NodeValue), FVersion) then
  begin
    if Assigned(OnReadInput) then
    begin
      OnReadInput(self, Format('Input version number = %d', [FVersion]));
    end;
    if not (FVersion in [FileVersion]) then
    begin
      raise EFootPrintWrongVersion.Create(
        Format('Unrecognized file version %0:d in %1:s', [FVersion, FFileName]));
    end;
  end
  else
  begin
    raise EFootPrintConvertError.Create(
      Format('Unable to convert "%0:s" to a version number in %s', [ChildNode.NodeValue, FFileName]));
  end;
end;

procedure TFootPrintFile.ReadHeader;
begin
  Assert(FSettingsFileReader <> nil);
  Assert(FSettingsFileReader.DocumentElement <> nil);

  if FSettingsFileReader.DocumentElement.Name <> FileHeader then
  begin
    raise EFootPrintHeaderError.Create(Format(StrSIsNotAFootprin, [FFileName]));
  end;
end;

procedure TFootPrintFile.InitializeArrays;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if (NumberOfRows > 0) and (NumberOfColumns > 0) then
  begin
    SetLength(FDepthRateIndex, NumberOfRows, NumberOfColumns);
    SetLength(FWithdrawals, NumberOfRows, NumberOfColumns);
    SetLength(FActive, NumberOfRows, NumberOfColumns);
    for RowIndex := 0 to NumberOfRows - 1 do
    begin
      for ColIndex := 0 to NumberOfColumns - 1 do
      begin
        FDepthRateIndex[RowIndex,ColIndex] := 0;
        FWithdrawals[RowIndex,ColIndex] := 0;
        FActive[RowIndex,ColIndex] := True;
      end;
    end;
  end;
end;

procedure TFootPrintFile.SetActive(Row, Col: Integer; const Value: Boolean);
begin
  CheckArrayIndicies(Row, Col);
  FActive[Row, Col] := Value;
end;

procedure TFootPrintFile.SetAsciiFileName(const Value: string);
begin
  FAsciiFileName := Value;
end;

procedure TFootPrintFile.SetBinaryFileName(const Value: string);
begin
  FBinaryFileName := Value;
end;

procedure TFootPrintFile.SetCapacity(Row, Col: Integer; const Value: double);
begin
  CheckArrayIndicies(Row, Col);
  FDepthRateIndex[Row, Col] := Value;
end;

procedure TFootPrintFile.SetCellSize(const Value: double);
begin
  FCellSize := Value;
end;

procedure TFootPrintFile.SetClosureCriterion(const Value: double);
begin
  FClosureCriterion := Value;
end;

procedure TFootPrintFile.SetGridAngle(const Value: Double);
begin
  FGridAngle := Value;
end;

procedure TFootPrintFile.SetInitialDistribution(const Value: boolean);
begin
  FInitialDistribution := Value;
end;

procedure TFootPrintFile.SetListingFileName(const Value: string);
begin
  FListingFileName := Value;
  if Assigned(OnReadListingFileName) then
  begin
    OnReadListingFileName(self);
  end;
end;

procedure TFootPrintFile.SetMaxIterations(const Value: integer);
begin
  FMaxIterations := Value;
end;

procedure TFootPrintFile.SetNumberOfColumns(const Value: Integer);
begin
  if Value <= 0 then
  begin
    raise EFootPrintColumnCountError.Create(
      Format(StrErrorSettingTheNu, ['columns', Value]));
  end;
  if FNumberOfColumns <> Value then
  begin
    FNumberOfColumns := Value;
    InitializeArrays;
  end;
end;

procedure TFootPrintFile.SetNumberOfRows(const Value: Integer);
begin
  if Value <= 0 then
  begin
    raise EFootPrintRowCountError.Create(
      Format(StrErrorSettingTheNu, ['rows', Value]));
  end;
  if FNumberOfRows <> Value then
  begin
    FNumberOfRows := Value;
    InitializeArrays;
  end;
end;

procedure TFootPrintFile.SetOutline(const Value: TPolygon2D);
begin
  FOutline := Value;
  SetLength(FOutline, Length(FOutline));
end;

procedure TFootPrintFile.SetRedistributionCriterion(const Value: Integer);
begin
  FRedistributionCriterion := Value;
end;

procedure TFootPrintFile.SetWithdrawals(Row, Col: Integer; const Value: double);
begin
  CheckArrayIndicies(Row, Col);
  FWithdrawals[Row, Col] := Value;
end;

function TFootPrintFile.GetActive(Row, Col: Integer): Boolean;
begin
  CheckArrayIndicies(Row, Col);
  result := FActive[Row, Col];
end;

function TFootPrintFile.GetActiveArray: TTwoDBooleanArray;
begin
  result := FActive;
  SetLength(Result, NumberOfRows, NumberOfColumns);
end;

function TFootPrintFile.GetAsciiFileName: string;
begin
  result := FAsciiFileName;
end;

function TFootPrintFile.GetBinaryFileName: string;
begin
  result := FBinaryFileName;
end;

function TFootPrintFile.GetDepthRateIndex(Row, Col: Integer): double;
begin
  CheckArrayIndicies(Row, Col);
  result := FDepthRateIndex[Row, Col];
end;

function TFootPrintFile.GetDepthRateIndexArray: TTwoDRealArray;
begin
  Result := FDepthRateIndex;
  SetLength(Result, NumberOfRows, NumberOfColumns);
end;

function TFootPrintFile.GetListingFileName: string;
begin
  result := FListingFileName;
end;

function TFootPrintFile.GetOutline: TPolygon2D;
begin
  result := FOutline;
  SetLength(result, Length(result));
end;

function TFootPrintFile.GetWithdrawals(Row, Col: Integer): double;
begin
  CheckArrayIndicies(Row, Col);
  result := FWithdrawals[Row, Col];
end;

function TFootPrintFile.GetWithdrawalsArray: TTwoDRealArray;
begin
  Result := FWithdrawals;
  SetLength(Result, NumberOfRows, NumberOfColumns);
end;

constructor TFootPrintFile.Create;
begin
  FNumberOfRows := -1;
  FNumberOfColumns := -1;
  FClosureCriterion := 0.01;
  FMaxIterations := 10000;
  FInitialDistribution := True;
  FRedistributionCriterion := 1;
  FCellSize := 0;
end;

procedure TFootPrintFile.WriteTaggedInteger(Tag: string; Value: Integer);
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileWriter <> nil);
  Assert(FSettingsFileWriter.DocumentElement <> nil);
  ChildNode := FSettingsFileWriter.DocumentElement.AddChild(Tag);
  ChildNode.NodeValue := IntToStr(Value);
end;

procedure TFootPrintFile.WriteTaggedReal(Tag: string; Value: double);
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileWriter <> nil);
  Assert(FSettingsFileWriter.DocumentElement <> nil);
  ChildNode := FSettingsFileWriter.DocumentElement.AddChild(Tag);
  ChildNode.NodeValue := FortranFloatToStr(Value);
end;

procedure TFootPrintFile.WriteTaggedBooleanArray(Tag: string;
  Value: TTwoDBooleanArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
  ArrayNode: TXmlNode;
  DirectNode: TXmlNode;
  RowNode: TXmlNode;
  ColumnNode: TXmlNode;
  ValueNode: TXmlNode;
  UniformValue: Boolean;
  UniformNode: TXmlNode;
  function IsUniform(out UniformValue: Boolean): Boolean;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    UniformValue := Value[0,0];
    result := False;
    for RowIndex := 0 to Length(Value) - 1 do
    begin
      for ColIndex := 0 to Length(Value[RowIndex]) - 1 do
      begin
        if Value[RowIndex,ColIndex] <> UniformValue then
        begin
          Exit;
        end;
      end;
    end;
    Result := True;
  end;
begin
  Assert(FSettingsFileWriter <> nil);
  Assert(FSettingsFileWriter.DocumentElement <> nil);
  ArrayNode := FSettingsFileWriter.DocumentElement.AddChild(Tag);
  if IsUniform(UniformValue) then  
  begin  
    UniformNode := ArrayNode.AddChild(UniformTag);
    if UniformValue then
    begin
      UniformNode.NodeValue := 'True';
    end
    else
    begin
      UniformNode.NodeValue := 'False';
    end;
  end    
  else  
  begin  
    DirectNode := ArrayNode.AddChild(DirectTag);

    for RowIndex := 0 to Length(Value) - 1 do
    begin
      RowNode := nil;

      for ColIndex := 0 to Length(Value[RowIndex]) - 1 do
      begin
        if not Value[RowIndex,ColIndex] then
        begin
          if RowNode = nil then
          begin
            RowNode := DirectNode.AddChild(RowTag);
            RowNode.NodeValue := IntToStr(RowIndex+1);
          end;
          ColumnNode := RowNode.AddChild(ColumnTag);
          ColumnNode.NodeValue := IntToStr(ColIndex+1);

          ValueNode := ColumnNode.AddChild(ValueTag);
            ValueNode.NodeValue := 'False';
        end;
      end;
    end;
  end;
end;

procedure TFootPrintFile.WriteTaggedRealArray(Tag: string;
  Value: TTwoDRealArray);
var
  RowIndex: Integer;
  ColIndex: Integer;
  ArrayNode: TXmlNode;
  DirectNode: TXmlNode;
  RowNode: TXmlNode;
  ColumnNode: TXmlNode;
  ValueNode: TXmlNode;
  UniformNode: TXmlNode;
  UniformValue: Double;
  function IsUniform(out UniformValue: Double): Boolean;
  var
    RowIndex: Integer;
    ColIndex: Integer;
  begin
    UniformValue := Value[0,0];
    result := False;
    for RowIndex := 0 to Length(Value) - 1 do
    begin
      for ColIndex := 0 to Length(Value[RowIndex]) - 1 do
      begin
        if Value[RowIndex,ColIndex] <> UniformValue then
        begin
          Exit;
        end;
      end;
    end;
    Result := True;
  end;
begin
  Assert(FSettingsFileWriter <> nil);
  Assert(FSettingsFileWriter.DocumentElement <> nil);
  ArrayNode := FSettingsFileWriter.DocumentElement.AddChild(Tag);

  if IsUniform(UniformValue) then  
  begin  
    UniformNode := ArrayNode.AddChild(UniformTag);
    UniformNode.NodeValue := FortranFloatToStr(UniformValue)
  end    
  else  
  begin  
    DirectNode := ArrayNode.AddChild(DirectTag);

    for RowIndex := 0 to Length(Value) - 1 do
    begin
      RowNode := nil;

      for ColIndex := 0 to Length(Value[RowIndex]) - 1 do
      begin
        if Value[RowIndex,ColIndex] <> 0 then
        begin
          if RowNode = nil then
          begin
            RowNode := DirectNode.AddChild(RowTag);
            RowNode.NodeValue := IntToStr(RowIndex+1);
          end;
          ColumnNode := RowNode.AddChild(ColumnTag);
          ColumnNode.NodeValue := IntToStr(ColIndex+1);

          ValueNode := ColumnNode.AddChild(ValueTag);
          ValueNode.NodeValue := FortranFloatToStr(Value[RowIndex,ColIndex]);
        end;
      end;
    end;
  end;    
end;

procedure TFootPrintFile.WriteTaggedString(Tag, Value: string);
var
  ChildNode: TXmlNode;
begin
  if Value <> '' then
  begin
    Assert(FSettingsFileWriter <> nil);
    Assert(FSettingsFileWriter.DocumentElement <> nil);
    ChildNode := FSettingsFileWriter.DocumentElement.AddChild(Tag);
    ChildNode.NodeValue := Value;
  end;
end;

procedure TFootPrintFile.WriteTaggedBoolean(Tag: string; Value: Boolean);
var
  ChildNode: TXmlNode;
begin
  Assert(FSettingsFileWriter <> nil);
  Assert(FSettingsFileWriter.DocumentElement <> nil);
  ChildNode := FSettingsFileWriter.DocumentElement.AddChild(Tag);
  if Value then
  begin
    ChildNode.NodeValue := 'True';
  end
  else
  begin
    ChildNode.NodeValue := 'False';
  end;
end;

end.
