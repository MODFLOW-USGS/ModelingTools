unit AsciiRasterReaderUnit;

// http://webhelp.esri.com/arcgisdesktop/9.1/index.cfm?TopicName=Raster%20to%20ASCII%20(Conversion)

interface

uses SysUtils, Classes, FastGEO, ComCtrls, Forms, RasterValuesAlongSegmentsUnit;

Type
  EAsciiRasterError = class(Exception);

  TCoordPos = (cpCorner, cpCenter);

  TRasterHeader = record
    NumberOfColumns: integer;
    NumberOfRows: integer;
    XCoordPos: TCoordPos;
    YCoordPos: TCoordPos;
    LowerLeftX: Extended;
    LowerLeftY: Extended;
    CellSize: Extended;
    IgnoreValue: Extended;
  end;

  TOnReadPointEvent = procedure (Sender: TObject; Point: TPoint3D) of object;

  TZArray = array of array of Extended;

  TAsciiRaster = class(TInterfacedObject, IRaster)
  private
    FRasterHeader: TRasterHeader;
    FZ: TZArray;
    function GetLowerLeft: TPoint2D;
    function GetXCount: integer;
    function GetYCount: integer;
    function GetXSpacing: Double;
    function GetYSpacing: Double;
    function GetZ(XIndex, YIndex: Integer): Double;
    function GetIgnore(XIndex, YIndex: Integer): Boolean;
  public
    // @name is the lower left corner of the grid. The data point for the
    // lower left cells is at
    // (LowerLeft.x + (XSpacing/2), LowerLeft.y + (YSpacing/2)).
    // ESRI ASCII grid files follow this format. Surfer Grid files need to
    // be adjusted because they give the coordinates of the cell centers
    // directly.
    property LowerLeft: TPoint2D read GetLowerLeft;
    property XCount: Integer read GetXCount;
    property YCount: Integer read GetYCount;
    property XSpacing: Double read GetXSpacing;
    property YSpacing: Double read GetYSpacing;
    property Z[XIndex, YIndex: Integer]: double read GetZ;
    property Ignore[XIndex, YIndex: Integer]: Boolean read GetIgnore;
  end;

  TAsciiRasterReader = class(TObject)
  private
    RasterFile: TextFile;
    FOnReadPoint: TOnReadPointEvent;
    FFileName: string;
    function ReadHeader(var ShouldReadNoDataValue: boolean): TRasterHeader;
    procedure ReadValues(var Values: TPoint3DArray; RasterHeader: TRasterHeader;
      Progress : TProgressBar);
    procedure ReadZValues(var Values: TZArray; RasterHeader: TRasterHeader;
      Progress : TProgressBar);
    procedure SetFileName(const Value: string);
    function GetValidFileHeader: boolean;
    function GetFileHeader: TRasterHeader;
  public
    procedure ReadAsciiRaster(var Values: TPoint3DArray;
      Progress : TProgressBar = nil); overload;
    // This version of @name is intended to be used with @link(OnReadPoint).
    procedure ReadAsciiRaster(Progress : TProgressBar = nil); overload;
    procedure ReadAsciiRaster(Raster: TAsciiRaster;
      Progress : TProgressBar = nil); overload;
    property OnReadPoint: TOnReadPointEvent read FOnReadPoint write FOnReadPoint;
    property FileName: string read FFileName write SetFileName;
    property ValidFileHeader: boolean read GetValidFileHeader;
    property FileHeader: TRasterHeader read GetFileHeader;
  end;

implementation

uses ModelMuseUtilities;

resourcestring
  StrErrorReadingNCOL = 'Error reading number of columns';
  StrErrorReadingNROW = 'Error reading number of rows';
  StrErrorReadingXOrig = 'Error reading X origin';
  StrErrorReadingYOrig = 'Error reading Y origin';
  StrErrorReadingCellS = 'Error reading cell size';
  StrErrorReadingNoDat = 'Error reading No-Data-Value';
  StrSDoesNotExist = '%s does not exist.';
  StrErrorCallingTAscii = 'Error calling TAsciiRasterReader.ReadValues';
  StrErrorReadingDataV = 'Error reading data value for row = %0:d, column = ' +
  '%1:d. Value to be converted = %2:s.';
//  StrThereWasAnErrorW = 'There was an error when attempting to access the fi' +
//  'le. Please check that the file is not locked by another process and that ' +
//  'the file is an ASCII raster.';

{ TAsciiRasterReader }

function TAsciiRasterReader.GetFileHeader: TRasterHeader;
var
  ShouldReadNoDataValue: Boolean;
begin
  if not FileExists(FileName) then
  begin
    raise EAsciiRasterError.Create(Format(StrSDoesNotExist, [FileName]));
  end;
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    ShouldReadNoDataValue := True;
    result := ReadHeader(ShouldReadNoDataValue);
  finally
    CloseFile(RasterFile);
  end;
end;

function TAsciiRasterReader.GetValidFileHeader: boolean;
var
  ShouldReadNoDataValue: Boolean;
begin
  if FileExists(FileName) then
  begin
    try
      AssignFile(RasterFile, FileName);
      try
        Reset(RasterFile);
        ShouldReadNoDataValue := True;
        ReadHeader(ShouldReadNoDataValue);
      finally
        CloseFile(RasterFile);
      end;
      result := True;
    except
      on E: EAsciiRasterError do
      begin
        result := False;
      end;
      on E: EInOutError do
      begin
//        Beep;
//        MessageDlg(StrThereWasAnErrorW, mtError, [mbOK], 0);
        result := False;
      end;
    end;
  end
  else
  begin
    result := False;
  end;
end;

procedure TAsciiRasterReader.ReadAsciiRaster(Progress: TProgressBar);
var
  ShouldReadNoDataValue: Boolean;
  RasterHeader: TRasterHeader;
  Values: TPoint3DArray;
begin
  // Determine if the NODATA_VALUE should be read.
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    ShouldReadNoDataValue := True;
    ReadHeader(ShouldReadNoDataValue);
  finally
    CloseFile(RasterFile);
  end;

  // Read file.
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    RasterHeader := ReadHeader(ShouldReadNoDataValue);
    Values := nil;
    ReadValues(Values, RasterHeader, Progress);
  finally
    CloseFile(RasterFile);
  end;
end;

function TAsciiRasterReader.ReadHeader(
  var ShouldReadNoDataValue: boolean): TRasterHeader;
var
  ALine: string;
  Lines: TStringList;
  AString: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Delimiter := ' ';
    
    ReadLn(RasterFile, ALine);
    Lines.DelimitedText := ALine;
    if (Lines.Count <> 2)  or (CompareText(Trim(Lines[0]), 'NCOLS') <> 0) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingNCOL);
    end;
    if not TryStrToInt(Lines[1], result.NumberOfColumns) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingNCOL);
    end;

    ReadLn(RasterFile, ALine);
    Lines.DelimitedText := ALine;
    if (Lines.Count <> 2)  or (CompareText(Trim(Lines[0]), 'NROWS') <> 0) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingNROW);
    end;
    if not TryStrToInt(Lines[1], result.NumberOfRows) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingNROW);
    end;

    ReadLn(RasterFile, ALine);
    Lines.DelimitedText := ALine;
    if (Lines.Count <> 2) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingXOrig);
    end;
    AString := Trim(Lines[0]);
    if CompareText(AString, 'XLLCORNER') = 0 then
    begin
      result.XCoordPos := cpCorner;
    end
    else if CompareText(AString, 'XLLCENTER') = 0 then
    begin
      result.XCoordPos := cpCenter;
    end
    else
    begin
      raise EAsciiRasterError.Create(StrErrorReadingXOrig);
    end;
    try
      result.LowerLeftX := FortranStrToFloat(Trim(Lines[1]));
    except on E: EConvertError do
      raise EAsciiRasterError.Create(StrErrorReadingXOrig);
    end;

    ReadLn(RasterFile, ALine);
    Lines.DelimitedText := ALine;
    if (Lines.Count <> 2) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingYOrig);
    end;
    AString := Trim(Lines[0]);
    if CompareText(AString, 'YLLCORNER') = 0 then
    begin
      result.YCoordPos := cpCorner;
    end
    else if CompareText(AString, 'YLLCENTER') = 0 then
    begin
      result.YCoordPos := cpCenter;
    end
    else
    begin
      raise EAsciiRasterError.Create(StrErrorReadingYOrig);
    end;
    try
      result.LowerLeftY := FortranStrToFloat(Trim(Lines[1]));
    except on E: EConvertError do
      raise EAsciiRasterError.Create(StrErrorReadingYOrig);
    end;

    ReadLn(RasterFile, ALine);
    Lines.DelimitedText := ALine;
    if (Lines.Count <> 2)
      or (CompareText(Trim(Lines[0]), 'CELLSIZE') <> 0) then
    begin
      raise EAsciiRasterError.Create(StrErrorReadingCellS);
    end;
    try
      result.CellSize := FortranStrToFloat(Trim(Lines[1]));
    except on EConvertError do
      raise EAsciiRasterError.Create(StrErrorReadingCellS);
    end;

    result.IgnoreValue := -9999;
    if ShouldReadNoDataValue then
    begin
      ReadLn(RasterFile, ALine);
      Lines.DelimitedText := ALine;
      if Lines.Count = 0 then
      begin
        raise EAsciiRasterError.Create(StrErrorReadingNoDat);
      end;
      if CompareText(Trim(Lines[0]), 'NODATA_VALUE') = 0 then
      begin
        if (Lines.Count = 2) then
        begin
          try
            result.IgnoreValue := FortranStrToFloat(Trim(Lines[1]));
          except on EConvertError do
            raise EAsciiRasterError.Create(StrErrorReadingNoDat);
          end;
        end
        else
        begin
          raise EAsciiRasterError.Create(StrErrorReadingNoDat);
        end;
      end
      else
      begin
        ShouldReadNoDataValue := False;
      end;
    end;

  finally
    Lines.Free;
  end;
end;

procedure TAsciiRasterReader.ReadValues(var Values: TPoint3DArray;
  RasterHeader: TRasterHeader; Progress : TProgressBar);
var
  ColIndex: Integer;
  RowIndex: Integer;
  PointCount: Integer;
  LineIndex: Integer;
  Lines: TStringList;
  AValue: Extended;
  AString: string;
  ALine: string;
  APoint: TPoint3D;
  UpperLeftY: double;
begin
  if(not Assigned(Values)) and not (Assigned(OnReadPoint)) then
  begin
    raise EAsciiRasterError.Create(StrErrorCallingTAscii);
  end;
  Lines := TStringList.Create;
  try
    Lines.Delimiter := ' ';
    LineIndex := 0;
    PointCount := 0;
    if Progress <> nil then
    begin
      Progress.Max := RasterHeader.NumberOfRows;
      Progress.Step := 1;
      Progress.Position := 0;
    end;
    UpperLeftY := RasterHeader.LowerLeftY +
      RasterHeader.CellSize * RasterHeader.NumberOfRows;
    for RowIndex := 0 to RasterHeader.NumberOfRows - 1 do
    begin
      for ColIndex := 0 to RasterHeader.NumberOfColumns - 1 do
      begin
        if LineIndex = Lines.Count then
        begin
          ReadLn(RasterFile, ALine);
          Lines.DelimitedText := ALine;
          LineIndex := 0;
        end;
        AString := Trim(Lines[LineIndex]);
        try
          AValue := FortranStrToFloat(AString);
        except on EConvertError do
          raise EAsciiRasterError.Create(Format(StrErrorReadingDataV,
            [RowIndex + 1, ColIndex + 1, AString]));
        end;
        if AValue <> RasterHeader.IgnoreValue then
        begin

          case RasterHeader.XCoordPos of
            cpCorner:
              begin
                APoint.x := RasterHeader.LowerLeftX
                  + RasterHeader.CellSize * ColIndex
                  + RasterHeader.CellSize / 2;
              end;
            cpCenter:
              begin
                APoint.x := RasterHeader.LowerLeftX
                  + RasterHeader.CellSize * ColIndex;
              end;
          else
            Assert(False);
          end;
          case RasterHeader.YCoordPos of
            cpCorner:
              begin
                APoint.y := UpperLeftY
                  - RasterHeader.CellSize * RowIndex
                  - RasterHeader.CellSize / 2;
              end;
            cpCenter:
              begin
                APoint.y := UpperLeftY
                  - RasterHeader.CellSize * RowIndex;
              end;
          else
            Assert(False);
          end;
          APoint.z := AValue;
          if Assigned(Values) then
          begin
            Values[PointCount] := APoint;
          end;
          if Assigned(OnReadPoint) then
          begin
            OnReadPoint(self, APoint);
          end;
          Inc(PointCount);
        end;
        Inc(LineIndex);
      end;
      if Progress <> nil then
      begin
        Progress.StepIt;
        Application.ProcessMessages;
      end;
    end;
  finally
    Lines.Free;
  end;
  if Assigned(Values) then
  begin
    SetLength(Values, PointCount);
  end;
end;

procedure TAsciiRasterReader.ReadZValues(var Values: TZArray;
  RasterHeader: TRasterHeader; Progress: TProgressBar);
var
  ColIndex: Integer;
  RowIndex: Integer;
  LineIndex: Integer;
  Lines: TStringList;
  AValue: Extended;
  AString: string;
  ALine: string;
  APoint: TPoint3D;
begin
  if not Assigned(Values) then
  begin
    raise EAsciiRasterError.Create(StrErrorCallingTAscii);
  end;
  Lines := TStringList.Create;
  try
    Lines.Delimiter := ' ';
    LineIndex := 0;
    if Progress <> nil then
    begin
      Progress.Max := RasterHeader.NumberOfRows;
      Progress.Step := 1;
      Progress.Position := 0;
    end;
    for RowIndex := 0 to RasterHeader.NumberOfRows - 1 do
    begin
      for ColIndex := 0 to RasterHeader.NumberOfColumns - 1 do
      begin
        if LineIndex = Lines.Count then
        begin
          ReadLn(RasterFile, ALine);
          Lines.DelimitedText := ALine;
          LineIndex := 0;
        end;
        AString := Trim(Lines[LineIndex]);
        try
          AValue := FortranStrToFloat(AString);
        except on EConvertError do
          raise EAsciiRasterError.Create(Format(StrErrorReadingDataV,
            [RowIndex + 1, ColIndex + 1, AString]));
        end;

        Values[RowIndex,ColIndex] := AValue;

        if Assigned(OnReadPoint) then
        begin
          OnReadPoint(self, APoint);
        end;
        Inc(LineIndex);
      end;
      if Progress <> nil then
      begin
        Progress.StepIt;
        Application.ProcessMessages;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TAsciiRasterReader.SetFileName(const Value: string);
begin
  FFileName := Value;
  if not FileExists(FFileName) then
  begin
    raise EAsciiRasterError.Create(Format(StrSDoesNotExist, [FileName]));
  end;
end;

procedure TAsciiRasterReader.ReadAsciiRaster(var Values: TPoint3DArray;
  Progress : TProgressBar = nil);
var
  ShouldReadNoDataValue: Boolean;
  RasterHeader: TRasterHeader;
begin
  if not FileExists(FileName) then
  begin
    raise EAsciiRasterError.Create(Format(StrSDoesNotExist, [FileName]));
  end;

  // Determine if the NODATA_VALUE should be read.
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    ShouldReadNoDataValue := True;
    ReadHeader(ShouldReadNoDataValue);
  finally
    CloseFile(RasterFile);
  end;

  // Read file.
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    RasterHeader := ReadHeader(ShouldReadNoDataValue);
    SetLength(Values, RasterHeader.NumberOfRows * RasterHeader.NumberOfColumns);
    ReadValues(Values, RasterHeader, Progress);
  finally
    CloseFile(RasterFile);
  end;
end;

procedure TAsciiRasterReader.ReadAsciiRaster(Raster: TAsciiRaster;
  Progress: TProgressBar);
var
  ShouldReadNoDataValue: Boolean;
begin
  if not FileExists(FileName) then
  begin
    raise EAsciiRasterError.Create(Format(StrSDoesNotExist, [FileName]));
  end;

  // Determine if the NODATA_VALUE should be read.
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    ShouldReadNoDataValue := True;
    ReadHeader(ShouldReadNoDataValue);
  finally
    CloseFile(RasterFile);
  end;

  // Read file.
  AssignFile(RasterFile, FileName);
  try
    Reset(RasterFile);
    Raster.FRasterHeader := ReadHeader(ShouldReadNoDataValue);
    SetLength(Raster.FZ, Raster.FRasterHeader.NumberOfRows,
      Raster.FRasterHeader.NumberOfColumns);
    ReadZValues(Raster.FZ, Raster.FRasterHeader, Progress);
  finally
    CloseFile(RasterFile);
  end;
end;

{ TAsciiRaster }

function TAsciiRaster.GetIgnore(XIndex, YIndex: Integer): Boolean;
begin
  result := FZ[FRasterHeader.NumberOfRows - 1 - YIndex, XIndex]
    = FRasterHeader.IgnoreValue;
end;

function TAsciiRaster.GetLowerLeft: TPoint2D;
begin
  result.x := FRasterHeader.LowerLeftX;
  result.Y := FRasterHeader.LowerLeftY;
  if FRasterHeader.XCoordPos = cpCenter then
  begin
    result.x := result.x - FRasterHeader.CellSize;
  end;
  if FRasterHeader.YCoordPos = cpCenter then
  begin
    result.Y := result.Y - FRasterHeader.CellSize;
  end;
end;

function TAsciiRaster.GetXCount: integer;
begin
  Result := FRasterHeader.NumberOfColumns;
end;

function TAsciiRaster.GetXSpacing: Double;
begin
  Result := FRasterHeader.CellSize;
end;

function TAsciiRaster.GetYCount: integer;
begin
  Result := FRasterHeader.NumberOfRows;
end;

function TAsciiRaster.GetYSpacing: Double;
begin
  Result := FRasterHeader.CellSize;
end;

function TAsciiRaster.GetZ(XIndex, YIndex: Integer): Double;
begin
  Result := FZ[FRasterHeader.NumberOfRows - 1 - YIndex, XIndex];
end;

end.
