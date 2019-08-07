{
See http://nationalmap.gov/standards/demstds.html for the supported format.
}
unit DemReaderUnit;

interface

uses SysUtils, Classes, Graphics, Contnrs, Forms, RealListUnit, ShapefileUnit;

type
  TCornerPoint = class(TObject)
  private
    FX: double;
    FY: double;
    procedure SetX(const Value: double);
    procedure SetY(const Value: double);
  public
    property X: double read FX write SetX;
    property Y: double read FY write SetY;
  end;

  TElevationPoint = class(TCornerPoint)
  private
    FElevation: double;
    FValue: integer;
    procedure SetElevation(const Value: double);
    procedure SetValue(const Value: integer);
  public
    property Elevation : double read FElevation write SetElevation;
    // used to identify values to ignore.
    property Value: integer read FValue write SetValue;
  end;

  TDemReader = class(TObject)
  private
    DemFile : File;
    CharCount: Integer;
    FCorners: TList;
    RangeList: TRealList;
    Angle: Double;
    ResolutionX: Double;
    ResolutionY: Double;
    ResolutionZ: Double;
    ColumnCount: Integer;
    FCancel: Boolean;
    FPoints: TList;
    FCentralMeridianRadians: Double;
    FCoordInSec: Boolean;
    FOnProgress: TProgressProcedure;
    function ReadCharacters(const Count: integer; Out EndOfLine: boolean): AnsiString;
    procedure ReadRecordA(const GetCentralMeridian : boolean);
    procedure ReadRecordB;
    procedure ReadRestOfRecord;
    procedure ReadRecordC;
    function FortranStrToFloat(AString: string): double;
    function UTMCentralMeridianRadians(LatitudeSeconds,
      LongitudeSeconds: double): double;
    function GetPoint(Index: integer): TElevationPoint;
    function GetPointCount: integer;
    function GetCorner(Index: integer): TCornerPoint;
    function GetCornerCount: Integer;
  public
    property Corners[Index: integer]: TCornerPoint read GetCorner;
    property CornerCount: Integer read GetCornerCount;
    property CoordInSec: Boolean read FCoordInSec;
    property CentralMeridianRadians: Double read FCentralMeridianRadians
      write FCentralMeridianRadians;
    procedure ReadFile(const FileName: string; ReadCentralMeridian : boolean);
    procedure ReadHeader(const FileName: string);
    Constructor Create;
    Destructor Destroy; override;
    procedure Cancel;
    property PointCount: integer read GetPointCount;
    property Points[Index: integer]: TElevationPoint read GetPoint;
    property OnProgress: TProgressProcedure read FOnProgress write FOnProgress;
  end;

implementation

uses
  CoordinateConversionUnit;

{ TDemReader }

procedure TDemReader.Cancel;
begin
  FCancel := True;
end;

constructor TDemReader.Create;
begin
  inherited;
  RangeList := TRealList.Create;
  FPoints := TObjectList.Create;
  FCorners := TObjectList.Create;
end;

destructor TDemReader.Destroy;
begin
  FCorners.Free;
  FPoints.Free;
  RangeList.Free;
  inherited;
end;

function TDemReader.FortranStrToFloat(AString: string): double;
var
  DPos : integer;
  Sub : string;
begin
  AString := Trim(AString);
  DPos := Pos('d', AString);
  if DPos > 0 then
  begin
    AString[DPos] := 'e';
  end;
  DPos := Pos('D', AString);
  if DPos > 0 then
  begin
    AString[DPos] := 'E';
  end;
  if FormatSettings.DecimalSeparator <> '.' then
  begin
    DPos := Pos(FormatSettings.DecimalSeparator, AString);
    if DPos > 0 then
    begin
      AString[DPos] := FormatSettings.DecimalSeparator;
    end;
  end;
  Sub := Copy(AString, 2, Length(AString));
  DPos := Pos('+', Sub);
  if DPos > 0 then
  begin
    if (AString[DPos] <> 'e') and (AString[DPos] <> 'E') then
    begin
      AString := Copy(AString, 1, DPos) + 'E' + Copy(AString, DPos + 1, Length(AString))
    end;
  end;
  DPos := Pos('-', Sub);
  if DPos > 0 then
  begin
    if (AString[DPos] <> 'e') and (AString[DPos] <> 'E') then
    begin
      AString := Copy(AString, 1, DPos) + 'E' + Copy(AString, DPos + 1, Length(AString))
    end;
  end;
  result := StrToFloat(AString);
end;

function TDemReader.GetCorner(Index: integer): TCornerPoint;
begin
  result := FCorners[Index];
end;

function TDemReader.GetCornerCount: Integer;
begin
  result := FCorners.Count;
end;

function TDemReader.GetPoint(Index: integer): TElevationPoint;
begin
  result := FPoints[Index];
end;

function TDemReader.GetPointCount: integer;
begin
  result := FPoints.Count;
end;

function TDemReader.ReadCharacters(const Count: integer;
  out EndOfLine: boolean): AnsiString;
var
  I : integer;
  AChar : AnsiChar;
  I2: integer;
begin
  EndOfLine := False;
  SetLength(result,Count);
  CharCount := CharCount + Count;
  I2 := 0;
  for I := 1 to Count do
  begin
    try
      BlockRead(DemFile,AChar, 1);
    except On EInOutError do
      begin
        EndOfLine := True;
        SetLength(result, I2);
        Exit;
      end;
    end;
    if CharInSet(AChar, [#10, #13]) then
    begin
      if I = 1 then
      begin
        Continue;
      end
      else
      begin
        EndOfLine := True;
        SetLength(result, I2);
        Exit;
      end;
    end;
    Inc(I2);
    result[I2] := AChar;
  end;
end;

procedure TDemReader.ReadFile(const FileName: string; ReadCentralMeridian : boolean);
var
  Index: Integer;
  OldDecSep: Char;
begin
  OldDecSep := FormatSettings.DecimalSeparator;
  FCancel := False;
  AssignFile(DemFile, FileName);

  try
    FormatSettings.DecimalSeparator := '.';
    Reset(DemFile, SizeOf(AnsiChar));
    ReadRecordA(ReadCentralMeridian);
    for Index := 1 to ColumnCount do
    begin
      ReadRecordB;
      if Assigned(FOnProgress) then
      begin
        FOnProgress(self, Index/ColumnCount);
      end;
    end;
    ReadRecordC;

  finally
    FormatSettings.DecimalSeparator := OldDecSep;
    CloseFile(DemFile);
  end;
end;

procedure TDemReader.ReadHeader(const FileName: string);
var
  OldDecSep: Char;
  CanCloseFile: boolean;
begin
  CanCloseFile := True;
  FCancel := False;
  OldDecSep := FormatSettings.DecimalSeparator;
  AssignFile(DemFile, FileName);
  try
    FormatSettings.DecimalSeparator := '.';
    try
      Reset(DemFile, SizeOf(AnsiChar));
    except on EInOutError do
      begin
        CanCloseFile := False;
        raise;
      end;
    end;
    ReadRecordA(True);
  finally
    FormatSettings.DecimalSeparator := OldDecSep;
    if CanCloseFile then
    begin
      CloseFile(DemFile);
    end;
  end;
end;

procedure TDemReader.ReadRecordA(const GetCentralMeridian: boolean);
const
  CornerCount = 4;
var
  Index : integer;
  ColumncountString : string;
  AString : string;
  XString, YString : string;
  X, Y : double;
  CornerPoint : TCornerPoint;
  MinOrMax : double;
  EndOfLine: boolean;
  DemName: AnsiString;
  Free_Format_Text: AnsiString;
  Filler: AnsiString;
  Process_Code: AnsiString;
  Sectional_Indicator: AnsiString;
  MC_origin_code: AnsiString;
  Code: AnsiString;
  Number: AnsiString;
  Accuracy_code: AnsiString;
  Rows: string;
  Largest: AnsiString;
  Source: AnsiString;
  Smallest: AnsiString;
  Data_source_date: AnsiString;
  Inspection: AnsiString;
  Data_date: AnsiString;
  Suspect: AnsiString;
  Vertical_datum: AnsiString;
  Percent_Void: AnsiString;
  DEM_level_code: AnsiString;
  Planimetric_code: AnsiString;
  Data_validation_flag: AnsiString;
  Horizontal_datum: AnsiString;
  Data_Edition: AnsiString;
begin
  FCorners.Clear;
  CharCount := 0;
  RangeList.Clear;
  DemName := ReadCharacters(40, EndOfLine);
  if EndOfLine then Exit;
  Free_Format_Text :=  ReadCharacters(40, EndOfLine);
  if EndOfLine then Exit;
  Filler := ReadCharacters(55, EndOfLine);
  if EndOfLine then Exit;
  Process_Code :=  ReadCharacters(1, EndOfLine);
  if EndOfLine then Exit;
  Filler := ReadCharacters(1, EndOfLine);
  if EndOfLine then Exit;
  Sectional_Indicator :=  ReadCharacters(3, EndOfLine);
  if EndOfLine then Exit;
  MC_origin_code := ReadCharacters(4, EndOfLine);
  if EndOfLine then Exit;
  DEM_level_code := ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;

  //  Code defining elevation pattern (regular or random)
  Code := ReadCharacters(6, EndOfLine);

  // Code defining ground planimetric reference system
  AString := string(ReadCharacters(6, EndOfLine));
  FCoordInSec := StrToInt(Trim(AString)) = 0;
  if EndOfLine then Exit;

  //  Code defining zone in ground planimetric reference system
  Code := ReadCharacters(6, EndOfLine);
  
  if EndOfLine then Exit;
  for Index := 1 to 15 do
  begin
    Planimetric_code :=  ReadCharacters(24, EndOfLine);
    if EndOfLine then Exit;
  end;
  // Code defining unit of measure for ground planimetric coordinates throughout the file
  Code := ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;

  //  Code defining unit of measure for elevation coordinates throughout the file
  Code := ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;

  //  Number (n) of sides in the polygon which defines the coverage of the DEM file
  Number := ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;
  for Index := 1 to CornerCount do
  begin
    XString := string(ReadCharacters(24, EndOfLine));
    if EndOfLine then Exit;
    YString := string(ReadCharacters(24, EndOfLine));
    X := FortranStrToFloat(XString);
    Y := FortranStrToFloat(YString);

    CornerPoint := TCornerPoint.Create;
    try
      CornerPoint.X := X;
      CornerPoint.Y := Y;
      FCorners.Add(CornerPoint);
    except
      CornerPoint.Free;
      raise;
    end;
    if EndOfLine then Exit;


  end;

  if CoordInSec and (FCorners.Count > 0) then
  begin
    if GetCentralMeridian then
    begin
      X := 0;
      Y := 0;
      for Index := FCorners.Count - CornerCount to FCorners.Count -1 do
      begin
        CornerPoint := Corners[Index];
        X := X + CornerPoint.X;
        Y := Y + CornerPoint.Y;
      end;
      X := X/CornerCount;
      Y := Y/CornerCount;
      FCentralMeridianRadians := UTMCentralMeridianRadians(Y,X);
    end;

  end;
  // min or max elevation
  AString := string(ReadCharacters(24, EndOfLine));
  MinOrMax := FortranStrToFloat(AString);
  RangeList.Add(MinOrMax);
  if EndOfLine then Exit;

  // min or max elevation
  AString := string(ReadCharacters(24, EndOfLine));
  MinOrMax := FortranStrToFloat(AString);
  RangeList.Add(MinOrMax);
  if EndOfLine then Exit;

  // angle (radians)
  AString := string(ReadCharacters(24, EndOfLine));
  Angle := FortranStrToFloat(AString);
  if EndOfLine then Exit;

  Accuracy_code := ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;

  AString := string(ReadCharacters(12, EndOfLine));
  ResolutionX := FortranStrToFloat(AString);
  if EndOfLine then Exit;

  AString := string(ReadCharacters(12, EndOfLine));
  ResolutionY := FortranStrToFloat(AString);
  if EndOfLine then Exit;

  AString := string(ReadCharacters(12, EndOfLine));
  ResolutionZ := FortranStrToFloat(AString);
  if EndOfLine then Exit;

  Rows := string(ReadCharacters(6, EndOfLine));
  if EndOfLine then Exit;

  ColumncountString := string(ReadCharacters(6, EndOfLine));
  ColumnCount := StrToInt(ColumncountString);
  if EndOfLine then Exit;

  FPoints.Clear;
  FPoints.Capacity := ColumnCount + StrToInt(Rows);

  // Largest primary contour interval
  Largest := ReadCharacters(5, EndOfLine);
  if EndOfLine then Exit;

  // source contour interval units
  Source := ReadCharacters(1, EndOfLine);
  if EndOfLine then Exit;

  // Smallest primary contour interval
  Smallest := ReadCharacters(5, EndOfLine);
  if EndOfLine then Exit;

  // source contour interval units
  Source := ReadCharacters(1, EndOfLine);
  if EndOfLine then Exit;

  // Data source date
  Data_source_date := ReadCharacters(4, EndOfLine);
  if EndOfLine then Exit;

  // Data inspection/revision date
  Data_date := ReadCharacters(4, EndOfLine);
  if EndOfLine then Exit;

  // Inspection/revision flag
  Inspection := ReadCharacters(1, EndOfLine);
  if EndOfLine then Exit;

  // Data validation flag
  Data_validation_flag := ReadCharacters(1, EndOfLine);
  if EndOfLine then Exit;

  // Suspect and void area flag
  Suspect := ReadCharacters(2, EndOfLine);
  if EndOfLine then Exit;

  // Vertical datum
  Vertical_datum := ReadCharacters(2, EndOfLine);
  if EndOfLine then Exit;

  // Horizontal datum
  Horizontal_datum := ReadCharacters(2, EndOfLine);
  if EndOfLine then Exit;

  // Data Edition
  Data_Edition := ReadCharacters(4, EndOfLine);
  if EndOfLine then Exit;

  // Percent Void
  Percent_Void := ReadCharacters(4, EndOfLine);
  if EndOfLine then Exit;
  ReadRestOfRecord;
end;

procedure TDemReader.ReadRecordB;
var
  m, n : integer;
  mString, nString : string;
  Index : integer;
  FirstX, FirstY : double;
  FirstXString, FirstYString, DatumString : string;
  X, Y, Datum : double;
  AString : string;
  ElevationPoint : TElevationPoint;
  EndOfLine: boolean;
begin
  CharCount := 0;
  // row
  ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;

  // column
  ReadCharacters(6, EndOfLine);
  if EndOfLine then Exit;

  mString := string(ReadCharacters(6, EndOfLine));
  nString := string(ReadCharacters(6, EndOfLine));
  if EndOfLine then Exit;
  FirstXString := string(ReadCharacters(24, EndOfLine));
  if EndOfLine then Exit;
  FirstYString := string(ReadCharacters(24, EndOfLine));
  if EndOfLine then Exit;
  DatumString := string(ReadCharacters(24, EndOfLine));
  if EndOfLine then Exit;

  // min or max
  ReadCharacters(24, EndOfLine);
  if EndOfLine then Exit;

  // min or max
  ReadCharacters(24, EndOfLine);
  if EndOfLine then Exit;

  FirstX := FortranStrToFloat(FirstXString);
  FirstY := FortranStrToFloat(FirstYString);
  Datum := FortranStrToFloat(DatumString);
  m := StrToInt(Trim(mString));
  n := StrToInt(Trim(nString));
  for Index := 1 to m * n do
  begin
    if Index mod 100 = 0 then
    begin
      Application.ProcessMessages;
      if FCancel then Exit;
    end;
    if CharCount > 1024 -6 then
    begin
      ReadRestOfRecord;
    end;
    if angle = 0 then
    begin
      X := FirstX;
      Y := FirstY + (Index-1) * ResolutionY;
    end
    else
    begin
      X := FirstX + (Index-1) * ResolutionX * Sin(angle);
      Y := FirstY + (Index-1) * ResolutionY * Cos(angle);
    end;

    if CoordInSec then
    begin
      ConvertToUTM(Y/60/60/180*PI, X/60/60/180*PI, CentralMeridianRadians,
        X, Y);
    end;

    AString := string(ReadCharacters(6, EndOfLine));
    if EndOfLine then Exit;


    ElevationPoint := TElevationPoint.Create;
    try
      ElevationPoint.X := X;
      ElevationPoint.Y := Y;
      ElevationPoint.Value := StrToInt(AString);
      ElevationPoint.Elevation := ElevationPoint.Value * ResolutionZ + Datum;
      FPoints.Add(ElevationPoint);
    except
      ElevationPoint.Free;
    end;

  end;
  if EndOfLine then Exit;
  ReadRestOfRecord

end;

procedure TDemReader.ReadRecordC;
var
  EndOfLine: boolean;
begin
  Try
    // Code indicating availability of statistics in data element 2
    ReadCharacters(6, EndOfLine);
    if EndOfLine then Exit;
    // RMSE of file''s datum relative to absolute datum (x, y, z)
    ReadCharacters(6, EndOfLine);
    ReadCharacters(6, EndOfLine);
    ReadCharacters(6, EndOfLine);
    if EndOfLine then Exit;

    // sample size
    ReadCharacters(6, EndOfLine);
    if EndOfLine then Exit;

    // Code indicating availability of statistics in data element 5
    ReadCharacters(6, EndOfLine);
    if EndOfLine then Exit;

    // RMSE of file''s datum relative to file''s datum (x, y, z)
    ReadCharacters(6, EndOfLine);
    ReadCharacters(6, EndOfLine);
    ReadCharacters(6, EndOfLine);
    if EndOfLine then Exit;

    // sample size
    ReadCharacters(6, EndOfLine);
    if EndOfLine then Exit;
  except on E : EInOutError do
    begin
//      record C is not present.
    end;
  end;
end;

procedure TDemReader.ReadRestOfRecord;
var
  remainder : integer;
  EndOfLine: boolean;
begin
  remainder := 1024 - CharCount mod 1024;
  if remainder <> 1024 then
  begin
     ReadCharacters(remainder, EndOfLine);
  end;
  CharCount := 0;
end;

function TDemReader.UTMCentralMeridianRadians(LatitudeSeconds,
  LongitudeSeconds: double): double;
var
  LongAngleDegrees : integer;
  CenterMeridianDegrees : integer;
begin
  LongAngleDegrees := Trunc(LongitudeSeconds/3600);
  CenterMeridianDegrees := LongAngleDegrees div 6;
  CenterMeridianDegrees := CenterMeridianDegrees * 6;
  if CenterMeridianDegrees < 0 then
  begin
    CenterMeridianDegrees := CenterMeridianDegrees - 3;
  end
  else
  begin
    CenterMeridianDegrees := CenterMeridianDegrees + 3;
  end;
  result := CenterMeridianDegrees*Pi/180;
end;

{ TElevationPoint }

procedure TElevationPoint.SetElevation(const Value: double);
begin
  FElevation := Value;
end;

procedure TElevationPoint.SetValue(const Value: integer);
begin
  FValue := Value;
end;

procedure TCornerPoint.SetX(const Value: double);
begin
  FX := Value;
end;

procedure TCornerPoint.SetY(const Value: double);
begin
  FY := Value;
end;

end.
