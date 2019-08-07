unit ReadSutraNodEleUnit;

interface

uses
  Windows, Classes, Dialogs, SysUtils, GoPhastTypes,
  Generics.Collections, Generics.Defaults;

type
  TStoredResults = class(TObject)
    TimeStep: Integer;
    Time: double;
  end;

  TStoredResultsArray = array of TStoredResults;

  TStoredResultsList = TObjectList<TStoredResults>;

  TStoredResultsComparer = TComparer<TStoredResults>;

  TCustomSutraOutputReader = class(TObject)
  private
    FReader: TStreamReader;
    FSplitter: TStringList;
    FStoredResults: TStoredResultsList;
    FCount: integer;
    FCurrentTimeStep: Integer;
    procedure ReadHeader;
  protected
    function CountSearchString: string; virtual; abstract;
    procedure ReadValueHeader; virtual; abstract;
    procedure ReadValues; virtual; abstract;
    procedure ZeroArrays; virtual; abstract;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    property StoredResults: TStoredResultsList read FStoredResults;
    property Count: Integer read FCount;
    procedure ReadNextResults;
    procedure SkipNextResults;
    property CurrentTimeStep: Integer read FCurrentTimeStep;

  end;

  TNodReader = class(TCustomSutraOutputReader)
  private
    FX: TOneDRealArray;
    FY: TOneDRealArray;
    FZ: TOneDRealArray;
    FPressure: TOneDRealArray;
    FU: TOneDRealArray;
    FSaturation: TOneDRealArray;
    FHasNode: Boolean;
  protected
    function CountSearchString: string; override;
    procedure ReadValueHeader; override;
    procedure ReadValues; override;
    procedure ZeroArrays; override;
  public
    property X: TOneDRealArray read FX;
    property Y: TOneDRealArray read FY;
    property Z: TOneDRealArray read FZ;
    property Pressure: TOneDRealArray read FPressure;
    property U: TOneDRealArray read FU;
    property Saturation: TOneDRealArray read FSaturation;
  end;

  TEleReader = class(TCustomSutraOutputReader)
    FX: TOneDRealArray;
    FY: TOneDRealArray;
    FZ: TOneDRealArray;
    FXVelocity: TOneDRealArray;
    FYVelocity: TOneDRealArray;
    FZVelocity: TOneDRealArray;
  private
    FHasElement: Boolean;
  protected
    function CountSearchString: string; override;
    procedure ReadValueHeader; override;
    procedure ReadValues; override;
    procedure ZeroArrays; override;
  public
    property X: TOneDRealArray read FX;
    property Y: TOneDRealArray read FY;
    property Z: TOneDRealArray read FZ;
    property XVelocity: TOneDRealArray read FXVelocity;
    property YVelocity: TOneDRealArray read FYVelocity;
    property ZVelocity: TOneDRealArray read FZVelocity;
  end;

implementation

uses
  IOUtils, ModelMuseUtilities;

{ TCustomSutraOutputReader }

constructor TCustomSutraOutputReader.Create(const FileName: string);
begin
  Assert(TFile.Exists(FileName));
  FReader := TFile.OpenText(FileName);
  FSplitter := TStringList.Create;
  FSplitter.Delimiter := ' ';
  FCurrentTimeStep := -1;
  FStoredResults:= TStoredResultsList.Create;
  ReadHeader;
end;

destructor TCustomSutraOutputReader.Destroy;
begin
  FSplitter.Free;
  FReader.Free;
  FStoredResults.Free;
  inherited;
end;

procedure TCustomSutraOutputReader.ReadHeader;
const
  SkipLines1 = 3;
  SkipLines2 = 2;
  SkipLines3 = 5;
  TimeStepPosition = 1;
  TimePosition = 2;
var
  LineIndex: Integer;
  NodeIndex: integer;
  TimeStepsIndex: Integer;
  TimeStepCount: Integer;
  StoredDecimalSeparator: Char;
  StoredR: TStoredResults;
begin
  StoredDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    for LineIndex := 0 to SkipLines1 - 1 do
    begin
      FReader.ReadLine;
    end;
    FSplitter.Delimitedtext := FReader.ReadLine;
    NodeIndex := FSplitter.IndexOf(CountSearchString)-1;
    Assert(NodeIndex >= 0);
    FCount := StrToInt(FSplitter[NodeIndex]);

    for LineIndex := 0 to SkipLines2 - 1 do
    begin
      FReader.ReadLine;
    end;
    FSplitter.Delimitedtext := FReader.ReadLine;
    TimeStepsIndex := FSplitter.IndexOf('Time')-1;
    Assert(TimeStepsIndex >= 0);
    TimeStepCount := StrToInt(FSplitter[TimeStepsIndex]);
    FStoredResults.Capacity := TimeStepCount;
//    SetLength(FStoredResults, TimeStepCount);

    for LineIndex := 0 to SkipLines3 - 1 do
    begin
      FReader.ReadLine;
    end;
    for LineIndex := 0 to TimeStepCount-1 do
    begin
      FSplitter.Delimitedtext := FReader.ReadLine;
      StoredR := TStoredResults.Create;
      FStoredResults.Add(StoredR);
      StoredR.TimeStep := StrToInt(FSplitter[TimeStepPosition]);
      StoredR.Time := FortranStrToFloat(FSplitter[TimePosition]);
    end;
  finally
    FormatSettings.DecimalSeparator := StoredDecimalSeparator;
  end;
end;

procedure TCustomSutraOutputReader.ReadNextResults;
const
  SkipLines1 = 2;
  SkipLines2 = 1;
  StepStringPos = 2;
  StepNumPos = 3;
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to SkipLines1 - 1 do
  begin
    FReader.ReadLine;
  end;
  if FReader.EndOfStream then
  begin
    FCurrentTimeStep := -1;
    ZeroArrays;
    Exit;
  end;
  FSplitter.DelimitedText := FReader.ReadLine;
  Assert(FSplitter[StepStringPos] = 'STEP');
  FCurrentTimeStep := StrToInt(FSplitter[StepNumPos]);
  for LineIndex := 0 to SkipLines2 - 1 do
  begin
    FReader.ReadLine;
  end;
  ReadValueHeader;
  ReadValues;
end;

procedure TCustomSutraOutputReader.SkipNextResults;
const
  SkipLines = 5;
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to SkipLines - 1 do
  begin
    FReader.ReadLine;
  end;
  for LineIndex := 0 to Count - 1 do
  begin
    FReader.ReadLine;
  end;
  FCurrentTimeStep := -1;
end;

{ TNodReader }

function TNodReader.CountSearchString: string;
begin
  result := 'Nodes';
end;

procedure TNodReader.ReadValueHeader;
var
  ALine: string;
begin
  ALine := FReader.ReadLine;
  FHasNode := Pos('Node', ALine) > 0;
  if Pos('X', ALine) > 0 then
  begin
    SetLength(FX, Count);
  end
  else
  begin
    SetLength(FX, 0);
  end;
  if Pos('Y', ALine) > 0 then
  begin
    SetLength(FY, Count);
  end
  else
  begin
    SetLength(FY, 0);
  end;
  if Pos('Z', ALine) > 0 then
  begin
    SetLength(FZ, Count);
  end
  else
  begin
    SetLength(FZ, 0);
  end;
  if Pos('Pressure', ALine) > 0 then
  begin
    SetLength(FPressure, Count);
  end
  else
  begin
    SetLength(FPressure, 0);
  end;
  if (Pos('Concentration', ALine) > 0) or (Pos('Temperature', ALine) > 0) then
  begin
    SetLength(FU, Count);
  end
  else
  begin
    SetLength(FU, 0);
  end;
  if Pos('Saturation', ALine) > 0 then
  begin
    SetLength(FSaturation, Count);
  end
  else
  begin
    SetLength(FSaturation, 0);
  end;
end;

procedure TNodReader.ReadValues;
var
  ItemIndex: integer;
  LineIndex: Integer;
  StoredDecimalSeparator: Char;
  procedure ReadItem(var AnArray: TOneDRealArray);
  begin
    if Length(AnArray) > 0 then
    begin
      AnArray[LineIndex] := FortranStrToFloat(FSplitter[ItemIndex]);
      Inc(ItemIndex);
    end;
  end;
begin
  StoredDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    for LineIndex := 0 to Count - 1 do
    begin
      FSplitter.DelimitedText := FReader.ReadLine;
      ItemIndex := 0;
      if FHasNode then
      begin
        Inc(ItemIndex);
      end;
      ReadItem(FX);
      ReadItem(FY);
      ReadItem(FZ);
      ReadItem(FPressure);
      ReadItem(FU);
      ReadItem(FSaturation);
    end;
  finally
    FormatSettings.DecimalSeparator := StoredDecimalSeparator;
  end;

end;

procedure TNodReader.ZeroArrays;
begin
  SetLength(FX, 0);
  SetLength(FY, 0);
  SetLength(FZ, 0);
  SetLength(FPressure, 0);
  SetLength(FU, 0);
  SetLength(FSaturation, 0);
end;

{ TEleReader }

function TEleReader.CountSearchString: string;
begin
  result := 'Elems';
end;

procedure TEleReader.ReadValueHeader;
var
  ALine: string;
begin
  ALine := FReader.ReadLine;
  FHasElement := Pos('Element', ALine) > 0;
  if Pos('X origin', ALine) > 0 then
  begin
    SetLength(FX, Count);
  end
  else
  begin
    SetLength(FX, 0);
  end;
  if Pos('Y origin', ALine) > 0 then
  begin
    SetLength(FY, Count);
  end
  else
  begin
    SetLength(FY, 0);
  end;
  if Pos('Z origin', ALine) > 0 then
  begin
    SetLength(FZ, Count);
  end
  else
  begin
    SetLength(FZ, 0);
  end;
  if Pos('X velocity', ALine) > 0 then
  begin
    SetLength(FXVelocity, Count);
  end
  else
  begin
    SetLength(FXVelocity, 0);
  end;
  if Pos('Y velocity', ALine) > 0 then
  begin
    SetLength(FYVelocity, Count);
  end
  else
  begin
    SetLength(FYVelocity, 0);
  end;
  if Pos('Z velocity', ALine) > 0 then
  begin
    SetLength(FZVelocity, Count);
  end
  else
  begin
    SetLength(FZVelocity, 0);
  end;
end;

procedure TEleReader.ReadValues;
var
  ItemIndex: integer;
  LineIndex: Integer;
  StoredDecimalSeparator: Char;
  procedure ReadItem(var AnArray: TOneDRealArray);
  begin
    if Length(AnArray) > 0 then
    begin
      AnArray[LineIndex] := FortranStrToFloat(FSplitter[ItemIndex]);
      Inc(ItemIndex);
    end;
  end;
begin
  StoredDecimalSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
  try
    for LineIndex := 0 to Count - 1 do
    begin
      FSplitter.DelimitedText := FReader.ReadLine;
      ItemIndex := 0;
      if FHasElement then
      begin
        Inc(ItemIndex);
      end;
      ReadItem(FX);
      ReadItem(FY);
      ReadItem(FZ);
      ReadItem(FXVelocity);
      ReadItem(FYVelocity);
      ReadItem(FZVelocity);
    end;
  finally
    FormatSettings.DecimalSeparator := StoredDecimalSeparator;
  end;
end;

procedure TEleReader.ZeroArrays;
begin
  SetLength(FX, 0);
  SetLength(FY, 0);
  SetLength(FZ, 0);
  SetLength(FXVelocity, 0);
  SetLength(FYVelocity, 0);
  SetLength(FZVelocity, 0);
end;

end.
