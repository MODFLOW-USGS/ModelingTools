unit ModflowTimeSeriesUnit;

interface

uses
  System.Generics.Collections, GoPhastTypes;

type
  TTimeSeries = class(TObject)
  private
    FSeriesCount: Integer;
    FTimeCount: Integer;
    FSeriesNames: array of string;
    FInterpolationMethods: array of TMf6InterpolationMethods;
    FScaleFactors: array of double;
    FTimes: array of Double;
    FValues: array of array of Double;
    FObjectName: string;
    FParameterName: string;
    function GetInterpolationMethod(
      SeriesIndex: Integer): TMf6InterpolationMethods;
    function GetScaleFactor(SeriesIndex: Integer): double;
    function GetSeriesName(SeriesIndex: Integer): string;
    function GetTime(TimeIndex: Integer): double;
    function GetValue(SeriesIndex, TimeIndex: Integer): double;
    procedure SetInterpolationMethod(SeriesIndex: Integer;
      const Value: TMf6InterpolationMethods);
    procedure SetScaleFactor(SeriesIndex: Integer; const Value: double);
    procedure SetSeriesCount(const Value: Integer);
    procedure SetSeriesName(SeriesIndex: Integer; const Value: string);
    procedure SetTimeCount(const Value: Integer);
    procedure SetTime(TimeIndex: Integer; const Value: double);
    procedure SetValue(SeriesIndex, TimeIndex: Integer; const Value: double);
    procedure SetObjectName(const Value: string);
    procedure SetParameterName(const Value: string);
  public
    function UniformInterp: Boolean;
    function UniformScaleFactor: Boolean;
    property SeriesCount: Integer read FSeriesCount write SetSeriesCount;
    property TimeCount: Integer read FTimeCount write SetTimeCount;
    // series names are a maximum of 40 characters in length and must be unique
    // within a package. See LENTIMESERIESNAME in Constants.f90 in the
    // MODFLOW-6 source code.
    property SeriesNames[SeriesIndex: Integer]: string read GetSeriesName
      write SetSeriesName;
    property InterpolationMethods[SeriesIndex: Integer]: TMf6InterpolationMethods
      read GetInterpolationMethod write SetInterpolationMethod;
    property ScaleFactors[SeriesIndex: Integer]: double read GetScaleFactor
      write SetScaleFactor;
    property Times[TimeIndex: Integer]: double read GetTime write SetTime;
    property Values[SeriesIndex, TimeIndex: Integer]: double read GetValue
      write SetValue;
    property ObjectName: string read FObjectName write SetObjectName;
    property ParameterName: string read FParameterName write SetParameterName;
  end;

  TTimeSeriesList = TObjectList<TTimeSeries>;

  TParameterDictionary = class(TObjectDictionary<string, TTimeSeriesList>)
  public
    constructor Create;
  end;

implementation

uses
  PhastModelUnit;

{ TTimeSeries }

function TTimeSeries.GetInterpolationMethod(
  SeriesIndex: Integer): TMf6InterpolationMethods;
begin
  result := FInterpolationMethods[SeriesIndex];
end;

function TTimeSeries.GetScaleFactor(SeriesIndex: Integer): double;
begin
  result := FScaleFactors[SeriesIndex];
end;

function TTimeSeries.GetSeriesName(SeriesIndex: Integer): string;
begin
  result := FSeriesNames[SeriesIndex];
end;

function TTimeSeries.GetTime(TimeIndex: Integer): double;
begin
  result := FTimes[TimeIndex];
end;

function TTimeSeries.GetValue(SeriesIndex, TimeIndex: Integer): double;
begin
  result := FValues[SeriesIndex, TimeIndex];
end;

procedure TTimeSeries.SetInterpolationMethod(SeriesIndex: Integer;
  const Value: TMf6InterpolationMethods);
begin
  FInterpolationMethods[SeriesIndex] := Value;
end;

procedure TTimeSeries.SetObjectName(const Value: string);
begin
  FObjectName := Value;
end;

procedure TTimeSeries.SetParameterName(const Value: string);
begin
  FParameterName := Value;
end;

procedure TTimeSeries.SetScaleFactor(SeriesIndex: Integer;
  const Value: double);
begin
  FScaleFactors[SeriesIndex] := Value;
end;

procedure TTimeSeries.SetSeriesCount(const Value: Integer);
begin
  if FSeriesCount <> Value then
  begin
    FSeriesCount := Value;
    SetLength(FSeriesNames, FSeriesCount);
    SetLength(FInterpolationMethods, FSeriesCount);
    SetLength(FScaleFactors, FSeriesCount);
    SetLength(FValues, FSeriesCount, FTimeCount);
  end;
end;

procedure TTimeSeries.SetSeriesName(SeriesIndex: Integer; const Value: string);
begin
  FSeriesNames[SeriesIndex] := Copy(Value, 1, MaxBoundNameLength);
end;

procedure TTimeSeries.SetTimeCount(const Value: Integer);
begin
  if FTimeCount <> Value then
  begin
    FTimeCount := Value;
    SetLength(FTimes, FTimeCount);
    SetLength(FValues, FSeriesCount, FTimeCount);
  end;
end;

procedure TTimeSeries.SetTime(TimeIndex: Integer; const Value: double);
begin
  FTimes[TimeIndex] := Value;
end;

procedure TTimeSeries.SetValue(SeriesIndex, TimeIndex: Integer;
  const Value: double);
begin
  FValues[SeriesIndex, TimeIndex] := Value;
end;

function TTimeSeries.UniformInterp: Boolean;
var
  SeriesIndex: Integer;
begin
  result := True;
  for SeriesIndex := 1 to SeriesCount - 1 do
  begin
    result := InterpolationMethods[0] = InterpolationMethods[SeriesIndex];
    if not result then
    begin
      Exit;
    end;
  end;
end;

function TTimeSeries.UniformScaleFactor: Boolean;
var
  SeriesIndex: Integer;
begin
  result := True;
  for SeriesIndex := 1 to SeriesCount - 1 do
  begin
    result := ScaleFactors[0] = ScaleFactors[SeriesIndex];
    if not result then
    begin
      Exit;
    end;
  end;
end;


{ TParameterDictionary }

constructor TParameterDictionary.Create;
begin
  inherited Create([doOwnsValues]);

end;

end.
