unit Modflow6TimeSeriesUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TTimeSeries = class(TRealCollection)
  private
    FSeriesName: string;
    FScaleFactorParameter: string;
    FInterpolationMethod: TMf6InterpolationMethods;
    FStoredScaleFactor: TRealStorage;
    FParamMethod: TPestParamMethod;
    function GetScaleFactor: double;
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetScaleFactor(const Value: double);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: string);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    procedure SetParamMethod(const Value: TPestParamMethod);
    function IsSame(OtherTimeSeries: TTimeSeries): Boolean;
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
  published
    property SeriesName: string read FSeriesName write SetSeriesName;
    property InterpolationMethod: TMf6InterpolationMethods read FInterpolationMethod write SetInterpolationMethod;
    property StoredScaleFactor: TRealStorage read FStoredScaleFactor write SetStoredScaleFactor;
    property ScaleFactorParameter: string read FScaleFactorParameter write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read FParamMethod write SetParamMethod;
  end;

implementation

{ TTimeSeries }

procedure TTimeSeries.Assign(Source: TPersistent);
var
  TimeSeriesSource: TTimeSeries;
begin
  if Source is TTimeSeries then
  begin
    TimeSeriesSource := TTimeSeries(Source);
    SeriesName := TimeSeriesSource.SeriesName;
    InterpolationMethod := TimeSeriesSource.InterpolationMethod;
    ScaleFactor := TimeSeriesSource.ScaleFactor;
    ScaleFactorParameter := TimeSeriesSource.ScaleFactorParameter;
    ParamMethod := TimeSeriesSource.ParamMethod;
  end;
  inherited;
end;

constructor TTimeSeries.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredScaleFactor := TRealStorage.Create(InvalidateModelEvent);
end;

destructor TTimeSeries.Destroy;
begin
  FStoredScaleFactor.Free;
  inherited;
end;

function TTimeSeries.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

function TTimeSeries.IsSame(OtherTimeSeries: TTimeSeries): Boolean;
begin
  result := (ScaleFactor = OtherTimeSeries.ScaleFactor)
    and (SeriesName = OtherTimeSeries.SeriesName)
    and (InterpolationMethod = OtherTimeSeries.InterpolationMethod)
    and (ScaleFactorParameter = OtherTimeSeries.ScaleFactorParameter)
    and (ParamMethod = OtherTimeSeries.ParamMethod)
    and inherited IsSame(OtherTimeSeries);
end;

procedure TTimeSeries.SetInterpolationMethod(
  const Value: TMf6InterpolationMethods);
begin
  if FInterpolationMethod <> Value then
  begin
    FInterpolationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetParamMethod(const Value: TPestParamMethod);
begin
  if FParamMethod <> Value then
  begin
    FParamMethod := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetScaleFactor(const Value: double);
begin
  StoredScaleFactor.Value := Value;
end;

procedure TTimeSeries.SetScaleFactorParameter(const Value: string);
begin
  if FScaleFactorParameter <> Value then
  begin
    FScaleFactorParameter := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetSeriesName(Value: string);
begin
  Value := Copy(Value, 1, MaxTimeSeriesNameLength);
  if FSeriesName <> Value then
  begin
    FSeriesName := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetStoredScaleFactor(const Value: TRealStorage);
begin
  FStoredScaleFactor.Assign(Value);
end;

end.
