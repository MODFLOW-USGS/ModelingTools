unit Modflow6TimeSeriesUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TMf6TimeSeries = class(TRealCollection)
  private
    FSeriesName: AnsiString;
    FScaleFactorParameter: string;
    FInterpolationMethod: TMf6InterpolationMethods;
    FStoredScaleFactor: TRealStorage;
    FParamMethod: TPestParamMethod;
    FNotifierComponent: TComponent;
    function GetScaleFactor: double;
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetScaleFactor(const Value: double);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: AnsiString);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    procedure SetParamMethod(const Value: TPestParamMethod);
    function IsSame(OtherTimeSeries: TMf6TimeSeries): Boolean;
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    property NotifierComponent: TComponent read FNotifierComponent;
  published
    property SeriesName: AnsiString read FSeriesName write SetSeriesName;
    property InterpolationMethod: TMf6InterpolationMethods
      read FInterpolationMethod write SetInterpolationMethod;
    property StoredScaleFactor: TRealStorage read FStoredScaleFactor
      write SetStoredScaleFactor;
    property ScaleFactorParameter: string read FScaleFactorParameter
      write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read FParamMethod
      write SetParamMethod;
//    function GetInterpolatedValue(Model: TBaseModel; Time: double;
//      StartTimeOffset: double = 0): double;
  end;

implementation

uses
  System.SysUtils, System.Character, System.AnsiStrings;

//uses
//  PhastModelUnit, ModflowTimeUnit;

{ TTimeSeries }

procedure TMf6TimeSeries.Assign(Source: TPersistent);
var
  TimeSeriesSource: TMf6TimeSeries;
begin
  if Source is TMf6TimeSeries then
  begin
    TimeSeriesSource := TMf6TimeSeries(Source);
    SeriesName := TimeSeriesSource.SeriesName;
    InterpolationMethod := TimeSeriesSource.InterpolationMethod;
    ScaleFactor := TimeSeriesSource.ScaleFactor;
    ScaleFactorParameter := TimeSeriesSource.ScaleFactorParameter;
    ParamMethod := TimeSeriesSource.ParamMethod;
  end;
  inherited;
end;

constructor TMf6TimeSeries.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredScaleFactor := TRealStorage.Create(InvalidateModelEvent);
  FNotifierComponent := TComponent.Create(nil);
end;

destructor TMf6TimeSeries.Destroy;
begin
  FNotifierComponent.Free;
  FStoredScaleFactor.Free;
  inherited;
end;

//function TMf6TimeSeries.GetInterpolatedValue(Model: TBaseModel; Time: double;
//  StartTimeOffset: double = 0): double;
//var
//  LocalModel: TCustomModel;
//  Period: Integer;
//  Step: Integer;
//begin
//  LocalModel := Model as TCustomModel;
//  LocalModel.ModflowStressPeriods.TimeToPeriodAndStep(
//    Time-StartTimeOffset, Period, Step);
//  TimeStep := LocalModel.ModflowStressPeriods[Period].GetTimeStep(Step);
//
//end;

function TMf6TimeSeries.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

function TMf6TimeSeries.IsSame(OtherTimeSeries: TMf6TimeSeries): Boolean;
begin
  result := (ScaleFactor = OtherTimeSeries.ScaleFactor)
    and (SeriesName = OtherTimeSeries.SeriesName)
    and (InterpolationMethod = OtherTimeSeries.InterpolationMethod)
    and (ScaleFactorParameter = OtherTimeSeries.ScaleFactorParameter)
    and (ParamMethod = OtherTimeSeries.ParamMethod)
    and inherited IsSame(OtherTimeSeries);
end;

procedure TMf6TimeSeries.SetInterpolationMethod(
  const Value: TMf6InterpolationMethods);
begin
  if FInterpolationMethod <> Value then
  begin
    FInterpolationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMf6TimeSeries.SetParamMethod(const Value: TPestParamMethod);
begin
  if FParamMethod <> Value then
  begin
    FParamMethod := Value;
    InvalidateModel;
  end;
end;

procedure TMf6TimeSeries.SetScaleFactor(const Value: double);
begin
  StoredScaleFactor.Value := Value;
end;

procedure TMf6TimeSeries.SetScaleFactorParameter(const Value: string);
begin
  if FScaleFactorParameter <> Value then
  begin
    FScaleFactorParameter := Value;
    InvalidateModel;
  end;
end;

procedure TMf6TimeSeries.SetSeriesName(Value: AnsiString);
const
  AllowableChars = ['a'..'z', 'A'..'Z', '0'..'9', '@', '#', '$', '%', '^', '&',
     '*', '(', ')', '_', '-', '<', '>', '?', '.'];
var
  CharIndex: Integer;
  AChar: AnsiChar;
begin
  Value := Trim(Copy(Trim(Value), 1, MaxTimeSeriesNameLength));
  for CharIndex := 1 to Length(Value) do
  begin
    AChar := Value[CharIndex];
    if not (AChar in AllowableChars) then
    begin
      Value[CharIndex] := '_'
    end;
  end;
  if Value = '' then
  begin
    Value := '_';
  end;
  if FSeriesName <> Value then
  begin
    FSeriesName := Value;
    InvalidateModel;
  end;
end;

procedure TMf6TimeSeries.SetStoredScaleFactor(const Value: TRealStorage);
begin
  FStoredScaleFactor.Assign(Value);
end;

end.
