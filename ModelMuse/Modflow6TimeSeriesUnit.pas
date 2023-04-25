unit Modflow6TimeSeriesUnit;

interface

uses
  System.Classes, GoPhastTypes, Modflow6TimeSeriesInterfaceUnit;

type
  TMf6TimeSeries = class(TRealCollection, ITimeSeries)
  private
    FSeriesName: AnsiString;
    FScaleFactorParameter: string;
    FInterpolationMethod: TMf6InterpolationMethods;
    FStoredScaleFactor: TRealStorage;
    FParamMethod: TPestParamMethod;
    FNotifierComponent: TComponent;
    FDeleted: Boolean;
    function GetScaleFactor: double;
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetScaleFactor(const Value: double);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: AnsiString);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    procedure SetParamMethod(const Value: TPestParamMethod);
    function GetSeriesName: AnsiString;
    function GetInterpolationMethod: TMf6InterpolationMethods;
    function GetStoredScaleFactor: TRealStorage;
    function GetScaleFactorParameter: string;
    function GetParamMethod: TPestParamMethod;
//    function _AddRef: Integer; stdcall;
//    function _Release: Integer; stdcall;
//    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
//      virtual; stdcall;
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    property NotifierComponent: TComponent read FNotifierComponent;
    function IsSame(OtherTimeSeries: TMf6TimeSeries): Boolean;
  published
    property SeriesName: AnsiString read GetSeriesName write SetSeriesName;
    property InterpolationMethod: TMf6InterpolationMethods
      read GetInterpolationMethod write SetInterpolationMethod;
    property StoredScaleFactor: TRealStorage read GetStoredScaleFactor
      write SetStoredScaleFactor;
    property ScaleFactorParameter: string read GetScaleFactorParameter
      write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read GetParamMethod
      write SetParamMethod;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

implementation

uses
  System.SysUtils, System.AnsiStrings, System.IOUtils;

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
    Deleted := TimeSeriesSource.Deleted;
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

function TMf6TimeSeries.GetInterpolationMethod: TMf6InterpolationMethods;
begin
  result := FInterpolationMethod;
end;

function TMf6TimeSeries.GetParamMethod: TPestParamMethod;
begin
  result := FParamMethod;
end;

function TMf6TimeSeries.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

function TMf6TimeSeries.GetScaleFactorParameter: string;
begin
  Result := FScaleFactorParameter;
end;

function TMf6TimeSeries.GetSeriesName: AnsiString;
begin
  Result := FSeriesName;
end;

function TMf6TimeSeries.GetStoredScaleFactor: TRealStorage;
begin
  result := FStoredScaleFactor;
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

//function TMf6TimeSeries.QueryInterface(const IID: TGUID; out Obj): HRESULT;
//const
//  E_NOINTERFACE = HRESULT($80004002);
//begin
//  if GetInterface(IID, Obj) then
//    result := 0
//  else
//    result := E_NOINTERFACE;
//end;

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
var
  CharIndex: Integer;
  AChar: Char;
begin
  Value := Trim(Copy(Trim(Value), 1, MaxTimeSeriesNameLength));
  for CharIndex := 1 to Length(Value) do
  begin
    AChar := Char(Value[CharIndex]);
    if (AChar = ' ') or not (TPath.IsValidFileNameChar(AChar)) then
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

//function TMf6TimeSeries._AddRef: Integer;
//begin
//  Result := 1;
//end;
//
//function TMf6TimeSeries._Release: Integer;
//begin
//  Result := 1;
//end;

end.
