unit Modflow6TimeSeriesInterfaceUnit;

interface

uses
  GoPhastTypes;

type
  ITimeSeries = interface(IInterface)
    ['{41D81EBC-D817-4787-9A45-43A31E7D4025}']
    function GetSeriesName: AnsiString;
    procedure SetSeriesName(Value: AnsiString);
    property SeriesName: AnsiString read GetSeriesName write SetSeriesName;

    function GetScaleFactor: double;
    procedure SetScaleFactor(const Value: double);
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;

    function GetInterpolationMethod: TMf6InterpolationMethods;
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    property InterpolationMethod: TMf6InterpolationMethods
      read GetInterpolationMethod write SetInterpolationMethod;

    function GetScaleFactorParameter: string;
    procedure SetScaleFactorParameter(const Value: string);
    property ScaleFactorParameter: string read GetScaleFactorParameter
      write SetScaleFactorParameter;

    function GetParamMethod: TPestParamMethod;
    procedure SetParamMethod(const Value: TPestParamMethod);
    property ParamMethod: TPestParamMethod read GetParamMethod
      write SetParamMethod;

    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    property Count: Integer read GetCount write SetCount;
  end;

  IMf6TimeSeries = interface(ITimeSeries)
    ['{CABBFFCE-A6E3-48A7-9AEF-BCDD6E05B1DA}']

    function GetValue(Index: Integer): Double;
    procedure SetValue(Index: Integer; const Value: Double);
    property Values[Index: Integer]: Double read GetValue write SetValue;

    function GetDeleted: Boolean;
    procedure SetDeleted(const Value: Boolean);
    property Deleted: Boolean read GetDeleted write SetDeleted;
  end;


implementation

end.
