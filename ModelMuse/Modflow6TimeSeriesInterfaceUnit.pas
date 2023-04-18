unit Modflow6TimeSeriesInterfaceUnit;

interface

uses
  GoPhastTypes;

type
  ITimeSeriesInterface = interface(IInterface)
    ['{41D81EBC-D817-4787-9A45-43A31E7D4025}']
    function GetScaleFactor: double;
    function GetSeriesName: AnsiString;
    function GetInterpolationMethod: TMf6InterpolationMethods;
    function GetScaleFactorParameter: string;
    function GetParamMethod: TPestParamMethod;
    function GetCount: Integer;
    procedure SetCount(const Value: Integer);

    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetScaleFactor(const Value: double);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: AnsiString);
    procedure SetParamMethod(const Value: TPestParamMethod);

    property SeriesName: AnsiString read GetSeriesName write SetSeriesName;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    property InterpolationMethod: TMf6InterpolationMethods
      read GetInterpolationMethod write SetInterpolationMethod;
    property ScaleFactorParameter: string read GetScaleFactorParameter
      write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read GetParamMethod
      write SetParamMethod;
    property Count: Integer read GetCount write SetCount;
  end;

implementation

end.
