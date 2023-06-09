unit ModflowParameterInterfaceUnit;

interface

uses
  GoPhastTypes;

type
  IModflowParameter = interface
    ['{D608B20F-55ED-4E3B-8DD8-82F5CE615D40}']
    function GetParameterName: string;
    procedure SetParameterName(const Value: string);
    property ParameterName: string read GetParameterName write SetParameterName;
    function GetValue: double;
    procedure SetValue(AValue : double);
    property Value: double read GetValue write SetValue;
  end;

implementation

end.
