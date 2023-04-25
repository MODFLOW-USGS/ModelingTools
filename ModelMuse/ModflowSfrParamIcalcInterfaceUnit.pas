unit ModflowSfrParamIcalcInterfaceUnit;

interface

uses
  GoPhastTypes;

type
  ISfrParamIcalcItem = interface
    ['{4DE1D070-5920-479A-A2E5-726DDE3AB3DE}']
    function GetParam: string;
    procedure SetParam(const Value: string);
    property Param: string read GetParam write SetParam;
  end;

implementation

end.
