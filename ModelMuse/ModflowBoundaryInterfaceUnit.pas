unit ModflowBoundaryInterfaceUnit;

interface

uses
  GoPhastTypes, ModflowParameterInterfaceUnit, ModflowTransientListParameterInterfaceUnit;

type
  IModflowParamBoundary = interface
    ['{FCCB14EF-079F-47AF-B8E9-42A6293C7F51}']
    procedure DeleteParam(Param: IModflowParameter);
  end;

  IModflowParameters = interface
    ['{0DC2B67B-C3E2-4109-84DF-0596F9694518}']
    function IndexOfParam(AParam: IModflowTransientListParameter): integer; overload;
    procedure Delete(Index: Integer);
  end;

implementation

end.
