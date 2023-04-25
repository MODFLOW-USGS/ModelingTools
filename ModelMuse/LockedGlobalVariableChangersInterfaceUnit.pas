unit LockedGlobalVariableChangersInterfaceUnit;

interface

uses
  System.Classes, GoPhastTypes, GlobalVariablesInterfaceUnit;

type
  IModelForTCustomDefinedGlobalObject = interface(IModelMuseModel)
    ['{70E1EDFA-1BEC-4843-95A9-1F4A2D3C283D}']
    procedure UpdateFormulas(OldNames, NewNames: TStringList);
    function GetGlobalVariablesI: IGlobalVariables;
    procedure SetGlobalVariablesI(const Value: IGlobalVariables);
    property GlobalVariablesI: IGlobalVariables read GetGlobalVariablesI
      write SetGlobalVariablesI;
    procedure RestoreSubscriptions;
    procedure ChangeGlobalVariable(const ParameterName: string;
      const Value: TParameterType);
  end;

implementation

end.
