unit ChangeGlobalVariablesInterfaceUnit;

interface

uses
  System.Classes, GoPhastTypes, GlobalVariablesUnit;

type
  IModelForChangeGlobalVariables = interface(IModelMuseModel)
    ['{70E1EDFA-1BEC-4843-95A9-1F4A2D3C283D}']
    procedure UpdateFormulas(OldNames, NewNames: TStringList);
    function GetGlobalVariables: TGlobalVariables;
    procedure SetGlobalVariables(const Value: TGlobalVariables);
    property GlobalVariables: TGlobalVariables read GetGlobalVariables
      write SetGlobalVariables;
    procedure RestoreSubscriptions;
  end;

implementation

end.
