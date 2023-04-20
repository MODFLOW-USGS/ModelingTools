unit DataArrayInterfaceUnit;

interface

uses
  GoPhastTypes, System.Generics.Collections, System.Classes;

type
  IDataArray = interface(IInterface)
    ['{A6E5A6B8-72D1-42BE-AA4E-20FB5A76BAD6}']
    function GetName: TComponentName;
    procedure SetName(const Value: TComponentName);
    property Name: TComponentName read GetName write SetName;
    procedure Free;
  end;

  TIDataArrayList = class(TList<IDataArray>);

implementation

end.
