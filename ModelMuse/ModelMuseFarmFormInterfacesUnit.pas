unit ModelMuseFarmFormInterfacesUnit;

interface

uses
  GoPhastTypes, RbwParser;

type

  IFarmFormInterface = interface(IModelMuseModel)
    procedure RegisterGlobalVariables(Parser: TRbwParser;
      IgnoreDuplicates: Boolean = False);
//    function GetFarms: TFarmCollection;
//    procedure SetFarms(const Value: TFarmCollection);
//    property Farms: TFarmCollection read GetFarms write SetFarms;

  end;


implementation

end.
