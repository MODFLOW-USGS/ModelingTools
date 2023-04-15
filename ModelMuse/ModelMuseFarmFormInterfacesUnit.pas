unit ModelMuseFarmFormInterfacesUnit;

interface

uses
  GoPhastTypes, RbwParser;

type

  IFarmFormInterface = interface(IModelMuseModel)
    ['{EFD930E7-E2AB-4032-BBBD-AEA4F7BA99ED}']
    procedure RegisterGlobalVariables(Parser: TRbwParser;
      IgnoreDuplicates: Boolean = False);
//    function GetFarms: TFarmCollection;
//    procedure SetFarms(const Value: TFarmCollection);
//    property Farms: TFarmCollection read GetFarms write SetFarms;

  end;


implementation

end.
