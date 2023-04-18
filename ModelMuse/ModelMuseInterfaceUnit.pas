unit ModelMuseInterfaceUnit;

interface

uses
  DataArrayInterfaceUnit;


type
  ISimpleDataArrayManager = interface(IInterface)
    ['{AE58DF4D-2555-4636-AA54-67C6C65E1CC9}']
    function GetDataSetByNameI(const DataSetName: string): IDataArray;
    procedure ExtractDataSetI(const DataSet: IDataArray);
  end;

implementation

end.
