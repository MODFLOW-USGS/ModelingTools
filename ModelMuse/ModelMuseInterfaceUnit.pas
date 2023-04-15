unit ModelMuseInterfaceUnit;

interface

uses
  DataSetUnit;

type
  ISimpleDataArrayManager = interface(IInterface)
    ['{AE58DF4D-2555-4636-AA54-67C6C65E1CC9}']
    function GetDataSetByName(const DataSetName: string): TDataArray;
    procedure ExtractDataSet(const DataSet: TDataArray);
  end;

implementation

end.
