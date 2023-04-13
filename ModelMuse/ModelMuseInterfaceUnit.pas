unit ModelMuseInterfaceUnit;

interface

uses
  DataSetUnit;

type
  ISimpleDataArrayManager = interface(IInterface)
    function GetDataSetByName(const DataSetName: string): TDataArray;
    procedure ExtractDataSet(const DataSet: TDataArray);
  end;

implementation

end.
