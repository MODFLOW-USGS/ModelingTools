unit Modflow6TimeSeriesCollectionsInterfaceUnit;

interface

uses
  GoPhastTypes;

type
  IModelTimesSeriesInterface = interface(IModelMuseModel)
    ['{C73F73F3-EE7F-476F-8280-7AF9A970F681}']
    function TimeToTimeStepTimes(ATime: double; out StartTime, EndTime: double): Boolean;
    function GetPestParameterValueByName(PestParamName: string; out Value: double): Boolean;
  end;

implementation

end.
