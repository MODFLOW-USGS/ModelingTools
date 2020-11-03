unit ObsInterfaceUnit;

interface

type
  ITimeObservationItem = interface(IUnknown)
    function GetName: string;
    function GetObservedValue: double;
    function GetTime: double;
    function GetWeight: Double;
    function GetObservationGroup: string;

    procedure SetObservedValue(const Value: double);
    procedure SetTime(const Value: double);
    procedure SetObservationGroup(const Value: string);

    property Name: string read GetName;
    property ObservedValue: double read GetObservedValue
      write SetObservedValue;
    property Time: double read GetTime write SetTime;
    property Weight: Double read GetWeight;
    property ObservationGroup: string read GetObservationGroup
      write SetObservationGroup;
  end;



implementation

end.
