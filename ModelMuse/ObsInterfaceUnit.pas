unit ObsInterfaceUnit;

interface

type
  IObservationItem = interface(IUnknown)
    function GetName: string;
    function GetGUID: string;

    property Name: string read GetName;
    property GUID: string read GetGUID;
  end;

  ITimeObservationItem = interface(IObservationItem)
//    function GetName: string;
    function GetObservedValue: double;
    function GetTime: double;
    function GetWeight: Double;
    function GetObservationGroup: string;

    procedure SetObservedValue(const Value: double);
    procedure SetTime(const Value: double);

//    property Name: string read GetName;
    property ObservedValue: double read GetObservedValue
      write SetObservedValue;
    property Time: double read GetTime write SetTime;
    property Weight: Double read GetWeight;
    property ObservationGroup: string read GetObservationGroup;
  end;

  IObservationGroup = interface(IUnknown)
    function GetAbsoluteCorrelationFileName: string;
    function GetGroupTarget: Double;
    function GetObsGroupName: string;
    function GetUseGroupTarget: Boolean;

    procedure SetAbsoluteCorrelationFileName(const Value: string);
    procedure SetGroupTarget(const Value: Double);
    procedure SetObsGroupName(Value: string);
    procedure SetUseGroupTarget(const Value: Boolean);

    property AbsoluteCorrelationFileName: string
      read GetAbsoluteCorrelationFileName write SetAbsoluteCorrelationFileName;
    property GroupTarget: Double read GetGroupTarget write SetGroupTarget;
    property ObsGroupName: string read GetObsGroupName write SetObsGroupName;
    property UseGroupTarget: Boolean read GetUseGroupTarget
      write SetUseGroupTarget;

  end;


implementation

end.
