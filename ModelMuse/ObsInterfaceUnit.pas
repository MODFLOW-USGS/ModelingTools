unit ObsInterfaceUnit;

interface

uses
  System.Generics.Collections;

type
  IObservationItem = interface(IUnknown) ['{4355136D-762F-44C2-AD0B-E29FBC7EC7AD}']
    function GetName: string;
    function GetExportedName: string;
    function GetGUID: string;
    function GetScreenObject: TObject;
    function GetObservedValue: double;
    function GetWeight: Double;
    function GetObservationGroup: string;

    function ObservationType: string;

    procedure SetObservedValue(const Value: double);

    property Name: string read GetName;
    property ExportedName: string read GetExportedName;
    property GUID: string read GetGUID;
    property ScreenObject: TObject read GetScreenObject;
    property ObservedValue: double read GetObservedValue
      write SetObservedValue;
    property Weight: Double read GetWeight;
    property ObservationGroup: string read GetObservationGroup;
  end;

  ITimeObservationItem = interface(IObservationItem) ['{13D232D3-243D-4175-9744-557A27ADCC2D}']
//    function GetName: string;
//    function GetObservedValue: double;
    function GetTime: double;
    function GetWeight: Double;
    function GetObservationGroup: string;

//    procedure SetObservedValue(const Value: double);
    procedure SetTime(const Value: double);

//    property Name: string read GetName;
//    property ObservedValue: double read GetObservedValue
//      write SetObservedValue;
    property Time: double read GetTime write SetTime;
    property Weight: Double read GetWeight;
    property ObservationGroup: string read GetObservationGroup;
  end;

  TObservationInterfaceList = TList<IObservationItem>;

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
