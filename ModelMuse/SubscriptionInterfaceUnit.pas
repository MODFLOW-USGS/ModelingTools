unit SubscriptionInterfaceUnit;

interface

uses
  GoPhastTypes, System.Classes;

type
  IObserver = interface
    ['{09D89465-BB64-4C1A-8075-9AEF799FF46B}']
    function GetName: TComponentName;
    procedure SetName(const Value: TComponentName);
    property Name: TComponentName read GetName write SetName;
    procedure StopTalkingToAnyone;
    function GetUpToDate: boolean;
    procedure SetUpToDate(const Value: boolean);
    property UpToDate: boolean read GetUpToDate write SetUpToDate;
  end;



implementation

end.
