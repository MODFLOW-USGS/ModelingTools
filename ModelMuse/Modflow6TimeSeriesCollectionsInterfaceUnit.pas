unit Modflow6TimeSeriesCollectionsInterfaceUnit;

interface

uses
  GoPhastTypes, Modflow6TimeSeriesInterfaceUnit, OrderedCollectionInterfaceUnit;

type
  ITimeSeriesItem = interface
    ['{0898E1CE-26BE-44FD-8E78-9D13804D83D7}']
    function GetTimeSeriesI: ITimeSeries;
    property TimeSeriesI: ITimeSeries read GetTimeSeriesI;
  end;

  ITimesSeriesCollection = interface(IOrderedCollection)
    ['{69DF209F-0771-455C-93A6-2E59B9610981}']
    function GetTimeCount: Integer;
    procedure SetTimeCount(const Value: Integer);
    property TimeCount: Integer read GetTimeCount write SetTimeCount;

    function GetTimes: TRealCollection;
    procedure SetTimes(const Value: TRealCollection);
    property Times: TRealCollection read GetTimes write SetTimes;

    function GetGroupName: AnsiString;
    procedure SetGroupName(Value: AnsiString);
    property GroupName: AnsiString read GetGroupName write SetGroupName;

    function AddI: ITimeSeriesItem;

    function GetCount: Integer;
    procedure SetCount(const Value: Integer);
    property Count: Integer read GetCount write SetCount;

    function GetItemI(Index: Integer): ITimeSeriesItem;
    procedure SetItemI(Index: Integer; const Value: ITimeSeriesItem);
    property ItemsI[Index: Integer]: ITimeSeriesItem read GetItemI write SetItemI;
  end;

  ITimeSeriesCollectionItem = interface
    ['{7E46DB61-C7AD-4F1E-BA7D-3E0F4F002B32}']
    function GetTimesSeriesCollectionI: ITimesSeriesCollection;
    property TimesSeriesCollectionI: ITimesSeriesCollection read GetTimesSeriesCollectionI;
    procedure Free;
  end;

  ITimesSeriesCollections = interface
    ['{136A260B-2DDA-4DF2-A911-FA951FA5D0B9}']
    function AddI: ITimeSeriesCollectionItem;
    function DefaultGroupName: AnsiString;
    function DefaultTimeSeriesName: AnsiString;
  end;

  IModelForTimesSeriesInterface = interface(IModelMuseModel)
    ['{C73F73F3-EE7F-476F-8280-7AF9A970F681}']
    function TimeToTimeStepTimes(ATime: double; out StartTime, EndTime: double): Boolean;
    function GetPestParameterValueByName(PestParamName: string; out Value: double): Boolean;
    function GetMf6TimesSeriesI: ITimesSeriesCollections;
    property Mf6TimesSeriesI: ITimesSeriesCollections read GetMf6TimesSeriesI;
  end;

implementation

end.
