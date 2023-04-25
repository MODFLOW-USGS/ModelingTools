unit Modflow6DynamicTimeSeriesInterfaceUnit;

interface

uses
  System.Classes, GoPhastTypes, SubscriptionUnit,
  OrderedCollectionInterfaceUnit, Modflow6TimeSeriesInterfaceUnit,
  System.Generics.Defaults, System.Hash, System.Generics.Collections, RbwParser,
  FormulaManagerInterfaceUnit;

type
  IModelForDynamicTimeSeries = interface(IModelMuseModel)
    ['{3ED8161F-DA7D-49D4-BB2C-AC774A26B683}']
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
    property FormulaCompiler[const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt]:TRbwParser read GetCompiler;
    function AddFormulaObject: IFormulaObject;
    procedure RemoveFormulaObject(FormulaObject: IFormulaObject;
      OnRemoveSubscription, OnRestoreSubscription:TChangeSubscription;
      Subject: TObject); overload;
    function GetObserverByName(const ObserverName: string): TObserver;
  end;

  TTimeSeriesLocation = record
    Layer: Integer;
    Row: Integer;
    Column: Integer;
  end;

  TTTimeSeriesLocationComparer = class(TEqualityComparer<TTimeSeriesLocation>)
    function Equals(const Left, Right: TTimeSeriesLocation): Boolean; override;
    function GetHashCode(const Value: TTimeSeriesLocation): Integer; override;
  end;

  TTimeSeriesLocationDictionary = class(TDictionary<
    TTimeSeriesLocation, ITimeSeries>)
    constructor Create;
  end;

  IDynamicTimeSeriesFormulaItem = interface(IOrderedItem)
    ['{490D60F0-33BB-47E7-9447-05FCE89BAA37}']
    procedure SetValue(const Value: string);
    function GetValue: string;
    property Value: string read GetValue write SetValue;
  end;

  IDynamicTimeSeries = interface(ITimeSeries)
    ['{C3064432-B8D5-4D5A-81B6-BE42AACF39D4}']
    function GetCount: Integer;
    procedure SetCount(Const Value: Integer);
    property Count: Integer read GetCount write SetCount;
    function GetItems(Index: Integer): IDynamicTimeSeriesFormulaItem;
    procedure SetItems(Index: Integer; const Value: IDynamicTimeSeriesFormulaItem);
    property  Items[Index: Integer]: IDynamicTimeSeriesFormulaItem read GetItems
      write SetItems; default;
    function GetOrientation: TDataSetOrientation;
    procedure SetOrientation(const Value: TDataSetOrientation);
    property Orientation: TDataSetOrientation read GetOrientation
      write SetOrientation;
    function IsSame(DynamicTimeSeriesCollection: IDynamicTimeSeries): Boolean;
    function GetDeleted: Boolean;
    procedure SetDeleted(const Value: Boolean);
    property Deleted: Boolean read GetDeleted write SetDeleted;
  end;

  IDynamicTimeSeriesItem = interface(IOrderedItem)
    ['{5FAB459C-747A-46F8-81F9-FA7AEF8BC635}']
    procedure SetTimeSeries(const Value: IDynamicTimeSeries);
    function GetDynamicTimeSeries: IDynamicTimeSeries;
    property TimeSeries: IDynamicTimeSeries read GetDynamicTimeSeries
      write SetTimeSeries;
  end;

  IDyanmicTimesSeriesCollection= interface(IOrderedCollection)
    ['{9A08902D-6085-4D6C-B081-F3108116D897}']
  end;

implementation

{ TTTimeSeriesLocationComparer }

function TTTimeSeriesLocationComparer.Equals(const Left,
  Right: TTimeSeriesLocation): Boolean;
begin
  Result := (Left.Layer = Right.Layer)
    and (Left.Row = Right.Row)
    and (Left.Column = Right.Column)

end;

function TTTimeSeriesLocationComparer.GetHashCode(
  const Value: TTimeSeriesLocation): Integer;
begin
  {$IF CompilerVersion > 28}
  Result := THashBobJenkins.GetHashValue(Value.Layer, SizeOf(Value.Layer), 0);
  Result := THashBobJenkins.GetHashValue(Value.Row, SizeOf(Value.Row), Result);
  Result := THashBobJenkins.GetHashValue(Value.Column, SizeOf(Value.Column), Result);
  {$ELSE}
  Result := BobJenkinsHash(Value.Layer, SizeOf(Value.Layer), 0);
  Result := BobJenkinsHash(Value.Row, SizeOf(Value.Row), Result);
  Result := BobJenkinsHash(Value.Column, SizeOf(Value.Column), Result);
  {$ENDIF}
end;

{ TTimeSeriesLocationDictionary }

constructor TTimeSeriesLocationDictionary.Create;
begin
  inherited Create(TTTimeSeriesLocationComparer.Create);
end;

end.
