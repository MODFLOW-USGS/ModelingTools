unit Modflow6DynamicTimeSeriesUnit;

interface

uses
  System.Classes, GoPhastTypes,
  OrderedCollectionUnit,
  OrderedCollectionInterfaceUnit,
  SubscriptionUnit, SubscriptionInterfaceUnit,
  FormulaManagerInterfaceUnit,
  System.IOUtils,
  Modflow6DynamicTimeSeriesInterfaceUnit, Modflow6TimeSeriesInterfaceUnit,
  Modflow6TimeSeriesCollectionsUnit, System.SysUtils,
  ScreenObjectInterfaceUnit, Modflow6TimeSeriesCollectionsInterfaceUnit,
  RbwParser, System.Generics.Collections, PhastModelInterfaceUnit;

type
  TDynamicTimeSeriesFormulaItem = class (TFormulaOrderedItem,
    IDynamicTimeSeriesFormulaItem)
  private
    FObserver: TObserver;
    FValue: IFormulaObject;
    FModel: IModelForDynamicTimeSeries;
    FScreenObject: IScreenObject;
    procedure SetValue(const Value: string);
    function GetValue: string;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
  protected
    function GetScreenObject: TObject; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
  public
    property Observer: TObserver read FObserver;
    property ValueObject: IFormulaObject read FValue write FValue;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TDynamicTimeSeriesItem = class;

  TDynamicTimeSeries = class(TOrderedCollection, ITimeSeries,
    IDynamicTimeSeries)
  private
    FDeleted: Boolean;
    FSeriesName: AnsiString;
    FScaleFactorParameter: string;
    FParamMethod: TPestParamMethod;
    FInterpolationMethod: TMf6InterpolationMethods;
    FStoredScaleFactor: TRealStorage;
    FNotifierComponent: TComponent;
    FModel: IModelForDynamicTimeSeries;
    FOrientation: TDataSetOrientation;
    FScreenObject: IScreenObject;
    FTimeSeriesLocationDictionary: TTimeSeriesLocationDictionary;
    FGroup: TDynamicTimeSeriesItem;
    FUsesList: TStringList;
    function GetScaleFactor: double;
    function GetItems(Index: Integer): IDynamicTimeSeriesFormulaItem;
    procedure SetItems(Index: Integer; const Value: IDynamicTimeSeriesFormulaItem);
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetParamMethod(const Value: TPestParamMethod);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: AnsiString);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    procedure SetScaleFactor(const Value: double);
    procedure SetOrientation(const Value: TDataSetOrientation);
    function GetSeriesName: AnsiString;
    function GetInterpolationMethod: TMf6InterpolationMethods;
    function GetScaleFactorParameter: string;
    function GetParamMethod: TPestParamMethod;
    function GetOrientation: TDataSetOrientation;
    function GetDeleted: Boolean;
    procedure SetDeleted(const Value: Boolean);
    function GetStaticTimeSeries(Location: TTimeSeriesLocation): IMf6TimeSeries;
    function GetUsesList: TStringList;
    procedure Invalidate;
    function IsSameI(DynamicTimeSeriesCollection: IDynamicTimeSeries): Boolean;
  public
    constructor Create(ModelInterface: IModelForDynamicTimeSeries;
      ScreenObject: IScreenObject; Group: TDynamicTimeSeriesItem);
    destructor Destroy; override;
    function IsSame(AnOrderedCollection: TOrderedCollection): Boolean; override;
    property  Items[Index: Integer]: IDynamicTimeSeriesFormulaItem read GetItems
      write SetItems; default;
    function Add: IDynamicTimeSeriesFormulaItem;
    procedure Assign(Source: TPersistent); override;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    property NotifierComponent: TComponent read FNotifierComponent;
    property StaticTimeSeries[Location: TTimeSeriesLocation]: IMf6TimeSeries
      read GetStaticTimeSeries;
    property Orientation: TDataSetOrientation read GetOrientation
      write SetOrientation;
    property UsesList: TStringList read GetUsesList;
  published
    property SeriesName: AnsiString read GetSeriesName write SetSeriesName;
    property InterpolationMethod: TMf6InterpolationMethods
      read GetInterpolationMethod write SetInterpolationMethod;
    property StoredScaleFactor: TRealStorage read FStoredScaleFactor
      write SetStoredScaleFactor;
    property ScaleFactorParameter: string read GetScaleFactorParameter
      write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read GetParamMethod
      write SetParamMethod;
    property Deleted: Boolean read GetDeleted write SetDeleted;
  end;

  TDyanmicTimesSeriesCollection = class;

  TDynamicTimeSeriesItem = class(TOrderedItem,  IDynamicTimeSeriesItem, ITimeSeriesItem)
  private
    FDynamicTimeSeries: TDynamicTimeSeries;
    FModel: IModelForDynamicTimeSeries;
    FScreenObject: IScreenObject;
    FStaticGroup: ITimesSeriesCollection;
    FDyanmicCollection: TDyanmicTimesSeriesCollection;
    FStaticCollection: ITimesSeriesCollections;
    FStaticItem: ITimeSeriesCollectionItem;
    procedure SetTimeSeriesI(const Value: IDynamicTimeSeries);
    function GetDynamicTimeSeriesI: IDynamicTimeSeries;
    function GetTimeSeriesI: ITimeSeries;
    procedure SetTimeSeries(const Value: TDynamicTimeSeries);
    function GetStaticGroup: ITimesSeriesCollection;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetTimesSeriesCollectionI: IDyanmicTimesSeriesCollection;
    property TimesSeriesCollectionI: IDyanmicTimesSeriesCollection read GetTimesSeriesCollectionI;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TimeSeriesI: IDynamicTimeSeries read GetDynamicTimeSeriesI
      write SetTimeSeriesI;
    property StaticGroup: ITimesSeriesCollection read GetStaticGroup;
  published
    property TimeSeries: TDynamicTimeSeries read FDynamicTimeSeries
      write SetTimeSeries;
  end;

  TDyanmicTimesSeriesCollection = class(TCustomTimesSeriesCollection,
    IDyanmicTimesSeriesCollection, ITimesSeriesCollection)
  private
    FTimeSeriesDictionary: TCacheDictionary<string, IDynamicTimeSeries>;
    FModel: IModelMuseModel;
    FScreenObject: IScreenObject;
    function GetItemI(Index: Integer): ITimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItemI(Index: Integer; const Value: ITimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
    function GetItem(Index: Integer): TDynamicTimeSeriesItem;
    procedure SetItem(Index: Integer; const Value: TDynamicTimeSeriesItem);
    procedure Invalidate;
  public
    Constructor Create(Model: IModelMuseModel; ScreenObject: IScreenObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): IDynamicTimeSeries;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property ItemsI[Index: Integer]: ITimeSeriesItem read GetItemI write SetItemI; default;
    property Items[Index: Integer]: TDynamicTimeSeriesItem read GetItem write SetItem;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    function Add: TDynamicTimeSeriesItem;
    function AddI: ITimeSeriesItem;
    procedure Loaded;
  end;

  TDynamicTimeSeriesCollectionItem = class(TOrderedItem, ITimeSeriesCollectionItem)
  private
    FTimesSeriesCollection: TDyanmicTimesSeriesCollection;
    function GetTimesSeriesCollectionI: ITimesSeriesCollection;
    procedure SetTimesSeriesCollection(const Value: TDyanmicTimesSeriesCollection);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TimesSeriesCollectionI: ITimesSeriesCollection read GetTimesSeriesCollectionI;
  published
    property TimesSeriesCollection: TDyanmicTimesSeriesCollection
      read FTimesSeriesCollection write SetTimesSeriesCollection;
  end;

  TDynamicTimesSeriesGroups = TObjectList<TDyanmicTimesSeriesCollection>;

  TDynamicTimesSeriesCollections = class(TOrderedCollection)
  private
    FTimeSeriesGroupsDictionary: TCacheDictionary<string, TDyanmicTimesSeriesCollection>;
    FTimeSeriesDictionary: TCacheDictionary<string, TDynamicTimeSeries>;
    FTimeSeriesNames: TStringList;
    FScreenObject: IScreenObject;
    function GetItem(Index: Integer): TDynamicTimeSeriesCollectionItem;
    procedure SetItem(Index: Integer; const Value: TDynamicTimeSeriesCollectionItem);
    function GetTimeSeriesNames: TStringList;
  public
    Constructor Create(Model: IModelForTOrderedCollection; ScreenObject: IScreenObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TDynamicTimeSeriesCollectionItem
      read GetItem write SetItem; default;
    function Add: TDynamicTimeSeriesCollectionItem;
    function AddI: ITimeSeriesCollectionItem;
    function GetTimeSeriesByName(ASeriesName: String): TDynamicTimeSeries;
    procedure GetTimesSeriesGroups(SeriesNames: TStrings;
      Groups: TDynamicTimesSeriesGroups);
    function GetTimesSeriesCollectionBySeriesName(
      const ASeriesName: string): TDyanmicTimesSeriesCollection;
    property TimeSeriesNames: TStringList read GetTimeSeriesNames;
    procedure Loaded;
    procedure Invalidate;
  end;


procedure DynamicTimeItemRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure DynamicTimeItemRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

{ TDynamicTimeSeriesItem }

procedure TDynamicTimeSeriesFormulaItem.Assign(Source: TPersistent);
begin
  if Source is TDynamicTimeSeriesFormulaItem then
  begin
    Value := TDynamicTimeSeriesFormulaItem(Source).Value;
  end;
  inherited;
end;

constructor TDynamicTimeSeriesFormulaItem.Create(Collection: TCollection);
var
  TimeSeries: TDynamicTimeSeries;
  LocalModel: IModelForDynamicTimeSeries;
begin
  Assert(Collection <> nil);
  inherited;
  FObserver:= TObserver.Create(nil);
  TimeSeries := (Collection as TDynamicTimeSeries);
  FScreenObject := TimeSeries.FScreenObject;
  FModel := TimeSeries.FModel;

  if FModel <> nil then
  begin
    OnRemoveSubscription := DynamicTimeItemRemoveSubscription;
    OnRestoreSubscription := DynamicTimeItemRestoreSubscription;
    FValue := FModel.AddFormulaObject;
    FValue.Parser := FModel.FormulaCompiler[TimeSeries.Orientation, eaBlocks];
    FValue.AddSubscriptionEvents(DynamicTimeItemRemoveSubscription,
    DynamicTimeItemRestoreSubscription, self);
  end
  else
  begin
    if IGlobalModel.QueryInterface(IModelForDynamicTimeSeries, LocalModel) <> 0 then
    begin
      Assert(False)
    end;
    FValue := LocalModel.AddFormulaObject;
    FValue.Parser := LocalModel.FormulaCompiler[TimeSeries.Orientation, eaBlocks];
  end;
end;

destructor TDynamicTimeSeriesFormulaItem.Destroy;
var
  LocalModel: IModelForDynamicTimeSeries;
begin
  Value := '0';
  if FModel <> nil then
  begin
    FModel.RemoveFormulaObject(FValue,
      DynamicTimeItemRemoveSubscription,
      DynamicTimeItemRestoreSubscription, self);
  end
  else
  begin
    if IGlobalModel.QueryInterface(IModelForDynamicTimeSeries, LocalModel) <> 0 then
    begin
      Assert(False);
    end;
    LocalModel.RemoveFormulaObject(FValue,
      nil,
      nil, self);
  end;
  FObserver.Free;
  inherited;
end;

function TDynamicTimeSeriesFormulaItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserver;
end;

function TDynamicTimeSeriesFormulaItem.GetScreenObject: TObject;
begin
   if FScreenObject = nil then
  begin
    result := nil;
  end
  else
  begin
    result := FScreenObject as TObject;
  end;
end;

function TDynamicTimeSeriesFormulaItem.GetValue: string;
begin
  Result := FValue.Formula;
  FObserver.UpToDate := True;
end;

function TDynamicTimeSeriesFormulaItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TDynamicTimeSeriesFormulaItem then
  begin
    result := Value = TDynamicTimeSeriesFormulaItem(AnotherItem).Value;
  end
  else
  begin
    result := false;
  end;
end;

procedure TDynamicTimeSeriesFormulaItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := FModel.GetObserverByName(AName);
  DS.StopsTalkingTo(FObserver);
end;

procedure TDynamicTimeSeriesFormulaItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := FModel.GetObserverByName(AName);
  DS.TalksTo(FObserver);
  FObserver.UpToDate := False;
end;

procedure TDynamicTimeSeriesFormulaItem.SetValue(const Value: string);
var
  Dummy: Integer;
begin
  Dummy := 0;
  UpdateFormulaBlocks(Value, Dummy, FValue);
end;

{ TDynamicTimeSeriesCollection }

function TDynamicTimeSeries.Add: IDynamicTimeSeriesFormulaItem;
begin
  Result := inherited Add as TDynamicTimeSeriesFormulaItem;
end;

procedure TDynamicTimeSeries.Assign(Source: TPersistent);
var
  TimeSeriesSource: TDynamicTimeSeries;
begin
  if Source is TDynamicTimeSeries then
  begin
    TimeSeriesSource := TDynamicTimeSeries(Source);
    SeriesName := TimeSeriesSource.SeriesName;
    InterpolationMethod := TimeSeriesSource.InterpolationMethod;
    ScaleFactor := TimeSeriesSource.ScaleFactor;
    ScaleFactorParameter := TimeSeriesSource.ScaleFactorParameter;
    ParamMethod := TimeSeriesSource.ParamMethod;
    Orientation := TimeSeriesSource.Orientation;
    Deleted := TimeSeriesSource.Deleted;
  end;
  inherited;
end;

constructor TDynamicTimeSeries.Create(
  ModelInterface: IModelForDynamicTimeSeries; ScreenObject: IScreenObject;
  Group: TDynamicTimeSeriesItem);
var
  NotifyEvent: TNotifyEvent;
  ModelI: IModelForTOrderedCollection;
begin
  if ModelInterface = nil then
  begin
    NotifyEvent := nil;
    ModelI := nil;
  end
  else
  begin
    NotifyEvent := ModelInterface.Invalidate;
    if ModelInterface.QueryInterface(IModelForTOrderedCollection, ModelI) <> 0 then
    begin
      Assert(False);
    end;
  end;
  FScreenObject := ScreenObject;
  inherited Create(TDynamicTimeSeriesFormulaItem, ModelI);
  FStoredScaleFactor := TRealStorage.Create(NotifyEvent);
  FNotifierComponent := TComponent.Create(nil);
  FModel := ModelInterface;
  FTimeSeriesLocationDictionary := TTimeSeriesLocationDictionary.Create;
  FGroup := Group;
end;

destructor TDynamicTimeSeries.Destroy;
begin
  FUsesList.Free;
  FTimeSeriesLocationDictionary.Free;
  FNotifierComponent.Free;
  FStoredScaleFactor.Free;
  inherited;
end;

function TDynamicTimeSeries.GetDeleted: Boolean;
begin
  Result := FDeleted;
end;

function TDynamicTimeSeries.GetInterpolationMethod: TMf6InterpolationMethods;
begin
  result := FInterpolationMethod;
end;

function TDynamicTimeSeries.GetItems(
  Index: Integer): IDynamicTimeSeriesFormulaItem;
begin
  result := inherited Items[Index] as TDynamicTimeSeriesFormulaItem
end;

function TDynamicTimeSeries.GetOrientation: TDataSetOrientation;
begin
  if FScreenObject <> nil then
  begin
    if FScreenObject.ElevationCount = ecZero then
    begin
      FOrientation := dsoTop;
    end
    else
    begin
      FOrientation := dso3D;
    end;
  end;
  result := FOrientation;
end;

function TDynamicTimeSeries.GetParamMethod: TPestParamMethod;
begin
  result := FParamMethod;
end;

function TDynamicTimeSeries.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

function TDynamicTimeSeries.GetScaleFactorParameter: string;
begin
  Result := FScaleFactorParameter;
end;

function TDynamicTimeSeries.GetSeriesName: AnsiString;
begin
  result := FSeriesName;
end;

function TDynamicTimeSeries.GetStaticTimeSeries(
  Location: TTimeSeriesLocation): IMf6TimeSeries;
var
  Index: Integer;
  Formula: string;
  Compiler: TRbwParser;
  Expression: TExpression;
//  VariablesUsed: TStringList;
//  VariableIndex: Integer;
  TimeSeriesI: ITimeSeries;
begin
  if not FTimeSeriesLocationDictionary.TryGetValue(Location, result) then
  begin
    TimeSeriesI := FGroup.StaticGroup.AddI.TimeSeriesI;
    if TimeSeriesI.QueryInterface(IMf6TimeSeries, result) <> 0 then
    begin
      Assert(False);
    end;
//    result := FGroup.StaticGroup.AddI.TimeSeriesI;
    Assert(FGroup.FStaticCollection <> nil);
    result.SeriesName := FGroup.FStaticCollection.DefaultTimeSeriesName;
    result.ScaleFactor := ScaleFactor;
    result.InterpolationMethod := InterpolationMethod;
    result.ScaleFactorParameter := ScaleFactorParameter;
    result.ParamMethod := ParamMethod;

    result.Count := Count;
    Compiler := FModel.FormulaCompiler[Orientation, eaBlocks];

    for Index := 0 to Count - 1 do
    begin
      Formula := Items[Index].Value;
      try
        Compiler.Compile(Formula);
      except on ERbwParserError do
        begin
          Formula := '0';
          Compiler.Compile(Formula);
        end;
      end;
      Expression := Compiler.CurrentExpression;
      if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
      begin
        Formula := '0';
        Compiler.Compile(Formula);
        Expression := Compiler.CurrentExpression;
      end;
      Expression.Evaluate;
      result.Values[Index] := Expression.DoubleResult;
    end;

    FTimeSeriesLocationDictionary.Add(Location, result)
  end;
end;

function TDynamicTimeSeries.GetUsesList: TStringList;
var
  index: Integer;
  Compiler: TRbwParser;
  Formula: string;
  Expression: TExpression;
begin
  if FUsesList = nil then
  begin
    FUsesList := TStringList.Create;
  end;
  FUsesList.Clear;
  FUsesList.Sorted := True;
  FUsesList.Duplicates := dupIgnore;
  FUsesList.CaseSensitive := False;
  Compiler := FModel.FormulaCompiler[Orientation, eaBlocks];

  for Index := 0 to Count - 1 do
  begin
    Formula := Items[Index].Value;
    try
      Compiler.Compile(Formula);
    except on ERbwParserError do
      begin
        Formula := '0';
        Compiler.Compile(Formula);
      end;
    end;
    Expression := Compiler.CurrentExpression;
    if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
    begin
      Formula := '0';
      Compiler.Compile(Formula);
      Expression := Compiler.CurrentExpression;
    end;
    FUsesList.AddStrings(Expression.VariablesUsed);
  end;
  result := FUsesList;
end;

procedure TDynamicTimeSeries.Invalidate;
begin
  FTimeSeriesLocationDictionary.Clear;
end;

function TDynamicTimeSeries.IsSame(AnOrderedCollection: TOrderedCollection): Boolean;
var
//  index: Integer;
  DynamicTimeSeriesCollection: TDynamicTimeSeries;
begin
  // if Assign is updated, update IsSame too.
  result := (AnOrderedCollection is TDynamicTimeSeries) and inherited;

  if result then
  begin
    DynamicTimeSeriesCollection := TDynamicTimeSeries(AnOrderedCollection);
    result := (Count = DynamicTimeSeriesCollection.Count)
      and (SeriesName = DynamicTimeSeriesCollection.SeriesName)
      and (ScaleFactor = DynamicTimeSeriesCollection.ScaleFactor)
      and (InterpolationMethod = DynamicTimeSeriesCollection.InterpolationMethod)
      and (ScaleFactorParameter = DynamicTimeSeriesCollection.ScaleFactorParameter)
      and (ParamMethod = DynamicTimeSeriesCollection.ParamMethod)
      and (Orientation = DynamicTimeSeriesCollection.Orientation)
      and (Deleted = DynamicTimeSeriesCollection.Deleted);

//    if result then
//    begin
//      for index := 0 to Count - 1 do
//      begin
//        result := Items[index].IsSame(DynamicTimeSeriesCollection[index]);
//        if not result then
//        begin
//          Exit;
//        end;
//      end;
//    end;
  end;

end;

function TDynamicTimeSeries.IsSameI(
  DynamicTimeSeriesCollection: IDynamicTimeSeries): Boolean;
begin
  Result := IsSame(DynamicTimeSeriesCollection as TDynamicTimeSeries)
end;

procedure TDynamicTimeSeries.SetDeleted(const Value: Boolean);
begin
  FDeleted := Value;
end;

procedure TDynamicTimeSeries.SetInterpolationMethod(
  const Value: TMf6InterpolationMethods);
begin
  if FInterpolationMethod <> Value then
  begin
    FInterpolationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeries.SetItems(Index: Integer;
  const Value: IDynamicTimeSeriesFormulaItem);
begin
  inherited Items[Index] := Value as TDynamicTimeSeriesFormulaItem;
end;

procedure TDynamicTimeSeries.SetOrientation(
  const Value: TDataSetOrientation);
begin
  FOrientation := Value;
end;

procedure TDynamicTimeSeries.SetParamMethod(
  const Value: TPestParamMethod);
begin
  if FParamMethod <> Value then
  begin
    FParamMethod := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeries.SetScaleFactor(const Value: double);
begin
  StoredScaleFactor.Value := Value;
end;

procedure TDynamicTimeSeries.SetScaleFactorParameter(
  const Value: string);
begin
  if FScaleFactorParameter <> Value then
  begin
    FScaleFactorParameter := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeries.SetSeriesName(Value: AnsiString);
var
  CharIndex: Integer;
  AChar: Char;
begin
  Value := AnsiString(Trim(Copy(Trim(string(Value)), 1, MaxTimeSeriesNameLength)));
  for CharIndex := 1 to Length(Value) do
  begin
    AChar := Char(Value[CharIndex]);
    if (AChar = ' ') or not (TPath.IsValidFileNameChar(AChar)) then
    begin
      Value[CharIndex] := '_'
    end;
  end;
  if Value = '' then
  begin
    Value := '_';
  end;
  if FSeriesName <> Value then
  begin
    FSeriesName := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeries.SetStoredScaleFactor(
  const Value: TRealStorage);
begin
  FStoredScaleFactor.Assign(Value);
end;

{ TDynamicTimeSeriesItem }

procedure TDynamicTimeSeriesItem.Assign(Source: TPersistent);
begin
  if Source is TDynamicTimeSeriesItem then
  begin
    TimeSeriesI := TDynamicTimeSeriesItem(Source).TimeSeriesI;
  end
  else
  begin
    inherited;
  end;
end;

constructor TDynamicTimeSeriesItem.Create(Collection: TCollection);
begin
  Assert(Collection <> nil);
  inherited;
  FDyanmicCollection := (Collection as TDyanmicTimesSeriesCollection);
  FScreenObject := FDyanmicCollection.FScreenObject;
  FModel := FDyanmicCollection.FModel as IModelForDynamicTimeSeries;
  FDynamicTimeSeries := TDynamicTimeSeries.Create(
    FModel as IModelForDynamicTimeSeries, FScreenObject, self);
end;

destructor TDynamicTimeSeriesItem.Destroy;
begin
  FDynamicTimeSeries.Free;
  inherited;
end;

function TDynamicTimeSeriesItem.GetDynamicTimeSeriesI: IDynamicTimeSeries;
begin
  result := FDynamicTimeSeries;
end;

function TDynamicTimeSeriesItem.GetStaticGroup: ITimesSeriesCollection;
begin
  if FStaticGroup = nil then
  begin
    Assert(FModel <> nil);
    FStaticCollection :=  FModel.GetMf6TimesSeriesI;
    FStaticItem := FStaticCollection.AddI;
    FStaticGroup := FStaticItem.TimesSeriesCollectionI;
    FStaticGroup.GroupName := FStaticCollection.DefaultGroupName;
    FStaticGroup.Times := FDyanmicCollection.Times;
  end;
  result := FStaticGroup;
end;

function TDynamicTimeSeriesItem.GetTimeSeriesI: ITimeSeries;
begin
  result := FDynamicTimeSeries;
end;

function TDynamicTimeSeriesItem.GetTimesSeriesCollectionI: IDyanmicTimesSeriesCollection;
begin
  result := FDyanmicCollection;
end;

function TDynamicTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDynamicTimeSeriesItem)
    and (TimeSeriesI.IsSameI(TDynamicTimeSeriesItem(AnotherItem).TimeSeriesI));
end;

procedure TDynamicTimeSeriesItem.SetTimeSeries(const Value: TDynamicTimeSeries);
begin
  FDynamicTimeSeries.Assign(Value);
end;

procedure TDynamicTimeSeriesItem.SetTimeSeriesI(
  const Value: IDynamicTimeSeries);
begin
  TimeSeries := Value as TDynamicTimeSeries;
end;

{ TDyanmicTimesSeriesCollection }

function TDyanmicTimesSeriesCollection.Add: TDynamicTimeSeriesItem;
begin
  result := inherited Add as TDynamicTimeSeriesItem;
end;

function TDyanmicTimesSeriesCollection.AddI: ITimeSeriesItem;
begin
  result := inherited Add as TDynamicTimeSeriesItem;
end;

procedure TDyanmicTimesSeriesCollection.Assign(Source: TPersistent);
begin
  FTimeSeriesDictionary.Clear;
  inherited;
end;

constructor TDyanmicTimesSeriesCollection.Create(Model: IModelMuseModel;
  ScreenObject: IScreenObject);
var
  LocalInterface : IModelForTOrderedCollection;
begin
  FScreenObject := ScreenObject;
  LocalInterface := Model as IModelForTOrderedCollection;
  inherited Create(TDynamicTimeSeriesItem, LocalInterface);
  FTimeSeriesDictionary := TCacheDictionary<string, IDynamicTimeSeries>.Create;
  FModel := Model
end;

destructor TDyanmicTimesSeriesCollection.Destroy;
begin
  FTimeSeriesDictionary.Free;
  inherited;
end;

function TDyanmicTimesSeriesCollection.GetItem(
  Index: Integer): TDynamicTimeSeriesItem;
begin
  result := inherited Items[Index] as TDynamicTimeSeriesItem;
end;

function TDyanmicTimesSeriesCollection.GetItemI(
  Index: Integer): ITimeSeriesItem;
begin
  result := inherited Items[Index] as TDynamicTimeSeriesItem;
end;

function TDyanmicTimesSeriesCollection.GetTimeCount: Integer;
begin
  result := Times.Count;
end;

function TDyanmicTimesSeriesCollection.GetValuesByName(
  const AName: string): IDynamicTimeSeries;
var
  ItemIndex: Integer;
  TimeSeries: IDynamicTimeSeries;
  TimeSeriesI: ITimeSeries;
begin
  result := nil;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;

  if (Count > 0) and (FTimeSeriesDictionary.Count = 0) then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      TimeSeriesI := ItemsI[ItemIndex].TimeSeriesI;
      if TimeSeriesI.QueryInterface(IMf6TimeSeries, TimeSeries) <> 0 then
      begin
        Assert(False);
      end;
      if TimeSeries.Deleted then
      begin
        Continue;
      end;
      FTimeSeriesDictionary.Add(UpperCase(String(TimeSeries.SeriesName)), TimeSeries);
    end;
  end;
  if not FTimeSeriesDictionary.TryGetValue(UpperCase(AName), result) then
  begin
    result := nil;
  end;
end;

procedure TDyanmicTimesSeriesCollection.Invalidate;
var
  index: Integer;
  Item: TDynamicTimeSeriesItem;
begin
  for index := 0 to Count - 1 do
  begin
    Item := Items[index];
    if Item.FStaticGroup <> nil then
    begin
      Item.FStaticItem.Free;
      Item.FStaticItem := nil;
      Item.FStaticGroup := nil;
    end;
    Item.TimeSeries.Invalidate;
  end;
end;

function TDyanmicTimesSeriesCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := (AnOrderedCollection is TDyanmicTimesSeriesCollection)
    and inherited IsSame(AnOrderedCollection);
end;

procedure TDyanmicTimesSeriesCollection.Loaded;
var
  SeriesIndex: Integer;
begin
  for SeriesIndex := Count - 1 downto 0 do
  begin
    if ItemsI[SeriesIndex].TimeSeriesI.Deleted then
    begin
      (ItemsI[SeriesIndex] as TDynamicTimeSeriesItem).Free;
    end;
  end;
  FTimeSeriesDictionary.Clear;
end;

procedure TDyanmicTimesSeriesCollection.SetItem(Index: Integer;
  const Value: TDynamicTimeSeriesItem);
begin
  inherited Items[Index] := Value as TDynamicTimeSeriesItem;
end;

procedure TDyanmicTimesSeriesCollection.SetItemI(Index: Integer;
  const Value: ITimeSeriesItem);
begin
  inherited Items[Index] := Value as TDynamicTimeSeriesItem;
end;

procedure TDyanmicTimesSeriesCollection.SetTimeCount(const Value: Integer);
var
  ItemIndex: Integer;
  TimeIndex: Integer;
  AnItem: IDynamicTimeSeriesItem;
  AnItemI: ITimeSeriesItem;
begin
  for ItemIndex := 0 to Count-1 do
  begin
    AnItemI := ItemsI[ItemIndex];
    if AnItemI.QueryInterface(IDynamicTimeSeriesItem, AnItem) <> 0 then
    begin
      Assert(False);
    end;
    AnItem.TimeSeriesI.Count := Value;
    for TimeIndex := Times.Count to Value -1 do
    begin
      AnItem.TimeSeriesI[TimeIndex].Value := '3.0E30';
    end;
  end;
  Times.Count := Value;
end;

procedure DynamicTimeItemRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDynamicTimeSeriesFormulaItem).RemoveSubscription(Sender, AName);
end;

procedure DynamicTimeItemRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDynamicTimeSeriesFormulaItem).RestoreSubscription(Sender, AName);
end;

{ TDynamicTimeSeriesCollectionItem }

procedure TDynamicTimeSeriesCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TDynamicTimeSeriesCollectionItem then
  begin
    TimesSeriesCollection := TDynamicTimeSeriesCollectionItem(Source).TimesSeriesCollection
  end
  else
  begin
    inherited;
  end;
end;

constructor TDynamicTimeSeriesCollectionItem.Create(Collection: TCollection);
var
  ScreenObject: IScreenObject;
begin
  inherited;
  ScreenObject := (Collection as TDynamicTimesSeriesCollections).FScreenObject;
  FTimesSeriesCollection := TDyanmicTimesSeriesCollection.Create(Model, ScreenObject)
end;

destructor TDynamicTimeSeriesCollectionItem.Destroy;
begin
  FTimesSeriesCollection.Free;
  inherited;
end;

function TDynamicTimeSeriesCollectionItem.GetTimesSeriesCollectionI: ITimesSeriesCollection;
begin
  result := FTimesSeriesCollection;
end;

function TDynamicTimeSeriesCollectionItem.IsSame(
  AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDynamicTimeSeriesCollectionItem)
    and (TimesSeriesCollection.IsSame(
    TDynamicTimeSeriesCollectionItem(AnotherItem).TimesSeriesCollection));
end;

procedure TDynamicTimeSeriesCollectionItem.SetTimesSeriesCollection(
  const Value: TDyanmicTimesSeriesCollection);
begin
  FTimesSeriesCollection.Assign(Value);
end;

{ TDynamicTimesSeriesCollections }

function TDynamicTimesSeriesCollections.Add: TDynamicTimeSeriesCollectionItem;
begin
  result := inherited Add as TDynamicTimeSeriesCollectionItem;
end;

function TDynamicTimesSeriesCollections.AddI: ITimeSeriesCollectionItem;
begin
  result := Add;
end;

procedure TDynamicTimesSeriesCollections.Assign(Source: TPersistent);
begin
  inherited;
  FTimeSeriesGroupsDictionary.Clear;
  FTimeSeriesDictionary.Clear;
end;

constructor TDynamicTimesSeriesCollections.Create(
  Model: IModelForTOrderedCollection; ScreenObject: IScreenObject);
begin
  inherited Create(TDynamicTimeSeriesCollectionItem, Model);
  FScreenObject := ScreenObject;

  FTimeSeriesGroupsDictionary := TCacheDictionary<string, TDyanmicTimesSeriesCollection>.Create;
  FTimeSeriesDictionary := TCacheDictionary<string, TDynamicTimeSeries>.Create;
end;

destructor TDynamicTimesSeriesCollections.Destroy;
begin
  FTimeSeriesNames.Free;
  FTimeSeriesDictionary.Free;
  FTimeSeriesGroupsDictionary.Free;
  inherited;
end;

function TDynamicTimesSeriesCollections.GetItem(
  Index: Integer): TDynamicTimeSeriesCollectionItem;
begin
  result := inherited Items[Index] as TDynamicTimeSeriesCollectionItem;
end;

function TDynamicTimesSeriesCollections.GetTimeSeriesByName(
  ASeriesName: String): TDynamicTimeSeries;
var
  GroupIndex: Integer;
  SeriesIndex: Integer;
  AGroup: TDyanmicTimesSeriesCollection;
  ASeries: TDynamicTimeSeries;
begin
  result := nil;
  if IGlobalModel = nil then
  begin
    Exit;
  end;
  if IGlobalModel.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  if (Count > 0) and(FTimeSeriesDictionary.Count = 0) then
  begin
    for GroupIndex := 0 to Count - 1 do
    begin
      AGroup := Items[GroupIndex].TimesSeriesCollection;
      if AGroup.Deleted then
      begin
        Continue;
      end;
      for SeriesIndex := 0 to AGroup.Count - 1 do
      begin
        ASeries := AGroup.Items[SeriesIndex].TimeSeries;
        if ASeries.Deleted then
        begin
          Continue;
        end;
        FTimeSeriesDictionary.Add(UpperCase(string(ASeries.SeriesName)), ASeries);
      end;
    end;
  end;
  if not FTimeSeriesDictionary.TryGetValue(UpperCase(ASeriesName), result) then
  begin
    result := nil;
  end;
end;

function TDynamicTimesSeriesCollections.GetTimeSeriesNames: TStringList;
var
  GroupIndex: Integer;
  AGroup: TDyanmicTimesSeriesCollection;
  SeriesIndex: Integer;
  TimeSeries: TDynamicTimeSeries;
begin
  if FTimeSeriesNames = nil then
  begin
    FTimeSeriesNames := TStringList.Create;
  end
  else
  begin
    FTimeSeriesNames.Clear
  end;
  for GroupIndex := 0 to Count - 1 do
  begin
    AGroup := Items[GroupIndex].TimesSeriesCollection;
    if AGroup.Deleted then
    begin
      Continue;
    end;
    for SeriesIndex := 0 to AGroup.Count - 1 do
    begin
      TimeSeries := AGroup.Items[SeriesIndex].TimeSeries;
      if TimeSeries.Deleted then
      begin
        Continue;
      end;
      FTimeSeriesNames.AddObject(string(TimeSeries.SeriesName), TimeSeries);
    end;
  end;
  result := FTimeSeriesNames
end;

function TDynamicTimesSeriesCollections.GetTimesSeriesCollectionBySeriesName(
  const ASeriesName: string): TDyanmicTimesSeriesCollection;
var
  GroupIndex: Integer;
  SeriesIndex: Integer;
  AGroup: TDyanmicTimesSeriesCollection;
  TimeSeries: TDynamicTimeSeries;
begin
  result := nil;
  if (Count > 0) then
  begin
    if (FTimeSeriesGroupsDictionary.Count = 0) then
    begin
      for GroupIndex := 0 to Count - 1 do
      begin
        AGroup := Items[GroupIndex].TimesSeriesCollection;
        if AGroup.Deleted then
        begin
          Continue;
        end;
        for SeriesIndex := 0 to AGroup.Count - 1 do
        begin
          TimeSeries := AGroup.Items[SeriesIndex].TimeSeries;
          if TimeSeries.Deleted then
          begin
            Continue;
          end;
          FTimeSeriesGroupsDictionary.Add(UpperCase(String(TimeSeries.SeriesName)), AGroup);
        end;
      end;
    end;
    if not FTimeSeriesGroupsDictionary.TryGetValue(UpperCase(ASeriesName), result) then
    begin
      result := nil;
    end;
  end;
end;

procedure TDynamicTimesSeriesCollections.GetTimesSeriesGroups(
  SeriesNames: TStrings; Groups: TDynamicTimesSeriesGroups);
var
  SeriesIndex: Integer;
  GroupIndex: Integer;
  LocalSeriesNames: TStringList;
  SeriesName: string;
  AGroup: TDyanmicTimesSeriesCollection;
  UsedGroup: TDyanmicTimesSeriesCollection;
  TimeSeries: IDynamicTimeSeries;
begin
  Groups.Clear;
  if (SeriesNames.Count = 1) and (SeriesNames[0] = '') then
  begin
    Exit;
  end;
  LocalSeriesNames := TStringList.Create;
  try
    LocalSeriesNames.AddStrings(SeriesNames);
    for GroupIndex := 0 to Count - 1 do
    begin
      AGroup := Items[GroupIndex].TimesSeriesCollection;
      if AGroup.Deleted then
      begin
        Continue;
      end;
      UsedGroup := nil;
      for SeriesIndex := LocalSeriesNames.Count - 1 downto 0 do
      begin
        SeriesName := LocalSeriesNames[SeriesIndex];
        if SeriesName <> '' then
        begin
          TimeSeries := AGroup.GetValuesByName(SeriesName);
          if (TimeSeries <> nil) and not TimeSeries.Deleted then
          begin
            if UsedGroup = nil then
            begin
              UsedGroup := TDyanmicTimesSeriesCollection.Create(nil, nil);
              UsedGroup.Times := AGroup.Times;
              UsedGroup.GroupName := AGroup.GroupName;
            end;
            UsedGroup.Add.TimeSeries := TimeSeries as TDynamicTimeSeries;
            LocalSeriesNames.Delete(SeriesIndex);
          end;
        end
        else
        begin
          LocalSeriesNames.Delete(SeriesIndex);
        end;
      end;
      if UsedGroup <> nil then
      begin
        Groups.Add(UsedGroup);
      end;
    end;
  finally
    LocalSeriesNames.Free;
  end;
end;

procedure TDynamicTimesSeriesCollections.Invalidate;
var
  index: Integer;
  TimeSeriesCollection: TDyanmicTimesSeriesCollection;
begin
  for index := 0 to Count - 1 do
  begin
    TimeSeriesCollection := Items[index].TimesSeriesCollection;
    TimeSeriesCollection.Invalidate;
  end;
end;

procedure TDynamicTimesSeriesCollections.Loaded;
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    if Items[Index].TimesSeriesCollection.Deleted then
    begin
      Items[Index].Free;
    end
    else
    begin
      Items[Index].TimesSeriesCollection.Loaded;
    end;
  end;
  FTimeSeriesDictionary.Clear;
  FTimeSeriesGroupsDictionary.Clear;
end;

procedure TDynamicTimesSeriesCollections.SetItem(Index: Integer;
  const Value: TDynamicTimeSeriesCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.
