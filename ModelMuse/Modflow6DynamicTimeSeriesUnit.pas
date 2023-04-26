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
  RbwParser;

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

  TDynamicTimeSeries = class(TPhastCollection, ITimeSeries,
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
  public
    constructor Create(ModelInterface: IModelForDynamicTimeSeries;
      ScreenObject: IScreenObject; Group: TDynamicTimeSeriesItem);
    destructor Destroy; override;
    function IsSame(DynamicTimeSeriesCollection: IDynamicTimeSeries): Boolean;
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

  TDynamicTimeSeriesItem = class(TOrderedItem, IDynamicTimeSeriesItem)
  private
    FDynamicTimeSeries: TDynamicTimeSeries;
    FModel: IModelForDynamicTimeSeries;
    FScreenObject: IScreenObject;
    FStaticGroup: ITimesSeriesCollection;
    FDyanmicCollection: TDyanmicTimesSeriesCollection;
    FStaticCollection: ITimesSeriesCollections;
    procedure SetTimeSeriesI(const Value: IDynamicTimeSeries);
    function GetDynamicTimeSeriesI: IDynamicTimeSeries;
    procedure SetTimeSeries(const Value: TDynamicTimeSeries);
    function GetStaticGroup: ITimesSeriesCollection;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
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
    IDyanmicTimesSeriesCollection)
  private
    FTimeSeriesDictionary: TCacheDictionary<string, IDynamicTimeSeries>;
    FModel: IModelMuseModel;
    FScreenObject: IScreenObject;
    function GetItem(Index: Integer): IDynamicTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: IDynamicTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
  public
    Constructor Create(Model: IModelMuseModel; ScreenObject: IScreenObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): IDynamicTimeSeries;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: IDynamicTimeSeriesItem read GetItem write SetItem; default;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    function Add: IDynamicTimeSeriesItem;
    procedure Loaded;
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
begin
  Assert(Collection <> nil);
  inherited;
  FObserver:= TObserver.Create(nil);
  TimeSeries := (Collection as TDynamicTimeSeries);
  FScreenObject := TimeSeries.FScreenObject;
  FModel := TimeSeries.FModel;

  OnRemoveSubscription := DynamicTimeItemRemoveSubscription;
  OnRestoreSubscription := DynamicTimeItemRestoreSubscription;
  FValue := FModel.AddFormulaObject;
  FValue.Parser := FModel.FormulaCompiler[TimeSeries.Orientation, eaBlocks];
  FValue.AddSubscriptionEvents(DynamicTimeItemRemoveSubscription,
  DynamicTimeItemRestoreSubscription, self);
end;

destructor TDynamicTimeSeriesFormulaItem.Destroy;
begin
  Value := '0';
  FModel.RemoveFormulaObject(FValue,
    DynamicTimeItemRemoveSubscription,
    DynamicTimeItemRestoreSubscription, self);
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
    Deleted := TimeSeriesSource.Deleted;
  end;
  inherited;
end;

constructor TDynamicTimeSeries.Create(
  ModelInterface: IModelForDynamicTimeSeries; ScreenObject: IScreenObject;
  Group: TDynamicTimeSeriesItem);
var
  NotifyEvent: TNotifyEvent;
begin
  if ModelInterface = nil then
  begin
    NotifyEvent := nil;
  end
  else
  begin
    NotifyEvent := ModelInterface.Invalidate;
  end;
  FScreenObject := ScreenObject;
  inherited Create(TDynamicTimeSeriesFormulaItem, NotifyEvent);
  FStoredScaleFactor := TRealStorage.Create(NotifyEvent);
  FNotifierComponent := TComponent.Create(nil);
  FModel := ModelInterface;
  FTimeSeriesLocationDictionary := TTimeSeriesLocationDictionary.Create;
  FGroup := Group;
end;

destructor TDynamicTimeSeries.Destroy;
begin
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
  VariablesUsed: TStringList;
  VariableIndex: Integer;
begin
  if not FTimeSeriesLocationDictionary.TryGetValue(Location, result) then
  begin
    result := FGroup.StaticGroup.AddI.TimeSeriesI;
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

function TDynamicTimeSeries.IsSame(
  DynamicTimeSeriesCollection: IDynamicTimeSeries): Boolean;
var
  index: Integer;
begin
  // if Assign is updated, update IsSame too.
  result := (Count = DynamicTimeSeriesCollection.Count)
    and (SeriesName = DynamicTimeSeriesCollection.SeriesName)
    and (ScaleFactor = DynamicTimeSeriesCollection.ScaleFactor)
    and (InterpolationMethod = DynamicTimeSeriesCollection.InterpolationMethod)
    and (ScaleFactorParameter = DynamicTimeSeriesCollection.ScaleFactorParameter)
    and (ParamMethod = DynamicTimeSeriesCollection.ParamMethod)
    and (Orientation = DynamicTimeSeriesCollection.Orientation);

  if result then
  begin
    for index := 0 to Count - 1 do
    begin
      result := Items[index].IsSame(DynamicTimeSeriesCollection[index]);
      if not result then
      begin
        Exit;
      end;
    end;
  end;

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
var
//  FStaticCollection: ITimesSeriesCollections;
  Item: ITimeSeriesCollectionItem;
  ChildCollection: ITimesSeriesCollection;
begin
  if FStaticGroup = nil then
  begin
    Assert(FModel <> nil);
    FStaticCollection :=  FModel.GetMf6TimesSeriesI;
    Item := FStaticCollection.AddI;
    result := Item.TimesSeriesCollectionI;
    result.GroupName := FStaticCollection.DefaultGroupName;
    result.Times := FDyanmicCollection.Times;
  end;
  result := FStaticGroup;
end;

function TDynamicTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDynamicTimeSeriesItem)
    and (TimeSeriesI.IsSame(TDynamicTimeSeriesItem(AnotherItem).TimeSeriesI));
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

function TDyanmicTimesSeriesCollection.Add: IDynamicTimeSeriesItem;
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
  Index: Integer): IDynamicTimeSeriesItem;
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
      TimeSeries := Items[ItemIndex].TimeSeriesI;
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
//
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
    if Items[SeriesIndex].TimeSeriesI.Deleted then
    begin
      (Items[SeriesIndex] as TDynamicTimeSeriesItem).Free;
    end;
  end;
  FTimeSeriesDictionary.Clear;
end;

procedure TDyanmicTimesSeriesCollection.SetItem(Index: Integer;
  const Value: IDynamicTimeSeriesItem);
begin
  inherited Items[Index] := Value as TDynamicTimeSeriesItem;
end;

procedure TDyanmicTimesSeriesCollection.SetTimeCount(const Value: Integer);
var
  ItemIndex: Integer;
  TimeIndex: Integer;
  AnItem: IDynamicTimeSeriesItem;
begin
  for ItemIndex := 0 to Count-1 do
  begin
    AnItem := Items[ItemIndex];
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

end.
