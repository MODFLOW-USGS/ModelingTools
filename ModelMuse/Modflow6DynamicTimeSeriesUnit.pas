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
  ScreenObjectInterfaceUnit;

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
  public
    constructor Create(ModelInterface: IModelForDynamicTimeSeries;
      ScreenObject: IScreenObject);
    destructor Destroy; override;
    function IsSame(DynamicTimeSeriesCollection: IDynamicTimeSeries): Boolean;
    property  Items[Index: Integer]: IDynamicTimeSeriesFormulaItem read GetItems
      write SetItems; default;
    function Add: IDynamicTimeSeriesFormulaItem;
    procedure Assign(Source: TPersistent); override;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    property NotifierComponent: TComponent read FNotifierComponent;
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
    property Orientation: TDataSetOrientation read GetOrientation write SetOrientation;
  end;

  TDynamicTimeSeriesItem = class(TOrderedItem, IDynamicTimeSeriesItem)
  private
    FDynamicTimeSeries: TDynamicTimeSeries;
    FModel: IModelMuseModel;
    FScreenObject: IScreenObject;
    procedure SetTimeSeries(const Value: IDynamicTimeSeries);
    function GetDynamicTimeSeries: IDynamicTimeSeries;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeSeries: IDynamicTimeSeries read GetDynamicTimeSeries
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
  ModelInterface: IModelForDynamicTimeSeries; ScreenObject: IScreenObject);
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
    TimeSeries := TDynamicTimeSeriesItem(Source).TimeSeries;
  end
  else
  begin
    inherited;
  end;
end;

constructor TDynamicTimeSeriesItem.Create(Collection: TCollection);
var
  DyanmicCollection: TDyanmicTimesSeriesCollection;
begin
  Assert(Collection <> nil);
  inherited;
  DyanmicCollection := (Collection as TDyanmicTimesSeriesCollection);
  FScreenObject := DyanmicCollection.FScreenObject;
  FModel := DyanmicCollection.FModel;
  FDynamicTimeSeries := TDynamicTimeSeries.Create(
    FModel as IModelForDynamicTimeSeries, FScreenObject);
end;

destructor TDynamicTimeSeriesItem.Destroy;
begin
  FDynamicTimeSeries.Free;
  inherited;
end;

function TDynamicTimeSeriesItem.GetDynamicTimeSeries: IDynamicTimeSeries;
begin
  result := FDynamicTimeSeries;
end;

function TDynamicTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDynamicTimeSeriesItem)
    and (TimeSeries.IsSame(TDynamicTimeSeriesItem(AnotherItem).TimeSeries));
end;

procedure TDynamicTimeSeriesItem.SetTimeSeries(
  const Value: IDynamicTimeSeries);
begin
  FDynamicTimeSeries.Assign(Value as TDynamicTimeSeries)
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
  if (Count > 0) and (FTimeSeriesDictionary.Count = 0) then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      TimeSeries := Items[ItemIndex].TimeSeries;
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
    if Items[SeriesIndex].TimeSeries.Deleted then
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
    AnItem.TimeSeries.Count := Value;
    for TimeIndex := Times.Count to Value -1 do
    begin
      AnItem.TimeSeries[TimeIndex].Value := '3.0E30';
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
