unit Modflow6DynamicTimeSeriesUnit;

interface

uses
  System.Classes, GoPhastTypes, OrderedCollectionUnit, SubscriptionUnit,
  FormulaManagerUnit, System.IOUtils, //System.AnsiStrings,
  Modflow6DynamicTimeSeriesInterfaceUnit, Modflow6TimeSeriesInterfaceUnit,
  Modflow6TimeSeriesCollectionsUnit, System.SysUtils,
  OrderedCollectionInterfaceUnit;

type
  TDynamicTimeSeriesFormulaItem = class abstract(TFormulaOrderedItem)
  private
    FObserver: TObserver;
    FValue: TFormulaObject;
    FModel: IModelForDynamicTimeSeries;
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
    property ValueObject: TFormulaObject read FValue write FValue;
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
  end;

  TDynamicTimeSeries = class(TPhastCollection, ITimeSeriesInterface)
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
    function GetScaleFactor: double;
    function GetItems(Index: Integer): TDynamicTimeSeriesFormulaItem;
    procedure SetItems(Index: Integer; const Value: TDynamicTimeSeriesFormulaItem);
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
//    function _AddRef: Integer; stdcall;
//    function _Release: Integer; stdcall;
//    function QueryInterface(const IID: TGUID; out Obj): HRESULT;
//      virtual; stdcall;
  public
    constructor Create(ModelInterface: IModelForDynamicTimeSeries);
    destructor Destroy; override;
    function IsSame(DynamicTimeSeriesCollection: TDynamicTimeSeries): Boolean;
    property  Items[Index: Integer]: TDynamicTimeSeriesFormulaItem read GetItems
      write SetItems; default;
    function Add: TDynamicTimeSeriesFormulaItem;
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
    property Deleted: Boolean read FDeleted write FDeleted;
    property Orientation: TDataSetOrientation read GetOrientation write SetOrientation;
  end;

  TDynamicTimeSeriesItem = class(TOrderedItem)
  private
    FDynamicTimeSeries: TDynamicTimeSeries;
    FModel: IModelMuseModel;
    procedure SetTimeSeries(const Value: TDynamicTimeSeries);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeSeries: TDynamicTimeSeries read FDynamicTimeSeries
      write SetTimeSeries;
  end;

  TDyanmicTimesSeriesCollection = class(TCustomTimesSeriesCollection)
  private
    FTimeSeriesDictionary: TCacheDictionary<string, TDynamicTimeSeries>;
    FModel: IModelMuseModel;
    function GetItem(Index: Integer): TDynamicTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TDynamicTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
  public
    Constructor Create(Model: IModelMuseModel);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): TDynamicTimeSeries;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: TDynamicTimeSeriesItem read GetItem write SetItem; default;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    function Add: TDynamicTimeSeriesItem;
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
  result := nil;
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

function TDynamicTimeSeries.Add: TDynamicTimeSeriesFormulaItem;
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
  ModelInterface: IModelForDynamicTimeSeries);
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
  inherited Create(TDynamicTimeSeriesFormulaItem, NotifyEvent);
  FStoredScaleFactor := TRealStorage.Create(NotifyEvent);
  FNotifierComponent := TComponent.Create(nil);
  FModel := ModelInterface;
end;

destructor TDynamicTimeSeries.Destroy;
begin
  FNotifierComponent.Free;
  FStoredScaleFactor.Free;
  inherited;
end;

function TDynamicTimeSeries.GetInterpolationMethod: TMf6InterpolationMethods;
begin
  result := FInterpolationMethod;
end;

function TDynamicTimeSeries.GetItems(
  Index: Integer): TDynamicTimeSeriesFormulaItem;
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
  DynamicTimeSeriesCollection: TDynamicTimeSeries): Boolean;
var
  index: Integer;
begin
  // if Assign is updated, update IsSame too.
  result := (Count = DynamicTimeSeriesCollection.Count);
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

//function TDynamicTimeSeries.QueryInterface(const IID: TGUID;
//  out Obj): HRESULT;
//const
//  E_NOINTERFACE = HRESULT($80004002);
//begin
//  if GetInterface(IID, Obj) then
//    result := 0
//  else
//    result := E_NOINTERFACE;
//end;

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
  const Value: TDynamicTimeSeriesFormulaItem);
begin
  inherited Items[Index] := Value;
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

//function TDynamicTimeSeries._AddRef: Integer;
//begin
//  result := 1;
//end;
//
//function TDynamicTimeSeries._Release: Integer;
//begin
//  result := 1;
//end;

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
  FModel := DyanmicCollection.FModel;
  FDynamicTimeSeries := TDynamicTimeSeries.Create(FModel as IModelForDynamicTimeSeries);
end;

destructor TDynamicTimeSeriesItem.Destroy;
begin
  FDynamicTimeSeries.Free;
  inherited;
end;

function TDynamicTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDynamicTimeSeriesItem)
    and (TimeSeries.IsSame(TDynamicTimeSeriesItem(AnotherItem).TimeSeries));
end;

procedure TDynamicTimeSeriesItem.SetTimeSeries(
  const Value: TDynamicTimeSeries);
begin

end;

{ TDyanmicTimesSeriesCollection }

function TDyanmicTimesSeriesCollection.Add: TDynamicTimeSeriesItem;
begin
  result := inherited Add as TDynamicTimeSeriesItem;
end;

procedure TDyanmicTimesSeriesCollection.Assign(Source: TPersistent);
begin
  FTimeSeriesDictionary.Clear;
  inherited;
end;

constructor TDyanmicTimesSeriesCollection.Create(Model: IModelMuseModel);
var
  LocalInterface : IModelForTOrderedCollection;
begin
  LocalInterface := Model as IModelForTOrderedCollection;
  inherited Create(TDynamicTimeSeriesItem, LocalInterface);
  FTimeSeriesDictionary := TCacheDictionary<string, TDynamicTimeSeries>.Create;
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

function TDyanmicTimesSeriesCollection.GetTimeCount: Integer;
begin
  result := Times.Count;
end;

function TDyanmicTimesSeriesCollection.GetValuesByName(
  const AName: string): TDynamicTimeSeries;
var
  ItemIndex: Integer;
  TimeSeries: TDynamicTimeSeries;
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
//var
//  OtherCollection: TDyanmicTimesSeriesCollection;
begin
  result := (AnOrderedCollection is TDyanmicTimesSeriesCollection)
    and inherited IsSame(AnOrderedCollection);
//  if result then
//  begin
//    OtherCollection := TDyanmicTimesSeriesCollection(AnOrderedCollection);
//    result := (GroupName = OtherCollection.GroupName)
//      and (Times.IsSame(OtherCollection.Times));
//  end;
end;

procedure TDyanmicTimesSeriesCollection.Loaded;
var
  SeriesIndex: Integer;
begin
  for SeriesIndex := Count - 1 downto 0 do
  begin
    if Items[SeriesIndex].TimeSeries.Deleted then
    begin
      Items[SeriesIndex].Free;
    end;
  end;
  FTimeSeriesDictionary.Clear;
end;

procedure TDyanmicTimesSeriesCollection.SetItem(Index: Integer;
  const Value: TDynamicTimeSeriesItem);
begin
  inherited Items[Index] := Value;
end;

procedure TDyanmicTimesSeriesCollection.SetTimeCount(const Value: Integer);
var
  ItemIndex: Integer;
  TimeIndex: Integer;
  AnItem: TDynamicTimeSeriesItem;
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
