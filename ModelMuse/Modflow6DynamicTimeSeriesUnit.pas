unit Modflow6DynamicTimeSeriesUnit;

interface

uses
  System.Classes, GoPhastTypes, OrderedCollectionUnit, SubscriptionUnit,
  FormulaManagerUnit, System.IOUtils, System.AnsiStrings;

type
  TDynamicTimeSeriesItem = class abstract(TFormulaOrderedItem)
  private
    FObserver: TObserver;
    FValue: TFormulaObject;
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



  TDynamicTimeSeriesCollection = class(TPhastCollection)
  private
    FDeleted: Boolean;
    FSeriesName: AnsiString;
    FScaleFactorParameter: string;
    FParamMethod: TPestParamMethod;
    FInterpolationMethod: TMf6InterpolationMethods;
    FStoredScaleFactor: TRealStorage;
    FNotifierComponent: TComponent;
    function GetScaleFactor: double;
    function GetItems(Index: Integer): TDynamicTimeSeriesItem;
    procedure SetItems(Index: Integer; const Value: TDynamicTimeSeriesItem);
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetParamMethod(const Value: TPestParamMethod);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: AnsiString);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    procedure SetScaleFactor(const Value: double);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    function IsSame(DynamicTimeSeriesCollection: TDynamicTimeSeriesCollection): Boolean;
    property  Items[Index: Integer]: TDynamicTimeSeriesItem read GetItems
      write SetItems; default;
    function Add: TDynamicTimeSeriesItem;
    procedure Assign(Source: TPersistent); override;
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    property NotifierComponent: TComponent read FNotifierComponent;
  published
    property SeriesName: AnsiString read FSeriesName write SetSeriesName;
    property InterpolationMethod: TMf6InterpolationMethods
      read FInterpolationMethod write SetInterpolationMethod;
    property StoredScaleFactor: TRealStorage read FStoredScaleFactor
      write SetStoredScaleFactor;
    property ScaleFactorParameter: string read FScaleFactorParameter
      write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read FParamMethod
      write SetParamMethod;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

procedure DynamicTimeItemRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

procedure DynamicTimeItemRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses
  frmGoPhastUnit;

{ TDynamicTimeSeriesItem }

procedure TDynamicTimeSeriesItem.Assign(Source: TPersistent);
begin
  if Source is TDynamicTimeSeriesItem then
  begin
    Value := TDynamicTimeSeriesItem(Source).Value;
  end;
  inherited;
end;

constructor TDynamicTimeSeriesItem.Create(Collection: TCollection);
begin
  inherited;
  FObserver:= TObserver.Create(nil);

  OnRemoveSubscription := DynamicTimeItemRemoveSubscription;
  OnRestoreSubscription := DynamicTimeItemRestoreSubscription;
  FValue := frmGoPhast.PhastModel.FormulaManager.Add;
  FValue.Parser := frmGoPhast.PhastModel.rpTopFormulaCompiler;
  FValue.AddSubscriptionEvents(DynamicTimeItemRemoveSubscription,
  DynamicTimeItemRestoreSubscription, self);
end;

destructor TDynamicTimeSeriesItem.Destroy;
begin
  Value := '0';
  frmGoPhast.PhastModel.FormulaManager.Remove(FValue,
    DynamicTimeItemRemoveSubscription,
    DynamicTimeItemRestoreSubscription, self);
  FObserver.Free;
  inherited;
end;

function TDynamicTimeSeriesItem.GetObserver(Index: Integer): TObserver;
begin
  result := FObserver;
end;

function TDynamicTimeSeriesItem.GetScreenObject: TObject;
begin
  result := nil;
end;

function TDynamicTimeSeriesItem.GetValue: string;
begin
  Result := FValue.Formula;
  FObserver.UpToDate := True;
end;

function TDynamicTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  if AnotherItem is TDynamicTimeSeriesItem then
  begin
    result := Value = TDynamicTimeSeriesItem(AnotherItem).Value;
  end
  else
  begin
    result := false;
  end;
end;

procedure TDynamicTimeSeriesItem.RemoveSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.StopsTalkingTo(FObserver);
end;

procedure TDynamicTimeSeriesItem.RestoreSubscription(Sender: TObject;
  const AName: string);
var
  DS: TObserver;
begin
  DS := frmGoPhast.PhastModel.GetObserverByName(AName);
  DS.TalksTo(FObserver);
  FObserver.UpToDate := False;
end;

procedure TDynamicTimeSeriesItem.SetValue(const Value: string);
var
  Dummy: Integer;
begin
  Dummy := 0;
  UpdateFormulaBlocks(Value, Dummy, FValue);
end;

{ TDynamicTimeSeriesCollection }

function TDynamicTimeSeriesCollection.Add: TDynamicTimeSeriesItem;
begin
  Result := inherited Add as TDynamicTimeSeriesItem;
end;

procedure TDynamicTimeSeriesCollection.Assign(Source: TPersistent);
var
  TimeSeriesSource: TDynamicTimeSeriesCollection;
begin
  if Source is TDynamicTimeSeriesCollection then
  begin
    TimeSeriesSource := TDynamicTimeSeriesCollection(Source);
    SeriesName := TimeSeriesSource.SeriesName;
    InterpolationMethod := TimeSeriesSource.InterpolationMethod;
    ScaleFactor := TimeSeriesSource.ScaleFactor;
    ScaleFactorParameter := TimeSeriesSource.ScaleFactorParameter;
    ParamMethod := TimeSeriesSource.ParamMethod;
    Deleted := TimeSeriesSource.Deleted;
  end;
  inherited;
end;

constructor TDynamicTimeSeriesCollection.Create(
  InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TDynamicTimeSeriesItem, InvalidateModelEvent);
  FStoredScaleFactor := TRealStorage.Create(InvalidateModelEvent);
  FNotifierComponent := TComponent.Create(nil);
end;

destructor TDynamicTimeSeriesCollection.Destroy;
begin
  FNotifierComponent.Free;
  FStoredScaleFactor.Free;
  inherited;
end;

function TDynamicTimeSeriesCollection.GetItems(
  Index: Integer): TDynamicTimeSeriesItem;
begin
  result := inherited Items[Index] as TDynamicTimeSeriesItem
end;

function TDynamicTimeSeriesCollection.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

function TDynamicTimeSeriesCollection.IsSame(
  DynamicTimeSeriesCollection: TDynamicTimeSeriesCollection): Boolean;
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

procedure TDynamicTimeSeriesCollection.SetInterpolationMethod(
  const Value: TMf6InterpolationMethods);
begin
  if FInterpolationMethod <> Value then
  begin
    FInterpolationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeriesCollection.SetItems(Index: Integer;
  const Value: TDynamicTimeSeriesItem);
begin
  inherited Items[Index] := Value;
end;

procedure TDynamicTimeSeriesCollection.SetParamMethod(
  const Value: TPestParamMethod);
begin
  if FParamMethod <> Value then
  begin
    FParamMethod := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeriesCollection.SetScaleFactor(const Value: double);
begin
  StoredScaleFactor.Value := Value;
end;

procedure TDynamicTimeSeriesCollection.SetScaleFactorParameter(
  const Value: string);
begin
  if FScaleFactorParameter <> Value then
  begin
    FScaleFactorParameter := Value;
    InvalidateModel;
  end;
end;

procedure TDynamicTimeSeriesCollection.SetSeriesName(Value: AnsiString);
var
  CharIndex: Integer;
  AChar: Char;
begin
  Value := Trim(Copy(Trim(Value), 1, MaxTimeSeriesNameLength));
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

procedure TDynamicTimeSeriesCollection.SetStoredScaleFactor(
  const Value: TRealStorage);
begin
  FStoredScaleFactor.Assign(Value);
end;

procedure DynamicTimeItemRemoveSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDynamicTimeSeriesItem).RemoveSubscription(Sender, AName);
end;

procedure DynamicTimeItemRestoreSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  (Subject as TDynamicTimeSeriesItem).RestoreSubscription(Sender, AName);
end;

end.
