unit Modflow6TimeSeriesUnit;

interface

uses
  System.SysUtils, System.Classes, GoPhastTypes;

type
  TTimeSeries = class(TRealCollection)
  private
    FSeriesName: string;
    FScaleFactorParameter: string;
    FInterpolationMethod: TMf6InterpolationMethods;
    FStoredScaleFactor: TRealStorage;
    FParamMethod: TPestParamMethod;
    function GetScaleFactor: double;
    procedure SetInterpolationMethod(const Value: TMf6InterpolationMethods);
    procedure SetScaleFactor(const Value: double);
    procedure SetScaleFactorParameter(const Value: string);
    procedure SetSeriesName(Value: string);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    procedure SetParamMethod(const Value: TPestParamMethod);
  public
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    constructor Create(InvalidateModelEvent: TNotifyEvent); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property SeriesName: string read FSeriesName write SetSeriesName;
    property InterpolationMethod: TMf6InterpolationMethods read FInterpolationMethod write SetInterpolationMethod;
    property StoredScaleFactor: TRealStorage read FStoredScaleFactor write SetStoredScaleFactor;
    property ScaleFactorParameter: string read FScaleFactorParameter write SetScaleFactorParameter;
    property ParamMethod: TPestParamMethod read FParamMethod write SetParamMethod;
  end;

  TTimeSeriesItem = class(TPhastCollectionItem)
  private
    FTimeSeries: TTimeSeries;
    procedure SetTimeSeries(const Value: TTimeSeries);
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeSeries: TTimeSeries read FTimeSeries write SetTimeSeries;
  end;

  TTimesSeriesCollection = class(TPhastCollection)
  private
    FTimes: TRealCollection;
    function GetItem(Index: Integer): TTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
    procedure SetTimes(const Value: TRealCollection);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): TTimeSeriesItem;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: TTimeSeriesItem read GetItem write SetItem;
  published
    property Times: TRealCollection read FTimes write SetTimes;
  end;

  TimeSeriesCollectionItem = class(TPhastCollectionItem)
  private
    FTimesSeriesCollection: TTimesSeriesCollection;
    procedure SetTimesSeriesCollection(const Value: TTimesSeriesCollection);
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimesSeriesCollection: TTimesSeriesCollection
      read FTimesSeriesCollection write SetTimesSeriesCollection;
  end;

implementation

{ TTimeSeries }

procedure TTimeSeries.Assign(Source: TPersistent);
var
  TimeSeriesSource: TTimeSeries;
begin
  if Source is TTimeSeries then
  begin
    TimeSeriesSource := TTimeSeries(Source);
    SeriesName := TimeSeriesSource.SeriesName;
    InterpolationMethod := TimeSeriesSource.InterpolationMethod;
    ScaleFactor := TimeSeriesSource.ScaleFactor;
    ScaleFactorParameter := TimeSeriesSource.ScaleFactorParameter;
    ParamMethod := TimeSeriesSource.ParamMethod;
  end;
  inherited;
end;

constructor TTimeSeries.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredScaleFactor := TRealStorage.Create(InvalidateModelEvent);
end;

destructor TTimeSeries.Destroy;
begin
  FStoredScaleFactor.Free;
  inherited;
end;

function TTimeSeries.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

procedure TTimeSeries.SetInterpolationMethod(
  const Value: TMf6InterpolationMethods);
begin
  if FInterpolationMethod <> Value then
  begin
    FInterpolationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetParamMethod(const Value: TPestParamMethod);
begin
  if FParamMethod <> Value then
  begin
    FParamMethod := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetScaleFactor(const Value: double);
begin
  StoredScaleFactor.Value := Value;
end;

procedure TTimeSeries.SetScaleFactorParameter(const Value: string);
begin
  if FScaleFactorParameter <> Value then
  begin
    FScaleFactorParameter := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetSeriesName(Value: string);
begin
  Value := Copy(Value, 1, MaxTimeSeriesNameLength);
  if FSeriesName <> Value then
  begin
    FSeriesName := Value;
    InvalidateModel;
  end;
end;

procedure TTimeSeries.SetStoredScaleFactor(const Value: TRealStorage);
begin
  FStoredScaleFactor.Assign(Value);
end;

{ TTimeSeriesItem }

procedure TTimeSeriesItem.Assign(Source: TPersistent);
begin
  if Source is TTimeSeriesItem then
  begin
    TimeSeries := TTimeSeriesItem(Source).TimeSeries;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeriesItem.Create(Collection: TCollection);
begin
  inherited;
  FTimeSeries := TTimeSeries.Create(OnInvalidateModel);
end;

destructor TTimeSeriesItem.Destroy;
begin
  FTimeSeries.Free;
  inherited;
end;

procedure TTimeSeriesItem.SetTimeSeries(const Value: TTimeSeries);
begin
  FTimeSeries.Assign(Value);
end;

{ TTimesSeriesCollection }

procedure TTimesSeriesCollection.Assign(Source: TPersistent);
begin
  if Source is TTimesSeriesCollection then
  begin
    Times := TTimesSeriesCollection(Source).Times;
  end;
  inherited;
end;

constructor TTimesSeriesCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TTimeSeriesItem, InvalidateModelEvent);
  FTimes := TRealCollection.Create(InvalidateModelEvent);
end;

destructor TTimesSeriesCollection.Destroy;
begin
  FTimes.Free;
  inherited;
end;

function TTimesSeriesCollection.GetItem(Index: Integer): TTimeSeriesItem;
begin
  result := inherited Items[Index] as TTimeSeriesItem;
end;

function TTimesSeriesCollection.GetTimeCount: Integer;
begin
  result := FTimes.Count;
end;

function TTimesSeriesCollection.GetValuesByName(
  const AName: string): TTimeSeriesItem;
var
  ItemIndex: Integer;
  AnItem: TTimeSeriesItem;
begin
  result := nil;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex];
    if SameText(AnItem.TimeSeries.SeriesName, AName) then
    begin
      result := AnItem;
      break;
    end;
  end;
end;

procedure TTimesSeriesCollection.SetItem(Index: Integer;
  const Value: TTimeSeriesItem);
begin
  inherited Items[Index] := Value
end;

procedure TTimesSeriesCollection.SetTimeCount(const Value: Integer);
var
  ItemIndex: Integer;
  AnItem: TTimeSeriesItem;
  TimeIndex: Integer;
begin
  for ItemIndex := 0 to Count-1 do
  begin
    AnItem := Items[ItemIndex];
    AnItem.TimeSeries.Count := Value;
    for TimeIndex := FTimes.Count to Value -1 do 
    begin
      AnItem.TimeSeries[TimeIndex].Value := 3.0E30;
    end;
  end;
  FTimes.Count := Value;
end;

procedure TTimesSeriesCollection.SetTimes(const Value: TRealCollection);
begin
  FTimes.Assign(Value);
end;

{ TimeSeriesCollectionItem }

procedure TimeSeriesCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TimeSeriesCollectionItem then
  begin
    TimesSeriesCollection := TimeSeriesCollectionItem(Source).TimesSeriesCollection
  end
  else
  begin
    inherited;
  end;
end;

constructor TimeSeriesCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FTimesSeriesCollection := TTimesSeriesCollection.Create(OnInvalidateModel)
end;

destructor TimeSeriesCollectionItem.Destroy;
begin
  FTimesSeriesCollection.Free;
  inherited;
end;

procedure TimeSeriesCollectionItem.SetTimesSeriesCollection(
  const Value: TTimesSeriesCollection);
begin
  FTimesSeriesCollection.Assign(Value);
end;

end.
