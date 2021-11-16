unit Modflow6TimeSeriesCollectionsUnit;

interface

uses
  System.SysUtils, System.Classes, GoPhastTypes, OrderedCollectionUnit,
    Modflow6TimeSeriesUnit;

type
  TTimeSeriesItem = class(TOrderedItem)
  private
    FTimeSeries: TTimeSeries;
    procedure SetTimeSeries(const Value: TTimeSeries);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeSeries: TTimeSeries read FTimeSeries write SetTimeSeries;
  end;

  TTimesSeriesCollection = class(TOrderedCollection)
  private
    FTimes: TRealCollection;
    FGroupName: string;
    function GetItem(Index: Integer): TTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
    procedure SetTimes(const Value: TRealCollection);
    procedure SetGroupName(const Value: string);
  public
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): TTimeSeriesItem;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: TTimeSeriesItem read GetItem write SetItem; default;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
  published
    property Times: TRealCollection read FTimes write SetTimes;
    property GroupName: string read FGroupName write SetGroupName;
  end;

  TimeSeriesCollectionItem = class(TOrderedItem)
  private
    FTimesSeriesCollection: TTimesSeriesCollection;
    procedure SetTimesSeriesCollection(const Value: TTimesSeriesCollection);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimesSeriesCollection: TTimesSeriesCollection
      read FTimesSeriesCollection write SetTimesSeriesCollection;
  end;

  TTimesSeriesCollections = class(TOrderedCollection)
  private
    function GetItem(Index: Integer): TimeSeriesCollectionItem;
    procedure SetItem(Index: Integer; const Value: TimeSeriesCollectionItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TimeSeriesCollectionItem read GetItem write SetItem; default;
    function Add: TimeSeriesCollectionItem;
  end;

implementation

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
  FTimeSeries := TTimeSeries.Create(OnInvalidateModelEvent);
end;

destructor TTimeSeriesItem.Destroy;
begin
  FTimeSeries.Free;
  inherited;
end;

function TTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TTimeSeriesItem)
//    and (inherited IsSame(AnotherItem))
    and (TimeSeries.IsSame(TTimeSeriesItem(AnotherItem).TimeSeries));
end;

procedure TTimeSeriesItem.SetTimeSeries(const Value: TTimeSeries);
begin
  FTimeSeries.Assign(Value);
end;

{ TTimesSeriesCollection }

procedure TTimesSeriesCollection.Assign(Source: TPersistent);
var
  TSGroup: TTimesSeriesCollection;
begin
  if Source is TTimesSeriesCollection then
  begin
    TSGroup := TTimesSeriesCollection(Source);
    Times := TSGroup.Times;
    GroupName := TSGroup.GroupName;
  end;
  inherited;
end;

constructor TTimesSeriesCollection.Create(Model: TBaseModel);
begin
  inherited Create(TTimeSeriesItem, Model);
  if Model = nil then
  begin
    FTimes := TRealCollection.Create(nil);
  end
  else
  begin
    FTimes := TRealCollection.Create(Model.Invalidate);
  end;
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

function TTimesSeriesCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  OtherCollection: TTimesSeriesCollection;
begin
  result := (AnOrderedCollection is TTimesSeriesCollection)
    and inherited IsSame(AnOrderedCollection);
  if result then
  begin
    OtherCollection := TTimesSeriesCollection(TTimesSeriesCollection);
    result := (GroupName = OtherCollection.GroupName)
      and (Times.IsSame(OtherCollection.Times));
  end;
end;

procedure TTimesSeriesCollection.SetGroupName(const Value: string);
begin
  FGroupName := Value;
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
  FTimesSeriesCollection := TTimesSeriesCollection.Create(Model)
end;

destructor TimeSeriesCollectionItem.Destroy;
begin
  FTimesSeriesCollection.Free;
  inherited;
end;

function TimeSeriesCollectionItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TimeSeriesCollectionItem)
//    and (inherited IsSame(AnotherItem))
    and (TimesSeriesCollection.IsSame(
    TimeSeriesCollectionItem(AnotherItem).TimesSeriesCollection));
end;

procedure TimeSeriesCollectionItem.SetTimesSeriesCollection(
  const Value: TTimesSeriesCollection);
begin
  FTimesSeriesCollection.Assign(Value);
end;

{ TTimesSeriesCollections }

function TTimesSeriesCollections.Add: TimeSeriesCollectionItem;
begin
  result := inherited Add as TimeSeriesCollectionItem;
end;

constructor TTimesSeriesCollections.Create(Model: TBaseModel);
begin
  inherited Create(TimeSeriesCollectionItem, Model);
end;

function TTimesSeriesCollections.GetItem(
  Index: Integer): TimeSeriesCollectionItem;
begin
  result := inherited Items[Index] as TimeSeriesCollectionItem;
end;

procedure TTimesSeriesCollections.SetItem(Index: Integer;
  const Value: TimeSeriesCollectionItem);
begin
  inherited Items[Index] := Value;
end;


end.
