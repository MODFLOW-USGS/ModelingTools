unit PestObsUnit;

interface

uses
  System.SysUtils, System.Classes, GoPhastTypes, System.Generics.Collections;

type
  TCustomObservationItem = class(TPhastCollectionItem)
  private
    FName: string;
    FComment: string;
//    FObservedValue: double;
//    FWeight: Double;
    FGUID: string;
    FExportedName: string;
    FStoredWeight: TRealStorage;
    FStoredObservedValue: TRealStorage;
    procedure SetComment(const Value: string);
    procedure SetName(const Value: string);
    procedure SetObservedValue(const Value: double);
    procedure SetWeight(const Value: Double);
    function GetScreenObject: TObject;
    procedure SetStoredObservedValue(const Value: TRealStorage);
    procedure SetStoredWeight(const Value: TRealStorage);
    function GetObservedValue: double;
    function GetWeight: Double;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function ObservationType: string; virtual;
    function Units: string; virtual;
    property ScreenObject: TObject read GetScreenObject;
    // @name is a globally unique identifier used to store and extract
    // @classname in a dictionary. This helps with calculating derived
    // observations.
    property GUID: string read FGUID write FGUID;
    property ExportedName: string read FExportedName write FExportedName;
  published
    property Name: string read FName write SetName;
    property ObservedValue: double read GetObservedValue write SetObservedValue Stored False;
    property Weight: Double read GetWeight write SetWeight Stored False;
    property StoredObservedValue: TRealStorage read FStoredObservedValue
      write SetStoredObservedValue;
    property StoredWeight: TRealStorage read FStoredWeight
      write SetStoredWeight;
    property Comment: string read FComment write SetComment;
  end;

  TObservationList = TList<TCustomObservationItem>;

  TCustomTimeObservationItem = class(TCustomObservationItem)
  private
    FTime: double;
    procedure SetTime(const Value: double);
  protected
    function GetObsTypeIndex: Integer; virtual; abstract;
    procedure SetObsTypeIndex(Value: Integer); virtual; abstract;
    function GetObsTypeString: string; virtual; abstract;
    procedure SetObsTypeString(const Value: string); virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    property ObsTypeIndex: Integer read GetObsTypeIndex write SetObsTypeIndex;
    property ObsTypeString: string read GetObsTypeString write SetObsTypeString;
  published
    property Time: double read FTime write SetTime;
  end;

  // Compare two @link(TCustomObservationItem)s in the same object.
  TObsCompareItem = class(TCustomObservationItem)
  private
    FIndex2: Integer;
    FIndex1: Integer;
    procedure SetIndex1(const Value: Integer);
    procedure SetIndex2(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Index1: Integer read FIndex1 write SetIndex1;
    property Index2: Integer read FIndex2 write SetIndex2;
  end;

  TObsComparisons = class(TScreenObjectOwnerCollection)
  private
    function GetItem(Index: Integer): TObsCompareItem;
    procedure SetItem(Index: Integer; const Value: TObsCompareItem);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TObsCompareItem read GetItem
      write SetItem; default;
    function Add: TObsCompareItem;
   end;

  TCustomComparisonCollection = class(TScreenObjectOwnerCollection)
  private
    FComparisons: TObsComparisons;
    procedure SetComparisons(const Value: TObsComparisons);
    function GetItem(Index: Integer): TCustomTimeObservationItem;
    procedure SetItem(Index: Integer; const Value: TCustomTimeObservationItem);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(ItemClass: TCollectionItemClass;
      InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    Destructor Destroy; override;
    property Items[Index: Integer]: TCustomTimeObservationItem  read GetItem
      write SetItem; default;
    function Add: TCustomTimeObservationItem;
    procedure Clear;
  published
    property Comparisons: TObsComparisons read FComparisons write SetComparisons;
  end;

  TObsItemDictionary = TDictionary<string, TCustomObservationItem>;

function PrefixedObsName(Prefix: string; ObjectIndex: Integer; Obs: TCustomObservationItem): string;

implementation

uses
  ModelMuseUtilities;

function PrefixedObsName(Prefix: string; ObjectIndex: Integer; Obs: TCustomObservationItem): string;
var
  MaxPrefixLength: Integer;
begin
// The maximum allowed length of an observation name in PEST is 20.
  MaxPrefixLength := 19 - Length((ObjectIndex+1).ToString + Obs.Name);
  Prefix := Copy(Prefix, 1, MaxPrefixLength);
  Result := Format('%0:s_%1:d%2:s', [Prefix, ObjectIndex+1, Obs.Name]);
  Result := PestObsName(Result);
  Obs.ExportedName := Result;
end;

{ TCustomObservationItem }

procedure TCustomObservationItem.Assign(Source: TPersistent);
var
  ObsSource: TCustomObservationItem;
begin
  if Source is TCustomObservationItem then
  begin
    ObsSource := TCustomObservationItem(Source);
    Name := ObsSource.Name;
    ObservedValue := ObsSource.ObservedValue;
    Weight := ObsSource.Weight;
    Comment := ObsSource.Comment;
    GUID := ObsSource.GUID;
  end
  else
  begin
    inherited;
  end;
end;

constructor TCustomObservationItem.Create(Collection: TCollection);
var
  MyGuid : TGUID;
begin
  if Collection <> nil then
  begin
    Assert(Collection is TScreenObjectOwnerCollection);
  end;
  inherited;
  if CreateGUID(MyGuid) = 0 then
  begin
    FGUID := GUIDToString(MyGuid);
  end;
  FStoredObservedValue := TRealStorage.Create;
  FStoredObservedValue.OnChange := OnInvalidateModel;
  FStoredWeight := TRealStorage.Create;
  FStoredWeight.OnChange := OnInvalidateModel;
end;

destructor TCustomObservationItem.Destroy;
begin
  FStoredWeight.Free;
  FStoredObservedValue.Free;
  inherited;
end;

function TCustomObservationItem.GetObservedValue: double;
begin
  result := StoredObservedValue.Value;
end;

function TCustomObservationItem.GetScreenObject: TObject;
begin
  result := nil;
  if Collection <> nil then
  begin
    Result := (Collection as TScreenObjectOwnerCollection).ScreenObject;
  end;
end;

function TCustomObservationItem.GetWeight: Double;
begin
  result := StoredWeight.Value;
end;

function TCustomObservationItem.ObservationType: string;
begin
  result := ClassName;
end;

procedure TCustomObservationItem.SetComment(const Value: string);
begin
  SetStringProperty(FComment, Value);
end;

procedure TCustomObservationItem.SetName(const Value: string);
begin
  SetStringProperty(FName, Value);
end;

procedure TCustomObservationItem.SetObservedValue(const Value: double);
begin
  StoredObservedValue.Value := Value;
end;

procedure TCustomObservationItem.SetStoredObservedValue(
  const Value: TRealStorage);
begin
  FStoredObservedValue.Assign(Value);
end;

procedure TCustomObservationItem.SetStoredWeight(const Value: TRealStorage);
begin
  FStoredWeight.Assign(Value);
end;

procedure TCustomObservationItem.SetWeight(const Value: Double);
begin
  StoredWeight.Value := Value;
end;

function TCustomObservationItem.Units: string;
begin
  result := 'unknown';
end;

{ TCustomTimeObservationItem }

procedure TCustomTimeObservationItem.Assign(Source: TPersistent);
var
  ObsSource: TCustomTimeObservationItem;
begin
  if Source is TCustomTimeObservationItem then
  begin
    ObsSource := TCustomTimeObservationItem(Source);
    Time := ObsSource.Time;
  end;
  inherited;

end;

procedure TCustomTimeObservationItem.SetTime(const Value: double);
begin
  SetRealProperty(FTime, Value);
end;

{ TObsCompareItem }

procedure TObsCompareItem.Assign(Source: TPersistent);
var
  CompareItem: TObsCompareItem;
begin
  if Source is TObsCompareItem then
  begin
    CompareItem := TObsCompareItem(Source);
    Index1 := CompareItem.Index1;
    Index2 := CompareItem.Index2;
  end;
  inherited;
end;

procedure TObsCompareItem.SetIndex1(const Value: Integer);
begin
  SetIntegerProperty(FIndex1, Value);
end;

procedure TObsCompareItem.SetIndex2(const Value: Integer);
begin
  SetIntegerProperty(FIndex2, Value);
end;

{ TObsComparisons }

function TObsComparisons.Add: TObsCompareItem;
begin
  result := inherited Add as TObsCompareItem;
end;

procedure TObsComparisons.Assign(Source: TPersistent);
var
  ObsSource: TObsComparisons;
  ItemIndex: Integer;
begin
  if Source is TObsComparisons then
  begin
    ObsSource := TObsComparisons(Source);
    Count :=  ObsSource.Count;
    for ItemIndex := 0 to ObsSource.Count - 1 do
    begin
      Items[ItemIndex].Assign(ObsSource.Items[ItemIndex]);
    end;
  end
  else
  begin
    inherited;
  end;
end;

constructor TObsComparisons.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TObsCompareItem, InvalidateModelEvent, ScreenObject);
end;

function TObsComparisons.GetItem(Index: Integer): TObsCompareItem;
begin
  result := inherited Items[Index] as TObsCompareItem
end;

procedure TObsComparisons.SetItem(Index: Integer; const Value: TObsCompareItem);
begin
  inherited Items[Index] := Value;
end;

{ TCustomComparisonCollection }

function TCustomComparisonCollection.Add: TCustomTimeObservationItem;
begin
  result := inherited Add as TCustomTimeObservationItem;
end;

procedure TCustomComparisonCollection.Assign(Source: TPersistent);
var
  CompSource: TCustomComparisonCollection;
  ItemIndex: Integer;
begin
  if Source is TCustomComparisonCollection then
  begin
    CompSource := TCustomComparisonCollection(Source);
    Comparisons := CompSource.Comparisons;
    Count :=  CompSource.Count;
    for ItemIndex := 0 to CompSource.Count - 1 do
    begin
      Items[ItemIndex].Assign(CompSource.Items[ItemIndex]);
    end;
  end;
    inherited;
end;

procedure TCustomComparisonCollection.Clear;
begin
  inherited;
  Comparisons.Clear;
end;

constructor TCustomComparisonCollection.Create(ItemClass: TCollectionItemClass;
  InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
begin
  inherited;
  FComparisons := TObsComparisons.Create(InvalidateModelEvent, ScreenObject);
end;

destructor TCustomComparisonCollection.Destroy;
begin
  FComparisons.Free;
  inherited;
end;

function TCustomComparisonCollection.GetItem(
  Index: Integer): TCustomTimeObservationItem;
begin
  result := inherited Items[Index] as TCustomTimeObservationItem;
end;

procedure TCustomComparisonCollection.SetComparisons(
  const Value: TObsComparisons);
begin
  FComparisons.Assign(Value);
end;

procedure TCustomComparisonCollection.SetItem(Index: Integer;
  const Value: TCustomTimeObservationItem);
begin
  inherited Items[Index] := Value;
end;

end.
