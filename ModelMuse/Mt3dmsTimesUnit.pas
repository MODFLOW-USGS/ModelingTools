unit Mt3dmsTimesUnit;

interface

uses
  Classes, GoPhastTypes, Generics.Collections, Generics.Defaults;

type
  TMt3dmsTimeItem = class(TPhastCollectionItem)
  private
    FStoredTimeStepMultiplier: TRealStorage;
    FStoredStartTime: TRealStorage;
    FMaxSteps: integer;
    FStoredEndTime: TRealStorage;
    FStoredStepSize: TRealStorage;
    FStoredMaxStepSize: TRealStorage;
    FSteadyState: boolean;
    procedure SetEndTime(const Value: double);
    procedure SetMaxSteps(const Value: integer);
    procedure SetMaxStepSize(const Value: double);
    procedure SetStartTime(const Value: double);
    procedure SetStepSize(const Value: double);
    procedure SetStoredEndTime(const Value: TRealStorage);
    procedure SetStoredMaxStepSize(const Value: TRealStorage);
    procedure SetStoredStartTime(const Value: TRealStorage);
    procedure SetStoredStepSize(const Value: TRealStorage);
    procedure SetStoredTimeStepMultiplier(const Value: TRealStorage);
    procedure SetTimeStepMultiplier(const Value: double);
    procedure PropertyChanged(Sender: TObject);
    function GetEndTime: double;
    function GetMaxStepSize: double;
    function GetStartTime: double;
    function GetStepSize: double;
    function GetTimeStepMuitiplier: double;
    procedure SetSteadyState(const Value: boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property StartTime: double read GetStartTime write SetStartTime;
    property EndTime: double read GetEndTime write SetEndTime;
    // DT0
    // The meaning of @name depends on whether or not the solution is
    // explicit or implicit. It is implicit if the GCG solver is chosen.
    // Otherwise it is explicit.
    property StepSize: double read GetStepSize write SetStepSize;
    // TTSMULT
    property TimeStepMultiplier: double read GetTimeStepMuitiplier
      write SetTimeStepMultiplier;
    // TTSMAX
    property MaxStepSize: double read GetMaxStepSize write SetMaxStepSize;
  published
    // MXSTRN
    property SteadyState: boolean read FSteadyState write SetSteadyState;
    property MaxSteps: integer read FMaxSteps write SetMaxSteps;
    property StoredStartTime: TRealStorage read FStoredStartTime write SetStoredStartTime;
    property StoredEndTime: TRealStorage read FStoredEndTime write SetStoredEndTime;
    // DT0
    property StoredStepSize: TRealStorage read FStoredStepSize write SetStoredStepSize;
    // TTSMULT
    property StoredTimeStepMultiplier: TRealStorage read FStoredTimeStepMultiplier write SetStoredTimeStepMultiplier;
    // TTSMAX
    property StoredMaxStepSize: TRealStorage read FStoredMaxStepSize write SetStoredMaxStepSize;
  end;

  TMt3dmsTimeCollection = class(TPhastCollection)
  private
    function GetItem(Index: integer): TMt3dmsTimeItem;
    procedure SetItem(Index: integer; const Value: TMt3dmsTimeItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    property Items[Index: integer]: TMt3dmsTimeItem read
      GetItem write SetItem; default;
    function GetItemFromTime(ATime: double): TMt3dmsTimeItem;
    function Add: TMt3dmsTimeItem;
  end;

  TMt3dmsTimeItemComparer = TComparer<TMt3dmsTimeItem>;

  TOuptputTimeItem = class(TPhastCollectionItem)
  private
    FStoredObservationTime: TRealStorage;
    procedure SetObservationTime(const Value: double);
    procedure SetStoredObservationTime(const Value: TRealStorage);
    function GetObservationTime: double;
    procedure PropertyChanged(Sender: TObject);
  public
    property ObservationTime: double read GetObservationTime
      write SetObservationTime;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property StoredObservationTime: TRealStorage read FStoredObservationTime
      write SetStoredObservationTime;
  end;

  TMt3dmsOutputTimeCollection = class(TPhastCollection)
  private
    function GetItem(Index: integer): TOuptputTimeItem;
    procedure SetItem(Index: integer; const Value: TOuptputTimeItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    property Items[Index: integer]: TOuptputTimeItem read
      GetItem write SetItem; default;
    function Add: TOuptputTimeItem;
  end;

  TMt3dmsObsTimeItemComparer = TComparer<TOuptputTimeItem>;


implementation

uses
  Math;



{ TMt3dmsTimeItem }

procedure TMt3dmsTimeItem.Assign(Source: TPersistent);
var
  SourceItem: TMt3dmsTimeItem;
begin
  if Source is TMt3dmsTimeItem then
  begin
    SourceItem := TMt3dmsTimeItem(Source);
    StartTime := SourceItem.StartTime;
    EndTime := SourceItem.EndTime;
    StepSize := SourceItem.StepSize;
    TimeStepMultiplier := SourceItem.TimeStepMultiplier;
    MaxStepSize := SourceItem.MaxStepSize;
    MaxSteps := SourceItem.MaxSteps;
    SteadyState := SourceItem.SteadyState;
  end
  else
  begin
    inherited;
  end;
end;

constructor TMt3dmsTimeItem.Create(Collection: TCollection);
begin
  inherited;

  FStoredStartTime := TRealStorage.Create;
  FStoredStartTime.OnChange := PropertyChanged;

  FStoredTimeStepMultiplier := TRealStorage.Create;
  FStoredTimeStepMultiplier.OnChange := PropertyChanged;

  FStoredEndTime := TRealStorage.Create;
  FStoredEndTime.OnChange := PropertyChanged;

  FStoredStepSize := TRealStorage.Create;
  FStoredStepSize.OnChange := PropertyChanged;

  FStoredMaxStepSize := TRealStorage.Create;
  FStoredMaxStepSize.OnChange := PropertyChanged;
end;

destructor TMt3dmsTimeItem.Destroy;
begin
  FStoredMaxStepSize.Free;
  FStoredStepSize.Free;
  FStoredEndTime.Free;
  FStoredTimeStepMultiplier.Free;
  FStoredStartTime.Free;
  inherited;
end;

function TMt3dmsTimeItem.GetEndTime: double;
begin
  result := StoredEndTime.Value;
end;

function TMt3dmsTimeItem.GetMaxStepSize: double;
begin
  result := StoredMaxStepSize.Value;
end;

function TMt3dmsTimeItem.GetStartTime: double;
begin
  result := StoredStartTime.Value;
end;

function TMt3dmsTimeItem.GetStepSize: double;
begin
  result := StoredStepSize.Value;
end;

function TMt3dmsTimeItem.GetTimeStepMuitiplier: double;
begin
  result := StoredTimeStepMultiplier.Value;
end;

procedure TMt3dmsTimeItem.PropertyChanged(Sender: TObject);
begin
  InvalidateModel;
end;

procedure TMt3dmsTimeItem.SetEndTime(const Value: double);
begin
  StoredEndTime.Value := Value;
end;

procedure TMt3dmsTimeItem.SetMaxSteps(const Value: integer);
begin
  SetIntegerProperty(FMaxSteps, Value);
end;

procedure TMt3dmsTimeItem.SetMaxStepSize(const Value: double);
begin
  StoredMaxStepSize.Value := Value;
end;

procedure TMt3dmsTimeItem.SetStartTime(const Value: double);
begin
  StoredStartTime.Value := Value;
end;

procedure TMt3dmsTimeItem.SetSteadyState(const Value: boolean);
begin
  SetBooleanProperty(FSteadyState, Value);
end;

procedure TMt3dmsTimeItem.SetStepSize(const Value: double);
begin
  StoredStepSize.Value := Value;
end;

procedure TMt3dmsTimeItem.SetStoredEndTime(const Value: TRealStorage);
begin
  FStoredEndTime.Assign(Value);
end;

procedure TMt3dmsTimeItem.SetStoredMaxStepSize(const Value: TRealStorage);
begin
  FStoredMaxStepSize.Assign(Value);
end;

procedure TMt3dmsTimeItem.SetStoredStartTime(const Value: TRealStorage);
begin
  FStoredStartTime.Assign(Value);
end;

procedure TMt3dmsTimeItem.SetStoredStepSize(const Value: TRealStorage);
begin
  FStoredStepSize.Assign(Value);
end;

procedure TMt3dmsTimeItem.SetStoredTimeStepMultiplier(
  const Value: TRealStorage);
begin
  FStoredTimeStepMultiplier.Assign(Value);
end;

procedure TMt3dmsTimeItem.SetTimeStepMultiplier(const Value: double);
begin
  StoredTimeStepMultiplier.Value := Value;
end;

{ TMt3dmsTimeCollection }

function TMt3dmsTimeCollection.Add: TMt3dmsTimeItem;
begin
  result := inherited Add as TMt3dmsTimeItem;
end;

procedure TMt3dmsTimeCollection.Assign(Source: TPersistent);
var
  Sorter: TList<TMt3dmsTimeItem>;
  Index: Integer;
begin
  inherited;
  Sorter := TList<TMt3dmsTimeItem>.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      Sorter.Add(Items[Index]);
    end;
    Sorter.Sort(TMt3dmsTimeItemComparer.Construct(
      function (const L, R: TMt3dmsTimeItem): integer
      begin
        result := Sign(L.StartTime - R.StartTime);
        if result = 0 then
        begin
          result := Sign(L.EndTime - R.EndTime);
        end;
      end));
    for Index := 0 to Sorter.Count - 1 do
    begin
      Sorter[Index].Index := Index;
    end;
  finally
    Sorter.Free;
  end;
end;

constructor TMt3dmsTimeCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TMt3dmsTimeItem, InvalidateModelEvent);
end;

function TMt3dmsTimeCollection.GetItem(Index: integer): TMt3dmsTimeItem;
begin
  result := inherited Items[Index] as TMt3dmsTimeItem;
end;

function TMt3dmsTimeCollection.GetItemFromTime(ATime: double): TMt3dmsTimeItem;
var
  Index: Integer;
begin
  result := nil;
  if Count > 0 then
  begin
    if Items[0].StartTime > ATime then
    begin
      result := Items[0];
      Exit;
    end;
  end;
  for Index := 0 to Count - 1 do
  begin
    result := Items[Index];
    if (result.EndTime > ATime) and (result.StartTime <= ATime) then
    begin
      Exit;
    end;
  end;
end;

procedure TMt3dmsTimeCollection.SetItem(Index: integer;
  const Value: TMt3dmsTimeItem);
begin
  inherited Items[Index] := Value;
end;

{ TObservationTimeItem }

procedure TOuptputTimeItem.Assign(Source: TPersistent);
begin
  if Source is TOuptputTimeItem then
  begin
    ObservationTime := TOuptputTimeItem(Source).ObservationTime;
  end
  else
  begin
    inherited;
  end;
end;

constructor TOuptputTimeItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredObservationTime := TRealStorage.Create;
  FStoredObservationTime.OnChange := PropertyChanged;
end;

destructor TOuptputTimeItem.Destroy;
begin
  FStoredObservationTime.Free;
  inherited;
end;

function TOuptputTimeItem.GetObservationTime: double;
begin
  result := FStoredObservationTime.Value;
end;

procedure TOuptputTimeItem.PropertyChanged(Sender: TObject);
begin
  InvalidateModel;
end;

procedure TOuptputTimeItem.SetObservationTime(const Value: double);
begin
  FStoredObservationTime.Value := Value;
end;

procedure TOuptputTimeItem.SetStoredObservationTime(
  const Value: TRealStorage);
begin
  FStoredObservationTime.Assign(Value);
end;

{ TMt3dmsObservationTimeCollection }

function TMt3dmsOutputTimeCollection.Add: TOuptputTimeItem;
begin
  result := inherited Add as TOuptputTimeItem;
end;

procedure TMt3dmsOutputTimeCollection.Assign(Source: TPersistent);
var
  Sorter: TList<TOuptputTimeItem>;
  Index: Integer;
begin
  inherited;
  Sorter := TList<TOuptputTimeItem>.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      Sorter.Add(Items[Index]);
    end;
    Sorter.Sort(TMt3dmsObsTimeItemComparer.Construct(
      function (const L, R: TOuptputTimeItem): integer
      begin
        result := Sign(L.ObservationTime - R.ObservationTime);
      end));
    for Index := 0 to Sorter.Count - 1 do
    begin
      Sorter[Index].Index := Index;
    end;
  finally
    Sorter.Free;
  end;
end;

constructor TMt3dmsOutputTimeCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TOuptputTimeItem, InvalidateModelEvent);
end;

function TMt3dmsOutputTimeCollection.GetItem(
  Index: integer): TOuptputTimeItem;
begin
  result := inherited Items[Index] as TOuptputTimeItem
end;

procedure TMt3dmsOutputTimeCollection.SetItem(Index: integer;
  const Value: TOuptputTimeItem);
begin
  inherited Items[Index] := Value;
end;

end.
