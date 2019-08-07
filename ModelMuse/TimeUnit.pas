{@abstract(@name provides a collection that stores the stress period
  information for the PHAST model.  See @link(TTimeCollection).)}
unit TimeUnit;

interface

uses Classes, GoPhastTypes;

type
  {@abstract(@name represents one stress period in PHAST.)}
  TTimeItem = class(TCollectionItem)
  private
    // @name: double;
    // See @link(EndingTime).
    FEndingTime: double;
    // @name: double;
    // See @link(TimeStepLength).
    FTimeStepLength: double;
    // See @link(EndingTime).
    procedure SetEndingTime(const Value: double);
    // See @link(TimeStepLength).
    procedure SetTimeStepLength(const Value: double);
  public
    // If Source is a @classname,
    // @name copies the published properties of Source
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(Collection: TCollection); override;
  published
    // @name is the length of the stress period.
    property TimeStepLength: double read FTimeStepLength write
      SetTimeStepLength;
    // @name is the ending time of the stress period.
    property EndingTime: double read FEndingTime write SetEndingTime;
  end;

  {@abstract(@name represents a collection of stress periods in PHAST.)}
  TTimeCollection = class(TPhastCollection)
  private
    FStartTime: TRealStorage;
    procedure SetStartTime(const Value: TRealStorage);
    function GetTimeItem(Index: Integer): TTimeItem;
    procedure SetTimeItem(Index: Integer; const Value: TTimeItem);
  public
    // @name creates an instance of @classname.
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    // @name returns the time-step length of the first @link(TTimeItem)
    // whose ending time is greater than Time.
    function TimeStepLength(const Time: double): double;
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Initialize;
    property Items[Index: Integer]: TTimeItem read GetTimeItem
      write SetTimeItem; default;
  published
    property StartTime: TRealStorage read FStartTime write SetStartTime;
  end;

implementation

uses frmGoPhastUnit;

{ TTimeItem }

procedure TTimeItem.Assign(Source: TPersistent);
var
  SourceItem: TTimeItem;
begin
  if Source is TTimeItem then
  begin
    SourceItem := TTimeItem(Source);
    EndingTime := SourceItem.EndingTime;
    TimeStepLength := SourceItem.TimeStepLength;
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeItem.Create(Collection: TCollection);
begin
  inherited;
  EndingTime := 1;
  TimeStepLength := 1;
end;

procedure TTimeItem.SetEndingTime(const Value: double);
begin
  if FEndingTime <> Value then
  begin
    FEndingTime := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TTimeItem.SetTimeStepLength(const Value: double);
begin
  if FTimeStepLength <> Value then
  begin
    FTimeStepLength := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TTimeCollection }

procedure TTimeCollection.Assign(Source: TPersistent);
begin
  if Source is TTimeCollection then
  begin
    StartTime := TTimeCollection(Source).StartTime;
  end;
  inherited;
end;

constructor TTimeCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TTimeItem, InvalidateModelEvent);
  FStartTime:= TRealStorage.Create;
  Initialize;
end;

destructor TTimeCollection.Destroy;
begin
  FStartTime.Free;
  inherited;
end;

function TTimeCollection.GetTimeItem(Index: Integer): TTimeItem;
begin
  result := inherited Items[Index] as TTimeItem;
end;

procedure TTimeCollection.Initialize;
begin
  FStartTime.Value := 0;
end;

procedure TTimeCollection.SetStartTime(const Value: TRealStorage);
begin
  if FStartTime.Value <> Value.Value then
  begin
    FStartTime.Assign(Value);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TTimeCollection.SetTimeItem(Index: Integer; const Value: TTimeItem);
begin
  inherited Items[Index] := Value;
end;

function TTimeCollection.TimeStepLength(const Time: double): double;
var
  Index: integer;
  TimeItem: TTimeItem;
begin
  Assert(Time >= 0);
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    TimeItem := Items[Index] as TTimeItem;
    if TimeItem.EndingTime > Time then
    begin
      result := TimeItem.TimeStepLength;
      Exit;
    end;
  end;
end;

end.

