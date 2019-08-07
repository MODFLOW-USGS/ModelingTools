unit SutraTimeScheduleUnit;

interface

uses
  GoPhastTypes, Classes, RealListUnit;

type
  TScheduleType = (stTimeList, stTimeCycle, stStepList, stStepCycle);
  TSutraTimeChoice = (stcAbsolute, stcElapsed);

  TSutraTimeSchedules = class;

  TSutraTimeSchedule = class(TGoPhastPersistent)
  private
    FName: AnsiString;
    FScheduleType: TScheduleType;
    FInitialTimeStep: integer;
    FIncrementUpdateCount: integer;
    FMaxTimeCycle: integer;
    FTimeMultiplierStored: TRealStorage;
    FTimes: TRealCollection;
    FLimitingTimeStep: integer;
    FMaxIncrementStored: TRealStorage;
    FInitialTimeStored: TRealStorage;
    FSteps: TIntegerCollection;
    FTimeStepIncrement: integer;
    FMaxSteps: integer;
    FInitialTimeIncrementStored: TRealStorage;
    FLimitingTimeStored: TRealStorage;
    FMinIncrementStored: TRealStorage;
    FScaleFactorStored: TRealStorage;
    FSutraTimeChoice: TSutraTimeChoice;
    procedure SetIncrementUpdateCount(const Value: integer);
    procedure SetInitialTime(const Value: double);
    procedure SetInitialTimeIncrement(const Value: double);
    procedure SetInitialTimeIncrementStored(const Value: TRealStorage);
    procedure SetInitialTimeStep(const Value: integer);
    procedure SetInitialTimeStored(const Value: TRealStorage);
    procedure SetLimitingTime(const Value: double);
    procedure SetLimitingTimeStep(const Value: integer);
    procedure SetLimitingTimeStored(const Value: TRealStorage);
    procedure SetMaxIncrement(const Value: double);
    procedure SetMaxIncrementStored(const Value: TRealStorage);
    procedure SetMaxSteps(const Value: integer);
    procedure SetMaxTimeCycle(const Value: integer);
    procedure SetMinIncrement(const Value: double);
    procedure SetMinIncrementStored(const Value: TRealStorage);
    procedure SetName(Value: AnsiString);
    procedure SetScaleFactor(const Value: double);
    procedure SetScaleFactorStored(const Value: TRealStorage);
    procedure SetScheduleType(const Value: TScheduleType);
    procedure SetSteps(const Value: TIntegerCollection);
    procedure SetTimeMultiplier(const Value: double);
    procedure SetTimeMultiplierStored(const Value: TRealStorage);
    procedure SetTimes(const Value: TRealCollection);
    procedure SetTimeStepIncrement(const Value: integer);
    function GetInitialTime: double;
    function GetInitialTimeIncrement: double;
    function GetLimitingTime: double;
    function GetMaxIncrement: double;
    function GetMinIncrement: double;
    function GetScaleFactor: double;
    function GetTimeMultiplier: double;
    procedure ValueChanged(Sender: TObject);
    procedure SetSutraTimeChoice(const Value: TSutraTimeChoice);
    procedure Initialize(Index: integer); overload;
    procedure Initialize; overload;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    // SCALT
    // Scale factor to be applied to each time value in the list.
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;
    // TIMEI
    property InitialTime: double read GetInitialTime write SetInitialTime;
    // TIMEL
    property LimitingTime: double read GetLimitingTime write SetLimitingTime;
    // TIMEC
    property InitialTimeIncrement: double read GetInitialTimeIncrement
      write SetInitialTimeIncrement;
    // TCMULT
    property TimeMultiplier: double read GetTimeMultiplier
      write SetTimeMultiplier;
    // TCMIN
    property MinIncrement: double read GetMinIncrement write SetMinIncrement;
    // TCMAX
    property MaxIncrement: double read GetMaxIncrement write SetMaxIncrement;
    function TimeValues(InitialTime: double; AllSchedules: TSutraTimeSchedules)
      : TOneDRealArray;
  published
    // SCHNAM - maximum 10 characters, spaces are allowed.
    property Name: AnsiString read FName write SetName;
    // SCHTYP
    property ScheduleType: TScheduleType read FScheduleType
      write SetScheduleType;

    // For Time List and Time Cycle schedules
    // SCALT
    // Scale factor to be applied to each time value in the list.
    property ScaleFactorStored: TRealStorage read FScaleFactorStored
      write SetScaleFactorStored;
    // CREFT
    property SutraTimeChoice: TSutraTimeChoice read FSutraTimeChoice
      write SetSutraTimeChoice;

    // For Time List schedules
    // TLIST
    // The list of times. (May be continued over multiple lines of input.)
    property Times: TRealCollection read FTimes write SetTimes;

    // For Time Cycle schedules
    // NTMAX
    // Maximum number of time cycles allowed, i.e., the maximum number of
    // times allowed in the schedule, not including the initial time.
    property MaxTimeCycle: integer read FMaxTimeCycle write SetMaxTimeCycle;
    // TIMEI
    // Initial time. Cycling begins at time = TIMEI.
    property InitialTimeStored: TRealStorage read FInitialTimeStored
      write SetInitialTimeStored;
    // TIMEL
    // Limiting time. Cycling continues until time >= TIMEL.
    property LimitingTimeStored: TRealStorage read FLimitingTimeStored
      write SetLimitingTimeStored;
    // TIMEC
    // Initial time increment.
    property InitialTimeIncrementStored: TRealStorage
      read FInitialTimeIncrementStored write SetInitialTimeIncrementStored;
    // NTCYC - minimum value = 1.
    // Number of cycles after which the time increment is updated. The current
    // time increment is multiplied by TCMULT (see below) after every NTCYC
    // cycles. (The value of the time increment is limited by TCMIN and TCMAX;
    // see below.)
    property IncrementUpdateCount: integer read FIncrementUpdateCount
      write SetIncrementUpdateCount;
    // TCMULT
    // Factor by which the time increment is multiplied after every NTCYC cycles.
    property TimeMultiplierStored: TRealStorage read FTimeMultiplierStored
      write SetTimeMultiplierStored;
    // TCMIN
    // Minimum time increment allowed.
    property MinIncrementStored: TRealStorage read FMinIncrementStored
      write SetMinIncrementStored;
    // TCMAX
    // Maximum time increment allowed.
    property MaxIncrementStored: TRealStorage read FMaxIncrementStored
      write SetMaxIncrementStored;

    // For Step list schedules
    // ISLIST
    // The list of (integer) time steps. (May be continued over multiple lines
    // of input.)
    property Steps: TIntegerCollection read FSteps write SetSteps;

    // for Step Cycle schedules
    // NSMAX
    // Max time steps not including initial time step.
    // Maximum number of time step cycles allowed, i.e., the maximum number
    // of time steps allowed in the schedule, not including the
    // initial time step.
    property MaxSteps: integer read FMaxSteps write SetMaxSteps;
    // ISTEPI
    // Cycling begins at time step ISTEPI.
    // Initial time step. Cycling begins at time step = ISTEPI.
    property InitialTimeStep: integer read FInitialTimeStep
      write SetInitialTimeStep;
    // ISTEPL
    // Cycling continues until time step >= ISTEPL
    // Limiting time step. Cycling continues until time step >= ISTEPL.
    property LimitingTimeStep: integer read FLimitingTimeStep
      write SetLimitingTimeStep;
    // ISTEPC
    // Time step increment.
    property TimeStepIncrement: integer read FTimeStepIncrement
      write SetTimeStepIncrement;
  end;

  TSutraTimeScheduleItem = class(TPhastCollectionItem)
  private
    FSchedule: TSutraTimeSchedule;
    procedure SetSchedule(const Value: TSutraTimeSchedule);
    function GetSchedule: TSutraTimeSchedule;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Schedule: TSutraTimeSchedule read GetSchedule write SetSchedule;
  end;

  TSutraTimeOptions = class;

  TSutraTimeSchedules = class(TPhastCollection)
  private
    FSutraTime: TSutraTimeOptions;
    function GetItem(Index: integer): TSutraTimeScheduleItem;
    procedure SetItem(Index: integer; const Value: TSutraTimeScheduleItem);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent; SutraTime: TSutraTimeOptions);
    function Add: TSutraTimeScheduleItem;
    property Items[Index: integer]: TSutraTimeScheduleItem read GetItem
      write SetItem; default;
    function GetScheduleByName(AName: string): TSutraTimeSchedule;
  end;

  TSutraTimeOptions = class(TGophastPersistent)
  private
    FTransportSolutionCycleSteps: integer;
    FHydraulicSolutionCycleSteps: integer;
    FInitialTime: Double;
    FAllTimes: TRealList;
    FSchedules: TSutraTimeSchedules;
    procedure SetHydraulicSolutionCycleSteps(const Value: integer);
    procedure SetInitialTime(const Value: Double);
    procedure SetTransportSolutionCycleSteps(const Value: integer);
    procedure SetSchedules(const Value: TSutraTimeSchedules);
    function GetSchedules: TSutraTimeSchedules;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Initialize;
    procedure CalculateAllTimes;
    property AllTimes: TRealList read FAllTimes;
  published
    // TICS
    property InitialTime: Double read FInitialTime write SetInitialTime;
    // NPCYC
    property HydraulicSolutionCycleSteps: integer
      read FHydraulicSolutionCycleSteps write SetHydraulicSolutionCycleSteps
      default 1;
    // NUCYC
    property TransportSolutionCycleSteps: integer
      read FTransportSolutionCycleSteps write SetTransportSolutionCycleSteps
      default 1;
    property Schedules: TSutraTimeSchedules read GetSchedules write SetSchedules;
  end;


implementation

uses
  SysUtils, frmGoPhastUnit, ScreenObjectUnit, SutraBoundariesUnit, Math,
  frmErrorsAndWarningsUnit;

resourcestring
  StrASchedule = 'A_Schedule';
  StrScheduled = 'Schedule%d';
  StrTimeScheduleAdjust = 'Time schedule adjusted';
  StrTheObject0sAdds = 'The object %0:s adds %1:d times to the time step sch' +
  'edule';

  { TCustomSutraTimeSchedule }

procedure TSutraTimeSchedule.Assign(Source: TPersistent);
var
  SourceSchedule: TSutraTimeSchedule;
begin
  if Source is TSutraTimeSchedule then
  begin
    SourceSchedule := TSutraTimeSchedule(Source);
    Name := SourceSchedule.Name;
    ScaleFactorStored := SourceSchedule.ScaleFactorStored;
    Times := SourceSchedule.Times;
    Steps := SourceSchedule.Steps;
    // Times and Steps should be set before ScheduleType because setting
    // ScheduleType can modify Times and/or Steps.
    ScheduleType := SourceSchedule.ScheduleType;
    MaxTimeCycle := SourceSchedule.MaxTimeCycle;
    InitialTimeStored := SourceSchedule.InitialTimeStored;
    LimitingTimeStored := SourceSchedule.LimitingTimeStored;
    InitialTimeIncrementStored := SourceSchedule.InitialTimeIncrementStored;
    IncrementUpdateCount := SourceSchedule.IncrementUpdateCount;
    TimeMultiplierStored := SourceSchedule.TimeMultiplierStored;
    MinIncrementStored := SourceSchedule.MinIncrementStored;
    MaxIncrementStored := SourceSchedule.MaxIncrementStored;
    MaxSteps := SourceSchedule.MaxSteps;
    InitialTimeStep := SourceSchedule.InitialTimeStep;
    LimitingTimeStep := SourceSchedule.LimitingTimeStep;
    TimeStepIncrement := SourceSchedule.TimeStepIncrement;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraTimeSchedule.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(InvalidateModelEvent);
  FTimeMultiplierStored := TRealStorage.Create;
  FTimeMultiplierStored.OnChange := ValueChanged;

  FMaxIncrementStored := TRealStorage.Create;
  FMaxIncrementStored.OnChange := ValueChanged;

  FInitialTimeStored := TRealStorage.Create;
  FInitialTimeStored.OnChange := ValueChanged;

  FInitialTimeIncrementStored := TRealStorage.Create;
  FInitialTimeIncrementStored.OnChange := ValueChanged;

  FLimitingTimeStored := TRealStorage.Create;
  FLimitingTimeStored.OnChange := ValueChanged;

  FMinIncrementStored := TRealStorage.Create;
  FMinIncrementStored.OnChange := ValueChanged;

  FScaleFactorStored := TRealStorage.Create;
  FScaleFactorStored.OnChange := ValueChanged;

  FTimes := TRealCollection.Create(InvalidateModelEvent);
  FSteps := TIntegerCollection.Create(InvalidateModelEvent);
end;

destructor TSutraTimeSchedule.Destroy;
begin
  FSteps.Free;
  FTimes.Free;
  FScaleFactorStored.Free;
  FMinIncrementStored.Free;
  FLimitingTimeStored.Free;
  FInitialTimeIncrementStored.Free;
  FInitialTimeStored.Free;
  FMaxIncrementStored.Free;
  FTimeMultiplierStored.Free;
  inherited;
end;

function TSutraTimeSchedule.GetInitialTime: double;
begin
  result := InitialTimeStored.Value;
end;

function TSutraTimeSchedule.GetInitialTimeIncrement: double;
begin
  result := InitialTimeIncrementStored.Value;
end;

function TSutraTimeSchedule.GetLimitingTime: double;
begin
  result := LimitingTimeStored.Value;
end;

function TSutraTimeSchedule.GetMaxIncrement: double;
begin
  result := MaxIncrementStored.Value;
end;

function TSutraTimeSchedule.GetMinIncrement: double;
begin
  result := MinIncrementStored.Value;
end;

function TSutraTimeSchedule.GetScaleFactor: double;
begin
  result := ScaleFactorStored.Value;
end;

function TSutraTimeSchedule.GetTimeMultiplier: double;
begin
  result := TimeMultiplierStored.Value;
end;

procedure TSutraTimeSchedule.Initialize;
begin
  FScheduleType := stTimeCycle;
  FInitialTimeStep := 0;
  FIncrementUpdateCount := 9999;
  FMaxTimeCycle := 1;
  FTimeMultiplierStored.Value := 1;
  FLimitingTimeStep := MAXINT;
  FMaxIncrementStored.Value := 1E99;
  FInitialTimeStored.Value := 0;
  FTimeStepIncrement := 1;
  FMaxSteps := 1;
  FInitialTimeIncrementStored.Value := 1;
  FLimitingTimeStored.Value := 1E99;
  FMinIncrementStored.Value := 1E-20;
  FScaleFactorStored.Value := 1;
  FSutraTimeChoice := stcElapsed;
  FName := AnsiString(StrASchedule);
end;

procedure TSutraTimeSchedule.Initialize(Index: integer);
begin
  Initialize;
  FName := AnsiString(Format(StrScheduled, [Index + 1]));
end;

procedure TSutraTimeSchedule.SetIncrementUpdateCount(const Value: integer);
begin
  SetIntegerProperty(FIncrementUpdateCount, Value);
end;

procedure TSutraTimeSchedule.SetInitialTime(const Value: double);
begin
  InitialTimeStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetInitialTimeIncrement(const Value: double);
begin
  InitialTimeIncrementStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetInitialTimeIncrementStored
  (const Value: TRealStorage);
begin
  FInitialTimeIncrementStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetInitialTimeStep(const Value: integer);
begin
  SetIntegerProperty(FInitialTimeStep, Value);
end;

procedure TSutraTimeSchedule.SetInitialTimeStored(const Value: TRealStorage);
begin
  FInitialTimeStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetLimitingTime(const Value: double);
begin
  LimitingTimeStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetLimitingTimeStep(const Value: integer);
begin
  SetIntegerProperty(FLimitingTimeStep, Value);
end;

procedure TSutraTimeSchedule.SetLimitingTimeStored(const Value: TRealStorage);
begin
  FLimitingTimeStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetMaxIncrement(const Value: double);
begin
  MaxIncrementStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetMaxIncrementStored(const Value: TRealStorage);
begin
  FMaxIncrementStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetMaxSteps(const Value: integer);
begin
  SetIntegerProperty(FMaxSteps, Value);
end;

procedure TSutraTimeSchedule.SetMaxTimeCycle(const Value: integer);
begin
  SetIntegerProperty(FMaxTimeCycle, Value);
end;

procedure TSutraTimeSchedule.SetMinIncrement(const Value: double);
begin
  MinIncrementStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetMinIncrementStored(const Value: TRealStorage);
begin
  FMinIncrementStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetName(Value: AnsiString);
begin
  // maximum length = 10.
  Value := Copy(Value, 1, 10);
  SetAnsiStringProperty(FName, Value);
end;

procedure TSutraTimeSchedule.SetScaleFactor(const Value: double);
begin
  ScaleFactorStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetScaleFactorStored(const Value: TRealStorage);
begin
  FScaleFactorStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetScheduleType(const Value: TScheduleType);
begin
  if FScheduleType <> Value then
  begin
    FScheduleType := Value;
    if FScheduleType <> stTimeList then
    begin
      Times.Clear;
    end;
    if FScheduleType <> stStepList then
    begin
      Steps.Clear;
    end;
    InvalidateModel;
  end;
end;

procedure TSutraTimeSchedule.SetSteps(const Value: TIntegerCollection);
begin
  FSteps.Assign(Value);
end;

procedure TSutraTimeSchedule.SetSutraTimeChoice(const Value: TSutraTimeChoice);
begin
  if FSutraTimeChoice <> Value then
  begin
    FSutraTimeChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSutraTimeSchedule.SetTimeMultiplier(const Value: double);
begin
  TimeMultiplierStored.Value := Value;
end;

procedure TSutraTimeSchedule.SetTimeMultiplierStored(const Value: TRealStorage);
begin
  FTimeMultiplierStored.Assign(Value);
end;

procedure TSutraTimeSchedule.SetTimes(const Value: TRealCollection);
begin
  FTimes.Assign(Value);
end;

procedure TSutraTimeSchedule.SetTimeStepIncrement(const Value: integer);
begin
  SetIntegerProperty(FTimeStepIncrement, Value);
end;

function TSutraTimeSchedule.TimeValues(InitialTime: double;
  AllSchedules: TSutraTimeSchedules): TOneDRealArray;
var
  Index: integer;
  Increment: double;
  ATime: double;
  TimeSteps: TOneDRealArray;
  TimeCount: integer;
  TimeIndex: integer;
begin
  case ScheduleType of
    stTimeList:
      begin
        SetLength(result, Times.Count);
        for Index := 0 to Times.Count - 1 do
        begin
          result[Index] := Times[Index].Value * ScaleFactor;
          if SutraTimeChoice = stcElapsed then
          begin
            result[Index] := result[Index] + InitialTime;
          end;
        end;
      end;
    stTimeCycle:
      begin
        SetLength(result, MaxTimeCycle + 1);
        ATime := InitialTime;
        Increment := InitialTimeIncrement;
        Index := 0;
        while (Index < Length(result)) do
        begin
          result[Index] := ATime;
          ATime := ATime + Increment;

          Inc(Index);
          if ATime >= LimitingTime then
          begin
            ATime := LimitingTime;
            if Index < Length(result) then
            begin
              result[Index] := ATime;
              Inc(Index);
            end;
            SetLength(result, Index);
            break;
          end;
          if (Index mod IncrementUpdateCount) = 0 then
          begin
            Increment := Increment * TimeMultiplier;
            if Increment > MaxIncrement then
            begin
              Increment := MaxIncrement;
            end;
            if Increment < MinIncrement then
            begin
              Increment := MinIncrement;
            end;
          end;
        end;
        for Index := 0 to Length(result) - 1 do
        begin
          if ScaleFactor <> 1 then
          begin
            result[Index] := result[Index] * ScaleFactor;
          end;
          if SutraTimeChoice = stcElapsed then
          begin
            result[Index] := result[Index] + InitialTime;
          end;
        end;
      end;
    stStepList:
      begin
        TimeSteps := AllSchedules[0].Schedule.TimeValues(InitialTime,
          AllSchedules);

        SetLength(result, Steps.Count);
        TimeCount := 0;
        for Index := 0 to Steps.Count - 1 do
        begin
          TimeIndex := Steps[Index].Value - 1;
          if (TimeIndex >= 0) and (TimeIndex < Length(TimeSteps)) then
          begin
            result[TimeCount] := TimeSteps[TimeIndex];
            Inc(TimeCount);
          end;
        end;
        if TimeCount <> Steps.Count then
        begin
          SetLength(result, TimeCount);
        end;
      end;
    stStepCycle:
      begin
        TimeSteps := AllSchedules[0].Schedule.TimeValues(InitialTime,
          AllSchedules);

        SetLength(result, Min(MaxSteps, Length(TimeSteps)));
        TimeIndex := InitialTimeStep;
        TimeCount := 0;
        while (TimeCount >= 0) and (TimeCount < Length(result)) do
        begin
          if (TimeIndex >= Length(TimeSteps)) then
          begin
            break;
          end;
          result[TimeCount] := TimeSteps[TimeIndex];
          Inc(TimeCount);
          Inc(TimeIndex, TimeStepIncrement);
          if TimeIndex > LimitingTimeStep - 1 then
          begin
            break;
          end;
        end;
        if TimeCount <> MaxSteps then
        begin
          SetLength(result, TimeCount);
        end;
      end;
  else
    begin
      SetLength(result, 0);
      Assert(False);
    end;
  end;
end;

procedure TSutraTimeSchedule.ValueChanged(Sender: TObject);
begin
  InvalidateModel;
end;

{ TSutraTimeSchedules }

function TSutraTimeSchedules.Add: TSutraTimeScheduleItem;
begin
  result := inherited Add as TSutraTimeScheduleItem;
end;

constructor TSutraTimeSchedules.Create(InvalidateModelEvent: TNotifyEvent; SutraTime: TSutraTimeOptions);
begin
  Assert(SutraTime <> nil);
  inherited Create(TSutraTimeScheduleItem, InvalidateModelEvent);
  FSutraTime := SutraTime;
end;

function TSutraTimeSchedules.GetItem(Index: integer): TSutraTimeScheduleItem;
begin
  result := inherited Items[Index] as TSutraTimeScheduleItem
end;

function TSutraTimeSchedules.GetScheduleByName(
  AName: string): TSutraTimeSchedule;
var
  index: Integer;
  AnItem: TSutraTimeScheduleItem;
begin
  result := nil;
  for index := 0 to Count - 1 do
  begin
    AnItem := Items[index];
    if AnsiSameText(string(AnItem.Schedule.Name), AName) then
    begin
      result := AnItem.Schedule;
      exit;
    end;
  end;
end;

procedure TSutraTimeSchedules.SetItem(Index: integer;
  const Value: TSutraTimeScheduleItem);
begin
  inherited Items[Index] := Value;
end;

{ TSutraTimeScheduleItem }

procedure TSutraTimeScheduleItem.Assign(Source: TPersistent);
begin
  if Source is TSutraTimeScheduleItem then
  begin
    Schedule := TSutraTimeScheduleItem(Source).Schedule;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraTimeScheduleItem.Create(Collection: TCollection);
begin
  inherited;
  FSchedule := TSutraTimeSchedule.Create(OnInvalidateModel);
  FSchedule.Initialize(Index);
end;

destructor TSutraTimeScheduleItem.Destroy;
begin
  FSchedule.Free;
  inherited;
end;

function TSutraTimeScheduleItem.GetSchedule: TSutraTimeSchedule;
var
  InitialTime: Double;
  AllSchedules: TSutraTimeSchedules;
  FirstSchedule: TSutraTimeSchedule;
  Times: TOneDRealArray;
  RealList: TRealList;
  TimeIndex: Integer;
begin
  result := FSchedule;
  if (Index <> 0) then
  begin
    if (FSchedule.ScheduleType = stTimeList) then
    begin
      { TODO : It would probably be a good idea to only do this when the
        first schedule has been changed and this has not been rechecked. }
      AllSchedules := (Collection as TSutraTimeSchedules);
      InitialTime := AllSchedules.FSutraTime.InitialTime;
      FirstSchedule := AllSchedules.Items[0].Schedule;
      Times := FirstSchedule.TimeValues(InitialTime, AllSchedules);
      RealList := TRealList.Create;
      try
        RealList.Capacity := Length(Times);
        for TimeIndex := 0 to Length(Times) - 1 do
        begin
          RealList.Add(Times[TimeIndex]);
        end;
        RealList.Sort;

        for TimeIndex := FSchedule.Times.Count - 1 downto 0 do
        begin
          if RealList.IndexOf(FSchedule.Times[TimeIndex].Value) < 0 then
          begin
            FSchedule.Times.Delete(TimeIndex);
          end;
        end;
      finally
        RealList.Free;
      end;
    end;
  end;
end;

procedure TSutraTimeScheduleItem.SetSchedule(const Value: TSutraTimeSchedule);
begin
  FSchedule.Assign(Value);
end;

{ TSutraTimeOptions }

procedure TSutraTimeOptions.Assign(Source: TPersistent);
var
  SourceOptions: TSutraTimeOptions;
begin
  if Source is TSutraTimeOptions then
  begin
    SourceOptions := TSutraTimeOptions(Source);
    InitialTime := SourceOptions.InitialTime;
    HydraulicSolutionCycleSteps := SourceOptions.HydraulicSolutionCycleSteps;
    TransportSolutionCycleSteps := SourceOptions.TransportSolutionCycleSteps;
    Schedules := SourceOptions.Schedules;
  end
  else
  begin
    inherited;
  end;
end;

procedure TSutraTimeOptions.CalculateAllTimes;
var
  ASchedule: TSutraTimeSchedule;
  Times: TOneDRealArray;
  TimeIndex: Integer;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  Boundaries: TSutraBoundaries;
  AValue1: single;
  AValue2: single;
  LastTime: Double;
  AValue: Double;
  StartingCount: Integer;
begin
  frmErrorsAndWarnings.RemoveWarningGroup(frmGoPhast.PhastModel,
        StrTimeScheduleAdjust);

  { TODO -cSUTRA : Only do this when the FAllTimes is out of date. }
  FAllTimes.Clear;
  FAllTimes.Sorted := True;
  ASchedule := Schedules[0].Schedule;
  Times := ASchedule.TimeValues(InitialTime, Schedules);
  for TimeIndex := 0 to Length(Times) - 1 do
  begin
    FAllTimes.AddUnique(Times[TimeIndex]);
  end;
  LastTime := FAllTimes.Last;

  // The first schedule has all the time steps.
  // Subsequent ones have subsets of those time steps.

//  for ScheduleIndex := 0 to Schedules.Count - 1 do
//  begin
//    ASchedule := Schedules[ScheduleIndex].Schedule;
//    Times := ASchedule.TimeValues(InitialTime, Schedules);
//    for TimeIndex := 0 to Length(Times) - 1 do
//    begin
//      FAllTimes.AddUnique(Times[TimeIndex]);
//    end;
//  end;

  // Custom times may be in boundaries or observations.
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    StartingCount := FAllTimes.Count;
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundaries := AScreenObject.SutraBoundaries;
    if Boundaries.Observations.Used then
    begin
      for TimeIndex := 0 to Boundaries.Observations.Times.Count - 1 do
      begin
        AValue := Boundaries.Observations.Times[TimeIndex].Value;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if Boundaries.FluidSource.Used then
    begin
      for TimeIndex := 0 to Boundaries.FluidSource.Values.Count - 1 do
      begin
        AValue := Boundaries.FluidSource.Values[TimeIndex].StartTime;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if Boundaries.MassEnergySource.Used then
    begin
      for TimeIndex := 0 to Boundaries.MassEnergySource.Values.Count - 1 do
      begin
        AValue := Boundaries.MassEnergySource.Values[TimeIndex].StartTime;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if Boundaries.SpecifiedPressure.Used then
    begin
      for TimeIndex := 0 to Boundaries.SpecifiedPressure.Values.Count - 1 do
      begin
        AValue := Boundaries.SpecifiedPressure.Values[TimeIndex].StartTime;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if Boundaries.SpecifiedConcTemp.Used then
    begin
      for TimeIndex := 0 to Boundaries.SpecifiedConcTemp.Values.Count - 1 do
      begin
        AValue := Boundaries.SpecifiedConcTemp.Values[TimeIndex].StartTime;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if Boundaries.GeneralFlowBoundary.Used then
    begin
      for TimeIndex := 0 to Boundaries.GeneralFlowBoundary.Values.Count - 1 do
      begin
        AValue := Boundaries.GeneralFlowBoundary.Values[TimeIndex].StartTime;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if Boundaries.GenTransportBoundary.Used then
    begin
      for TimeIndex := 0 to Boundaries.GenTransportBoundary.Values.Count - 1 do
      begin
        AValue := Boundaries.GenTransportBoundary.Values[TimeIndex].StartTime;
        if AValue < LastTime then
        begin
          FAllTimes.AddUnique(AValue);
        end;
      end;
    end;
    if StartingCount <> FAllTimes.Count then
    begin
      frmErrorsAndWarnings.AddWarning(frmGoPhast.PhastModel,
        StrTimeScheduleAdjust,
        Format(StrTheObject0sAdds,
        [AScreenObject.Name, FAllTimes.Count-StartingCount]), AScreenObject);
    end;
  end;
  for TimeIndex := FAllTimes.Count - 1 downto 1 do
  begin
    try
      AValue1 := FAllTimes[TimeIndex];
      AValue2 := FAllTimes[TimeIndex-1];
    except on EOverflow Do
      begin
        Continue;
      end;
    end;
    if AValue1 = AValue2 then
    begin
      FAllTimes.Delete(TimeIndex);
    end;
  end;
end;

constructor TSutraTimeOptions.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(InvalidateModelEvent);
  FSchedules := TSutraTimeSchedules.Create(InvalidateModelEvent, self);
  FAllTimes := TRealList.Create;
  Initialize;
end;

destructor TSutraTimeOptions.Destroy;
begin
  FAllTimes.Free;
  FSchedules.Free;
  inherited;
end;

function TSutraTimeOptions.GetSchedules: TSutraTimeSchedules;
begin
  result := FSchedules;
  if result.Count = 0 then
  begin
    result.Add.Schedule.Name := 'TIME_STEPS';
  end;
end;

procedure TSutraTimeOptions.Initialize;
begin
  FInitialTime := 0;
  FHydraulicSolutionCycleSteps := 1;
  FTransportSolutionCycleSteps := 1;
  FSchedules.Clear;
end;

procedure TSutraTimeOptions.SetHydraulicSolutionCycleSteps(
  const Value: integer);
begin
  SetIntegerProperty(FHydraulicSolutionCycleSteps, Value);
end;

procedure TSutraTimeOptions.SetInitialTime(const Value: Double);
begin
  SetRealProperty(FInitialTime, Value);
end;

procedure TSutraTimeOptions.SetSchedules(const Value: TSutraTimeSchedules);
begin
  FSchedules.Assign(Value);
end;

procedure TSutraTimeOptions.SetTransportSolutionCycleSteps(
  const Value: integer);
begin
  SetIntegerProperty(FTransportSolutionCycleSteps, Value);
end;

end.
