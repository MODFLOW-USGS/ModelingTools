unit ModflowTimeUnit;

interface

uses SysUtils, Classes, RbwDataGrid4, GoPhastTypes;

type
  // @name defines the types of stress periods supported by MODFLOW.
  // @value(sptSteadyState Steady-state stress period)
  // @value(sptTransient Transient stress period)
  TStressPeriodType = (sptSteadyState, sptTransient);

  TTimeStep = record
    StartTime: double;
    EndTime: double;
  end;

  // @name represents a single stress period in MODFLOW.
  TModflowStressPeriod = class(TCollectionItem)
  private
    // See @link(EndTime).
    FEndTime: double;
    // See @link(MaxLengthOfFirstTimeStep).
    FMaxLengthOfFirstTimeStep: double;
    // See @link(PeriodLength).
    FPeriodLength: double;
    // See @link(StartTime).
    FStartTime: double;
    // See @link(StressPeriodType).
    FStressPeriodType: TStressPeriodType;
    // See @link(TimeStepMultiplier).
    FTimeStepMultiplier: double;
    FDrawDownReference: boolean;
    FAtsUsed: Boolean;
    FStoredAtsInitialStepSize: TRealStorage;
    FStoredAtsMaximumStepSize: TRealStorage;
    FStoredAtsMinimumStepSize: TRealStorage;
    FStoredAtsAdjustmentFactor: TRealStorage;
    FStoredAtsFailureFactor: TRealStorage;
    // @name calls @link(TBaseModel.Invalidate) indirectly.
    procedure InvalidateModel;
    procedure InvalModel(Sender: TObject);
    // See @link(EndTime).
    procedure SetEndTime(Value: double);
    // See @link(MaxLengthOfFirstTimeStep).
    procedure SetMaxLengthOfFirstTimeStep(const Value: double);
    // See @link(PeriodLength).
    procedure SetPeriodLength(const Value: double);
    // See @link(StartTime).
    procedure SetStartTime(Value: double);
    // See @link(StressPeriodType).
    procedure SetStressPeriodType(const Value: TStressPeriodType);
    // See @link(TimeStepMultiplier).
    procedure SetTimeStepMultiplier(const Value: double);
    procedure SetDrawDownReference(const Value: boolean);
    procedure SetAtsUsed(const Value: Boolean);
    procedure SetStoredAtsInitialStepSize(const Value: TRealStorage);
    procedure SetStoredAtsAdjustmentFactor(const Value: TRealStorage);
    procedure SetStoredAtsFailureFactor(const Value: TRealStorage);
    procedure SetStoredAtsMaximumStepSize(const Value: TRealStorage);
    procedure SetStoredAtsMinimumStepSize(const Value: TRealStorage);
    function GetAtsAdjustmentFactor: double;
    function GetAtsFailureFactor: double;
    function GetAtsInitialStepSize: double;
    function GetAtsMaximumStepSize: double;
    function GetAtsMinimumStepSize: double;
    procedure SetAtsAdjustmentFactor(const Value: double);
    procedure SetAtsFailureFactor(const Value: double);
    procedure SetAtsInitialStepSize(const Value: double);
    procedure SetAtsMaximumStepSize(const Value: double);
    procedure SetAtsMinimumStepSize(const Value: double);
  public
    // @name copies Source to the current @classname.
    procedure Assign(Source: TPersistent); override;
    // @name initializes the properties of the @classname
    // and calls @link(InvalidateModel).
    constructor Create(Collection: TCollection); override;
    // @name calls @link(InvalidateModel).
    destructor Destroy; override;
    // @name is the number of steps in a stress period.
    function NumberOfSteps: integer;
    function LengthOfFirstTimeStep: double;
    function GetTimeStep(Step: Integer): TTimeStep;
    property AtsInitialStepSize: double read GetAtsInitialStepSize write SetAtsInitialStepSize;
    property AtsMinimumStepSize: double read GetAtsMinimumStepSize write SetAtsMinimumStepSize;
    property AtsMaximumStepSize: double read GetAtsMaximumStepSize write SetAtsMaximumStepSize;
    property AtsAdjustmentFactor: double read GetAtsAdjustmentFactor write SetAtsAdjustmentFactor;
    property AtsFailureFactor: double read GetAtsFailureFactor write SetAtsFailureFactor;
  published
    // If @name is true, the head at the end of this stress period
    // will be used as a reference head for computing drawdown.
    property DrawDownReference: boolean read FDrawDownReference write SetDrawDownReference;
    // @name is the time of the end of the stress period.
    // @name is not exported but it is used in @link(TfrmModflowTime)
    // to calculate the length of the stress period (PeriodLength).
    property EndTime: double read FEndTime write SetEndTime;
    // @name is the maximum allowable length (as specified by the user)
    // of the first time step in a stress period.
    property MaxLengthOfFirstTimeStep: double read FMaxLengthOfFirstTimeStep
      write SetMaxLengthOfFirstTimeStep;
    // @name is the length of the stress period.
    property PeriodLength: double read FPeriodLength write SetPeriodLength;
    // @name is the time of the start of the stress period.
    // @name is not exported but it is used in @link(TfrmModflowTime)
    // to calculate the length of the stress period (PeriodLength).
    property StartTime: double read FStartTime write SetStartTime;
    // @name indicates whether this is a steady-state or transient
    // stress period.
    property StressPeriodType: TStressPeriodType read FStressPeriodType
      write SetStressPeriodType;
    // @name indicates the fraction by which each time step increases in size
    // over the length of the previous time step in the same stress period.
    property TimeStepMultiplier: double read FTimeStepMultiplier
      write SetTimeStepMultiplier;
    property AtsUsed: Boolean read FAtsUsed write SetAtsUsed;
    property StoredAtsInitialStepSize: TRealStorage
      read FStoredAtsInitialStepSize write SetStoredAtsInitialStepSize;
    property StoredAtsMinimumStepSize: TRealStorage
      read FStoredAtsMinimumStepSize write SetStoredAtsMinimumStepSize;
    property StoredAtsMaximumStepSize: TRealStorage
      read FStoredAtsMaximumStepSize write SetStoredAtsMaximumStepSize;
    property StoredAtsAdjustmentFactor: TRealStorage
      read FStoredAtsAdjustmentFactor write SetStoredAtsAdjustmentFactor;
    property StoredAtsFailureFactor: TRealStorage
      read FStoredAtsFailureFactor write SetStoredAtsFailureFactor;
  end;

  // @name is a collection of the data defining all the stress periods
  // in the model.
  TModflowStressPeriods = class(TCollection)
  private
    // @name is is used to invalidate a model
    FOnInvalidateModel: TNotifyEvent;
    // See @link(Items).
    function GetItems(Index: Integer): TModflowStressPeriod;
    // @name calls @link(TBaseModel.Invalidate) indirectly.
    procedure InvalidateModel;
    // See @link(Items).
    procedure SetItems(Index: Integer; const Value: TModflowStressPeriod);
    function GetNumberOfSteps: integer;
    function GetFirst: TModflowStressPeriod;
    function GetLast: TModflowStressPeriod;
  public
    // @name copies Source to the current @classname.
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    // @name is used to access a @link(TModflowStressPeriod)
    // stored in @classname.
    property Items[Index: Integer]: TModflowStressPeriod
      read GetItems write SetItems; default;
    // @name writes the stress periods to the MODFLOW discretization file.
    procedure WriteStressPeriods(const DiscretizationWriter: TObject);
    procedure WriteMt3dmsStressPeriods(const Mt3dmsBasicWriter: TObject);
    // @name returns true if any stress period in the model is transient.
    function TransientModel: boolean;
    // @name returns @true if every stress period is transient.
    Function CompletelyTransient: boolean;
    procedure FillPickListWithEndTimes(Grid: TRbwDataGrid4; Col: integer);
    procedure FillPickListWithStartTimes(Grid: TRbwDataGrid4; Col: integer);
    // @name determines the stress period and time step corresponding
    // to a particular time.  Period and Step are zero based.
    procedure TimeToPeriodAndStep(ATime: double; out Period, Step: integer);
    property NumberOfSteps: integer read GetNumberOfSteps;
    procedure FillStringsWithStartTimes(Strings: TStrings);
    procedure FillStringsWithEndTimes(Strings: TStrings);
    function MaxStepsInAnyStressPeriod: integer;
    // @name returns the number of the first stress period that contains ATime.
    // @name starts at zero.
    function FindStressPeriod(ATime: double): integer;
    function FindEndStressPeriod(ATime: double): integer;
    property First: TModflowStressPeriod read GetFirst;
    property Last: TModflowStressPeriod read GetLast;
    function FmpTimeStepSizeWarning(Model: TObject): string;
  end;

function GetNumberOfTimeSteps(const PerLength, MaxFirstTimeStepLength,
  TimeStepMultiplier: real): integer;

resourcestring
  StrTooManyStressPeri = 'Too many stress periods for formatted output';
  StrTimeStepToShort = 'Time step to short';

implementation

uses RTLConsts, Math, ModflowDiscretizationWriterUnit,
  frmErrorsAndWarningsUnit, Mt3dmsBtnWriterUnit, Mt3dmsTimesUnit,
  ModflowPackageSelectionUnit, PhastModelUnit, ModelMuseUtilities;

resourcestring
  StrUnusualUseOfDrawd = 'Unusual use of Drawdown reference option';
  StrTheFirstAndOnlyS = 'The first and only stress period in the model is '
    + 'also used as a reference period for computing drawdown.  Drawdown will '
    + 'always be zero under these conditions.';
  StrInStressPeriodD = 'In stress period %d, a transient stress period is us' +
  'ed as a reference stress period for computing drawdown.';
  StrTheMaximumNumberO = 'The maximum number of stress periods compatible wi' +
  'th formatted head or drawdown files is 9999. This model as %d stress peri' +
  'ods.';
  StrTooManyTimeSteps = 'Too many time steps for formatted output';
  StrInStressPeriod1 = 'In stress period %0:d, the number of steps (%1:d) is not co' +
  'mpatible with formatted output files.';
  StrWhenUsingTheFarm = 'When using the Farm Process with Crop Consumptive u' +
  'se Concept 1, the minimum step length should be 1 day or more because of ' +
  'the assumption that the soil moisture is at steady state. In this model, ' +
  'the minimum step length is %g days.';

{ TModflowStressPeriod }

function GetNumberOfTimeSteps(const PerLength, MaxFirstTimeStepLength,
  TimeStepMultiplier: real): integer;
const
  Epsilon = 1e-8;
  AdjustmentFactor = 1-Epsilon;
var
  TimeStepLength: double;
  TotalTime: double;
begin
  if PerLength <= 0 then
  begin
    result := 1;
  end
  else if MaxFirstTimeStepLength <= 0 then
  begin
    result := MAXINT;
  end
  else if TimeStepMultiplier = 1 then
  begin
    result := Ceil((PerLength/MaxFirstTimeStepLength)*AdjustmentFactor);
  end
  else
  begin
    TimeStepLength := MaxFirstTimeStepLength;
    TotalTime := MaxFirstTimeStepLength;
    result := 1;
    while TotalTime < PerLength*AdjustmentFactor do
    begin
      TimeStepLength := TimeStepLength * TimeStepMultiplier;
      TotalTime := TotalTime + TimeStepLength;
      Inc(result);
    end;
  end;
end;


procedure TModflowStressPeriod.Assign(Source: TPersistent);
var
  SourceMFStressPeriod: TModflowStressPeriod;
begin
  if Source is TModflowStressPeriod then
  begin
    SourceMFStressPeriod := TModflowStressPeriod(Source);
    StartTime := SourceMFStressPeriod.StartTime;
    EndTime := SourceMFStressPeriod.EndTime;
    MaxLengthOfFirstTimeStep := SourceMFStressPeriod.MaxLengthOfFirstTimeStep;
    PeriodLength := SourceMFStressPeriod.PeriodLength;
    TimeStepMultiplier := SourceMFStressPeriod.TimeStepMultiplier;
    StressPeriodType := SourceMFStressPeriod.StressPeriodType;
    DrawDownReference := SourceMFStressPeriod.DrawDownReference;

    AtsUsed := SourceMFStressPeriod.AtsUsed;
    AtsInitialStepSize := SourceMFStressPeriod.AtsInitialStepSize;
    AtsMinimumStepSize := SourceMFStressPeriod.AtsMinimumStepSize;
    AtsMaximumStepSize := Min(SourceMFStressPeriod.AtsMaximumStepSize, PeriodLength);
    AtsAdjustmentFactor := SourceMFStressPeriod.AtsAdjustmentFactor;
    AtsFailureFactor := SourceMFStressPeriod.AtsFailureFactor;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowStressPeriod.Create(Collection: TCollection);
begin
  inherited;
  FStartTime := 0;
  FEndTime := 0;
  FPeriodLength := 0;
  FTimeStepMultiplier := 0;
  FDrawDownReference := False;
  FStressPeriodType := sptSteadyState;

  FStoredAtsInitialStepSize := TRealStorage.Create(InvalModel);
  FStoredAtsMaximumStepSize := TRealStorage.Create(InvalModel);
  FStoredAtsMinimumStepSize := TRealStorage.Create(InvalModel);
  FStoredAtsAdjustmentFactor := TRealStorage.Create(InvalModel);
  FStoredAtsFailureFactor := TRealStorage.Create(InvalModel);

  FStoredAtsInitialStepSize.Value := 0;
  FStoredAtsMaximumStepSize.Value := 1;
  FStoredAtsMinimumStepSize.Value := 1E-5;
  FStoredAtsAdjustmentFactor.Value := 2;
  FStoredAtsFailureFactor.Value := 5;

  InvalidateModel;
end;

destructor TModflowStressPeriod.Destroy;
begin
  FStoredAtsInitialStepSize.Free;
  FStoredAtsMaximumStepSize.Free;
  FStoredAtsMinimumStepSize.Free;
  FStoredAtsAdjustmentFactor.Free;
  FStoredAtsFailureFactor.Free;

  InvalidateModel;
  inherited;
end;

function TModflowStressPeriod.GetAtsAdjustmentFactor: double;
begin
  result := StoredAtsAdjustmentFactor.Value;
end;

function TModflowStressPeriod.GetAtsFailureFactor: double;
begin
  result := StoredAtsFailureFactor.Value;
end;

function TModflowStressPeriod.GetAtsInitialStepSize: double;
begin
  result := StoredAtsInitialStepSize.Value;
end;

function TModflowStressPeriod.GetAtsMaximumStepSize: double;
begin
  result := StoredAtsMaximumStepSize.Value;
end;

function TModflowStressPeriod.GetAtsMinimumStepSize: double;
begin
  result := StoredAtsMinimumStepSize.Value;
end;

function TModflowStressPeriod.GetTimeStep(Step: Integer): TTimeStep;
var
  InitialStepSize: Double;
  StepSize: Double;
  StepIndex: Integer;
begin
  if Step < 0 then
  begin
    result.StartTime := StartTime;
    result.EndTime := StartTime;
  end
  else if Step >= NumberOfSteps then
  begin
    result.StartTime := EndTime;
    result.EndTime := EndTime;
  end
  else if Step = 0 then
  begin
    result.StartTime := StartTime;
    result.EndTime := StartTime + LengthOfFirstTimeStep;
  end
  else
  begin
    InitialStepSize := LengthOfFirstTimeStep;
    if TimeStepMultiplier = 1 then
    begin
      result.StartTime := StartTime + InitialStepSize*Step;
      result.EndTime := StartTime +  + InitialStepSize*(Step+1);
    end
    else
    begin
      StepSize := InitialStepSize;
      result.StartTime := StartTime;
      for StepIndex := 0 to Step - 1 do
      begin
        result.StartTime := result.StartTime + StepSize;
        StepSize := StepSize*TimeStepMultiplier
      end;
      StepSize := StepSize*TimeStepMultiplier;
      result.EndTime := result.StartTime +  + StepSize;
    end;
  end;
  if result.EndTime > EndTime then
  begin
    result.EndTime := EndTime;
  end;
end;

procedure TModflowStressPeriod.InvalidateModel;
begin
  (Collection as TModflowStressPeriods).InvalidateModel;
end;

procedure TModflowStressPeriod.InvalModel(Sender: TObject);
begin
  InvalidateModel;
end;

function TModflowStressPeriod.LengthOfFirstTimeStep: double;
begin
  if TimeStepMultiplier = 1 then
  begin
    result := PeriodLength/NumberOfSteps;
  end
  else
  begin
    result := PeriodLength*(TimeStepMultiplier-1)/
      (Power(TimeStepMultiplier,NumberOfSteps) - 1);
  end;
end;

function TModflowStressPeriod.NumberOfSteps: integer;
begin
  result := GetNumberOfTimeSteps(PeriodLength, MaxLengthOfFirstTimeStep,
    TimeStepMultiplier);
end;

procedure TModflowStressPeriod.SetMaxLengthOfFirstTimeStep(
  const Value: double);
begin
  if FMaxLengthOfFirstTimeStep <> Value then
  begin
    FMaxLengthOfFirstTimeStep := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetStoredAtsAdjustmentFactor(
  const Value: TRealStorage);
begin
  FStoredAtsAdjustmentFactor.Assign(Value);
end;

procedure TModflowStressPeriod.SetStoredAtsFailureFactor(
  const Value: TRealStorage);
begin
  FStoredAtsFailureFactor.Assign(Value);
end;

procedure TModflowStressPeriod.SetStoredAtsInitialStepSize(const Value: TRealStorage);
begin
  FStoredAtsInitialStepSize.Assign(Value);
end;

procedure TModflowStressPeriod.SetStoredAtsMaximumStepSize(
  const Value: TRealStorage);
begin
  FStoredAtsMaximumStepSize.Assign(Value);
end;

procedure TModflowStressPeriod.SetStoredAtsMinimumStepSize(
  const Value: TRealStorage);
begin
  FStoredAtsMinimumStepSize.Assign(Value);
end;

procedure TModflowStressPeriod.SetAtsAdjustmentFactor(const Value: double);
begin
  StoredAtsAdjustmentFactor.Value := Value;
end;

procedure TModflowStressPeriod.SetAtsFailureFactor(const Value: double);
begin
  StoredAtsFailureFactor.Value := Value;
end;

procedure TModflowStressPeriod.SetAtsInitialStepSize(const Value: double);
begin
  StoredAtsInitialStepSize.Value := Value;
end;

procedure TModflowStressPeriod.SetAtsMaximumStepSize(const Value: double);
begin
  StoredAtsMaximumStepSize.Value := Value;
end;

procedure TModflowStressPeriod.SetAtsMinimumStepSize(const Value: double);
begin
  StoredAtsMinimumStepSize.Value := Value;
end;

procedure TModflowStressPeriod.SetAtsUsed(const Value: Boolean);
begin
  if FAtsUsed <> Value then
  begin
    FAtsUsed := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetDrawDownReference(const Value: boolean);
begin
  if FDrawDownReference <> Value then
  begin
    FDrawDownReference := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetEndTime(Value: double);
begin
  // prevent rounding errors in saved files from being used.
  Value := FortranStrToFloat(FortranFloatToStr(Value));
  if FEndTime <> Value then
  begin
    FEndTime := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetPeriodLength(const Value: double);
begin
  if FPeriodLength <> Value then
  begin
    FPeriodLength := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetStartTime(Value: double);
begin
  // prevent rounding errors in saved files from being used.
  Value := FortranStrToFloat(FortranFloatToStr(Value));
  if FStartTime <> Value then
  begin
    FStartTime := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetStressPeriodType(
  const Value: TStressPeriodType);
begin
  if FStressPeriodType <> Value then
  begin
    FStressPeriodType := Value;
    InvalidateModel;
  end;
end;

procedure TModflowStressPeriod.SetTimeStepMultiplier(const Value: double);
begin
  if FTimeStepMultiplier <> Value then
  begin
    FTimeStepMultiplier := Value;
    InvalidateModel;
  end;
end;

{ TModflowStressPeriods }

procedure TModflowStressPeriods.Assign(Source: TPersistent);
var
  SourceName: string;
  SourceMFStressPeriods: TModflowStressPeriods;
  Index: Integer;
begin
  if Source is TModflowStressPeriods then
  begin
    SourceMFStressPeriods := TModflowStressPeriods(Source);
    if Count = SourceMFStressPeriods.Count then
    begin
      for Index := 0 to Count - 1 do
      begin
        Items[Index].Assign(SourceMFStressPeriods[Index]);
      end;
    end
    else
    begin
      inherited Assign(Source);
    end;
  end
  else
  begin
    if Source <> nil then
      SourceName := Source.ClassName else
      SourceName := 'nil';
    raise EConvertError.CreateResFmt(@SAssignError, [SourceName, ClassName]);
  end;
end;

function TModflowStressPeriods.CompletelyTransient: boolean;
var
  Index: Integer;
begin
  result := True;
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].StressPeriodType <> sptTransient then
    begin
      result := False;
      Exit;
    end;
  end;
end;

constructor TModflowStressPeriods.Create(InvalidateModelEvent: TNotifyEvent);
begin
  FOnInvalidateModel := InvalidateModelEvent;
  inherited Create(TModflowStressPeriod);
end;

procedure TModflowStressPeriods.FillPickListWithEndTimes(Grid: TRbwDataGrid4; Col: integer);
var
  Strings: TStrings;
begin
  Strings := Grid.Columns[Col].PickList;
  FillStringsWithEndTimes(Strings);
end;

procedure TModflowStressPeriods.FillPickListWithStartTimes(Grid: TRbwDataGrid4;
  Col: integer);
var
  Strings: TStrings;
begin
  Strings := Grid.Columns[Col].PickList;
  FillStringsWithStartTimes(Strings);
end;

function TModflowStressPeriods.GetFirst: TModflowStressPeriod;
begin
  result := Items[0];
end;

function TModflowStressPeriods.GetItems(Index: Integer): TModflowStressPeriod;
begin
  result := inherited Items[Index] as TModflowStressPeriod;
end;

function TModflowStressPeriods.GetLast: TModflowStressPeriod;
begin
  result := Items[Count-1];
end;

function TModflowStressPeriods.GetNumberOfSteps: integer;
var
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    result := result + StressPeriod.NumberOfSteps;
  end;
end;

procedure TModflowStressPeriods.InvalidateModel;
begin
  if Assigned(FOnInvalidateModel) then
  begin
    FOnInvalidateModel(self);
  end;
end;

function TModflowStressPeriods.MaxStepsInAnyStressPeriod: integer;
var
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    if result < StressPeriod.NumberOfSteps then
    begin
      result := StressPeriod.NumberOfSteps;
    end;
  end;
end;

procedure TModflowStressPeriods.SetItems(Index: Integer;
  const Value: TModflowStressPeriod);
begin
  inherited Items[Index] := Value;
end;

procedure TModflowStressPeriods.TimeToPeriodAndStep(ATime: double; out Period,
  Step: integer);
var
  PeriodIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  ElapsedTime: Double;
  StepCount: Integer;
  TimeStepLength: Double;
  StepIndex: Integer;
begin
  Period := -1;
  Step := -1;
  for PeriodIndex := 0 to Count - 1 do
  begin
    StressPeriod := Items[PeriodIndex];
    if (StressPeriod.StartTime <= ATime) and (StressPeriod.EndTime > ATime) then
    begin
      Period := PeriodIndex;
      ElapsedTime := StressPeriod.StartTime;
      StepCount := StressPeriod.NumberOfSteps;
      TimeStepLength := StressPeriod.LengthOfFirstTimeStep;
      for StepIndex := 0 to StepCount - 1 do
      begin
        ElapsedTime := ElapsedTime + TimeStepLength;
        if (ElapsedTime >= ATime) then
        begin
          Step := StepIndex;
          Exit;
        end;
        TimeStepLength := TimeStepLength*StressPeriod.TimeStepMultiplier;
      end;
      Step := StepCount-1;
      Exit;
    end;
  end;
  Period := Count - 1;
  StressPeriod := Items[Period];
  Step := StressPeriod.NumberOfSteps-1;
end;

procedure TModflowStressPeriods.FillStringsWithEndTimes(Strings: TStrings);
var
  TimeIndex: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  Strings.Clear;
  Strings.Capacity := Count;
  for TimeIndex := 0 to Count - 1 do
  begin
    StressPeriod := Items[TimeIndex];
    Strings.Add(FloatToStr(StressPeriod.EndTime));
  end;
end;

procedure TModflowStressPeriods.FillStringsWithStartTimes(Strings: TStrings);
var
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
begin
  Strings.Clear;
  Strings.Capacity := Count;
  for TimeIndex := 0 to Count - 1 do
  begin
    StressPeriod := Items[TimeIndex];
    Strings.Add(FloatToStr(StressPeriod.StartTime));
  end;
end;

function TModflowStressPeriods.FindEndStressPeriod(ATime: double): integer;
var
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  result := -1;
  StressPeriod := Last;
  if ATime >= StressPeriod.EndTime then
  begin
    result := Count-1;
    Exit;
  end;
  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    if (StressPeriod.StartTime <= ATime)
      and (StressPeriod.EndTime >= ATime) then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

function TModflowStressPeriods.FindStressPeriod(ATime: double): integer;
var
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    if (StressPeriod.StartTime <= ATime) then
    begin
      if Index = Count -1 then
      begin
        if (StressPeriod.EndTime >= ATime) then
        begin
          result := Index;
          Exit;
        end;
      end
      else
      begin
        if (StressPeriod.EndTime > ATime) then
        begin
          result := Index;
          Exit;
        end;
      end;
    end;
  end;
end;

function TModflowStressPeriods.TransientModel: boolean;
var
  Index: Integer;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].StressPeriodType = sptTransient then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TModflowStressPeriods.WriteMt3dmsStressPeriods(
  const Mt3dmsBasicWriter: TObject);
var
  Mt3dmsBtnWriter: TMt3dmsBtnWriter;
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
  Mt3dmsTime: TMt3dmsTimeItem;
  Mt3dmsAdvection: TMt3dmsAdvection;
  SSFlag: Boolean;
begin
  Mt3dmsBtnWriter := Mt3dmsBasicWriter as TMt3dmsBtnWriter;
  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    Mt3dmsBtnWriter.WriteF10Float(StressPeriod.PeriodLength);
    Mt3dmsBtnWriter.WriteI10Integer(
      StressPeriod.NumberOfSteps, 'MT3DMS Basic, NSTP');
    Mt3dmsBtnWriter.WriteF10Float(StressPeriod.TimeStepMultiplier);


    Mt3dmsAdvection :=  Mt3dmsBtnWriter.Model.ModflowPackages.Mt3dmsAdvection;
    if not Mt3dmsAdvection.IsSelected
      or (Mt3dmsAdvection.AdvectionSolution <> asStandard) then
    begin
      SSFlag := False;
    end
    else
    begin
      Mt3dmsTime := Mt3dmsBtnWriter.Model.Mt3dmsTimes.GetItemFromTime(StressPeriod.StartTime);
      if Mt3dmsTime = nil then
      begin
        frmErrorsAndWarnings.AddError(Mt3dmsBtnWriter.Model, StrTimeDataForMT3DMS,
          Format(StrNoTimeDataHasBee, [StressPeriod.Index+1]) );
        Exit;
      end;
      SSFlag :=  Mt3dmsTime.SteadyState;
    end;
    if SSFlag then
    begin
      Mt3dmsBtnWriter.WriteString(' SSTATE');
    end;

    Mt3dmsBtnWriter.WriteString(
      ' # Data Set 21: PERLEN NSTP TSMULT SSflag (Stress period '
      + IntToStr(Index+1) + ')');
    Mt3dmsBtnWriter.NewLine;
    Mt3dmsBtnWriter.WriteDataSet22(StressPeriod);
    Mt3dmsBtnWriter.WriteDataSet23(StressPeriod);
  end;
end;

function TModflowStressPeriods.FmpTimeStepSizeWarning(Model: TObject): string;
var
  LocalModel: TCustomModel;
  FarmProcess: TFarmProcess;
  TimeUnit: Integer;
  ShortStepLength: double;
  StepLength: double;
  FoundFirst: Boolean;
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
begin
  result := '';

  LocalModel := Model as TCustomModel;

  FarmProcess := LocalModel.ModflowPackages.FarmProcess;
  TimeUnit := LocalModel.ModflowOptions.TimeUnit;
  if (LocalModel.ModelSelection <> msModflowFmp)
    or not FarmProcess.IsSelected
    or (TimeUnit <> 0)
    or (FarmProcess.CropConsumptiveConcept <> cccConcept1) then
  begin
    Exit;
  end; 
  FoundFirst := False;
  ShortStepLength := 0;
  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    if StressPeriod.StressPeriodType = sptTransient then
    begin
      if not FoundFirst then
      begin
        ShortStepLength := StressPeriod.LengthOfFirstTimeStep;
        FoundFirst := True;
      end 
      else 
      begin
        StepLength := StressPeriod.LengthOfFirstTimeStep;
        if StepLength < ShortStepLength then
        begin
          ShortStepLength := StressPeriod.LengthOfFirstTimeStep;
        end;
      end;
    end;
  end;
  if FoundFirst then
  begin
    // convert to time to days
    case TimeUnit of  
      1: // seconds 
        begin
          ShortStepLength := ShortStepLength/24/3600;
        end;
      2: // minutes
        begin
          ShortStepLength := ShortStepLength/24/60;
        end;
      3: // hours
        begin
          ShortStepLength := ShortStepLength/24;
        end;
      4: //days
        begin
        end;
      5: // years
        begin
          ShortStepLength := ShortStepLength * 365.25;
        end;
      else
        begin
          Assert(False)
        end;
    end;
    if ShortStepLength < 1 then
    begin
      result := Format(StrWhenUsingTheFarm, [ShortStepLength]);
    end;
  end;
end;

procedure TModflowStressPeriods.WriteStressPeriods(
  const DiscretizationWriter: TObject);
var
  DisWriter: TModflowDiscretizationWriter;
  Index: Integer;
  StressPeriod: TModflowStressPeriod;
  LocalModel: TCustomModel;
  FormattedOutputUsed: Boolean;
  NSTP: Integer;
  Warning: string;
begin
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  LocalModel := DisWriter.Model;

  FormattedOutputUsed := LocalModel.ModflowOutputControl.FormattedOutputUsed;
  if FormattedOutputUsed and (Count >= 10000) then
  begin
    frmErrorsAndWarnings.AddWarning(LocalModel, StrTooManyStressPeri,
      Format(StrTheMaximumNumberO, [Count]));
  end;

  Warning := FmpTimeStepSizeWarning(LocalModel);
  if Warning <> '' then
  begin
    frmErrorsAndWarnings.AddWarning(LocalModel, StrTimeStepToShort,
      Warning);
  end;

  for Index := 0 to Count - 1 do
  begin
    StressPeriod := Items[Index];
    DisWriter.WriteFloat(StressPeriod.PeriodLength);
    NSTP := StressPeriod.NumberOfSteps;
    if (NSTP >= 100000) and FormattedOutputUsed then
    begin
      frmErrorsAndWarnings.AddWarning(LocalModel, StrTooManyTimeSteps,
        Format(StrInStressPeriod1, [Index+1, NSTP]));
    end;
    DisWriter.WriteInteger(NSTP);
    DisWriter.WriteFloat(StressPeriod.TimeStepMultiplier);
    case StressPeriod.StressPeriodType of
      sptSteadyState: DisWriter.WriteString(' SS');
      sptTransient: DisWriter.WriteString(' TR');
      else Assert(False);
    end;
    DisWriter.WriteString(' # PERLEN NSTP TSMULT Ss/tr (Stress period '
      + IntToStr(Index+1) + ')');
    DisWriter.NewLine;

    if (Count = 1) then
    begin
      if StressPeriod.DrawDownReference then
      begin
        frmErrorsAndWarnings.AddWarning(LocalModel,
          StrUnusualUseOfDrawd, StrTheFirstAndOnlyS);
      end;
    end;
    if StressPeriod.DrawDownReference
      and (StressPeriod.StressPeriodType = sptTransient) then
    begin
      frmErrorsAndWarnings.AddWarning(LocalModel, StrUnusualUseOfDrawd,
        Format(StrInStressPeriodD, [Index+1]));
    end;
  end;
end;

end.
