unit SutraTimeScheduleWriterUnit;

interface

uses
  Windows, CustomModflowWriterUnit, PhastModelUnit, SutraTimeScheduleUnit, RealListUnit,
  Generics.Collections, SysUtils, GoPhastTypes, Classes, ScreenObjectUnit;

type
  TTimeValues = class(TObject)
  public
    Schedule: TSutraTimeSchedule;
    Used: Boolean;
    Times: TRealList;
    constructor Create;
    destructor Destroy; override;
  end;

  TTimeValuesList = TList<TTimeValues>;

  TTimeValuesDictionary = TObjectDictionary<string, TTimeValues>;

  TScreenObjectSchedule = class(TObject)
  private
    FTimes: TRealCollection;
    procedure SetTimes(const Value: TRealCollection);
  public
    Name: AnsiString;
    property Times: TRealCollection read FTimes write SetTimes;
    Constructor Create;
    destructor Destroy; override;
  end;

  TScreenObjectScheduleList = TObjectList<TScreenObjectSchedule>;

  TSutraTimeScheduleWriter = class(TCustomFileWriter)
  private
    FCustomScheduleIndex: Integer;
    FTimeOptions: TSutraTimeOptions;
    FSchedules: TSutraTimeSchedules;
    FItemDictionary: TTimeValuesDictionary;
    FCustomScheduleNames: TStringList;
    FFluidSourceTimes: TRealList;
    FUSourceTimes: TRealList;
    FSpecifiedPressureTimes: TRealList;
    FSpecifiedUTimes: TRealList;
    FGeneralFlowTimes: TRealList;
    FGeneralTransportTimes: TRealList;
    FPestObsTimes: TRealList;
    FAllTimes: TRealList;
    FCustomSchedules: TScreenObjectScheduleList;
    FFirstTimeValues: TTimeValues;
    FExtraTimesDefined: Boolean;
    FScheduleList: TTimeValuesList;
    FMaxTimes: Integer;
    FSchedulesList: TStringList;
    FBuffer: TStringBuilder;
    function CustomScheduleName(AScreenObject: TScreenObject): AnsiString;
    procedure Evaluate;
    procedure EvaluateDefinedSchedules;
    procedure EvaluateObjectSchedules;
    procedure MakeListOfUsedSchedules;
    procedure WriteLine1;
    procedure WriteSchedules;
    procedure WriteASchedule(ASchedule: TTimeValues); overload;
    procedure WriteASchedule(SCHNAM: AnsiString; Times: TRealList); overload;
    procedure WriteASchedule(SCHNAM: AnsiString; Times: TRealCollection); overload;
    procedure WriteABoundarySchedule(SCHNAM: AnsiString; Times: TRealList);
    function ReformatScheduleName(SCHNAM: AnsiString): AnsiString;
  protected
    class function Extension: string; override;
  public
    procedure NewLine; override;
    procedure WriteString(const Value: AnsiString); overload; override;
    Constructor Create(AModel: TCustomModel); reintroduce;
    destructor Destroy; override;
    procedure WriteFile(Schedules: TStringList);
  end;

function SameValues(List1, List2: TRealList): Boolean; overload;
function SameValues(List1: TRealList; List2: TRealCollection): Boolean;
  overload;

implementation

uses
  frmErrorsAndWarningsUnit, SutraBoundariesUnit, SutraBoundaryWriterUnit,
  Math, frmGoPhastUnit, SutraOptionsUnit, SutraBoundaryUnit,
  SutraGeneralBoundaryUnit, SutraGeneralFlowWriterUnit,
  SutraGeneralTransportWriterUnit, SutraGenTransBoundUnit, SutraPestObsUnit;

resourcestring
  StrTheFollowingTimeS = 'The following time schedule names are defined more' +
    ' than once. ';
  StrTheFollowingObject = 'The following objects define observation times th' +
  'at are different from those in the selected time schedule.';

function SameValues(List1, List2: TRealList): Boolean;
var
  index: Integer;
begin
  Assert(List1 <> nil);
  Assert(List2 <> nil);
  result := List1.Count = List2.Count;
  if result then
  begin
    for index := 0 to List1.Count - 1 do
    begin
      result := List1[index] = List2[index];
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

function SameValues(List1: TRealList; List2: TRealCollection): Boolean;
var
  index: Integer;
begin
  Assert(List1 <> nil);
  Assert(List2 <> nil);
  result := List1.Count = List2.Count;
  if result then
  begin
    for index := 0 to List1.Count - 1 do
    begin
      result := List1[index] = List2[index].Value;
      if not result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ TSutraTimeScheduleWriter }

constructor TSutraTimeScheduleWriter.Create(AModel: TCustomModel);
begin
  inherited Create(AModel, etExport);
  FTimeOptions := (AModel as TPhastModel).SutraTimeOptions;
  FSchedules := FTimeOptions.Schedules;
  FBuffer := TStringBuilder.Create;
end;

function TSutraTimeScheduleWriter.CustomScheduleName(AScreenObject: TScreenObject): AnsiString;
var
  StrResult: string;
begin
  // maximum length of a schedule name is 10 characters.
  StrResult := Copy(string(AnsiString(AScreenObject.Name)), 1, 10);
  while FItemDictionary.ContainsKey(StrResult) or
    (FCustomScheduleNames.IndexOf(StrResult) >= 0) do
  begin
    Inc(FCustomScheduleIndex);
    StrResult := 'SCHED_' + IntToStr(FCustomScheduleIndex);
  end;
  FCustomScheduleNames.Add(StrResult);
  result := AnsiString(StrResult);
end;

destructor TSutraTimeScheduleWriter.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TSutraTimeScheduleWriter.WriteASchedule(SCHNAM: AnsiString;
  Times: TRealList);
var
  SCHTYP: AnsiString;
  CREFT: AnsiString;
  SCALT: double;
  NTLIST: Integer;
  TimeIndex: Integer;
begin
  WriteString(SCHNAM);

  SCHTYP := ' ''TIME LIST''';
  CREFT  := ' ''ABSOLUTE''';
  SCALT := 1.0;
  NTLIST := Times.Count;


  WriteString(SCHTYP);
  WriteString(CREFT);
  WriteFloat(SCALT);
  WriteInteger(NTLIST);

  for TimeIndex := 0 to Times.Count - 1 do
  begin
    WriteFloat(Times[TimeIndex]);
    if ((TimeIndex + 1) mod 10) = 0 then
    begin
      NewLine;
    end;
  end;
  if (Times.Count mod 10) <> 0 then
  begin
    NewLine;
  end;

end;

procedure TSutraTimeScheduleWriter.WriteFile(Schedules: TStringList);
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidBoundaryTim);
  FSchedulesList := Schedules;
  FFluidSourceTimes := TRealList.Create;
  FUSourceTimes := TRealList.Create;
  FSpecifiedPressureTimes := TRealList.Create;
  FSpecifiedUTimes := TRealList.Create;
  FGeneralFlowTimes := TRealList.Create;
  FGeneralTransportTimes := TRealList.Create;
  FPestObsTimes := TRealList.Create;

  FCustomScheduleNames := TStringList.Create;
  FAllTimes := TRealList.Create;
  FCustomSchedules := TScreenObjectScheduleList.Create;
  FItemDictionary := TTimeValuesDictionary.Create([doOwnsValues]);
  FScheduleList := TTimeValuesList.Create;
  try
    FUSourceTimes.Sorted := True;
    FSpecifiedPressureTimes.Sorted := True;
    FSpecifiedUTimes.Sorted := True;
    FGeneralFlowTimes.Sorted := True;
    FGeneralTransportTimes.Sorted := True;
    FPestObsTimes.Sorted := True;

    Evaluate;

    try
      WriteLine1;
      WriteSchedules;
    finally
      if FBuffer.Length > 0 then
      begin
        NewLine;
      end;
    end;

  finally
    FPestObsTimes.Free;
    FScheduleList.Free;
    FAllTimes.Free;
    FItemDictionary.Free;
    FCustomSchedules.Free;
    FCustomScheduleNames.Free;

    FFluidSourceTimes.Free;
    FUSourceTimes.Free;
    FSpecifiedPressureTimes.Free;
    FSpecifiedUTimes.Free;
    FGeneralFlowTimes.Free;
    FGeneralTransportTimes.Free;

  end;
end;

function TSutraTimeScheduleWriter.ReformatScheduleName(SCHNAM: AnsiString):
  AnsiString;
var
  NameLength: Integer;
begin
  NameLength := Length(SCHNAM);
  SCHNAM := '''' + SCHNAM + '''';
  if NameLength < 10 then
  begin
    SCHNAM := SCHNAM + AnsiString(StringOfChar(' ', 10 - NameLength));
  end;
  result := SCHNAM;
end;

procedure TSutraTimeScheduleWriter.WriteLine1;
var
  NSCH: integer;
  TimeOptions: TSutraTimeOptions;
  NPCYC: Integer;
  NUCYC: Integer;
begin
  NSCH := FScheduleList.Count + FCustomSchedules.Count;
  if FFluidSourceTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if FUSourceTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if FSpecifiedPressureTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if FSpecifiedUTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if FGeneralFlowTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;
  if FGeneralTransportTimes.Count > 0 then
  begin
    Inc(NSCH);
  end;

  TimeOptions := (Model as TPhastModel).SutraTimeOptions;

  NPCYC := TimeOptions.HydraulicSolutionCycleSteps;
  NUCYC := TimeOptions.TransportSolutionCycleSteps;

  WriteInteger(NSCH);
  WriteInteger(NPCYC);
  WriteInteger(NUCYC);
  NewLine;
end;

procedure TSutraTimeScheduleWriter.WriteASchedule(ASchedule: TTimeValues);
var
  SCHNAM: AnsiString;
  SCHTYP: AnsiString;
  CREFT: AnsiString;
//  NameLength: Integer;
  SCALT: double;
  NTLIST: Integer;
  TimeIndex: Integer;
  NTMAX: Integer;
  TIMEI: Double;
  TIMEL: Double;
  NTCYC: Integer;
  TIMEC: Double;
  NSLIST: Integer;
  NSMAX: Integer;
  ISTEPI: Integer;
  ISTEPL: Integer;
  ISTEPC: Integer;
  TCMULT: Double;
  TCMIN: Double;
  TCMAX: Double;
begin
  SCHNAM := ReformatScheduleName(ASchedule.Schedule.Name);

  if FExtraTimesDefined then
  begin
    WriteASchedule(SCHNAM, ASchedule.Times);
  end
  else
  begin
    case ASchedule.Schedule.ScheduleType of
      stTimeList:
        begin
          SCHTYP := ' ''TIME LIST''';
          case ASchedule.Schedule.SutraTimeChoice of
            stcAbsolute: CREFT  := ' ''ABSOLUTE''';
            stcElapsed: CREFT  := ' ''ELAPSED''';
            else
              Assert(False);
          end;
          SCALT := ASchedule.Schedule.ScaleFactor;
          NTLIST := ASchedule.Schedule.Times.Count;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteString(CREFT);
          WriteFloat(SCALT);
          WriteInteger(NTLIST);

          for TimeIndex := 0 to ASchedule.Schedule.Times.Count - 1 do
          begin
            WriteFloat(ASchedule.Schedule.Times[TimeIndex].Value);
            if ((TimeIndex + 1) mod 10) = 0 then
            begin
              NewLine;
            end;
          end;
          if (ASchedule.Schedule.Times.Count mod 10) <> 0 then
          begin
            NewLine;
          end;
        end;
      stTimeCycle:
        begin
          SCHTYP := ' ''TIME CYCLE''';
          TIMEI := -MAXINT;
          CREFT := '';
          case ASchedule.Schedule.SutraTimeChoice of
            stcAbsolute:
              begin
                CREFT := ' ''ABSOLUTE''';
                TIMEI := ASchedule.Schedule.InitialTime;
              end;

            stcElapsed:
              begin
                CREFT := ' ''ELAPSED''';
                TIMEI := 0
              end;
            else
              Assert(False);
          end;
          SCALT := ASchedule.Schedule.ScaleFactor;
          NTMAX := ASchedule.Schedule.MaxTimeCycle;
          TIMEL := ASchedule.Schedule.LimitingTime;
          TIMEC := ASchedule.Schedule.InitialTimeIncrement;
          NTCYC := ASchedule.Schedule.IncrementUpdateCount;
          TCMULT := ASchedule.Schedule.TimeMultiplier;
          TCMIN := ASchedule.Schedule.MinIncrement;
          TCMAX := ASchedule.Schedule.MaxIncrement;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteString(CREFT);
          WriteFloat(SCALT);
          WriteInteger(NTMAX);
          WriteFloat(TIMEI);
          WriteFloat(TIMEL);
          WriteFloat(TIMEC);
          WriteInteger(NTCYC);
          WriteFloat(TCMULT);
          WriteFloat(TCMIN);
          WriteFloat(TCMAX);
          NewLine;
        end;
      stStepList:
        begin
          SCHTYP := ' ''STEP LIST''';

          NSLIST := ASchedule.Schedule.Steps.Count;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteInteger(NSLIST);
          for TimeIndex := 0 to ASchedule.Schedule.Steps.Count - 1 do
          begin
            WriteInteger(ASchedule.Schedule.Steps[TimeIndex].Value);
            if ((TimeIndex + 1) mod 10) = 0 then
            begin
              NewLine;
            end;
          end;
          if (ASchedule.Schedule.Steps.Count mod 10) <> 0 then
          begin
            NewLine;
          end;
        end;
      stStepCycle:
        begin
          SCHTYP := ' ''STEP CYCLE''';
          NSMAX := Min(ASchedule.Schedule.MaxSteps, FMaxTimes);
          ISTEPI := ASchedule.Schedule.InitialTimeStep;
          ISTEPL := ASchedule.Schedule.LimitingTimeStep;
          ISTEPC := ASchedule.Schedule.TimeStepIncrement;

          WriteString(SCHNAM);
          WriteString(SCHTYP);
          WriteInteger(NSMAX);
          WriteInteger(ISTEPI);
          WriteInteger(ISTEPL);
          WriteInteger(ISTEPC);
          NewLine;
        end;
      else
        Assert(False);
    end;
  end;

end;

procedure TSutraTimeScheduleWriter.WriteSchedules;
var
  ScheduleIndex: Integer;
  ASchedule: TTimeValues;
  ScheduleCount: integer;
  ACustomSchedule: TScreenObjectSchedule;
  FirstSchedule: TSutraTimeSchedule;
  TimeValues: TOneDRealArray;
begin
  ScheduleCount := 0;
  if (FScheduleList.Count > 0) and (not FExtraTimesDefined) then
  begin
    FirstSchedule := FSchedules[0].Schedule;

    TimeValues := FirstSchedule.TimeValues(FTimeOptions.InitialTime,
      FSchedules);

    FMaxTimes := Length(TimeValues);
  end;
  for ScheduleIndex := 0 to FScheduleList.Count - 1 do
  begin
    ASchedule := FScheduleList[ScheduleIndex];
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));

    if FExtraTimesDefined and (ScheduleIndex = 0) then
    begin
      WriteASchedule(ReformatScheduleName(ASchedule.Schedule.Name), FAllTimes);
    end
    else
    begin
      WriteASchedule(ASchedule);
    end;

  end;

  if FFluidSourceTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteABoundarySchedule(ReformatScheduleName(KFluidFlux), FFluidSourceTimes);
  end;

  if FUSourceTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteABoundarySchedule(ReformatScheduleName(KUFlux), FUSourceTimes);
  end;

  if FSpecifiedPressureTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteABoundarySchedule(ReformatScheduleName(KSpecifiedP), FSpecifiedPressureTimes);
  end;

  if FSpecifiedUTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteABoundarySchedule(ReformatScheduleName(KSpecifiedU), FSpecifiedUTimes);
  end;

  if FGeneralFlowTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteABoundarySchedule(ReformatScheduleName(KGeneralizedFlow), FGeneralFlowTimes);
  end;

  if FGeneralTransportTimes.Count > 0 then
  begin
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteABoundarySchedule(ReformatScheduleName(KGeneralizedTransport), FGeneralTransportTimes);
  end;

  for ScheduleIndex := 0 to FCustomSchedules.Count - 1 do
  begin
    ACustomSchedule := FCustomSchedules[ScheduleIndex];
    Inc(ScheduleCount);
    WriteCommentLine('Schedule ' + IntToStr(ScheduleCount));
    WriteASchedule(ReformatScheduleName(ACustomSchedule.Name),
      ACustomSchedule.Times);
  end;

  WriteString('-');
  NewLine;
end;

procedure TSutraTimeScheduleWriter.WriteString(const Value: AnsiString);
begin
//  inherited;
  FBuffer.Append(Value);
end;

procedure TSutraTimeScheduleWriter.MakeListOfUsedSchedules;
var
  ScheduleArray: TArray<TPair<string, TTimeValues>>;
  index: Integer;
  AValue: TTimeValues;
begin
  ScheduleArray := FItemDictionary.ToArray;
  FScheduleList.Add(FFirstTimeValues);
  for index := 0 to Length(ScheduleArray) - 1 do
  begin
    AValue := ScheduleArray[index].Value;
    if AValue.Used and (AValue <> FFirstTimeValues) then
    begin
      FScheduleList.Add(AValue);
    end;
  end;
end;

procedure TSutraTimeScheduleWriter.NewLine;
begin
//  inherited;
  FSchedulesList.Add(FBuffer.ToString);
  FBuffer.Clear;
end;

procedure TSutraTimeScheduleWriter.EvaluateObjectSchedules;
var
  TimeIndex: Integer;
  ScreenObjectIndex: Integer;
  UseScheduleTimes: Boolean;
  TimeValues: TTimeValues;
  Boundaries: TSutraBoundaries;
  AScreenObject: TScreenObject;
  AName: string;
  CustomSchedule: TScreenObjectSchedule;
  SutraTimeOptions: TSutraTimeOptions;
  AllTimes: TRealList;
  Initialtime: double;
  SimulationType: TSimulationType;
  ATime: Double;
  ObsTimeIndex: Integer;
  SutraStateObs: TSutraStateObservations;
  procedure DeleteDuplicateTimes(AList: TRealList);
  var
    TimeIndex: integer;
    ATime1: single;
    ATime2: single;
  begin
    for TimeIndex := AList.Count - 1 downto 1 do
    begin
      ATime1 := AList[TimeIndex-1];
      ATime2 := AList[TimeIndex];
      if ATime1 = ATime2 then
      begin
        AList.Delete(TimeIndex);
      end;
    end;
  end;
begin
  Initialtime := (Model as TPhastModel).SutraTimeOptions.InitialTime;
  SimulationType := Model.SutraOptions.SimulationType;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrTheFollowingObject);
  SutraTimeOptions := frmGoPhast.PhastModel.SutraTimeOptions;
  SutraTimeOptions.CalculateAllTimes;
  AllTimes := SutraTimeOptions.AllTimes;
  FCustomScheduleIndex := 0;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundaries := AScreenObject.SutraBoundaries;
//    UseScheduleTimes := False;
    CustomSchedule := nil;
    if Boundaries.Observations.Used then
    begin
      AName := UpperCase(string(Boundaries.Observations.ScheduleName));
      if (AName <> '') and FItemDictionary.ContainsKey(AName) then
      begin
        TimeValues := FItemDictionary.Items[AName];
        UseScheduleTimes := SameValues(TimeValues.Times,
          Boundaries.Observations.Times);
        TimeValues.Used := TimeValues.Used or UseScheduleTimes;
        // set default for the ExportScheduleName
        Boundaries.Observations.ExportScheduleName :=
          Boundaries.Observations.ScheduleName;
        if not UseScheduleTimes then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrTheFollowingObject,
            AScreenObject.Name, AScreenObject);
        end;
      end
      else
      begin
        UseScheduleTimes := False;
      end;
      if not UseScheduleTimes then
      begin
        Boundaries.Observations.ExportScheduleName := CustomScheduleName(AScreenObject);
        CustomSchedule := TScreenObjectSchedule.Create;
        FCustomSchedules.Add(CustomSchedule);
        CustomSchedule.Name := Boundaries.Observations.ExportScheduleName;
        CustomSchedule.Times := Boundaries.Observations.Times;
      end;
    end;

    if Model.PestUsed and Boundaries.SutraStateObs.Used then
    begin
      SutraStateObs := Boundaries.SutraStateObs;
      if CustomSchedule = nil then
      begin
        CustomSchedule := TScreenObjectSchedule.Create;
        FCustomSchedules.Add(CustomSchedule);
        CustomSchedule.Name := CustomScheduleName(AScreenObject);
      end;
      SutraStateObs.ScheduleName := CustomSchedule.Name;
      CustomSchedule.Times.Capacity := CustomSchedule.Times.Count + SutraStateObs.Count;
      for ObsTimeIndex := 0 to SutraStateObs.Count - 1 do
      begin
        CustomSchedule.Times.Add.Value := SutraStateObs[ObsTimeIndex].Time;
      end;
      CustomSchedule.Times.Sort;
    end;

    // The boundary conditions each use a separate schedule.
    // See SutraBoundaryWriterUnit.pas.
    if Boundaries.FluidSource.Used then
    begin
      for TimeIndex := 0 to Boundaries.FluidSource.Values.Count - 1 do
      begin
        ATime := FixTime( Boundaries.FluidSource.Values
          [TimeIndex] as TCustomSutraBoundaryItem, AllTimes);
        if (ATime <= Initialtime)
          and (SimulationType <> stSteadyFlowSteadyTransport) then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrInvalidBoundaryTim,
            Format(StrInSTheFirstSpe, [StrFluidSourcesAndSi]),
            AScreenObject
            );
        end;
        FFluidSourceTimes.AddUnique(ATime);
      end;
    end;
    if Boundaries.MassEnergySource.Used then
    begin
      for TimeIndex := 0 to Boundaries.MassEnergySource.Values.Count - 1 do
      begin
        FUSourceTimes.AddUnique(FixTime(
          Boundaries.MassEnergySource.Values[TimeIndex] as TCustomSutraBoundaryItem, AllTimes));
      end;
    end;
    if Boundaries.SpecifiedPressure.Used then
    begin
      for TimeIndex := 0 to Boundaries.SpecifiedPressure.Values.Count - 1 do
      begin
        ATime := FixTime(Boundaries.SpecifiedPressure.Values
          [TimeIndex] as TCustomSutraBoundaryItem, AllTimes);
        if (ATime <= Initialtime)
          and (SimulationType <> stSteadyFlowSteadyTransport) then
        begin
          frmErrorsAndWarnings.AddError(Model,
            StrInvalidBoundaryTim,
            Format(StrInSTheFirstSpe, [StrMassOrEnergySourc]),
            AScreenObject
            );
        end;
        FSpecifiedPressureTimes.AddUnique(ATime);
      end;
    end;
    if Boundaries.SpecifiedConcTemp.Used then
    begin
      for TimeIndex := 0 to Boundaries.SpecifiedConcTemp.Values.Count - 1 do
      begin
        FSpecifiedUTimes.AddUnique(FixTime(
          Boundaries.SpecifiedConcTemp.Values[TimeIndex] as TCustomSutraBoundaryItem, AllTimes));
      end;
    end;
    if Boundaries.GeneralFlowBoundary.Used then
    begin
      for TimeIndex := 0 to Boundaries.GeneralFlowBoundary.Values.Count - 1 do
      begin
        FGeneralFlowTimes.AddUnique(FixTime(
          Boundaries.GeneralFlowBoundary.Values[TimeIndex] as TSutraGeneralFlowItem, AllTimes));
      end;
    end;
    if Boundaries.GenTransportBoundary.Used then
    begin
      for TimeIndex := 0 to Boundaries.GenTransportBoundary.Values.Count - 1 do
      begin
        FGeneralTransportTimes.AddUnique(FixTime(
          Boundaries.GenTransportBoundary.Values[TimeIndex] as TSutraGenTransportItem, AllTimes));
      end;
    end;
  end;

  DeleteDuplicateTimes(FFluidSourceTimes);
  DeleteDuplicateTimes(FUSourceTimes);
  DeleteDuplicateTimes(FSpecifiedPressureTimes);
  DeleteDuplicateTimes(FSpecifiedUTimes);
  DeleteDuplicateTimes(FGeneralFlowTimes);
  DeleteDuplicateTimes(FGeneralTransportTimes);

end;

class function TSutraTimeScheduleWriter.Extension: string;
begin
  Assert(False);
end;

procedure TSutraTimeScheduleWriter.EvaluateDefinedSchedules;
var
  ScheduleIndex: Integer;
  TimeValues: TTimeValues;
  TimeIndex: Integer;
  Times: TOneDRealArray;
  AName: string;
  ASchedule: TSutraTimeSchedule;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrTheFollowingTimeS);
  FFirstTimeValues := nil;
  // Compute all the times for each time schedule and store them
  // in a TimeValues.Times for each schedule.
  for ScheduleIndex := 0 to FSchedules.Count - 1 do
  begin
    ASchedule := FSchedules[ScheduleIndex].Schedule;
    Times := ASchedule.TimeValues(FTimeOptions.InitialTime, FSchedules);
    TimeValues := TTimeValues.Create;
    if ScheduleIndex = 0 then
    begin
      FFirstTimeValues := TimeValues;
    end;
    AName := UpperCase(string(ASchedule.Name));
    if FItemDictionary.ContainsKey(AName) then
    begin
      Beep;
      frmErrorsAndWarnings.AddError(Model, StrTheFollowingTimeS,
        string(ASchedule.Name));
      Continue;
    end
    else
    begin
      FItemDictionary.Add(AName, TimeValues);
    end;
    TimeValues.Schedule := ASchedule;
    for TimeIndex := 0 to Length(Times) - 1 do
    begin
      TimeValues.Times.Add(Times[TimeIndex]);
    end;
  end;
end;

procedure TSutraTimeScheduleWriter.Evaluate;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    FCustomScheduleNames.CaseSensitive := False;
    FCustomScheduleNames.Sorted := True;
    FTimeOptions.CalculateAllTimes;
    FAllTimes.Assign(FTimeOptions.AllTimes);

    EvaluateDefinedSchedules;

    // Determine if the screen objects defined any other times not
    // listed in the main schedule.
    FExtraTimesDefined := not SameValues(FAllTimes, FFirstTimeValues.Times);
    FFirstTimeValues.Used := FExtraTimesDefined;

    EvaluateObjectSchedules;
    MakeListOfUsedSchedules;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TSutraTimeScheduleWriter.WriteABoundarySchedule(SCHNAM: AnsiString;
  Times: TRealList);
var
  SCHTYP: AnsiString;
  NSLIST: Integer;
  TimeIndex: Integer;
  ISLIST: Integer;
  ATime1: Single;
  ATime2: Single;
  LastTime: double;
  LastScheduleTime: double;
begin
  WriteString(SCHNAM);

  SCHTYP := ' ''STEP LIST''';

  LastTime := FAllTimes.Last;
  while (Times.Count > 0) do
  begin
    LastScheduleTime := Times.Last;
    if (LastScheduleTime - LastTime) > Abs(LastTime)/1e6 then
    begin
      Times.Delete(Times.Count -1);
    end
    else
    begin
      break;
    end;
  end;


  NSLIST := Times.Count;

  WriteString(SCHTYP);
  WriteInteger(NSLIST);
  for TimeIndex := 0 to Times.Count - 1 do
  begin
    ISLIST := FAllTimes.IndexOf(Times[TimeIndex]);
    if ISLIST < 0 then
    begin
      ISLIST := FAllTimes.IndexOfClosest(Times[TimeIndex]);
      Assert(ISLIST >= 0);
      // Reduce precision to single precision.
      ATime1 := Times[TimeIndex];
      ATime2 := FAllTimes[ISLIST];
      Assert(ATime1 = ATime2);
    end;
    Inc(ISLIST);
    WriteInteger(ISLIST);
    if (TimeIndex+1) mod 10 = 0 then
    begin
      NewLine;
    end;
  end;
  if Times.Count mod 10 <> 0 then
  begin
    NewLine;
  end;

end;

procedure TSutraTimeScheduleWriter.WriteASchedule(SCHNAM: AnsiString;
  Times: TRealCollection);
var
  TimeList: TRealList;
  TimeIndex: Integer;
begin
  TimeList := TRealList.Create;
  try
    TimeList.Capacity := Times.Count;
    for TimeIndex := 0 to Times.Count - 1 do
    begin
      TimeList.Add(Times[TimeIndex].Value)
    end;
    WriteASchedule(SCHNAM, TimeList);
  finally
    TimeList.Free;
  end;
end;

{ TTimeValues }

constructor TTimeValues.Create;
begin
  Times := TRealList.Create;
  Times.Sorted := True;
  Used := False;
end;

destructor TTimeValues.Destroy;
begin
  Times.Free;
  inherited;
end;

{ TScreenObjectSchedule }

constructor TScreenObjectSchedule.Create;
var
  InvalidateModelEvent: TNotifyEvent;
begin
  InvalidateModelEvent := nil;
  FTimes := TRealCollection.Create(InvalidateModelEvent);
end;

destructor TScreenObjectSchedule.Destroy;
begin
  FTimes.Free;
  inherited;
end;

procedure TScreenObjectSchedule.SetTimes(const Value: TRealCollection);
begin
  FTimes.Assign(Value);
end;

end.
