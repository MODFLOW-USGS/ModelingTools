unit Modflow6TimeSeriesCollectionsUnit;

interface

uses
  System.SysUtils, System.Classes, GoPhastTypes, OrderedCollectionUnit,
  Modflow6TimeSeriesUnit, Generics.Collections, System.IOUtils,
  System.Character;

type
  TTimeSeriesItem = class(TOrderedItem)
  private
    FTimeSeries: TMf6TimeSeries;
    procedure SetTimeSeries(const Value: TMf6TimeSeries);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeSeries: TMf6TimeSeries read FTimeSeries write SetTimeSeries;
  end;

  TTimesSeriesCollection = class(TOrderedCollection)
  private
    FTimes: TRealCollection;
    FGroupName: AnsiString;
    FInputFile: TStreamReader;
    FTimeSeriesDictionary: TDictionary<string, TMf6TimeSeries>;
    function GetItem(Index: Integer): TTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
    procedure SetTimes(const Value: TRealCollection);
    procedure SetGroupName(Value: AnsiString);
    procedure ReadAttributes;
    procedure ReadTimeSeries;
  public
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): TMf6TimeSeries;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: TTimeSeriesItem read GetItem write SetItem; default;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    function Add: TTimeSeriesItem;
    procedure ReadFromFile(const AFileName: string);
    function GetInterpolatedValue(Model: TBaseModel; Time: double; const
      SeriesName: string; StartTimeOffset: double = 0): double;
  published
    property Times: TRealCollection read FTimes write SetTimes;
    property GroupName: AnsiString read FGroupName write SetGroupName;
  end;

  TTimesSeriesGroups = TObjectList<TTimesSeriesCollection>;

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
    FTimeSeriesGroupsDictionary: TDictionary<string, TTimesSeriesCollection>;
    FTimeSeriesDictionary: TDictionary<string, TMf6TimeSeries>;
    FTimeSeriesNames: TStringList;
    function GetItem(Index: Integer): TimeSeriesCollectionItem;
    procedure SetItem(Index: Integer; const Value: TimeSeriesCollectionItem);
    function GetTimeSeriesNames: TStringList;
  public
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TimeSeriesCollectionItem read GetItem write SetItem; default;
    function Add: TimeSeriesCollectionItem;
    function GetTimeSeriesByName(ASeriesName: String): TMf6TimeSeries;
    procedure GetTimesSeriesGroups(SeriesNames: TStrings;
      Groups: TTimesSeriesGroups);
    function GetTimesSeriesCollectionBySeriesName(
      const ASeriesName: string): TTimesSeriesCollection;
    property TimeSeriesNames: TStringList read GetTimeSeriesNames;
  end;

implementation

uses
  ModelMuseUtilities, PhastModelUnit, ModflowTimeUnit, ModflowParameterUnit;

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
  FTimeSeries := TMf6TimeSeries.Create(OnInvalidateModelEvent);
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

procedure TTimeSeriesItem.SetTimeSeries(const Value: TMf6TimeSeries);
begin
  FTimeSeries.Assign(Value);
end;

{ TTimesSeriesCollection }

function TTimesSeriesCollection.Add: TTimeSeriesItem;
begin
  result := inherited Add as TTimeSeriesItem;
end;

procedure TTimesSeriesCollection.Assign(Source: TPersistent);
var
  TSGroup: TTimesSeriesCollection;
begin
  FTimeSeriesDictionary.Clear;
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
  FTimeSeriesDictionary := TDictionary<string, TMf6TimeSeries>.Create;
end;

destructor TTimesSeriesCollection.Destroy;
begin
  FTimeSeriesDictionary.Free;
  FTimes.Free;
  inherited;
end;

function TTimesSeriesCollection.GetInterpolatedValue(Model: TBaseModel;
  Time: double; const SeriesName: string; StartTimeOffset: double): double;
const
  NoValue = 3.0E30;
  Epsilon = 1E-8;
var
  LocalModel: TCustomModel;
  Period: Integer;
  Step: Integer;
  TimeStep: TTimeStep;
  TimeIndex: Integer;
  UsedTime: double;
  Series: TMf6TimeSeries;
  StartTimeIndex: Integer;
  EndTimeIndex: Integer;
  PreviousTimeIndex: Integer;
  UsedTimes: TList<Double>;
  UsedValues: TList<Double>;
  NextTimeIndex: Integer;
  FirstValue: Double;
  LastValue: Double;
  StressPeriod: TModflowStressPeriod;
  Param: TModflowSteadyParameter;
  ScaleFactor: Double;
  function NearlyTheSame(A, B: double): Boolean;
  begin
    result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
  end;
begin
  Series := GetValuesByName(SeriesName);
  Assert(Series <> nil);

  UsedTime := Time -StartTimeOffset;

  LocalModel := Model as TCustomModel;
  LocalModel.ModflowStressPeriods.TimeToPeriodAndStep(Time, Period, Step);
  StressPeriod := LocalModel.ModflowStressPeriods[Period];
  TimeStep := StressPeriod.GetTimeStep(Step);

  TimeStep.StartTime := TimeStep.StartTime-StartTimeOffset;
  TimeStep.EndTime := TimeStep.EndTime-StartTimeOffset;

  StartTimeIndex := 0;
  for TimeIndex := 0 to TimeCount - 1 do
  begin
    if (Times[TimeIndex].Value <= TimeStep.StartTime)
      and not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
    begin
      StartTimeIndex := TimeIndex;
    end;
    if (Times[TimeIndex].Value > TimeStep.StartTime) then
    begin
      break;
    end;
  end;
  EndTimeIndex := StartTimeIndex;
  for TimeIndex := StartTimeIndex to TimeCount - 1 do
  begin
    if (Times[TimeIndex].Value >= TimeStep.EndTime)
      and not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
    begin
      EndTimeIndex := TimeIndex;
      break;
    end;
  end;

  result := 0;
  case Series.InterpolationMethod of
    mimStepwise:
      begin
        for TimeIndex := StartTimeIndex to EndTimeIndex do
        begin
          if (Times[TimeIndex].Value <= UsedTime)
            and not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
          begin
            result := Series[TimeIndex].Value;
          end
          else
          begin
            break;
          end;
        end;
      end;
    mimLinear:
      begin
        UsedTimes := TList<Double>.Create;
        UsedValues := TList<Double>.Create;
        try
          NextTimeIndex := EndTimeIndex;
          for TimeIndex := StartTimeIndex +1 to EndTimeIndex do
          begin
            if not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
            begin
              NextTimeIndex := TimeIndex;
              break;
            end;
		      end;
          if NearlyTheSame(Times[StartTimeIndex].Value, UsedTime) then
          begin
            FirstValue := Series[StartTimeIndex].Value
          end
//          else if NearlyTheSame(Times[NextTimeIndex].Value, UsedTime) then
//          begin
//            FirstValue := Series[NextTimeIndex].Value
//          end
          else
          begin
            FirstValue := Interpolate(TimeStep.StartTime,
              Times[StartTimeIndex].Value, Times[NextTimeIndex].Value,
              Series[StartTimeIndex].Value, Series[NextTimeIndex].Value);
    		  end;
          UsedTimes.Add(TimeStep.StartTime);
          UsedValues.Add(FirstValue);
          for TimeIndex := StartTimeIndex + 1 to EndTimeIndex - 1 do
          begin
            if Not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
            begin
              UsedTimes.Add(Times[TimeIndex].Value);
              UsedValues.Add(Series[TimeIndex].Value);
            end;
          end;
          PreviousTimeIndex :=  StartTimeIndex;
          for TimeIndex := EndTimeIndex-1 downto StartTimeIndex do
          begin
            if not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
            begin
              PreviousTimeIndex := TimeIndex;
              break;
            end;
          end;
          if NearlyTheSame(Times[PreviousTimeIndex].Value, UsedTime) then
          begin
            LastValue := Series[PreviousTimeIndex].Value
          end
          else if NearlyTheSame(Times[EndTimeIndex].Value, UsedTime) then
          begin
            LastValue := Series[EndTimeIndex].Value
          end
          else
          begin
            LastValue := Interpolate(TimeStep.EndTime,
              Times[PreviousTimeIndex].Value, Times[EndTimeIndex].Value,
              Series[PreviousTimeIndex].Value, Series[EndTimeIndex].Value);
          end;
          UsedTimes.Add(TimeStep.EndTime);
          UsedValues.Add(LastValue);
          if (TimeStep.StartTime = TimeStep.EndTime) then
          begin
            result := UsedValues[0];
          end
          else
          begin
            result := 0;
            for TimeIndex := 1 to UsedTimes.Count - 1 do
            begin
              result := result + (UsedValues[TimeIndex-1] + UsedValues[TimeIndex])
                * (UsedTimes[TimeIndex] - UsedTimes[TimeIndex-1]);
            end;
            result := result/(UsedTimes.Last - UsedTimes.First)/2;
          end;
        finally
          UsedTimes.Free;
          UsedValues.Free;
        end;
      end;
    mimLinearEnd:
      begin
        PreviousTimeIndex :=  StartTimeIndex;
        for TimeIndex := EndTimeIndex-1 downto StartTimeIndex do
        begin
          if not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
          begin
            PreviousTimeIndex := TimeIndex;
            break;
          end;
        end;
        if NearlyTheSame(Times[PreviousTimeIndex].Value, UsedTime) then
        begin
          result := Series[PreviousTimeIndex].Value
        end
        else if NearlyTheSame(Times[EndTimeIndex].Value, UsedTime) then
        begin
          result := Series[EndTimeIndex].Value
        end
        else
        begin
          result := Interpolate(TimeStep.EndTime,
            Times[PreviousTimeIndex].Value, Times[EndTimeIndex].Value,
            Series[PreviousTimeIndex].Value, Series[EndTimeIndex].Value);
        end;
      end;
    else
      Assert(False);
  end;
  ScaleFactor := Series.ScaleFactor;
  if Series.ScaleFactorParameter <> '' then
  begin
    Param := LocalModel.GetPestParameterByName(Series.ScaleFactorParameter);
    Assert(Param <> nil);
    case Series.ParamMethod of
      ppmMultiply:
        ScaleFactor := ScaleFactor * Param.Value;
      ppmAdd:
        ScaleFactor := ScaleFactor + Param.Value;
      else
        Assert(False);
    end;
  end;
  result := result * ScaleFactor
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
  const AName: string): TMf6TimeSeries;
var
  ItemIndex: Integer;
  AnItem: TTimeSeriesItem;
  TimeSeries: TMf6TimeSeries;
begin
  result := nil;
  if (Count > 0) and (FTimeSeriesDictionary.Count = 0) then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      TimeSeries := Items[ItemIndex].TimeSeries;
      FTimeSeriesDictionary.Add(UpperCase(TimeSeries.SeriesName), TimeSeries);
    end;
  end;
  if not FTimeSeriesDictionary.TryGetValue(UpperCase(AName), result) then
  begin
    result := nil;
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

procedure TTimesSeriesCollection.ReadAttributes;
var
  ALine: string;
  Splitter: TStringList;
  NameIndex: Integer;
  MethodIndex: Integer;
  Methods: TStringList;
begin
  Methods := TStringList.Create;
  Splitter := TStringList.Create;
  try
    Methods.Add('STEPWISE');
    Methods.Add('LINEAR');
    Methods.Add('LINEAREND');

    while not FInputFile.EndOfStream do
    begin
      ALine := ExtractNonCommentLine(FInputFile.ReadLine);
      if ALine = '' then
      begin
        Continue;
      end;
      if IsEndOfSection(ALine) then
      begin
        Exit;
      end;
      Splitter.DelimitedText := ALine;
      if (Splitter[0] = 'NAME')or (Splitter[0] = 'NAMES') then
      begin
        Count := Splitter.Count - 1;
        for NameIndex := 1 to Splitter.Count - 1 do
        begin
          Items[NameIndex-1].TimeSeries.SeriesName := Splitter[NameIndex];
          Items[NameIndex-1].TimeSeries.ScaleFactor := 1;
        end;
      end
      else if (Splitter[0] = 'METHOD')or (Splitter[0] = 'METHODS') then
      begin
        Assert(Count = Splitter.Count - 1);
        for NameIndex := 1 to Splitter.Count - 1 do
        begin
          MethodIndex := Methods.IndexOf(Splitter[NameIndex]);
          if MethodIndex >= 0 then
          begin
            Items[NameIndex-1].TimeSeries.InterpolationMethod :=
              TMf6InterpolationMethods(MethodIndex);
          end;
        end;
      end
      else if (Splitter[0] = 'SFAC')or (Splitter[0] = 'SFACS') then
      begin
        Assert(Count = Splitter.Count - 1);
        for NameIndex := 1 to Splitter.Count - 1 do
        begin
          begin
            Items[NameIndex-1].TimeSeries.ScaleFactor :=
              FortranStrToFloat(Splitter[NameIndex]);
          end;
        end;
      end;
    end;
  finally
    Methods.Free;
    Splitter.Free;
  end;
end;

procedure TTimesSeriesCollection.ReadFromFile(const AFileName: string);
var
  ALine: string;
  Section: string;
begin
  FInputFile := TFile.OpenText(AFileName);
  try
    while not FInputFile.EndOfStream do
    begin
      ALine := ExtractNonCommentLine(FInputFile.ReadLine);
      if ALine = '' then
      begin
        Continue;
      end;
      if IsBeginningOfSection(ALine, Section) then
      begin
        if SameText(Section, 'ATTRIBUTES') then
        begin
          ReadAttributes
        end
        else if SameText(Section, 'TIMESERIES') then
        begin
          ReadTimeSeries;
          Exit;
        end;
      end;
    end;
  finally
    FInputFile.Free;
  end;
end;

procedure TTimesSeriesCollection.ReadTimeSeries;
var
  Splitter: TStringList;
  ALine: string;
  NameIndex: Integer;
begin
  Splitter := TStringList.Create;
  try
    while not FInputFile.EndOfStream do
    begin
      ALine := ExtractNonCommentLine(FInputFile.ReadLine);
      if ALine = '' then
      begin
        Continue;
      end;
      if IsEndOfSection(ALine) then
      begin
        Exit;
      end;
      Splitter.DelimitedText := ALine;

      Assert(Count = Splitter.Count - 1);
      TimeCount := TimeCount + 1;
      Times[TimeCount-1].Value := FortranStrToFloat(Splitter[0]);
      for NameIndex := 1 to Splitter.Count - 1 do
      begin
        begin
          Items[NameIndex-1].TimeSeries[TimeCount-1].Value :=
            FortranStrToFloat(Splitter[NameIndex]);
        end;
      end;

    end;
  finally
    Splitter.Free;
  end;
end;

procedure TTimesSeriesCollection.SetGroupName(Value: AnsiString);
const
  AllowableChars = ['a'..'z', 'A'..'Z', '0'..'9', '@', '#', '$', '%', '^', '&',
     '*', '(', ')', '_', '-', '<', '>', '?', '.'];
var
  CharIndex: Integer;
  AChar: AnsiChar;
begin
  Value := Trim(Value);
  for CharIndex := 1 to Length(Value) do
  begin
    AChar := Value[CharIndex];
    if not (AChar in AllowableChars) then
    begin
      Value[CharIndex] := '_'
    end;
  end;
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    InvalidateModel;
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

procedure TTimesSeriesCollections.Assign(Source: TPersistent);
begin
  inherited;
  FTimeSeriesGroupsDictionary.Clear;
  FTimeSeriesDictionary.Clear;
end;

constructor TTimesSeriesCollections.Create(Model: TBaseModel);
begin
  inherited Create(TimeSeriesCollectionItem, Model);
  FTimeSeriesGroupsDictionary := TDictionary<string, TTimesSeriesCollection>.Create;
  FTimeSeriesDictionary := TDictionary<string, TMf6TimeSeries>.Create;
end;

destructor TTimesSeriesCollections.Destroy;
begin
  FTimeSeriesDictionary.Free;
  FTimeSeriesGroupsDictionary.Free;
  inherited;
end;

function TTimesSeriesCollections.GetItem(
  Index: Integer): TimeSeriesCollectionItem;
begin
  result := inherited Items[Index] as TimeSeriesCollectionItem;
end;

function TTimesSeriesCollections.GetTimeSeriesByName(
  ASeriesName: String): TMf6TimeSeries;
var
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  SeriesIndex: Integer;
  ASeries: TMf6TimeSeries;
begin
  result := nil;
  if Model.ModelSelection <> msModflow2015 then
  begin
    Exit;
  end;
  if (Count > 0) and(FTimeSeriesDictionary.Count = 0) then
  begin
    for GroupIndex := 0 to Count - 1 do
    begin
      AGroup := Items[GroupIndex].TimesSeriesCollection;
      for SeriesIndex := 0 to AGroup.Count - 1 do
      begin
        ASeries := AGroup[SeriesIndex].TimeSeries;
        FTimeSeriesDictionary.Add(UpperCase(ASeries.SeriesName), ASeries);
      end;
    end;
  end;
  if not FTimeSeriesDictionary.TryGetValue(UpperCase(ASeriesName), result) then
  begin
    result := nil;
  end;

end;

function TTimesSeriesCollections.GetTimeSeriesNames: TStringList;
var
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  SeriesIndex: Integer;
  SeriesName:String;
begin
  if FTimeSeriesNames = nil then
  begin
    FTimeSeriesNames := TStringList.Create;
  end
  else
  begin
    FTimeSeriesNames.Clear
  end;
  for GroupIndex := 0 to Count - 1 do
  begin
    AGroup := Items[GroupIndex].TimesSeriesCollection;
    for SeriesIndex := 0 to AGroup.Count - 1 do
    begin
      SeriesName := AGroup[SeriesIndex].TimeSeries.SeriesName;
      FTimeSeriesNames.Add(SeriesName);
    end;
  end;
  result := FTimeSeriesNames
end;

function TTimesSeriesCollections.GetTimesSeriesCollectionBySeriesName(
  const ASeriesName: string): TTimesSeriesCollection;
var
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  SeriesIndex: Integer;
  SeriesName:String;
begin
  result := nil;
  if (Count > 0) then
  begin
    if (FTimeSeriesGroupsDictionary.Count = 0) then
    begin
      for GroupIndex := 0 to Count - 1 do
      begin
        AGroup := Items[GroupIndex].TimesSeriesCollection;
        for SeriesIndex := 0 to AGroup.Count - 1 do
        begin
          SeriesName := AGroup[SeriesIndex].TimeSeries.SeriesName;
          FTimeSeriesGroupsDictionary.Add(UpperCase(SeriesName), AGroup);
        end;
      end;
    end;
    if not FTimeSeriesGroupsDictionary.TryGetValue(UpperCase(ASeriesName), result) then
    begin
      result := nil;
    end;
  end;
end;

procedure TTimesSeriesCollections.GetTimesSeriesGroups(SeriesNames: TStrings;
  Groups: TTimesSeriesGroups);
var
  SeriesIndex: Integer;
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  LocalSeriesNames: TStringList;
  SeriesName: string;
  UsedGroup: TTimesSeriesCollection;
  TimeSeries: TMf6TimeSeries;
begin
  Groups.Clear;
  if (SeriesNames.Count = 1) and (SeriesNames[0] = '') then
  begin
    Exit;
  end;
  LocalSeriesNames := TStringList.Create;
  try
    LocalSeriesNames.AddStrings(SeriesNames);
    for GroupIndex := 0 to Count - 1 do
    begin
      AGroup := Items[GroupIndex].TimesSeriesCollection;
      UsedGroup := nil;
      for SeriesIndex := LocalSeriesNames.Count - 1 downto 0 do
      begin
        SeriesName := LocalSeriesNames[SeriesIndex];
        if SeriesName <> '' then
        begin
          TimeSeries := AGroup.GetValuesByName(SeriesName);
          if TimeSeries <> nil then
          begin
            if UsedGroup = nil then
            begin
              UsedGroup := TTimesSeriesCollection.Create(nil);
              UsedGroup.Times := AGroup.Times;
              UsedGroup.GroupName := AGroup.GroupName;
            end;
            UsedGroup.Add.TimeSeries := TimeSeries;
            LocalSeriesNames.Delete(SeriesIndex);
          end;
        end
        else
        begin
          LocalSeriesNames.Delete(SeriesIndex);
        end;
      end;
      if UsedGroup <> nil then
      begin
        Groups.Add(UsedGroup);
      end;
    end;
  finally
    LocalSeriesNames.Free;
  end;
end;

procedure TTimesSeriesCollections.SetItem(Index: Integer;
  const Value: TimeSeriesCollectionItem);
begin
  inherited Items[Index] := Value;
end;


end.
