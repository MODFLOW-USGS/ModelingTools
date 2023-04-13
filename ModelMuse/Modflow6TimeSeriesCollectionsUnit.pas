unit Modflow6TimeSeriesCollectionsUnit;

interface

uses
  System.AnsiStrings, System.SysUtils, System.Classes, GoPhastTypes,
  OrderedCollectionUnit, Modflow6TimeSeriesUnit, Generics.Collections,
  System.IOUtils, RealListUnit, Modflow6DynamicTimeSeriesUnit;

type
  TCacheDictionary<K,V> = class(TDictionary<K, V>)
  private
    FUseCachedValue: Boolean;
    FCachedKey: K;
    FCachedValue: V;
    FCachedResult: Boolean;
  public
    constructor Create;
    procedure Clear;
    function TryGetValue(const Key: K; var Value: V): Boolean;
  end;

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

  TCustomTimesSeriesCollection = class(TOrderedCollection)
  private
    FTimes: TRealCollection;
    FDeleted: Boolean;
    FSortedTimes: TRealList;
    FGroupName: AnsiString;
    procedure SetTimes(const Value: TRealCollection);
    procedure SetGroupName(Value: AnsiString);
  protected
    procedure OnTimesChanged(Sender: TObject); virtual;
  public
    Constructor Create(ItemClass: TCollectionItemClass; Model: ICustomModelInterfaceForTOrderedCollection);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
  published
    property Times: TRealCollection read FTimes write SetTimes;
    property GroupName: AnsiString read FGroupName write SetGroupName;
    property Deleted: Boolean read FDeleted write FDeleted;
  end;

  TTimesSeriesCollection = class(TCustomTimesSeriesCollection)
  private
    FInputFile: TStreamReader;
    FTimeSeriesDictionary: TCacheDictionary<string, TMf6TimeSeries>;
    function GetItem(Index: Integer): TTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
    procedure ReadAttributes;
    procedure ReadTimeSeries;
  protected
    procedure OnTimesChanged(Sender: TObject); override;
  public
    Constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
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
    procedure Loaded;
  end;

  TTimesSeriesGroups = TObjectList<TTimesSeriesCollection>;

  TDynamicTimeSeriesItem = class(TOrderedItem)
  private
    FDynamicTimeSeries: TDynamicTimeSeriesCollection;
    procedure SetTimeSeries(const Value: TDynamicTimeSeriesCollection);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property TimeSeries: TDynamicTimeSeriesCollection read FDynamicTimeSeries
      write SetTimeSeries;
  end;

  TDyanmicTimesSeriesCollection = class(TCustomTimesSeriesCollection)
  private
    FTimeSeriesDictionary: TCacheDictionary<string, TDynamicTimeSeriesCollection>;
    function GetItem(Index: Integer): TDynamicTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TDynamicTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
  public
    Constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): TDynamicTimeSeriesCollection;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: TDynamicTimeSeriesItem read GetItem write SetItem; default;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    function Add: TDynamicTimeSeriesItem;
    procedure Loaded;
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
    FTimeSeriesGroupsDictionary: TCacheDictionary<string, TTimesSeriesCollection>;
    FTimeSeriesDictionary: TCacheDictionary<string, TMf6TimeSeries>;
    FTimeSeriesNames: TStringList;
    function GetItem(Index: Integer): TimeSeriesCollectionItem;
    procedure SetItem(Index: Integer; const Value: TimeSeriesCollectionItem);
    function GetTimeSeriesNames: TStringList;
  public
    Constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
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
    procedure Loaded;
    function GetInterpolatedValue(Model: TBaseModel; Time: double; const
      SeriesName: string; StartTimeOffset: double = 0): double;
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
  if Collection <> nil then
  begin
    (Collection as TTimesSeriesCollection).OnTimesChanged(self)
  end;
end;

destructor TTimeSeriesItem.Destroy;
begin
  if Collection <> nil then
  begin
    (Collection as TTimesSeriesCollection).OnTimesChanged(self)
  end;
  FTimeSeries.Free;
  inherited;
end;

function TTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TTimeSeriesItem)
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
begin
  FTimeSeriesDictionary.Clear;
  inherited;
end;

constructor TTimesSeriesCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TTimeSeriesItem, Model);
  FTimeSeriesDictionary := TCacheDictionary<string, TMf6TimeSeries>.Create;
end;

destructor TTimesSeriesCollection.Destroy;
begin
  FTimeSeriesDictionary.Free;
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
  StartSearch: Integer;
  EndSearch: Integer;
  function NearlyTheSame(A, B: double): Boolean;
  begin
    result := Abs(A - B) / (Abs(A) + Abs(B)) < Epsilon;
  end;
begin
  result := 0;
  Series := GetValuesByName(SeriesName);
  if Series = nil then
  begin
    Exit;
  end;

  UsedTime := Time -StartTimeOffset;

  LocalModel := Model as TCustomModel;
  LocalModel.ModflowStressPeriods.TimeToPeriodAndStep(Time, Period, Step);
  if (Period < 0) or (Step < 0) then
  begin
    Exit;
  end;
  StressPeriod := LocalModel.ModflowStressPeriods[Period];
  TimeStep := StressPeriod.GetTimeStep(Step);

  TimeStep.StartTime := TimeStep.StartTime-StartTimeOffset;
  TimeStep.EndTime := TimeStep.EndTime-StartTimeOffset;

  if FSortedTimes.Count = 0 then
  begin
    for TimeIndex := 0 to TimeCount - 1 do
    begin
      FSortedTimes.Add(Times[TimeIndex].Value);
    end;
    FSortedTimes.Sorted := True;
  end;

  StartSearch := FSortedTimes.IndexOfClosest(TimeStep.StartTime);
  if StartSearch < 0 then
  begin
    StartSearch := 0;
  end
  else if StartSearch > 0 then
  begin
    Dec(StartSearch);
  end;
  EndSearch := FSortedTimes.IndexOfClosest(TimeStep.StartTime);
  if EndSearch < 0 then
  begin
    EndSearch := -1;
  end
  else if EndSearch < FSortedTimes.Count -1 then
  begin
    Inc(EndSearch);
  end;

  StartTimeIndex := 0;
  for TimeIndex := StartSearch to EndSearch do
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
  for TimeIndex := StartTimeIndex to EndSearch do
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
//  AnItem: TTimeSeriesItem;
  TimeSeries: TMf6TimeSeries;
begin
  result := nil;
  if (Count > 0) and (FTimeSeriesDictionary.Count = 0) then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      TimeSeries := Items[ItemIndex].TimeSeries;
      if TimeSeries.Deleted then
      begin
        Continue;
      end;
      FTimeSeriesDictionary.Add(UpperCase(String(TimeSeries.SeriesName)), TimeSeries);
    end;
  end;
  if not FTimeSeriesDictionary.TryGetValue(UpperCase(AName), result) then
  begin
    result := nil;
  end;
end;

function TTimesSeriesCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
//var
//  OtherCollection: TTimesSeriesCollection;
begin
  result := (AnOrderedCollection is TTimesSeriesCollection)
    and inherited IsSame(AnOrderedCollection);
//  if result then
//  begin
//    OtherCollection := TTimesSeriesCollection(AnOrderedCollection);
//    result := (GroupName = OtherCollection.GroupName)
//      and (Times.IsSame(OtherCollection.Times));
//  end;
end;

procedure TTimesSeriesCollection.Loaded;
var
  SeriesIndex: Integer;
begin
  for SeriesIndex := Count - 1 downto 0 do
  begin
    if Items[SeriesIndex].TimeSeries.Deleted then
    begin
      Items[SeriesIndex].Free;
    end;
  end;
  FTimeSeriesDictionary.Clear;
end;

procedure TTimesSeriesCollection.OnTimesChanged(Sender: TObject);
begin
  inherited;
  if FTimeSeriesDictionary <> nil then
  begin
    FTimeSeriesDictionary.Clear;
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
          Items[NameIndex-1].TimeSeries.SeriesName := AnsiString(Splitter[NameIndex]);
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

//procedure TTimesSeriesCollection.SetGroupName(Value: AnsiString);
//var
//  CharIndex: Integer;
//  AChar: Char;
//begin
//  Value := System.AnsiStrings.Trim(Value);
//  for CharIndex := 1 to Length(Value) do
//  begin
//    AChar := Char(Value[CharIndex]);
//    if (AChar = ' ') or not (TPath.IsValidFileNameChar(AChar)) then
//    begin
//      Value[CharIndex] := '_'
//    end;
//  end;
//  if Value = '' then
//  begin
//    Value := '_';
//  end;
//  if FGroupName <> Value then
//  begin
//    FGroupName := Value;
//    InvalidateModel;
//  end;
//end;

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

//procedure TTimesSeriesCollection.SetTimes(const Value: TRealCollection);
//begin
//  FTimes.Assign(Value);
//end;

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
  FTimesSeriesCollection := TTimesSeriesCollection.Create(Model as TCustomModel)
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

constructor TTimesSeriesCollections.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TimeSeriesCollectionItem, Model);
//  {$IF CompilerVersion > 28}
  FTimeSeriesGroupsDictionary := TCacheDictionary<string, TTimesSeriesCollection>.Create;
  FTimeSeriesDictionary := TCacheDictionary<string, TMf6TimeSeries>.Create;
//  {$ELSE}
//  FTimeSeriesGroupsDictionary := TDictionary<string, TTimesSeriesCollection>.Create;
//  FTimeSeriesDictionary := TDictionary<string, TMf6TimeSeries>.Create;
//  {$ENDIF}
end;

destructor TTimesSeriesCollections.Destroy;
begin
  FTimeSeriesNames.Free;
  FTimeSeriesDictionary.Free;
  FTimeSeriesGroupsDictionary.Free;
  inherited;
end;

function TTimesSeriesCollections.GetInterpolatedValue(Model: TBaseModel;
  Time: double; const SeriesName: string; StartTimeOffset: double): double;
var
  TimeSeriesCollection: TTimesSeriesCollection;
begin
  TimeSeriesCollection := GetTimesSeriesCollectionBySeriesName(SeriesName);
  if TimeSeriesCollection <> nil then
  begin
    result := TimeSeriesCollection.GetInterpolatedValue(Model, Time, SeriesName,
      StartTimeOffset);
  end
  else
  begin
    result := 0;
  end;
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
      if AGroup.Deleted then
      begin
        Continue;
      end;
      for SeriesIndex := 0 to AGroup.Count - 1 do
      begin
        ASeries := AGroup[SeriesIndex].TimeSeries;
        if ASeries.Deleted then
        begin
          Continue;
        end;
        FTimeSeriesDictionary.Add(UpperCase(string(ASeries.SeriesName)), ASeries);
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
  TimeSeries: TMf6TimeSeries;
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
    if AGroup.Deleted then
    begin
      Continue;
    end;
    for SeriesIndex := 0 to AGroup.Count - 1 do
    begin
      TimeSeries := AGroup[SeriesIndex].TimeSeries;
      if TimeSeries.Deleted then
      begin
        Continue;
      end;
      FTimeSeriesNames.Add(string(TimeSeries.SeriesName));
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
  TimeSeries: TMf6TimeSeries;
begin
  result := nil;
  if (Count > 0) then
  begin
    if (FTimeSeriesGroupsDictionary.Count = 0) then
    begin
      for GroupIndex := 0 to Count - 1 do
      begin
        AGroup := Items[GroupIndex].TimesSeriesCollection;
        if AGroup.Deleted then
        begin
          Continue;
        end;
        for SeriesIndex := 0 to AGroup.Count - 1 do
        begin
          TimeSeries := AGroup[SeriesIndex].TimeSeries;
          if TimeSeries.Deleted then
          begin
            Continue;
          end;
          FTimeSeriesGroupsDictionary.Add(UpperCase(String(TimeSeries.SeriesName)), AGroup);
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
      if AGroup.Deleted then
      begin
        Continue;
      end;
      UsedGroup := nil;
      for SeriesIndex := LocalSeriesNames.Count - 1 downto 0 do
      begin
        SeriesName := LocalSeriesNames[SeriesIndex];
        if SeriesName <> '' then
        begin
          TimeSeries := AGroup.GetValuesByName(SeriesName);
          if (TimeSeries <> nil) and not TimeSeries.Deleted then
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

procedure TTimesSeriesCollections.Loaded;
var
  Index: Integer;
begin
  for Index := Count - 1 downto 0 do
  begin
    if Items[Index].TimesSeriesCollection.Deleted then
    begin
      Items[Index].Free;
    end
    else
    begin
      Items[Index].TimesSeriesCollection.Loaded;
    end;
  end;
  FTimeSeriesDictionary.Clear;
  FTimeSeriesGroupsDictionary.Clear;
end;

procedure TTimesSeriesCollections.SetItem(Index: Integer;
  const Value: TimeSeriesCollectionItem);
begin
  inherited Items[Index] := Value;
end;


{ TCacheDictionary }

procedure TCacheDictionary<K,V>.Clear;
begin
  FUseCachedValue := False;
  FCachedResult := False;
  inherited Clear;
end;

constructor TCacheDictionary<K, V>.Create;
begin
  inherited;
  FUseCachedValue := False;
  FCachedResult := False;
end;

function TCacheDictionary<K,V>.TryGetValue(const Key: K; var Value: V): Boolean;
begin
//  {$IF CompilerVersion > 28}
  if FUseCachedValue and (Comparer.Equals(Key, FCachedKey)) then
  begin
    Value := FCachedValue;
    result := FCachedResult;
  end
  else
  begin
    result := inherited TryGetValue(Key, Value);
    FCachedKey := Key;
    FCachedValue := Value;
    FUseCachedValue := True;
    FCachedResult := result;
  end;
//  {$ELSE}
//    result := inherited TryGetValue(Key, Value);
//  {$ENDIF}
end;

{ TDynamicTimeSeriesItem }

procedure TDynamicTimeSeriesItem.Assign(Source: TPersistent);
begin
  if Source is TDynamicTimeSeriesItem then
  begin
    TimeSeries := TDynamicTimeSeriesItem(Source).TimeSeries;
  end
  else
  begin
    inherited;
  end;
end;

constructor TDynamicTimeSeriesItem.Create(Collection: TCollection);
begin
  inherited;

end;

destructor TDynamicTimeSeriesItem.Destroy;
begin

  inherited;
end;

function TDynamicTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TDynamicTimeSeriesItem)
    and (TimeSeries.IsSame(TDynamicTimeSeriesItem(AnotherItem).TimeSeries));
end;

procedure TDynamicTimeSeriesItem.SetTimeSeries(
  const Value: TDynamicTimeSeriesCollection);
begin

end;

{ TCustomTimesSeriesCollection }

procedure TCustomTimesSeriesCollection.Assign(Source: TPersistent);
var
  TSGroup: TCustomTimesSeriesCollection;
begin
  inherited;
  if Source is TCustomTimesSeriesCollection then
  begin
    TSGroup := TCustomTimesSeriesCollection(Source);
    Times := TSGroup.Times;
    GroupName := TSGroup.GroupName;
    Deleted := TSGroup.Deleted;
  end;
end;

constructor TCustomTimesSeriesCollection.Create(ItemClass: TCollectionItemClass; Model: ICustomModelInterfaceForTOrderedCollection);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  inherited Create(ItemClass, Model);
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
    FTimes := TRealCollection.Create(InvalidateModelEvent);
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
    FSortedTimes := TRealList.Create;
    FTimes := TRealCollection.Create(InvalidateModelEvent);
    FTimes.OnChange := OnTimesChanged;
  end;
//  {$IF CompilerVersion > 28}
//  FTimeSeriesDictionary := TCacheDictionary<string, TMf6TimeSeries>.Create;
end;

destructor TCustomTimesSeriesCollection.Destroy;
begin
  FTimes.Free;
  FSortedTimes.Free;
  inherited;
end;

function TCustomTimesSeriesCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
var
  OtherCollection: TCustomTimesSeriesCollection;
begin
  result := (AnOrderedCollection is TCustomTimesSeriesCollection)
    and inherited;
  if result then
  begin
    OtherCollection := TCustomTimesSeriesCollection(AnOrderedCollection);
    result := Times.IsSame(OtherCollection.Times)
      and (GroupName = OtherCollection.GroupName)
      and (Deleted = OtherCollection.Deleted)
  end;
end;

procedure TCustomTimesSeriesCollection.OnTimesChanged(Sender: TObject);
begin
  if FSortedTimes <> nil then
  begin
    FSortedTimes.Clear;
  end;
end;

procedure TCustomTimesSeriesCollection.SetGroupName(Value: AnsiString);
var
  CharIndex: Integer;
  AChar: Char;
begin
  Value := System.AnsiStrings.Trim(Value);
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
  if FGroupName <> Value then
  begin
    FGroupName := Value;
    InvalidateModel;
  end;
end;

procedure TCustomTimesSeriesCollection.SetTimes(const Value: TRealCollection);
begin
  FTimes.Assign(Value);
end;

{ TDyanmicTimesSeriesCollection }

function TDyanmicTimesSeriesCollection.Add: TDynamicTimeSeriesItem;
begin
  result := inherited Add as TDynamicTimeSeriesItem; 
end;

procedure TDyanmicTimesSeriesCollection.Assign(Source: TPersistent);
begin
  FTimeSeriesDictionary.Clear;
  inherited;
end;

constructor TDyanmicTimesSeriesCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TDynamicTimeSeriesItem, Model);
  FTimeSeriesDictionary := TCacheDictionary<string, TDynamicTimeSeriesCollection>.Create;
end;

destructor TDyanmicTimesSeriesCollection.Destroy;
begin
  FTimeSeriesDictionary.Free;
  inherited;
end;

function TDyanmicTimesSeriesCollection.GetItem(
  Index: Integer): TDynamicTimeSeriesItem;
begin
  result := inherited Items[Index] as TDynamicTimeSeriesItem;
end;

function TDyanmicTimesSeriesCollection.GetTimeCount: Integer;
begin
  result := FTimes.Count;
end;

function TDyanmicTimesSeriesCollection.GetValuesByName(
  const AName: string): TDynamicTimeSeriesCollection;
var
  ItemIndex: Integer;
  TimeSeries: TDynamicTimeSeriesCollection;
begin
  result := nil;
  if (Count > 0) and (FTimeSeriesDictionary.Count = 0) then
  begin
    for ItemIndex := 0 to Count - 1 do
    begin
      TimeSeries := Items[ItemIndex].TimeSeries;
      if TimeSeries.Deleted then
      begin
        Continue;
      end;
      FTimeSeriesDictionary.Add(UpperCase(String(TimeSeries.SeriesName)), TimeSeries);
    end;
  end;
  if not FTimeSeriesDictionary.TryGetValue(UpperCase(AName), result) then
  begin
    result := nil;
  end;
end;

function TDyanmicTimesSeriesCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
//var
//  OtherCollection: TDyanmicTimesSeriesCollection;
begin
  result := (AnOrderedCollection is TDyanmicTimesSeriesCollection)
    and inherited IsSame(AnOrderedCollection);
//  if result then
//  begin
//    OtherCollection := TDyanmicTimesSeriesCollection(AnOrderedCollection);
//    result := (GroupName = OtherCollection.GroupName)
//      and (Times.IsSame(OtherCollection.Times));
//  end;
end;

procedure TDyanmicTimesSeriesCollection.Loaded;
var
  SeriesIndex: Integer;
begin
  for SeriesIndex := Count - 1 downto 0 do
  begin
    if Items[SeriesIndex].TimeSeries.Deleted then
    begin
      Items[SeriesIndex].Free;
    end;
  end;
  FTimeSeriesDictionary.Clear;
end;

procedure TDyanmicTimesSeriesCollection.SetItem(Index: Integer;
  const Value: TDynamicTimeSeriesItem);
begin
  inherited Items[Index] := Value;
end;

procedure TDyanmicTimesSeriesCollection.SetTimeCount(const Value: Integer);
var
  ItemIndex: Integer;
  TimeIndex: Integer;
  AnItem: TDynamicTimeSeriesItem;
begin
  for ItemIndex := 0 to Count-1 do
  begin
    AnItem := Items[ItemIndex];
    AnItem.TimeSeries.Count := Value;
    for TimeIndex := FTimes.Count to Value -1 do
    begin
      AnItem.TimeSeries[TimeIndex].Value := '3.0E30';
    end;
  end;
  FTimes.Count := Value;
end;

end.
