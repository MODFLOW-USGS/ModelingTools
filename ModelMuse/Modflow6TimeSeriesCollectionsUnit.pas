unit Modflow6TimeSeriesCollectionsUnit;

interface

uses
  System.AnsiStrings, System.SysUtils, System.Classes, GoPhastTypes,
  OrderedCollectionUnit, Modflow6TimeSeriesUnit, Generics.Collections,
  System.IOUtils, RealListUnit,
  ModflowTimeInterfaceUnit, Modflow6TimeSeriesInterfaceUnit,
  Modflow6TimeSeriesCollectionsInterfaceUnit, OrderedCollectionInterfaceUnit;

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

  TTimeSeriesItem = class(TOrderedItem, ITimeSeriesItem)
  private
    FTimeSeries: TMf6TimeSeries;
    procedure SetTimeSeries(const Value: TMf6TimeSeries);
    function GetTimeSeriesI: ITimeSeries;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TimeSeriesI: ITimeSeries read GetTimeSeriesI;
  published
    property TimeSeries: TMf6TimeSeries read FTimeSeries write SetTimeSeries;
  end;

  TCustomTimesSeriesCollection = class(TOrderedCollection)
  private
    FTimes: TRealCollection;
    FDeleted: Boolean;
    FSortedTimes: TRealList;
    FGroupName: AnsiString;
  protected
    function GetTimes: TRealCollection;
    procedure SetTimes(const Value: TRealCollection);
    procedure OnTimesChanged(Sender: TObject); virtual;
    function GetGroupName: AnsiString;
    procedure SetGroupName(Value: AnsiString);
    function GetDeleted: Boolean;
    procedure SetDeleted(const Value: Boolean);
  public
    Constructor Create(ItemClass: TCollectionItemClass;
      Model: IModelForTOrderedCollection);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
  published
    property Times: TRealCollection read GetTimes write SetTimes;
    property GroupName: AnsiString read GetGroupName write SetGroupName;
    property Deleted: Boolean read GetDeleted write SetDeleted;
  end;

  // @name contains @link(TTimeSeriesItem)s.
  // Each @link(TTimeSeriesItem) has a @link(TMf6TimeSeries).
  TTimesSeriesCollection = class(TCustomTimesSeriesCollection,
    ITimesSeriesCollection)
  private
    FInputFile: TStreamReader;
    FTimeSeriesDictionary: TCacheDictionary<string, TMf6TimeSeries>;
    function GetItem(Index: Integer): TTimeSeriesItem;
    function GetTimeCount: Integer;
    procedure SetItem(Index: Integer; const Value: TTimeSeriesItem);
    procedure SetTimeCount(const Value: Integer);
    procedure ReadAttributes;
    procedure ReadTimeSeries;
    function GetItemI(Index: Integer): ITimeSeriesItem;
    procedure SetItemI(Index: Integer; const Value: ITimeSeriesItem);
  protected
    procedure OnTimesChanged(Sender: TObject); override;
  public
    Constructor Create(Model: IModelMuseModel);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetValuesByName(const AName: string): TMf6TimeSeries;
    property TimeCount: Integer read GetTimeCount write SetTimeCount;
    property Items[Index: Integer]: TTimeSeriesItem read GetItem write SetItem; default;
    property ItemsI[Index: Integer]: ITimeSeriesItem read GetItemI write SetItemI;
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    function Add: TTimeSeriesItem;
    function AddI: ITimeSeriesItem;
    procedure ReadFromFile(const AFileName: string);
    function GetInterpolatedValue(Model: IModelMuseModel; Time: double; const
      SeriesName: string; StartTimeOffset: double = 0): double;
    procedure Loaded;
  end;

  TTimesSeriesGroups = TObjectList<TTimesSeriesCollection>;

  TTimeSeriesCollectionItem = class(TOrderedItem, ITimeSeriesCollectionItem)
  private
    FTimesSeriesCollection: TTimesSeriesCollection;
    procedure SetTimesSeriesCollection(const Value: TTimesSeriesCollection);
    function GetTimesSeriesCollectionI: ITimesSeriesCollection;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    Constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property TimesSeriesCollectionI: ITimesSeriesCollection read GetTimesSeriesCollectionI;
  published
    property TimesSeriesCollection: TTimesSeriesCollection
      read FTimesSeriesCollection write SetTimesSeriesCollection;
  end;

  TTimesSeriesCollections = class(TOrderedCollection, ITimesSeriesCollections)
  private
    FDefaultGroupNameCount: integer;
    FDefaultTimeSeriesNameCount: Integer;
    FTimeSeriesGroupsDictionary: TCacheDictionary<string, TTimesSeriesCollection>;
    FTimeSeriesDictionary: TCacheDictionary<string, TMf6TimeSeries>;
    FTimeSeriesNames: TStringList;
    function GetItem(Index: Integer): TTimeSeriesCollectionItem;
    procedure SetItem(Index: Integer; const Value: TTimeSeriesCollectionItem);
    function GetTimeSeriesNames: TStringList;
  public
    Constructor Create(Model: IModelForTOrderedCollection);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TTimeSeriesCollectionItem read GetItem
      write SetItem; default;
    function Add: TTimeSeriesCollectionItem;
    function AddI: ITimeSeriesCollectionItem;
    function GetTimeSeriesByName(ASeriesName: String): TMf6TimeSeries;
    procedure GetTimesSeriesGroups(SeriesNames: TStrings;
      Groups: TTimesSeriesGroups);
    function GetTimesSeriesCollectionBySeriesName(
      const ASeriesName: string): TTimesSeriesCollection;
    property TimeSeriesNames: TStringList read GetTimeSeriesNames;
    procedure Loaded;
    function GetInterpolatedValue(Model: IModelMuseModel; Time: double; const
      SeriesName: string; StartTimeOffset: double = 0): double;
    function DefaultGroupName: AnsiString;
    function DefaultTimeSeriesName: AnsiString;
  end;

implementation

uses
  ModelMuseUtilities;

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

function TTimeSeriesItem.GetTimeSeriesI: ITimeSeries;
begin
  Result := TimeSeries;
end;

function TTimeSeriesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TTimeSeriesItem);
  if result then
  begin
    result := (TimeSeries.IsSame(TTimeSeriesItem(AnotherItem).TimeSeries));
  end;
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

function TTimesSeriesCollection.AddI: ITimeSeriesItem;
begin
  result := Add;
end;

procedure TTimesSeriesCollection.Assign(Source: TPersistent);
begin
  FTimeSeriesDictionary.Clear;
  inherited;
end;

constructor TTimesSeriesCollection.Create(Model: IModelMuseModel);
begin
  inherited Create(TTimeSeriesItem, Model as IModelForTOrderedCollection);
  FTimeSeriesDictionary := TCacheDictionary<string, TMf6TimeSeries>.Create;
end;

destructor TTimesSeriesCollection.Destroy;
begin
  FTimeSeriesDictionary.Free;
  inherited;
end;

function TTimesSeriesCollection.GetInterpolatedValue(Model: IModelMuseModel;
  Time: double; const SeriesName: string; StartTimeOffset: double): double;
const
  NoValue = 3.0E30;
  Epsilon = 1E-8;
var
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
  ScaleFactor: Double;
  StartSearch: Integer;
  EndSearch: Integer;
  LocalModel: IModelForTimesSeriesInterface;
  StartTime: double;
  EndTime: double;
  ParamValue: Double;
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

  LocalModel := Model as IModelForTimesSeriesInterface;
  if not LocalModel.TimeToTimeStepTimes(Time, StartTime, EndTime) then
  begin
    Exit;
  end;

  StartTime := StartTime-StartTimeOffset;
  EndTime := EndTime-StartTimeOffset;

  if FSortedTimes.Count = 0 then
  begin
    for TimeIndex := 0 to TimeCount - 1 do
    begin
      FSortedTimes.Add(Times[TimeIndex].Value);
    end;
    FSortedTimes.Sorted := True;
  end;

  StartSearch := FSortedTimes.IndexOfClosest(StartTime);
  if StartSearch < 0 then
  begin
    StartSearch := 0;
  end
  else if StartSearch > 0 then
  begin
    Dec(StartSearch);
  end;
  EndSearch := FSortedTimes.IndexOfClosest(StartTime);
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
    if (Times[TimeIndex].Value <= StartTime)
      and not NearlyTheSame(Series[TimeIndex].Value, NoValue) then
    begin
      StartTimeIndex := TimeIndex;
    end;
    if (Times[TimeIndex].Value > StartTime) then
    begin
      break;
    end;
  end;
  EndTimeIndex := StartTimeIndex;
  for TimeIndex := StartTimeIndex to EndSearch do
  begin
    if (Times[TimeIndex].Value >= EndTime)
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
            LastValue := Interpolate(EndTime,
              Times[PreviousTimeIndex].Value, Times[EndTimeIndex].Value,
              Series[PreviousTimeIndex].Value, Series[EndTimeIndex].Value);
          end;
          UsedTimes.Add(EndTime);
          UsedValues.Add(LastValue);
          if (StartTime = EndTime) then
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
          result := Interpolate(EndTime,
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
    if LocalModel.GetPestParameterValueByName(Series.ScaleFactorParameter, ParamValue) then
    begin
      case Series.ParamMethod of
        ppmMultiply:
          ScaleFactor := ScaleFactor * ParamValue;
        ppmAdd:
          ScaleFactor := ScaleFactor + ParamValue;
        else
          Assert(False);
      end;
      result := result * ScaleFactor
    end;
  end;
end;

function TTimesSeriesCollection.GetItem(Index: Integer): TTimeSeriesItem;
begin
  result := inherited Items[Index] as TTimeSeriesItem;
end;

function TTimesSeriesCollection.GetItemI(Index: Integer): ITimeSeriesItem;
begin
  result := Items[index]
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

procedure TTimesSeriesCollection.SetItemI(Index: Integer;
  const Value: ITimeSeriesItem);
begin
  Items[index] := Value as TTimeSeriesItem;
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

{ TimeSeriesCollectionItem }

procedure TTimeSeriesCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TTimeSeriesCollectionItem then
  begin
    TimesSeriesCollection := TTimeSeriesCollectionItem(Source).TimesSeriesCollection
  end
  else
  begin
    inherited;
  end;
end;

constructor TTimeSeriesCollectionItem.Create(Collection: TCollection);
begin
  inherited;
  FTimesSeriesCollection := TTimesSeriesCollection.Create(Model)
end;

destructor TTimeSeriesCollectionItem.Destroy;
begin
  FTimesSeriesCollection.Free;
  inherited;
end;

function TTimeSeriesCollectionItem.GetTimesSeriesCollectionI: ITimesSeriesCollection;
begin
  result := TimesSeriesCollection;
end;

function TTimeSeriesCollectionItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TTimeSeriesCollectionItem)
    and (TimesSeriesCollection.IsSame(
    TTimeSeriesCollectionItem(AnotherItem).TimesSeriesCollection));
end;

procedure TTimeSeriesCollectionItem.SetTimesSeriesCollection(
  const Value: TTimesSeriesCollection);
begin
  FTimesSeriesCollection.Assign(Value);
end;

{ TTimesSeriesCollections }

function TTimesSeriesCollections.Add: TTimeSeriesCollectionItem;
begin
  result := inherited Add as TTimeSeriesCollectionItem;
end;

function TTimesSeriesCollections.AddI: ITimeSeriesCollectionItem;
begin
  Result := Add;
end;

procedure TTimesSeriesCollections.Assign(Source: TPersistent);
begin
  inherited;
  FTimeSeriesGroupsDictionary.Clear;
  FTimeSeriesDictionary.Clear;
end;

constructor TTimesSeriesCollections.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TTimeSeriesCollectionItem, Model);
//  {$IF CompilerVersion > 28}
  FTimeSeriesGroupsDictionary := TCacheDictionary<string, TTimesSeriesCollection>.Create;
  FTimeSeriesDictionary := TCacheDictionary<string, TMf6TimeSeries>.Create;
//  {$ELSE}
//  FTimeSeriesGroupsDictionary := TDictionary<string, TTimesSeriesCollection>.Create;
//  FTimeSeriesDictionary := TDictionary<string, TMf6TimeSeries>.Create;
//  {$ENDIF}
  FDefaultGroupNameCount := 0;
  FDefaultTimeSeriesNameCount := 0;
end;

function TTimesSeriesCollections.DefaultGroupName: AnsiString;
var
  Index: Integer;
  GroupName: string;
  NumberString: string;
  Value: Integer;
begin
  if FDefaultGroupNameCount = 0 then
  begin
    for Index := 0 to Count - 1 do
    begin
      GroupName := String(Items[Index].TimesSeriesCollection.GroupName);
      if Pos('group', LowerCase(GroupName)) = 1 then
      begin
        NumberString := Copy(GroupName, 6, MAXINT);
        if TryStrToInt(NumberString, Value) then
        begin
          if Value > FDefaultGroupNameCount then
          begin
            FDefaultGroupNameCount := Value;
          end;
        end;
      end;
    end;
  end;
  Inc(FDefaultGroupNameCount);
  result := 'Group' + AnsiString(IntToStr(FDefaultGroupNameCount));
end;

function TTimesSeriesCollections.DefaultTimeSeriesName: AnsiString;
var
  TSNames: TStringList;
  Index: Integer;
  TSName: string;
  NumberString: string;
  Value: Integer;
begin
  if FDefaultTimeSeriesNameCount = 0 then
  begin
    TSNames := TStringList.Create;
    try
      TSNames.Assign(TimeSeriesNames);
      for Index := 0 to TSNames.Count - 1 do
      begin
        TSName := LowerCase(TSNames[Index]);
        if Pos('ts_', TSName) = 1 then
        begin
          NumberString := Copy(TSName, 4, MaxInt);
          if TryStrToInt(NumberString, Value) then
          begin
            if Value > FDefaultTimeSeriesNameCount then
            begin
              FDefaultTimeSeriesNameCount := Value;
            end;
          end;
        end;
      end;
    finally
      TSNames.Free;
    end;
  end;
  Inc(FDefaultTimeSeriesNameCount);

  result := 'TS_' + AnsiString(IntToStr(FDefaultTimeSeriesNameCount));
end;

destructor TTimesSeriesCollections.Destroy;
begin
  FTimeSeriesNames.Free;
  FTimeSeriesDictionary.Free;
  FTimeSeriesGroupsDictionary.Free;
  inherited;
end;

function TTimesSeriesCollections.GetInterpolatedValue(Model: IModelMuseModel;
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
  Index: Integer): TTimeSeriesCollectionItem;
begin
  result := inherited Items[Index] as TTimeSeriesCollectionItem;
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
  const Value: TTimeSeriesCollectionItem);
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

constructor TCustomTimesSeriesCollection.Create(ItemClass: TCollectionItemClass; Model: IModelForTOrderedCollection);
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

function TCustomTimesSeriesCollection.GetDeleted: Boolean;
begin
  result := FDeleted;
end;

function TCustomTimesSeriesCollection.GetGroupName: AnsiString;
begin
  Result := FGroupName;
end;

function TCustomTimesSeriesCollection.GetTimes: TRealCollection;
begin
  result := FTimes;
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

procedure TCustomTimesSeriesCollection.SetDeleted(const Value: Boolean);
begin
  FDeleted := Value;
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

end.
