unit ModflowSwiObsUnit;

interface

uses
  ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit,
  System.Classes, ModflowCellUnit, System.ZLib, DataSetUnit, System.SysUtils,
  System.Generics.Collections;

type
  TSwiRecord = record
    Cell: TCellLocation;
    Zeta: double;
    Time: double;
    ZetaAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;


  TSwiObsItem = class(TCustomLocationObservation)
  private
    FName: string;
    FObservedValue: double;
    FStatistic: double;
    FStatFlag: TStatFlag;
    procedure SetObservedValue(const Value: double);
    procedure SetName(const Value: string);
    procedure SetStatFlag(const Value: TStatFlag);
    procedure SetStatistic(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
  published
    // @name is the observed head
    // of this zeta observation.
    property Name: string read FName write SetName;
    property ObservedValue: double read FObservedValue write SetObservedValue;
    property Statistic: double read FStatistic write SetStatistic;
    property StatFlag: TStatFlag read FStatFlag write SetStatFlag;
  end;

  TSwiObservationTimeList = class;

  TSwiObsTimesModelLink = class(TObject)
  private
    FObsTimes: TSwiObservationTimeList;
    FModel: TBaseModel;
  public
    Constructor Create(AModel: TBaseModel);
    Destructor Destroy; override;
  end;

  TSwiObsTimesModelLinkList = class(TObject)
  private
    FList: TObjectList<TSwiObsTimesModelLink>;
    function GetLink(AModel: TBaseModel): TSwiObsTimesModelLink;
  public
    property Links[AModel: TBaseModel]: TSwiObsTimesModelLink read GetLink;
    Constructor Create;
    Destructor Destroy; override;
    procedure RemoveLink(AModel: TBaseModel);
  end;

  TSwiObsBoundary = class;

  // @name represents MODFLOW Zeta observations
  // for a series of times.
  TSwiObsCollection = class(TCustomObjectOrderedCollection)
  private
    FBoundary: TSwiObsBoundary;
    FSwiObservations: TSwiObservationTimeList;
    FObsTimesModelLinkList: TSwiObsTimesModelLinkList;
    FObservationRowOffset: double;
    FObservationColumnOffset: double;
//    FScreenObject: TObject;
    function GetSwiItems(Index: integer): TSwiObsItem;
    function GetSwiObservations(AModel: TBaseModel): TSwiObservationTimeList;
  protected
    procedure InvalidateModel; override;
  public
    procedure RemoveModelLink(AModel: TBaseModel);
//    property ScreenObject: TObject read FScreenObject;
    // @name creates an instance of @classname
    constructor Create(Boundary: TSwiObsBoundary; Model: TBaseModel;
      ScreenObject: TObject);
    procedure EvaluateZetaObservations(AModel: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    property SwiItems[Index: integer]: TSwiObsItem read GetSwiItems;
    // ROFF
    property ObservationRowOffset: double read FObservationRowOffset;
    // COFF
    property ObservationColumnOffset: double read FObservationColumnOffset;
    property SwiObservations[AModel: TBaseModel]: TSwiObservationTimeList
      read GetSwiObservations;
    function CountObservationTimes(StartTime, EndTime: double): integer;
  end;

  TSwi_Cell = class(TValueCell)
  private
    Values: TSwiRecord;
    function GetZeta: double;
    function GetTime: double;
    function GetZetaAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property Zeta: double read GetZeta;
    property Time: double read GetTime;
    property ZetaAnnotation: string read GetZetaAnnotation;
  end;

  TSwiObsCellList = TObjectList<TSwi_Cell>;

  TSwiObsBoundary = class(TCustomLocationObsBoundary)
  private
    FValues: TSwiObsCollection;
    FZetaSurfaceNumber: integer;
    procedure SetValues(const Value: TSwiObsCollection);
    function GetCellList(Index: integer): TSwiObsCellList;
    function GetCellListCount: integer;
    procedure SetZetaSurfaceNumber(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name checks that the Purpose parameter matches @link(Purpose)
    // and, if so, calls @link(TSwiObsCollection.EvaluateSwiObservations
    // Values.EvaluateSwiObservations)
    procedure EvaluateSwiObservations(Purpose: TObservationPurpose;
      AModel: TBaseModel);
    function Used: boolean; override;
    procedure Clear; virtual;
    property CellLists[Index: integer]: TSwiObsCellList read GetCellList;
    property CellListCount: integer read GetCellListCount;
    procedure RemoveModelLink(AModel: TBaseModel);
  published
    // @name stores the MODFLOW boundaries that are NOT
    // associated with parameters.
    property Values: TSwiObsCollection read FValues write SetValues;
    //
    property ZetaSurfaceNumber: integer read FZetaSurfaceNumber write SetZetaSurfaceNumber;
  end;

  TSwiObservationTimeList = class(TCustomTimeList)
  private
    // See @link(OnInvalidate).
    FOnInvalidate: TNotifyEvent;
    FCellList: TObjectList<TSwiObsCellList>;
    function GetCellList(Index: integer): TSwiObsCellList;
  protected
    // @name calls the inherited @link(TCustomTimeList.SetUpToDate)
    // and then calls @link(OnInvalidate) if @link(OnInvalidate) is assigned.
    procedure SetUpToDate(const Value: boolean); override;
  public
    procedure Clear; override;
    constructor Create(Model: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name directly. Call Free instead.
    destructor Destroy; override;

    // @name takes the times and formulas in BoundaryValues and uses them
    // to determine the locations and values for those times.  These
    // locations and values are stored in @link(TRealSparseDataSet)s
    // accessed through @link(TCustomTimeList.Items Items).
    procedure Initialize(ObservationValues: TSwiObsCollection;
      ScreenObject: TObject; UseLgrEdgeCells: boolean; AModel: TBaseModel); reintroduce;
    // If assigned, @name is called with @link(UpToDate) is set to False.
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    property CellLists[Index: integer]: TSwiObsCellList read GetCellList; default;
  end;

resourcestring
  StrZetaObservationsError = 'Zeta observations can only be defined using ' +
    'objects with a single vertex.  The following objects need to be fixed.';
  StrOneOrMoreZetaObs = 'One or more head observation are on inactive cells.';

implementation

uses
  System.Contnrs, ScreenObjectUnit, PhastModelUnit, ModflowGridUnit, FastGEO,
  frmErrorsAndWarningsUnit, RealListUnit;

resourcestring
  ErrorRoot = 'Error: Duplicate zeta observation times';
  EarlyTimeWarning = 'Zeta observation times earlier than the beginning of the first stress period will be ignored.';
  LateTimeWarning = 'Zeta observation times later than the end of the last stress period will be ignored.';

{ TSwiObsItem }

procedure TSwiObsItem.Assign(Source: TPersistent);
var
  SourceItem: TSwiObsItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSwiObsItem then
  begin
    SourceItem := TSwiObsItem(Source);
    Name := SourceItem.Name;
    ObservedValue := SourceItem.ObservedValue;
    Statistic := SourceItem.Statistic;
    StatFlag := SourceItem.StatFlag;
  end;
  inherited;

end;

procedure TSwiObsItem.InvalidateModel;
begin
  (Collection as TSwiObsCollection).InvalidateModel;
end;

function TSwiObsItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSwiObsItem;
begin
  result := inherited IsSame(AnotherItem) and (AnotherItem is TSwiObsItem);
  if result then
  begin
    Item := TSwiObsItem(AnotherItem);
    result := (Item.Name = Name)
      and (Item.ObservedValue = ObservedValue)
      and (Item.Statistic = Statistic)
      and (Item.StatFlag = StatFlag);
  end;
end;

procedure TSwiObsItem.SetObservedValue(const Value: double);
begin
  SetRealProperty(FObservedValue, Value);
end;

procedure TSwiObsItem.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
end;

procedure TSwiObsItem.SetStatFlag(const Value: TStatFlag);
begin
  if FStatFlag <> Value then
  begin
    FStatFlag := Value;
    InvalidateModel;
  end;
end;

procedure TSwiObsItem.SetStatistic(const Value: double);
begin
  SetRealProperty(FStatistic, Value);
end;

{ TSwiObsTimesModelLink }

constructor TSwiObsTimesModelLink.Create(AModel: TBaseModel);
begin
  FModel := AModel;
  FObsTimes := TSwiObservationTimeList.Create(FModel);
end;

destructor TSwiObsTimesModelLink.Destroy;
begin
  FObsTimes.Free;
  inherited;
end;

{ TSwiObsTimesModelLinkList }

constructor TSwiObsTimesModelLinkList.Create;
begin
  FList := TObjectList<TSwiObsTimesModelLink>.Create;
end;

destructor TSwiObsTimesModelLinkList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TSwiObsTimesModelLinkList.GetLink(
  AModel: TBaseModel): TSwiObsTimesModelLink;
var
  ModelIndex: Integer;
  Item: TSwiObsTimesModelLink;
begin
  for ModelIndex := 0 to FList.Count - 1 do
  begin
    Item := FList[ModelIndex];
    if Item.FModel = AModel then
    begin
      result := Item;
      Exit;
    end;
  end;
  result := TSwiObsTimesModelLink.Create(AModel);
  FList.Add(result);
  if AModel <> nil then
  begin
    { TODO -cSWI OBS : This needs to be finished }
//    result.FObsTimes.OnInvalidate :=
//      (AModel as TCustomModel).InvalidateMfHobHeads;
  end;
end;

procedure TSwiObsTimesModelLinkList.RemoveLink(AModel: TBaseModel);
var
  Index: Integer;
  ALink: TSwiObsTimesModelLink;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    ALink := FList[Index];
    if ALink.FModel = AModel then
    begin
      FList.Delete(Index);
      Break;
    end;
  end;
end;

{ TSwiObsCollection }

function TSwiObsCollection.CountObservationTimes(StartTime,
  EndTime: double): integer;
var
  Index: Integer;
  Item: TSwiObsItem;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := SwiItems[Index];
    if (StartTime <= Item.Time) and (Item.Time <= EndTime) then
    begin
      Inc(result);
    end;
  end;
end;

constructor TSwiObsCollection.Create(Boundary: TSwiObsBoundary;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(TSwiObsItem, Model, ScreenObject);
  FBoundary := Boundary;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
  FObsTimesModelLinkList := TSwiObsTimesModelLinkList.Create;
end;

destructor TSwiObsCollection.Destroy;
begin
  FObsTimesModelLinkList.Free;
  inherited;
end;

procedure TSwiObsCollection.EvaluateZetaObservations(AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  ObservationPoint: TPoint2D;
  Row: Integer;
  Column: Integer;
//  Width: Real;
  Center: Real;
  CellList: TSwiObsCellList;
  Cell : TSwi_Cell;
begin
  SwiObservations[AModel].Initialize(self, ScreenObject, True, AModel);

  if FSwiObservations.FCellList.Count > 0 then
  begin
    CellList := FSwiObservations.FCellList[0];
    if CellList.Count > 0 then
    begin
      Assert(ScreenObject <> nil);
      LocalScreenObject := ScreenObject as TScreenObject;
      Assert(LocalScreenObject.ViewDirection = vdTop);
      Assert(Model <> nil);
      LocalModel := AModel as TCustomModel;
      Grid := LocalModel.ModflowGrid;
      Assert(Grid <> nil);

      if LocalScreenObject.Count > 1 then
      begin
//        FObservationRowOffset := -MAXINT;
//        FObservationColumnOffset := -MAXINT;
        frmErrorsAndWarnings.AddError(FSwiObservations.Model,
          StrZetaObservationsError, LocalScreenObject.Name, LocalScreenObject)
      end;
//      else
      begin
//        Assert(LocalScreenObject.Count = 1);
        ObservationPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
          LocalScreenObject.Points[0]);

        Cell := CellList[0];
        Row := Cell.Row;
        Column := Cell.Column;

//        Width := Grid.RowWidth[Row];
        Center := Grid.RowCenter(Row);
        FObservationRowOffset := -(ObservationPoint.y - Center);

//        Width := Grid.ColumnWidth[Column];
        Center := Grid.ColumnCenter(Column);
        FObservationColumnOffset := (ObservationPoint.x - Center);
      end;
    end;
  end;
end;

function TSwiObsCollection.GetSwiItems(Index: integer): TSwiObsItem;
begin
  result := Items[Index] as TSwiObsItem;
end;

function TSwiObsCollection.GetSwiObservations(
  AModel: TBaseModel): TSwiObservationTimeList;
begin
  FSwiObservations := FObsTimesModelLinkList.Links[AModel].FObsTimes;
  result := FSwiObservations;
end;

procedure TSwiObsCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  FBoundary.InvalidateModel;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateSwiObs(self);
  end;
end;

procedure TSwiObsCollection.RemoveModelLink(AModel: TBaseModel);
begin
  FObsTimesModelLinkList.RemoveLink(AModel);
end;

{ TSwiRecord }

procedure TSwiRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Zeta);
  WriteCompReal(Comp, Time);
  WriteCompInt(Comp, Strings.IndexOf(ZetaAnnotation));
end;

procedure TSwiRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ZetaAnnotation);
end;

procedure TSwiRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Zeta := ReadCompReal(Decomp);
  Time := ReadCompReal(Decomp);
  ZetaAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TSwi_Cell }

procedure TSwi_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
end;

function TSwi_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSwi_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSwi_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSwi_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSwi_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := ZetaAnnotation;
    else Assert(False);
  end;
end;

function TSwi_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := Zeta;
    else Assert(False);
  end;
end;

function TSwi_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSwi_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TSwi_Cell.GetTime: double;
begin
  result := Values.Time;
end;

function TSwi_Cell.GetZeta: double;
begin
  result := Values.Zeta;
end;

function TSwi_Cell.GetZetaAnnotation: string;
begin
  result := Values.ZetaAnnotation;
end;

procedure TSwi_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TSwi_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
end;

procedure TSwi_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TSwi_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TSwi_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TSwiObsCellList }

//function TSwiObsCellList.Add(Cell: TSwi_Cell): integer;
//begin
//  result := FList.Add(Cell);
//end;
//
//constructor TSwiObsCellList.Create;
//begin
//  FList := TObjectList<TSwi_Cell>.Create;
//end;
//
//procedure TSwiObsCellList.Delete(Index: Integer);
//begin
//  FList.Delete(Index);
//end;
//
//destructor TSwiObsCellList.Destroy;
//begin
//  FList.Free;
//  inherited;
//end;
//
//function TSwiObsCellList.GetCount: integer;
//begin
//  result := FList.Count;
//end;
//
//function TSwiObsCellList.GetItem(Index: integer): TSwi_Cell;
//begin
//  result := FList[Index];
//end;

{ TSwiObsBoundary }

procedure TSwiObsBoundary.Assign(Source: TPersistent);
var
  SwiSource: TSwiObsBoundary;
begin
  if Source is TSwiObsBoundary then
  begin
    SwiSource := TSwiObsBoundary(Source);
    Values := SwiSource.Values;
    ZetaSurfaceNumber := SwiSource.ZetaSurfaceNumber;
  end;
  inherited;
end;

procedure TSwiObsBoundary.Clear;
begin
  Values.Clear;
end;

constructor TSwiObsBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FValues:= TSwiObsCollection.Create(self, Model, ScreenObject);
  FZetaSurfaceNumber := 1;
end;

destructor TSwiObsBoundary.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TSwiObsBoundary.EvaluateSwiObservations(Purpose: TObservationPurpose;
  AModel: TBaseModel);
begin
  if self.Purpose = Purpose then
  begin
    Values.EvaluateZetaObservations(AModel);
  end;
end;

function TSwiObsBoundary.GetCellList(Index: integer): TSwiObsCellList;
begin
  result := Values.FSwiObservations.CellLists[Index];
end;

function TSwiObsBoundary.GetCellListCount: integer;
begin
  result := Values.FSwiObservations.FCellList.Count;
end;

procedure TSwiObsBoundary.RemoveModelLink(AModel: TBaseModel);
begin
  Values.RemoveModelLink(AModel);
end;

procedure TSwiObsBoundary.SetValues(const Value: TSwiObsCollection);
begin
  FValues.Assign(Value);
end;

procedure TSwiObsBoundary.SetZetaSurfaceNumber(const Value: integer);
begin
  if FZetaSurfaceNumber <> Value then
  begin
    FZetaSurfaceNumber := Value;
    InvalidateModel;
  end;
end;

function TSwiObsBoundary.Used: boolean;
begin
  result := FValues.Count > 0;
end;

{ TSwiObservationTimeList }

procedure TSwiObservationTimeList.Clear;
begin
  inherited;
  FCellList.Clear;
end;

constructor TSwiObservationTimeList.Create(Model: TBaseModel);
begin
  inherited;
  FCellList := TObjectList<TSwiObsCellList>.Create;
end;

destructor TSwiObservationTimeList.Destroy;
begin
  FCellList.Free;
  inherited;
end;

function TSwiObservationTimeList.GetCellList(Index: integer): TSwiObsCellList;
begin
  result := FCellList[Index];
end;

procedure TSwiObservationTimeList.Initialize(
  ObservationValues: TSwiObsCollection; ScreenObject: TObject;
  UseLgrEdgeCells: boolean; AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  Index: Integer;
  Time: double;
  DataArray: TCustomSparseDataSet;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  Value: double;
  StoredUpToDate: boolean;
  CellList: TSwiObsCellList;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Cell: TSwi_Cell;
  Times: TRealList;
  DuplicateTimes: string;
  EarliestAllowedTime: double;
  LatestAllowedTime: double;
  EarlyTimes: string;
  LateTimes: string;
  ActiveDataSet: TDataArray;
  InactiveList: TList<Integer>;
  ListPosition: integer;
  InactiveIndex: Integer;
begin
  if UpToDate then
    Exit;

  LocalScreenObject := ScreenObject as TScreenObject;
  Assert(LocalScreenObject <> nil);
  LocalModel := AModel as TCustomModel;
  if LocalModel.ModflowFullStressPeriods.Count > 0 then
  begin
    EarliestAllowedTime := LocalModel.ModflowFullStressPeriods.First.StartTime;
    LatestAllowedTime := LocalModel.ModflowFullStressPeriods.Last.EndTime;
  end
  else
  begin
    EarliestAllowedTime := LocalModel.ModflowStressPeriods.First.StartTime;
    LatestAllowedTime := LocalModel.ModflowStressPeriods.Last.EndTime;
  end;
  Assert(LocalModel <> nil);
  StoredUpToDate := LocalModel.UpToDate;
  Times := TRealList.Create;
  try
    DuplicateTimes := '';
    EarlyTimes := '';
    LateTimes := '';
    Times.Sorted := True;
    Clear;
    Grid := LocalModel.ModflowGrid;
    Assert(Grid <> nil);

    for Index := 0 to ObservationValues.Count - 1 do
    begin
      CellList := TSwiObsCellList.Create;
      FCellList.Add(CellList);
      InactiveList := TList<Integer>.Create;
      try
        Time := ObservationValues.SwiItems[Index].Time;
        if Times.IndexOf(Time) >= 0 then
        begin
          DuplicateTimes := DuplicateTimes + ' ' + FloatToStr(Time);
          Continue;
        end;

        if Time < EarliestAllowedTime then
        begin
          EarlyTimes := EarlyTimes + ' ' + FloatToStr(Time);
          Continue;
        end;

        if Time > LatestAllowedTime then
        begin
          LateTimes := LateTimes + ' ' + FloatToStr(Time);
          Continue;
        end;

        Times.Add(Time);
        Value := ObservationValues.SwiItems[Index].ObservedValue;
        DataArray := TRealSparseDataSet.Create(LocalModel,
          LocalModel.LayerCount, LocalModel.RowCount, LocalModel.ColumnCount);
        Add(Time, DataArray);
        DataArray.EvaluatedAt := eaBlocks;
        DataArray.Orientation := dso3D;
        DataArray.UpdateDimensions(LocalModel.LayerCount, LocalModel.RowCount,
          LocalModel.ColumnCount);

        ActiveDataSet := LocalModel.DataArrayManager.GetDataSetByName(rsActive);
        ActiveDataSet.Initialize;

        LocalScreenObject.AssignNumericValueToDataSet({Grid,} DataArray, Value,
          Model);
        for LayerIndex := 0 to DataArray.LayerCount - 1 do
        begin
          if not LocalModel.IsLayerSimulated(LayerIndex) then
          begin
            Continue;
          end;
          for RowIndex := 0 to DataArray.RowCount - 1 do
          begin
            for ColIndex := 0 to DataArray.ColumnCount - 1 do
            begin
              if DataArray.IsValue[LayerIndex, RowIndex,ColIndex] then
              begin
                Cell := TSwi_Cell.Create;
                ListPosition := CellList.Add(Cell);
                Cell.Values.Cell.Layer := LayerIndex;
                Cell.Values.Cell.Row := RowIndex;
                Cell.Values.Cell.Column := ColIndex;
                Cell.Values.Zeta := DataArray.RealData[LayerIndex, RowIndex,ColIndex];
                Cell.Values.ZetaAnnotation := DataArray.Annotation[LayerIndex, RowIndex,ColIndex];
                Cell.Values.Time := Time;
                if not ActiveDataSet.BooleanData[LayerIndex, RowIndex,ColIndex] then
                begin
                  InactiveList.Add(ListPosition);
                end;
              end;
            end;
          end;
        end;
        if (InactiveList.Count > 0) and (InactiveList.Count = CellList.Count) then
        begin
          frmErrorsAndWarnings.AddWarning(Model,
            StrOneOrMoreZetaObs, LocalScreenObject.Name, LocalScreenObject);
        end
        else
        begin
          for InactiveIndex := InactiveList.Count - 1 downto 0 do
          begin
            CellList.Delete(InactiveList[InactiveIndex]);
          end;
        end;
      finally
        InactiveList.Free;
      end;
    end;
    if DuplicateTimes <> '' then
    begin
      DuplicateTimes := Format(StrErrorObjectDuplicateTimes,
        [LocalScreenObject.Name, DuplicateTimes]);
      frmErrorsAndWarnings.AddError(Model, ErrorRoot, DuplicateTimes,
        LocalScreenObject);
    end;
    if EarlyTimes <> '' then
    begin
      EarlyTimes := Format(StrErrorObjectEarlyTimes,
        [LocalScreenObject.Name, EarlyTimes]);
      frmErrorsAndWarnings.AddWarning(Model, EarlyTimeWarning, EarlyTimes,
        LocalScreenObject);
    end;
    if LateTimes <> '' then
    begin
      LateTimes := Format(StrErrorObjectLateTimes,
        [LocalScreenObject.Name, LateTimes]);
      frmErrorsAndWarnings.AddWarning(Model, LateTimeWarning, LateTimes,
        LocalScreenObject);
    end;
  finally
    LocalModel.UpToDate := StoredUpToDate;
    Times.Free;
  end;
end;

procedure TSwiObservationTimeList.SetUpToDate(const Value: boolean);
begin
  inherited;
  if not Value then
  begin
    if Assigned(OnInvalidate) then
    begin
      OnInvalidate(Self);
    end;
  end;
end;

end.
