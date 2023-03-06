unit ModflowHobUnit;

interface

uses ZLib, SysUtils, Classes, Contnrs, GoPhastTypes, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, ObsInterfaceUnit,
  System.Generics.Collections;

type
  THobRecord = record
    Cell: TCellLocation;
    Head: double;
    Time: double;
    HeadAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TMultiObsMethod = (momAllHeads, momHeadAndDrawdown);

  THeadObsResults = record
    Imported: boolean;
    Observed: double;
    Simulated: double;
    Difference: double;
    class operator Equal(const Left, Right: THeadObsResults): Boolean;
    class operator NotEqual(const Left, Right: THeadObsResults): Boolean;
  end;

//  TCustomLocationObservation = class abstract(TOrderedItem)
//  private
//    FTime: double;
//    FComment: string;
//    procedure SetTime(const Value: double);
//    procedure SetComment(const Value: string);
//  protected
//    function IsSame(AnotherItem: TOrderedItem): boolean; override;
//  published
//    // @name copies Source to this @classname.
//    procedure Assign(Source: TPersistent); override;
//    // @name indicates the time of this observation.
//    property Time: double read FTime write SetTime;
//    property Comment: string read FComment write SetComment;
//  end;

  // @name represents a MODFLOW head observation for one time.
  // @name is stored by @link(THobCollection).
  THobItem = class(TCustomLocationObservation, IObservationItem, ITimeObservationItem)
  private
    FHead: double;
    FStatFlag: TStatFlag;
    FStatistic: double;
    FObservationGroup: string;
    FName: string;
    FGUID: string;
//    FObservationGroup: string;
    procedure SetHead(const Value: double);
    procedure SetStatFlag(const Value: TStatFlag);
    procedure SetStatistic(const Value: double);
    function GetStatFlag: TStatFlag;
    function GetHeadChange: Double;
    procedure SetHeadChange(const Value: Double);
    function GetHead: double;
    function GetObservedValue: double;
    procedure SetObservedValue(const Value: double);

//    procedure IObservationItem.SetObservedValue = SetHead;
//    function ITimeObservationItem.GetObservedValue = GetHead;
//    procedure ITimeObservationItem.SetObservedValue = SetHead;
    function GetWeight: Double;
    function GetName: string;
    function GetObservationGroup: string;
    procedure SetObservationGroup(const Value: string);
    procedure SetName(const Value: string);
    function GetGUID: string;
    procedure SetGUID(const Value: string);
    function GetScreenObject: TObject;
    function GetExportedName: string;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    property ScreenObject: TObject read GetScreenObject;
  public
    property HeadChange: Double read GetHeadChange write SetHeadChange;
    property Name: string read GetName write SetName;
    function ObservationType: string;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    // @name is the observed head
    // of this head observation.
    property Head: double read GetHead write SetHead;
    property Statistic: double read FStatistic write SetStatistic;
    property StatFlag: TStatFlag read GetStatFlag write SetStatFlag;
    property ObservationGroup: string read GetObservationGroup
      write SetObservationGroup;
    property GUID: string read GetGUID write SetGUID;
  end;

  THobList = TList<THobItem>;
  THobObjectList = TObjectList<THobItem>;

  TObservationTimeList = class;

  TObsTimesModelLink = class(TObject)
  private
    FObsTimes: TObservationTimeList;
    FModel: TBaseModel;
  public
    Constructor Create(AModel: TBaseModel);
    Destructor Destroy; override;
  end;

  TObsTimesModelLinkList = class(TObject)
  private
    // @name is actually a TObjectList.
    FList: TList;
    function GetLink(AModel: TBaseModel): TObsTimesModelLink;
  public
    property Links[AModel: TBaseModel]: TObsTimesModelLink read GetLink;
    Constructor Create;
    Destructor Destroy; override;
    procedure RemoveLink(AModel: TBaseModel);
  end;

  THobBoundary = class;

  // @name represents MODFLOW Head observations
  // for a series of times.
  THobCollection = class(TCustomObjectOrderedCollection)
  private
    FBoundary: THobBoundary;
    FObservationHeads: TObservationTimeList;
    FObsTimesModelLinkList: TObsTimesModelLinkList;
    FObservationRowOffset: double;
    FObservationColumnOffset: double;
//    FScreenObject: TObject;
    function GetHobItems(Index: integer): THobItem;
    function GetObservationHeads(AModel: TBaseModel): TObservationTimeList;
  protected
    procedure InvalidateModel; override;
  public
    procedure RemoveModelLink(AModel: TBaseModel);
    property ScreenObject;
    // @name creates an instance of @classname
    constructor Create(Boundary: THobBoundary; Model: TBaseModel;
      ScreenObject: TObject);
    procedure EvaluateHeadObservations(AModel: TBaseModel);
    // @name destroys the current instance of @classname.
    // Do not call @name; call Free instead.
    destructor Destroy; override;
    property HobItems[Index: integer]: THobItem read GetHobItems; default;
    // ROFF
    property ObservationRowOffset: double read FObservationRowOffset;
    // COFF
    property ObservationColumnOffset: double read FObservationColumnOffset;
    property ObservationHeads[AModel: TBaseModel]: TObservationTimeList
      read GetObservationHeads;
    function CountObservationTimes(StartTime, EndTime, Epsilon: double): integer;
  end;

  THob_Cell = class(TValueCell)
  private
    Values: THobRecord;
    function GetHead: double;
    function GetTime: double;
    function GetHeadAnnotation: string;
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
    property Head: double read GetHead;
    property Time: double read GetTime;
    property HeadAnnotation: string read GetHeadAnnotation;
  end;

  // @name ows the @link(THob_Cell)s it contains.
  TObsCellList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): THob_Cell;
  public
    Constructor Create;
    Destructor Destroy; override;
    function Add(Cell: THob_Cell): integer;
    property Count: integer read GetCount;
    property Items[Index: integer]: THob_Cell read GetItem; default;
    procedure Delete(Index: Integer);
  end;

  // @name represents the MODFLOW Head observations associated with
  // a single @link(TScreenObject).
  THobBoundary = class(TCustomMultilayerLocationObsBoundary)
  private
    FValues: THobCollection;
    FMultiObsMethod: TMultiObsMethod;
//    FObservationGroup: string;
    procedure SetValues(const Value: THobCollection);
    function GetCellList(Index: integer): TObsCellList;
    procedure SetMultiObsMethod(const Value: TMultiObsMethod);
    function GetCellListCount: integer;
//    procedure SetObservationGroup(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
    // @name creates an instance of @classname.
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name destroys the current instance of @classname.  Do not call
    // @name directly.  Call Free instead.
    Destructor Destroy; override;
    // @name checks that the Purpose parameter matches @link(Purpose)
    // and, if so, calls @link(THobCollection.EvaluateHeadObservations
    // Values.EvaluateHeadObservations)
    procedure EvaluateHeadObservations(Purpose: TObservationPurpose;
      AModel: TBaseModel);
    function Used: boolean; override;
    procedure Clear; virtual;
    property CellLists[Index: integer]: TObsCellList read GetCellList;
    property CellListCount: integer read GetCellListCount;
    procedure RemoveModelLink(AModel: TBaseModel);
  published
    // @name stores the MODFLOW boundaries that are NOT
    // associated with parameters.
    property Values: THobCollection read FValues write SetValues;
    // ITT
    property MultiObsMethod: TMultiObsMethod read FMultiObsMethod
      write SetMultiObsMethod default momHeadAndDrawdown;
//    property ObservationGroup: string read FObservationGroup
//      write SetObservationGroup;
  end;

  // @name is used to store a series of @link(TDataArray)s for head
  // observations in MODFLOW.
  TObservationTimeList = class(TCustomTimeList)
  private
    // See @link(OnInvalidate).
    FOnInvalidate: TNotifyEvent;
    FCellList: TList;
    function GetCellList(Index: integer): TObsCellList;
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
    procedure Initialize(ObservationValues: THobCollection;
      ScreenObject: TObject; UseLgrEdgeCells: boolean; AModel: TBaseModel); reintroduce;
    // If assigned, @name is called with @link(UpToDate) is set to False.
    property OnInvalidate: TNotifyEvent read FOnInvalidate write FOnInvalidate;
    property CellLists[Index: integer]: TObsCellList read GetCellList; default;
  end;

resourcestring
  StrHeadObservationsError = 'Head observations can only be defined using ' +
    'objects with a single vertex.  The following objects need to be fixed.';
  StrOneOrMoreHeadObs = 'One or more head observation are on inactive cells.';

const
  StrHobout = '.hob_out';

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowGridUnit, FastGEO,
  SubscriptionUnit, RealListUnit, frmErrorsAndWarningsUnit,
  Generics.Collections;

resourcestring
  ErrorRoot = 'Error: Duplicate head observation times';
  EarlyTimeWarning = 'Head observation times earlier than the beginning of the first stress period will be ignored.';
  LateTimeWarning = 'Head observation times later than the end of the last stress period will be ignored.';

{ THob_Cell }

procedure THob_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
end;

function THob_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function THob_Cell.GetHead: double;
begin
  result := Values.Head;
end;

function THob_Cell.GetHeadAnnotation: string;
begin
  result := Values.HeadAnnotation;
end;

function THob_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function THob_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function THob_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function THob_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := HeadAnnotation;
    else Assert(False);
  end;
end;

function THob_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := Head;
    else Assert(False);
  end;
end;

function THob_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function THob_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function THob_Cell.GetTime: double;
begin
  result := Values.Time;
end;

procedure THob_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure THob_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
end;

procedure THob_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure THob_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure THob_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ THobBoundary }

procedure THobBoundary.Assign(Source: TPersistent);
var
  HobSource: THobBoundary;
begin
  if Source is THobBoundary then
  begin
    HobSource := THobBoundary(Source);
    Values := HobSource.Values;
    MultiObsMethod := HobSource.MultiObsMethod;
//    ObservationGroup := HobSource.ObservationGroup
  end;
  inherited;
end;

procedure THobBoundary.Clear;
begin
  Values.Clear;
end;

constructor THobBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FValues:= THobCollection.Create(self, Model,
    ScreenObject);
  FMultiObsMethod := momHeadAndDrawdown;
end;

destructor THobBoundary.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure THobBoundary.EvaluateHeadObservations(Purpose: TObservationPurpose;
  AModel: TBaseModel);
begin
  if self.Purpose = Purpose then
  begin
    Values.EvaluateHeadObservations(AModel);
  end;
end;

function THobBoundary.GetCellList(Index: integer): TObsCellList;
begin
  result := Values.FObservationHeads.CellLists[Index];
end;

function THobBoundary.GetCellListCount: integer;
begin
  result := Values.FObservationHeads.FCellList.Count;
end;

procedure THobBoundary.RemoveModelLink(AModel: TBaseModel);
begin
  Values.RemoveModelLink(AModel);
end;

procedure THobBoundary.SetMultiObsMethod(const Value: TMultiObsMethod);
begin
  if FMultiObsMethod <> Value then
  begin
    InvalidateModel;
    FMultiObsMethod := Value;
  end;
end;

//procedure THobBoundary.SetObservationGroup(const Value: string);
//begin
//  FObservationGroup := Value;
//end;

procedure THobBoundary.SetValues(const Value: THobCollection);
begin
  FValues.Assign(Value);
end;

function THobBoundary.Used: boolean;
begin
  result := FValues.Count > 0;
end;

{ THobItem }

procedure THobItem.Assign(Source: TPersistent);
var
  SourceItem: THobItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is THobItem then
  begin
    SourceItem := THobItem(Source);
    Head := SourceItem.Head;
    Statistic := SourceItem.Statistic;
    StatFlag := SourceItem.StatFlag;
    ObservationGroup := SourceItem.ObservationGroup;
    Name := SourceItem.Name;
    GUID := SourceItem.GUID;
//    ObservationGroup := SourceItem.ObservationGroup;
  end;
  inherited;
end;

function THobItem.GetExportedName: string;
begin
  result := GetName;
end;

function THobItem.GetGUID: string;
var
  MyGuid : TGUID;
begin
  if (FGUID = '') and (CreateGUID(MyGuid) = 0) then
  begin
    FGUID := GUIDToString(MyGuid);
  end;
  result := FGUID;
end;

function THobItem.GetHead: double;
begin
  result := FHead;
end;

function THobItem.GetHeadChange: Double;
var
  LocalCollection: THobCollection;
  FirstItem: THobItem;
begin
  if (Index = 0) or (Collection = nil) then
  begin
    result := 0;
  end
  else
  begin
    LocalCollection := Collection as THobCollection;
    FirstItem := LocalCollection.Items[0] as THobItem;
    Result := Head - FirstItem.Head;
  end;
end;

function THobItem.GetName: string;
var
  HobCollection: THobCollection;
begin
  if Collection <> nil then
  begin
    HobCollection := Collection as THobCollection;
    Result := HobCollection.FBoundary.ObservationName;
    if HobCollection.Count > 1 then
    begin
      Result := HobCollection.FBoundary.ObservationName + '_' + IntToStr(Index + 1);
    end;
    if Length(Result) > 12 then
    begin
      Result := HobCollection.FBoundary.ObservationName + IntToStr(Index + 1);
    end;
  end
  else
  begin
    result := FName;
  end;
end;

function THobItem.GetObservationGroup: string;
//var
//  HobCollection: THobCollection;
begin
  result := FObservationGroup;
//  if Index > 0 then
//  begin
//    HobCollection := Collection as THobCollection;
//    if HobCollection.FBoundary.FMultiObsMethod = momAllHeads then
//    begin
//      Result := HobCollection.FBoundary.ObservationGroup;
//    end
//    else
//    begin
//      Result := HobCollection.FBoundary.ObservationGroup + '_Changes';
//    end;
//  end
//  else
//  begin
//    Result := HobCollection.FBoundary.ObservationGroup;
//  end;
end;

function THobItem.GetObservedValue: double;
begin
  result := GetHead;
end;

function THobItem.GetScreenObject: TObject;
var
  HobCollection: THobCollection;
begin
  if Collection = nil then
  begin
    result := nil;
    Exit;
  end;
  HobCollection := Collection as THobCollection;
  result := HobCollection.FBoundary.ScreenObject;
end;

function THobItem.GetStatFlag: TStatFlag;
var
  LocalCollection: THobCollection;
begin
  result := FStatFlag;
  if Collection <> nil then
  begin
    LocalCollection := Collection as THobCollection;
    if (LocalCollection.FBoundary.Purpose = ofPredicted)
      and (result > stStandardDev) then
    begin
      result := stVariance;
    end;
  end;
end;

function THobItem.GetWeight: Double;
begin
  result := 0;
  case StatFlag of
    stVariance:
      begin
        result := 1/Statistic;
      end;
    stStandardDev:
      begin
        result := 1/Sqr(Statistic);
      end;
    stCoefVar:
      begin
        // Coefficient of variation = mean/standard deviation
        // SD := Head/Statistic;
        //result :=  1/Sqr(Head/Statistic);
        result := Sqr(Statistic/Head);
      end;
    stWeight:
      begin
        result := Statistic;
      end;
    stSquaredWeight:
      begin
        result := Sqrt(Statistic);
      end;
  end;
end;

procedure THobItem.InvalidateModel;
begin
  if Collection <> nil then
  begin
    (Collection as THobCollection).InvalidateModel;
  end;
end;

function THobItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: THobItem;
begin
  result := inherited IsSame(AnotherItem) and (AnotherItem is THobItem);
  if result then
  begin
    Item := THobItem(AnotherItem);
    result := (Item.Head = Head)
      and (Item.Statistic = Statistic)
      and (Item.StatFlag = StatFlag)
      and (Item.ObservationGroup = ObservationGroup)
      and (Item.GUID = GUID)
  end;
end;

function THobItem.ObservationType: string;
begin
  result := 'HOB';
end;

function THobItem.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;

procedure THobItem.SetHeadChange(const Value: Double);
var
  LocalCollection: THobCollection;
  FirstItem: THobItem;
begin
  if Index > 0 then
  begin
    LocalCollection := Collection as THobCollection;
    FirstItem := LocalCollection.Items[0] as THobItem;
    Head := FirstItem.Head + Value;
  end;
end;

procedure THobItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure THobItem.SetObservationGroup(const Value: string);
begin
  FObservationGroup := Value;
end;

procedure THobItem.SetObservedValue(const Value: double);
begin
  SetHead(Value);
end;

procedure THobItem.SetGUID(const Value: string);
begin
  FGUID := Value;
end;

procedure THobItem.SetHead(const Value: double);
begin
  if FHead <> Value then
  begin
    FHead := Value;
    InvalidateModel;
  end;
end;

procedure THobItem.SetStatFlag(const Value: TStatFlag);
begin
  if FStatFlag <> Value then
  begin
    FStatFlag := Value;
    InvalidateModel;
  end;
end;

procedure THobItem.SetStatistic(const Value: double);
begin
  if FStatistic <> Value then
  begin
    FStatistic := Value;
    InvalidateModel;
  end;
end;
function THobItem._AddRef: Integer;
begin
  result := -1;
end;

function THobItem._Release: Integer;
begin
  result := -1;
end;

{ THobCollection }

function THobCollection.CountObservationTimes(StartTime,
  EndTime, Epsilon: double): integer;
var
  Index: Integer;
  Item: THobItem;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Item := HobItems[Index];
    if (StartTime <= Item.Time) and (Item.Time <= EndTime) then
    begin
      Inc(result);
    end
    else if ((Item.Time - StartTime)/(Abs(Item.Time) + Abs(StartTime))) < Epsilon then
    begin
      Inc(result);
    end
    else if ((EndTime - Item.Time)/(Abs(Item.Time) + Abs(EndTime))) < Epsilon then
    begin
      Inc(result);
    end;
  end;
end;

constructor THobCollection.Create(Boundary: THobBoundary; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited Create(THobItem, Model, ScreenObject);
  FBoundary := Boundary;
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
  FObsTimesModelLinkList := TObsTimesModelLinkList.Create;
end;

destructor THobCollection.Destroy;
begin
  FObsTimesModelLinkList.Free;
  inherited;
end;

procedure THobCollection.EvaluateHeadObservations(AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  ObservationPoint: TPoint2D;
  Row: Integer;
  Column: Integer;
  Width: Real;
  Center: Real;
  CellList: TObsCellList;
  Cell : THob_Cell;
begin
  ObservationHeads[AModel].Initialize(self, ScreenObject, True, AModel);

  if FObservationHeads.FCellList.Count > 0 then
  begin
    CellList := FObservationHeads.FCellList[0];
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
        FObservationRowOffset := -1000;
        FObservationColumnOffset := -1000;
        frmErrorsAndWarnings.AddError(FObservationHeads.Model,
          StrHeadObservationsError, LocalScreenObject.Name, LocalScreenObject)
      end
      else
      begin
        Assert(LocalScreenObject.Count = 1);
        ObservationPoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(
          LocalScreenObject.Points[0]);

        Cell := CellList[0];
        Row := Cell.Row;
        Column := Cell.Column;

        Width := Grid.RowWidth[Row];
        Center := Grid.RowCenter(Row);
        FObservationRowOffset := -(ObservationPoint.y - Center)/Width;

        Width := Grid.ColumnWidth[Column];
        Center := Grid.ColumnCenter(Column);
        FObservationColumnOffset := (ObservationPoint.x - Center)/Width;
      end;
    end;
  end;
end;

function THobCollection.GetHobItems(Index: integer): THobItem;
begin
  result := Items[Index] as THobItem;
end;

function THobCollection.GetObservationHeads(
  AModel: TBaseModel): TObservationTimeList;
begin
  FObservationHeads := FObsTimesModelLinkList.Links[AModel].FObsTimes;
  result := FObservationHeads;
end;

procedure THobCollection.InvalidateModel;
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
    PhastModel.InvalidateMfHobHeads(self);
  end;
end;

procedure THobCollection.RemoveModelLink(AModel: TBaseModel);
begin
  FObsTimesModelLinkList.RemoveLink(AModel);
end;

{ TObservationTimeList }

procedure TObservationTimeList.Clear;
begin
  inherited;
  FCellList.Clear;
end;

constructor TObservationTimeList.Create(Model: TBaseModel);
begin
  inherited;
  FCellList := TObjectList.Create;
end;

destructor TObservationTimeList.Destroy;
begin
  FCellList.Free;
  inherited;
end;

function TObservationTimeList.GetCellList(Index: integer): TObsCellList;
begin
  result := FCellList[Index];
end;

procedure TObservationTimeList.Initialize(ObservationValues: THobCollection;
  ScreenObject: TObject; UseLgrEdgeCells: boolean; AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  Index: Integer;
  Time: double;
  DataArray: TCustomSparseDataSet;
  LocalModel: TCustomModel;
  Grid: TModflowGrid;
  Value: double;
  StoredUpToDate: boolean;
  CellList: TObsCellList;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  Cell: THob_Cell;
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
      CellList := TObsCellList.Create;
      FCellList.Add(CellList);
      InactiveList := TList<Integer>.Create;
      try
        Time := ObservationValues.HobItems[Index].Time;
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
        Value := ObservationValues.HobItems[Index].Head;
        DataArray := TRealSparseDataSet.Create(LocalModel);
        Add(Time, DataArray);
        DataArray.EvaluatedAt := eaBlocks;
        DataArray.Orientation := dso3D;
        DataArray.UpdateDimensions(Grid.LayerCount, Grid.RowCount,
          Grid.ColumnCount);

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
                Cell := THob_Cell.Create;
                ListPosition := CellList.Add(Cell);
                Cell.Values.Cell.Layer := LayerIndex;
                Cell.Values.Cell.Row := RowIndex;
                Cell.Values.Cell.Column := ColIndex;
                Cell.Values.Head := DataArray.RealData[LayerIndex, RowIndex,ColIndex];
                Cell.Values.HeadAnnotation := DataArray.Annotation[LayerIndex, RowIndex,ColIndex];
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
            StrOneOrMoreHeadObs, LocalScreenObject.Name, LocalScreenObject);
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
//      DuplicateTimes := 'Error; Object = ' + LocalScreenObject.Name +
//        ' Duplicate Times = ' +  DuplicateTimes;
      frmErrorsAndWarnings.AddError(Model, ErrorRoot, DuplicateTimes,
        LocalScreenObject);
    end;
    if EarlyTimes <> '' then
    begin
      EarlyTimes := Format(StrErrorObjectEarlyTimes,
        [LocalScreenObject.Name, EarlyTimes]);
//      EarlyTimes := 'Error; Object = ' + LocalScreenObject.Name +
//        ' Early Times = ' +  EarlyTimes;
      frmErrorsAndWarnings.AddWarning(Model, EarlyTimeWarning, EarlyTimes,
        LocalScreenObject);
    end;
    if LateTimes <> '' then
    begin
      LateTimes := Format(StrErrorObjectLateTimes,
        [LocalScreenObject.Name, LateTimes]);
//      LateTimes := 'Error; Object = ' + LocalScreenObject.Name +
//        ' Late Times = ' +  LateTimes;
      frmErrorsAndWarnings.AddWarning(Model, LateTimeWarning, LateTimes,
        LocalScreenObject);
    end;
  finally
    LocalModel.UpToDate := StoredUpToDate;
    Times.Free;
  end;
end;

procedure TObservationTimeList.SetUpToDate(const Value: boolean);
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

{ TMultiHeadItem }

//procedure TMultiHeadItem.Assign(Source: TPersistent);
//var
//  SourceItem: TMultiHeadItem;
//begin
//  // if Assign is updated, update IsSame too.
//  if Source is TMultiHeadItem then
//  begin
//    SourceItem := TMultiHeadItem(Source);
//    Layer := SourceItem.Layer;
//    Proportion := SourceItem.Proportion;
//  end;
//  inherited;
//end;
//
//function TMultiHeadItem.IsSame(AnotherItem: TOrderedItem): boolean;
//var
//  Item: TMultiHeadItem;
//begin
//  result := AnotherItem is TMultiHeadItem;
//  if result then
//  begin
//    Item := TMultiHeadItem(AnotherItem);
//    result := (Item.Layer = Layer)
//      and (Item.Proportion = Proportion);
//  end;
//end;
//
//procedure TMultiHeadItem.SetLayer(const Value: integer);
//begin
//  if FLayer <> Value then
//  begin
//    InvalidateModel;
//    FLayer := Value;
//  end;
//end;
//
//procedure TMultiHeadItem.SetProportion(const Value: double);
//begin
//  if FProportion <> Value then
//  begin
//    InvalidateModel;
//    FProportion := Value;
//  end;
//end;

{ TMultiHeadCollection }

//constructor TMultiHeadCollection.Create(Model: TBaseModel;
//  ScreenObject: TObject);
//begin
//  inherited Create(TMultiHeadItem, Model);
//  Assert((ScreenObject = nil) or (ScreenObject is TScreenObject));
//  FScreenObject := ScreenObject;
//end;
//
//function TMultiHeadCollection.GetMultiHeadItem(Index: integer): TMultiHeadItem;
//begin
//  result := Items[Index] as TMultiHeadItem;
//end;

{ TObsCellList }

function TObsCellList.Add(Cell: THob_Cell): integer;
begin
  result := FList.Add(Cell);
end;

constructor TObsCellList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

procedure TObsCellList.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TObsCellList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TObsCellList.GetCount: integer;
begin
  result := FList.Count;
end;

function TObsCellList.GetItem(Index: integer): THob_Cell;
begin
  result := FList[Index];
end;

{ THobRecord }

procedure THobRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Head);
  WriteCompReal(Comp, Time);
  WriteCompInt(Comp, Strings.IndexOf(HeadAnnotation));
end;

procedure THobRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(HeadAnnotation);
end;

procedure THobRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Head := ReadCompReal(Decomp);
  Time := ReadCompReal(Decomp);
  HeadAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TFObsTimesModelLink }

constructor TObsTimesModelLink.Create(AModel: TBaseModel);
begin
  FModel := AModel;
  FObsTimes := TObservationTimeList.Create(FModel);
end;

destructor TObsTimesModelLink.Destroy;
begin
  FObsTimes.Free;
  inherited;
end;

{ TObsTimesModelLinkList }

constructor TObsTimesModelLinkList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TObsTimesModelLinkList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TObsTimesModelLinkList.GetLink(
  AModel: TBaseModel): TObsTimesModelLink;
var
  ModelIndex: Integer;
  Item: TObsTimesModelLink;
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
  result := TObsTimesModelLink.Create(AModel);
  FList.Add(result);
  if AModel <> nil then
  begin
    result.FObsTimes.OnInvalidate :=
      (AModel as TCustomModel).InvalidateMfHobHeads;
  end;
end;

procedure TObsTimesModelLinkList.RemoveLink(AModel: TBaseModel);
var
  Index: Integer;
  ALink: TObsTimesModelLink;
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

{ THeadObsResults }

class operator THeadObsResults.Equal(const Left,
  Right: THeadObsResults): Boolean;
begin
  result := (Left.Imported = Right.Imported)
    and (Left.Observed = Right.Observed)
    and (Left.Simulated = Right.Simulated)
    and (Left.Difference = Right.Difference);
end;

class operator THeadObsResults.NotEqual(const Left,
  Right: THeadObsResults): Boolean;
begin
  result := (Left.Imported <> Right.Imported)
    or (Left.Observed <> Right.Observed)
    or (Left.Simulated <> Right.Simulated)
    or (Left.Difference <> Right.Difference);
end;

//procedure TCustomLocationObsBoundary.SetObservationName(Value: string);
//begin
//  Value := Trim(Value);
//  Value := StringReplace(Value, ' ', '_', [rfReplaceAll]);
//  Value := StringReplace(Value, '"', '', [rfReplaceAll]);
//  Value := StringReplace(Value, '''', '', [rfReplaceAll]);
//  Value := StringReplace(Value, '/', '_', [rfReplaceAll]);
//  Value := StringReplace(Value, '\', '_', [rfReplaceAll]);
//  if Length(Value) > 12 then
//  begin
//    Value := Copy(Value, 1, 12);
//  end;
//  if FObservationName <> Value then
//  begin
//    InvalidateModel;
//    FObservationName := Value;
//  end;
//end;

//procedure TCustomLocationObsBoundary.Assign(Source: TPersistent);
//var
//  LocSource: TCustomLocationObsBoundary;
//begin
//  if Source is TCustomLocationObsBoundary then
//  begin
//    LocSource := TCustomLocationObsBoundary(Source);
//    LayerFractions := LocSource.LayerFractions;
//    ObservationName := LocSource.ObservationName;
//    Purpose := LocSource.Purpose;
//  end
//  else
//  begin
//    inherited;
//  end;
//end;

//function TCustomLocationObsBoundary.BoundaryObserverPrefix: string;
//begin
//  Result := '';
//  Assert(False);
//end;

//constructor TCustomLocationObsBoundary.Create(Model: TBaseModel;
//  ScreenObject: TObject);
//begin
//  inherited;
//  FLayerFractions := TMultiHeadCollection.Create(Model, ScreenObject);
//  FPurpose := ofObserved;
//end;

//destructor TCustomLocationObsBoundary.Destroy;
//begin
//  FLayerFractions.Free;
//  inherited;
//end;

//procedure TCustomLocationObsBoundary.SetLayerFractions
//  (const Value: TMultiHeadCollection);
//begin
//  FLayerFractions.Assign(Value);
//end;
//
//procedure TCustomLocationObsBoundary.SetPurpose(const Value
//  : TObservationPurpose);
//begin
//  if FPurpose <> Value then
//  begin
//    InvalidateModel;
//    FPurpose := Value;
//  end;
//end;

//procedure TCustomLocationObservation.SetTime(const Value: double);
//begin
//  if FTime <> Value then
//  begin
//    FTime := Value;
//    InvalidateModel;
//  end;
//end;

//procedure TCustomLocationObservation.Assign(Source: TPersistent);
//var
//  SourceItem: TCustomLocationObservation;
//begin
//  // if Assign is updated, update IsSame too.
//  if Source is TCustomLocationObservation then
//  begin
//    SourceItem := TCustomLocationObservation(Source);
//    Time := SourceItem.Time;
//    Comment := SourceItem.Comment;
//  end;
//  inherited;
//end;

//function TCustomLocationObservation.IsSame(AnotherItem: TOrderedItem): boolean;
//var
//  Item: TCustomLocationObservation;
//begin
//  result := AnotherItem is TCustomLocationObservation;
//  if result then
//  begin
//    Item := TCustomLocationObservation(AnotherItem);
//    result := (Item.Time = Time)
//      and (Item.Comment = Comment);
//  end;
//end;

//procedure TCustomLocationObservation.SetComment(const Value: string);
//begin
//  if FComment <> Value then
//  begin
//    FComment := Value;
//    InvalidateModel;
//  end;
//end;

//function TCustomLocationObsBoundary.GetItemObsName
//  (Item: TCustomLocationObservation): string;
//begin
//  // Assert(Item.Collection = Values);
//  if Item.Collection.Count = 1 then
//  begin
//    result := ObservationName;
//  end
//  else
//  begin
//    result := ObservationName + '_' + IntToStr(Item.Index + 1);
//    if Length(result) > 12 then
//    begin
//      result := ObservationName + IntToStr(Item.Index + 1);
//    end;
//    if Length(result) > 12 then
//    begin
//      // The GUI is designed to prevent this from ever being required.
//      SetLength(result, 12);
//    end;
//  end;
//end;

end.
