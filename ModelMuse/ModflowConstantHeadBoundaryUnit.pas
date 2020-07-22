unit ModflowConstantHeadBoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit;

type
  // @name stores data for one CHD cell in a time increment defined by
  // @link(StartingTime) and @link(EndingTime).
  // The @link(StartingTime) and @link(EndingTime) may or may not be
  // the starting and ending time of stress periods.
  //  @longcode(
  //  TChdRecord = record
  //    Cell: TCellLocation;
  //    StartingHead: double;
  //    EndingHead: double;
  //    StartingTime: double;
  //    EndingTime: double;
  //    StartAnnotation: string;
  //    EndAnnotation: string;
  //  end;
  //  )
  // @member(Cell Cell is the cell to which this boundary applies.)
  // @member(StartingHead StartingHead is the specified head
  //   for this boundary at @link(StartingTime).)
  // @member(EndingHead EndingHead is the specified head
  //   for this boundary at @link(EndingTime).)
  // @member(StartingTime StartingTime is when this boundary
  //   first begins to apply.)
  // @member(EndingTime EndingTime is when this boundary ceases to apply.)
  // @member(StartAnnotation StartAnnotation tells how
  //  @link(StartingHead) was assigned.)
  // @member(EndAnnotation EndAnnotation tells how
  //  @link(EndingHead) was assigned.)
  TChdRecord = record
    Cell: TCellLocation;
    StartingHead: double;
    EndingHead: double;
    StartingTime: double;
    EndingTime: double;
    StartAnnotation: string;
    EndAnnotation: string;
    TimeSeriesName: string;
    HeadParameterName: string;
    HeadParameterValue: double;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TChdArray = array of TChdRecord;

  TChdStorage = class(TCustomBoundaryStorage)
  private
    FChdArray: TChdArray;
    function GetChdArray: TChdArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property ChdArray: TChdArray read GetChdArray;
  end;

  TChdTimeListLink = class(TTimeListsModelLink)
  private
    FStartData: TModflowTimeList;
    FEndData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Constant-Head boundaries
  // for a series of time intervals.
  TChdCollection = class(TCustomMF_ListBoundColl)
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure InvalidateStartData(Sender: TObject);
    procedure InvalidateEndData(Sender: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    // @name returns @link(TChdItem).
    class function ItemClass: TBoundaryItemClass; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  // Each @name stores a @link(TChdCollection).
  // @classname is stored by @link(TModflowParameters).
  TChdParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;
  
  // @name represents a MODFLOW Constant-Head boundary for one time interval.
  // @name is stored by TChdCollection.
  TChdItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EndHead).
    FEndHead: TFormulaObject;
    // See @link(StartHead).
    FStartHead: TFormulaObject;
    // See @link(EndHead).
    procedure SetEndHead(const Value: string);
    // See @link(StartHead).
    procedure SetStartHead(const Value: string);
    function GetEndHead: string;
    function GetStartHead: string;
  protected
    procedure RemoveFormulaObjects; override;
    procedure CreateFormulaObjects; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the ending head
    // or the ending head multiplier of this boundary.
    property EndHead: string read GetEndHead write SetEndHead;
    // @name is the formula used to set the starting head
    // or the starting head multiplier of this boundary.
    property StartHead: string read GetStartHead write SetStartHead;
  end;

  TCHD_Cell = class(TValueCell)
  private
    Values: TChdRecord;
    StressPeriod: integer;
    function GetEndingHead: double;
    function GetStartingHead: double;
    function GetEndingHeadAnnotation: string;
    function GetStartingHeadAnnotation: string;
    function GetTimeSeriesName: string;
    function GetHeadParameterName: string;
    function GetHeadParameterValue: double;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    function GetSection: integer; override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property StartingHead: double read GetStartingHead;
    property EndingHead: double read GetEndingHead;
    property TimeSeriesName: string read GetTimeSeriesName;
    property StartingHeadAnnotation: string read GetStartingHeadAnnotation;
    property EndingHeadAnnotation: string read GetEndingHeadAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property HeadParameterName: string read GetHeadParameterName;
    property HeadParameterValue: double read GetHeadParameterValue;
  end;


  // @name represents the MODFLOW Constant-Head boundaries associated with
  // a single @link(TScreenObject).
  //See also TChdCollection in implementation section
  // @seealso(TModflowParameters)
  TChdBoundary = class(TModflowParamBoundary)
  private
    FCurrentParameter: TModflowTransientListParameter;
//    FInterp: TMf6InterpolationMethods;
//    procedure SetInterp(const Value: TMf6InterpolationMethods);
  protected
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel);  override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;
  public
    procedure Assign(Source: TPersistent);override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  published
    property Interp;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, ModflowTimeSeriesUnit,
  frmErrorsAndWarningsUnit;

const
  StartHeadPosition = 0;
  EndHeadPosition = 1;

resourcestring
  FormatString =
    'Assigned by interpolation between the starting head of %0:f at '
    + 't = %1:f (%2:s) and the ending head of %3:f at t = %4:f (%5:s).';
  StrStartingHeadMulti = ' starting head multiplier';
  StrEndingHeadMultipl = ' ending head multiplier';
  StrNoValidCHDBoundar = 'No valid CHD boundary times defined.';
  StrNoValidCHDBoundar2 = 'No valid CHD boundary times are defined for the p' +
  'arameter ';


{ TChdItem }

procedure TChdItem.Assign(Source: TPersistent);
var
  Chd: TChdItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TChdItem then
  begin
    Chd := TChdItem(Source);
    StartHead := Chd.StartHead;
    EndHead := Chd.EndHead;
  end;
  inherited;
end;

procedure TChdItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FStartHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEndHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TChdItem.CreateFormulaObjects;
begin
  FStartHead := CreateFormulaObject(dso3D);
  FEndHead := CreateFormulaObject(dso3D);
end;

destructor TChdItem.Destroy;
begin
  StartHead := '0';
  EndHead := '0';
  inherited;
end;

procedure TChdItem.AssignObserverEvents(Collection: TCollection);
var
  EndObserver: TObserver;
  StartObserver: TObserver;
  ParentCollection: TChdCollection;
begin
  ParentCollection := Collection as TChdCollection;
  StartObserver := FObserverList[StartHeadPosition];
  StartObserver.OnUpToDateSet := ParentCollection.InvalidateStartData;
  EndObserver := FObserverList[EndHeadPosition];
  EndObserver.OnUpToDateSet := ParentCollection.InvalidateEndData;
end;

function TChdItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

function TChdItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StartHeadPosition: result := StartHead;
    EndHeadPosition: result := EndHead;
    else Assert(False);
  end;
end;

function TChdItem.GetEndHead: string;
begin
  Result := FEndHead.Formula;
  ResetItemObserver(EndHeadPosition);
end;

procedure TChdItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FStartHead then
  begin
    List.Add(FObserverList[StartHeadPosition]);
  end;
  if Sender = FEndHead then
  begin
    List.Add(FObserverList[EndHeadPosition]);
  end;
end;

function TChdItem.GetStartHead: string;
begin
  Result := FStartHead.Formula;
  ResetItemObserver(StartHeadPosition);
end;

procedure TChdItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfChdStartingHead(self);
    PhastModel.InvalidateMfChdEndingHead(self);
  end;
end;

function TChdItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TChdItem;
begin
  result := (AnotherItem is TChdItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TChdItem(AnotherItem);
    result := (Item.EndHead = EndHead)
      and (Item.StartHead = StartHead);
  end;
end;

{ TChdParamItem }
procedure TChdItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    StartHeadPosition: StartHead := Value;
    EndHeadPosition: EndHead := Value;
    else Assert(False);
  end;
end;

procedure TChdItem.SetEndHead(const Value: string);
begin
  UpdateFormula(Value, EndHeadPosition, FEndHead);
end;

procedure TChdItem.SetStartHead(const Value: string);
begin
  UpdateFormula(Value, StartHeadPosition, FStartHead);
end;

{ TChdParamItem }

class function TChdParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TChdCollection;
end;

{ TChdCollection }

function TChdCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TChdTimeListLink;
end;

procedure TChdCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TChdStorage.Create(AModel));
end;

function TChdCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TChdItem;
begin
  Item := Items[ItemIndex] as TChdItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TChdCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject);
var
  ChdStorage: TChdStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(Expression <> nil);

  ChdStorage := BoundaryStorage as TChdStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    Expression.Evaluate;
    with ChdStorage.ChdArray[Index] do
    begin
      case BoundaryFunctionIndex of
        0:
          begin
            StartingHead := Expression.DoubleResult;
            StartAnnotation := ACell.Annotation;
          end;
        1:
          begin
            EndingHead := Expression.DoubleResult;
            EndAnnotation := ACell.Annotation;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TChdCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  ChdStorage: TChdStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  ChdStorage := BoundaryStorage as TChdStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with ChdStorage.ChdArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

constructor TChdCollection.Create(Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  ListDuplicatesAllowed := False;
end;

procedure TChdCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TChdStorage).FChdArray, BoundaryCount);
  inherited;
end;

procedure TChdCollection.InvalidateEndData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TChdTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TChdTimeListLink;
    Link.FEndData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TChdTimeListLink;
      Link.FEndData.Invalidate;
    end;
  end;
end;

procedure TChdCollection.InvalidateStartData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TChdTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TChdTimeListLink;
    Link.FStartData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TChdTimeListLink;
      Link.FStartData.Invalidate;
    end;
  end;
end;

class function TChdCollection.ItemClass: TBoundaryItemClass;
begin
  result := TChdItem;
end;

{ TChdBoundary }

procedure TChdBoundary.Assign(Source: TPersistent);
//var
//  SourceChd: TChdBoundary;
begin
//  if Source is TChdBoundary then
//  begin
//    SourceChd := TChdBoundary(Source);
//    Interp := SourceChd.Interp;
//  end;
  inherited;
end;

procedure TChdBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TCHD_Cell;
  BoundaryValues: TChdRecord;
  BoundaryIndex: Integer;
  EndHeadFactor: Double;
  StartFormatString: string;
  StartHeadFactor: Double;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TChdStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TChdStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCHD_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      // The starting head for each cell will be
      // StartHeadFactor * StartingHead + (1-StartHeadFactor)*EndingHead
      // The ending head for each cell will be
      // EndHeadFactor * StartingHead + (1-EndHeadFactor)*EndingHead
      if (StressPeriod.StartTime = LocalBoundaryStorage.StartingTime)
        or (LocalModel.ModelSelection = msModflow2015) then
      begin
        StartHeadFactor := 1;
        StartFormatString := '';
      end
      else
      begin
        StartHeadFactor := 1 -
          (StressPeriod.StartTime - LocalBoundaryStorage.StartingTime)
          / (LocalBoundaryStorage.EndingTime
          - LocalBoundaryStorage.StartingTime);
      end;
      if (StressPeriod.EndTime = LocalBoundaryStorage.EndingTime)
        or (LocalModel.ModelSelection = msModflow2015) then
      begin
        EndHeadFactor := 0;
      end
      else
      begin
        EndHeadFactor := 1 -
          (StressPeriod.EndTime - LocalBoundaryStorage.StartingTime)
          / (LocalBoundaryStorage.EndingTime
          - LocalBoundaryStorage.StartingTime);
      end;
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.ChdArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.ChdArray)
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.ChdArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.ChdArray[BoundaryIndex];
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.StartingHead :=
            BoundaryValues.StartingHead * FCurrentParameter.Value;
//          BoundaryValues.StartAnnotation :=
//            BoundaryValues.StartAnnotation
//            + ' multiplied by the parameter value for "'+ FCurrentParameter.ParameterName + '."';
          BoundaryValues.StartAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.StartAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.HeadParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.HeadParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.HeadParameterName := '';
          BoundaryValues.HeadParameterValue := 1;
        end;
        Cell := TCHD_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values.TimeSeriesName := BoundaryValues.TimeSeriesName;
        Cell.Values.Cell := BoundaryValues.Cell;
        Cell.Values.StartingHead :=
          StartHeadFactor * BoundaryValues.StartingHead
          + (1 - StartHeadFactor) * BoundaryValues.EndingHead;
        Cell.Values.EndingHead :=
          EndHeadFactor * BoundaryValues.StartingHead
          + (1 - EndHeadFactor) * BoundaryValues.EndingHead;
        if StartHeadFactor = 1 then
        begin
          Cell.Values.StartAnnotation := BoundaryValues.StartAnnotation;
        end
        else
        begin
          Cell.Values.StartAnnotation := Format(FormatString,
            [BoundaryValues.StartingHead, BoundaryValues.StartingTime,
            BoundaryValues.StartAnnotation, BoundaryValues.EndingHead,
            BoundaryValues.EndingTime, BoundaryValues.EndAnnotation]);
        end;
        if EndHeadFactor = 0 then
        begin
          Cell.Values.EndAnnotation := BoundaryValues.EndAnnotation;
        end
        else
        begin
          Cell.Values.EndAnnotation := Format(FormatString,
            [BoundaryValues.StartingHead, BoundaryValues.StartingTime,
            BoundaryValues.StartAnnotation, BoundaryValues.EndingHead,
            BoundaryValues.EndingTime, BoundaryValues.EndAnnotation]);
        end;
        Cell.ScreenObject := ScreenObject;
        // don't move CHD cells away from the edge of child  model grids.
//        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TChdBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TChdCollection;
end;

constructor TChdBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  Interp := mimLinearEnd;
end;

procedure TChdBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TChdStorage;
  ParamIndex: Integer;
  Param: TChdParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  LocalModel: TCustomModel;
//  BoundaryList: TList;
//  StressPeriods: TModflowStressPeriods;
//  StartTime: Double;
//  EndTime: Double;
//  TimeCount: Integer;
//  ItemIndex: Integer;
//  TimeSeriesList: TTimeSeriesList;
//  TimeSeries: TTimeSeries;
//  SeriesIndex: Integer;
//  InitialTime: Double;
begin
  LocalModel := AModel as TCustomModel;
  FCurrentParameter := nil;
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TChdStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex] as TChdParamItem;
    ParamName := Param.Param.ParamName;
    if LocalModel.ModelSelection = msModflow2015 then
    begin
      FCurrentParameter := LocalModel.ModflowTransientParameters.GetParamByName(ParamName);
    end
    else
    begin
      FCurrentParameter := nil;
    end;
    Position := ParamList.IndexOf(ParamName);
    if Position < 0 then
    begin
      Times := TObjectList.Create;
      ParamList.AddObject(ParamName, Times);
    end
    else
    begin
      Times := ParamList.Objects[Position] as TList;
    end;

//    if FCurrentParameter <> nil then
//    begin
//      BoundaryList := Param.Param.BoundaryList[AModel];
//      StressPeriods := (AModel as TCustomModel).ModflowFullStressPeriods;
//      StartTime := StressPeriods.First.StartTime;
//      EndTime := StressPeriods.Last.EndTime;
//      TimeCount := BoundaryList.Count;
//      if TimeCount = 0 then
//      begin
//        frmErrorsAndWarnings.AddError(LocalModel, StrNoValidCHDBoundar,
//          Format(StrNoValidCHDBoundar2, [FCurrentParameter.ParameterName]));
//        Exit;
//      end;
//      for ItemIndex := 0 to BoundaryList.Count - 1 do
//      begin
//        BoundaryStorage := BoundaryList[ItemIndex];
//        if BoundaryStorage.StartingTime > StartTime then
//        begin
//          Inc(TimeCount);
//        end;
//        StartTime := BoundaryStorage.EndingTime;
//      end;
//      BoundaryStorage := BoundaryList.Last;
//      if BoundaryStorage.EndingTime <= EndTime then
//      begin
//        Inc(TimeCount);
//      end;
//
//      TimeSeriesList := FCurrentParameter.TimeSeriesList;
//      TimeSeries := TTimeSeries.Create;
//      TimeSeriesList.Add(TimeSeries);
//      TimeSeries.SeriesCount := Length(BoundaryStorage.ChdArray);
//      TimeSeries.TimeCount := TimeCount;
//      TimeSeries.ParameterName := FCurrentParameter.ParameterName;
//      TimeSeries.ObjectName := (ScreenObject as TScreenObject).Name;
//      for SeriesIndex := 0 to Length(BoundaryStorage.ChdArray) - 1 do
//      begin
//        TimeSeries.SeriesNames[SeriesIndex] :=
//          Format('%0:s_%1d_%2:d', [TimeSeries.ParameterName,
//          TimeSeriesList.Count, SeriesIndex+1]);
//        TimeSeries.InterpolationMethods[SeriesIndex] := Interp;
//        TimeSeries.ScaleFactors[SeriesIndex] := FCurrentParameter.Value;
//      end;
//
//      TimeCount := 0;
//      StartTime := StressPeriods.First.StartTime;
//      InitialTime := StartTime;
//      for ItemIndex := 0 to BoundaryList.Count - 1 do
//      begin
//        BoundaryStorage := BoundaryList[ItemIndex];
//        if BoundaryStorage.StartingTime > StartTime then
//        begin
//          TimeSeries.Times[TimeCount] := StartTime - InitialTime;
//          for SeriesIndex := 0 to Length(BoundaryStorage.ChdArray) - 1 do
//          begin
//            if ItemIndex > 0 then
//            begin
//              TimeSeries.Values[SeriesIndex,TimeCount] := NoData;
//            end
//            else
//            begin
//              TimeSeries.Values[SeriesIndex,TimeCount] :=
//                BoundaryStorage.ChdArray[SeriesIndex].StartingHead;
//            end;
//          end;
//          Inc(TimeCount);
//        end;
//        TimeSeries.Times[TimeCount] := BoundaryStorage.StartingTime - InitialTime;
//        for SeriesIndex := 0 to Length(BoundaryStorage.ChdArray) - 1 do
//        begin
//          TimeSeries.Values[SeriesIndex,TimeCount] :=
//            BoundaryStorage.ChdArray[SeriesIndex].StartingHead;
//          BoundaryStorage.ChdArray[SeriesIndex].TimeSeriesName :=
//            TimeSeries.SeriesNames[SeriesIndex];
//        end;
//        StartTime := BoundaryStorage.EndingTime;
//        Inc(TimeCount);
//      end;
//      BoundaryStorage := BoundaryList.Last;
//      if BoundaryStorage.EndingTime <= EndTime then
//      begin
//        TimeSeries.Times[TimeCount] := EndTime - InitialTime;
//        for SeriesIndex := 0 to Length(BoundaryStorage.ChdArray) - 1 do
//        begin
//          TimeSeries.Values[SeriesIndex,TimeCount] :=
//            BoundaryStorage.ChdArray[SeriesIndex].EndingHead;
//        end;
//      end;
//    end;


    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      if ValueIndex < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TChdStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

procedure TChdBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfChdStartingHead(self);
    Model.InvalidateMfChdEndingHead(self);
  end;
end;

class function TChdBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TChdParamItem;
end;

function TChdBoundary.ParameterType: TParameterType;
begin
  result := ptCHD;
end;

//procedure TChdBoundary.SetInterp(const Value: TMf6InterpolationMethods);
//begin
//  if FInterp <> Value then
//  begin
//    InvalidateModel;
//    FInterp := Value;
//  end;
//end;

{ TCHD_Cell }

procedure TCHD_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TCHD_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TCHD_Cell.GetEndingHead: double;
begin
  result := Values.EndingHead;
end;

function TCHD_Cell.GetEndingHeadAnnotation: string;
begin
  result := Values.EndAnnotation;
end;

function TCHD_Cell.GetHeadParameterName: string;
begin
  result := Values.HeadParameterName;
end;

function TCHD_Cell.GetHeadParameterValue: double;
begin
  result := Values.HeadParameterValue;
end;

function TCHD_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TCHD_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TCHD_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TCHD_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    StartHeadPosition: result := StartingHeadAnnotation;
    EndHeadPosition: result := EndingHeadAnnotation;
    else Assert(False);
  end;
end;

function TCHD_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    StartHeadPosition: result := StartingHead;
    EndHeadPosition: result := EndingHead;
    else Assert(False);
  end;
end;

function TCHD_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TCHD_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TCHD_Cell.GetStartingHead: double;
begin
  result := Values.StartingHead;
end;

function TCHD_Cell.GetStartingHeadAnnotation: string;
begin
  result := Values.StartAnnotation;
end;

function TCHD_Cell.GetTimeSeriesName: string;
begin
  result := Values.TimeSeriesName;
end;

function TCHD_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  CHD_Cell: TCHD_Cell;
begin
  result := AnotherCell is TCHD_Cell;
  if result then
  begin
    CHD_Cell := TCHD_Cell(AnotherCell);
    result :=
      (StartingHead = CHD_Cell.StartingHead)
      and (EndingHead = CHD_Cell.EndingHead)
      and (IFace = CHD_Cell.IFace)
      and (Values.Cell = CHD_Cell.Values.Cell);
//      and (EndingHead = StartingHead);
  end;
end;

procedure TCHD_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TCHD_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TCHD_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TCHD_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TCHD_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TChdRecord }

procedure TChdRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingHead);
  WriteCompReal(Comp, EndingHead);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, HeadParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(StartAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EndAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(TimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(HeadParameterName));
end;

procedure TChdRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(StartAnnotation);
  Strings.Add(EndAnnotation);
  Strings.Add(TimeSeriesName);
  Strings.Add(HeadParameterName);
end;

procedure TChdRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StartingHead := ReadCompReal(Decomp);
  EndingHead := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  HeadParameterValue := ReadCompReal(Decomp);
  StartAnnotation := Annotations[ReadCompInt(Decomp)];
  EndAnnotation := Annotations[ReadCompInt(Decomp)];
  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  HeadParameterName := Annotations[ReadCompInt(Decomp)];
end;

{ TChdStorage }

procedure TChdStorage.Clear;
begin
  SetLength(FChdArray, 0);
  FCleared := True;
end;

procedure TChdStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Count := Length(FChdArray);
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    for Index := 0 to Count - 1 do
    begin
      FChdArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FChdArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TChdStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FChdArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FChdArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TChdStorage.GetChdArray: TChdArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FChdArray;

end;

{ TChdTimeListLink }

procedure TChdTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  FStartData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEndData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStartData.NonParamDescription := StrStartingHead;
  FStartData.ParamDescription := StrStartingHeadMulti;
  FEndData.NonParamDescription := StrEndingHead;
  FEndData.ParamDescription := StrEndingHeadMultipl;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FStartData.OnInvalidate := LocalModel.InvalidateMfChdStartingHead;
    FEndData.OnInvalidate := LocalModel.InvalidateMfChdEndingHead;
  end;
  AddTimeList(FStartData);
  AddTimeList(FEndData);
end;

destructor TChdTimeListLink.Destroy;
begin
  FStartData.Free;
  FEndData.Free;
  inherited;
end;

end.
