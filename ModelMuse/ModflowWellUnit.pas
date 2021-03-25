unit ModflowWellUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit, RealListUnit;

type
  {
    @longcode(
  TWellRecord = record
    Cell: TCellLocation;
    PumpingRate: double;
    StartingTime: double;
    EndingTime: double;
    PumpingAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
    )
    @name stores, the location, time and pumping rate for a well boundary.
  }
  TWellRecord = record
    Cell: TCellLocation;
    PumpingRate: double;
    StartingTime: double;
    EndingTime: double;
    PumpingRateAnnotation: string;
    PumpingRatePest: string;
    PumpingRatePestSeriesName: string;
    PumpingRatePestSeriesMethod: TPestParamMethod;
    TimeSeriesName: string;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    PumpingParameterName: string;
    PumpingParameterValue: double;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TWellRecord)s.
  TWellArray = array of TWellRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of wells.
  TWellStorage = class(TCustomBoundaryStorage)
  private
    FWellArray: TWellArray;
    function GetWellArray: TWellArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property WellArray: TWellArray read GetWellArray;
  end;

  // @name represents a MODFLOW well for one time interval.
  // @name is stored by @link(TWellCollection).
  TWellItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(PumpingRate).
    FPumpingRate: TFormulaObject;
    // See @link(PumpingRate).
    procedure SetPumpingRate(const Value: string);
    function GetPumpingRate: string;
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
  published
    // @name is the formula used to set the pumping rate
    // or the pumping rate multiplier of this boundary.
    property PumpingRate: string read GetPumpingRate write SetPumpingRate;
  end;

  TMfWelTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the pumping rates for a series of
    // Wells over a series of time intervals.
    FPumpingRateData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Well boundaries
  // for a series of time intervals.
  TWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidatePumpingRateData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TWellStorage.WellArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod); override;
  end;

  // Each @name stores a @link(TWellCollection).
  // @classname is stored by @link(TModflowParameters).
  TWellParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TWell_Cell = class(TValueCell)
  private
    Values: TWellRecord;
    StressPeriod: integer;
    function GetPumpingRate: double;
    function GetPumpingRateAnnotation: string;
    function GetTimeSeriesName: string;
    function GetMvrUsed: Boolean;
    function GetMvrIndex: Integer;
    function GetPumpingParameterName: string;
    function GetPumpingParameterValue: double;
    function GetPumpingRatePest: string;
    function GetPumpingRatePestSeriesMethod: TPestParamMethod;
    function GetPumpingRatePestSeriesName: string;
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
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
  public
    property PumpingRate: double read GetPumpingRate;
    property TimeSeriesName: string read GetTimeSeriesName;
    property PumpingRateAnnotation: string read GetPumpingRateAnnotation;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property PumpingParameterName: string read GetPumpingParameterName;
    property PumpingParameterValue: double read GetPumpingParameterValue;
    // PEST parameters
    property PumpingRatePest: string read GetPumpingRatePest;
    property PumpingRatePestSeries: string read GetPumpingRatePestSeriesName;
    property PumpingRatePestSeriesMethod: TPestParamMethod read GetPumpingRatePestSeriesMethod;
  end;

  // @name represents the MODFLOW Well boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TWellItem.PumpingRate
  // TWellItem.PumpingRate) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Pumping Rate / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Pumping Rate.)
  // )
  // @seealso(TWellCollection)
  TMfWellBoundary = class(TSpecificModflowBoundary)
  private
    FMaxTabCells: Integer;
    FTabFileName: string;
    FRelativeTabFileName: string;
    FTabFileLines: Integer;
    FPestPumpingRateMethod: TPestParamMethod;
    FPestPumpingRateFormula: TFormulaObject;
    FPestPumpingRateObserver: TObserver;
    FUsedObserver: TObserver;
    procedure SetTabFileName(const Value: string);
    function GetRelativeTabFileName: string;
    procedure SetRelativeTabFileName(const Value: string);
    procedure SetTabFileLines(const Value: Integer);
    function GetPestPumpingRateFormula: string;
    procedure SetPestPumpingRateFormula(const Value: string);
    procedure SetPestPumpingRateMethod(const Value: TPestParamMethod);
    function GetPestPumpingRateObserver: TObserver;
    procedure InvalidatePumpingRateData(Sender: TObject);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TWell_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    property PestPumpingRateObserver: TObserver
      read GetPestPumpingRateObserver;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    property MaxTabCells: Integer read FMaxTabCells;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TWellStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Well parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TWellStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    procedure UpdateTimes(Times: TRealList; StartTestTime, EndTestTime: double;
      var StartRangeExtended, EndRangeExtended: boolean; AModel: TBaseModel); override;
    property TabFileName: string read FTabFileName write SetTabFileName;
    property TabFileLines: Integer read FTabFileLines write SetTabFileLines;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property RelativeTabFileName: string read GetRelativeTabFileName
      write SetRelativeTabFileName;
    property Interp;
    property PestPumpingRateFormula: string read GetPestPumpingRateFormula
      write SetPestPumpingRateFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestPumpingRateMethod: TPestParamMethod read FPestPumpingRateMethod
      write SetPestPumpingRateMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
  end;

resourcestring
  StrWellFormulaError = 'Pumping rate set to zero because of a math error';

const
  WelPumpingRatePosition = 0;

implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, frmErrorsAndWarningsUnit,
  ModflowTimeSeriesUnit, ModflowMvrUnit;

resourcestring
  StrPumpingRateMultip = ' pumping rate multiplier';

{ TWellItem }

procedure TWellItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TWellItem then
  begin
    PumpingRate := TWellItem(Source).PumpingRate;
  end;
  inherited;
end;

procedure TWellItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TWellCollection;
  PumpingRateObserver: TObserver;
begin
  ParentCollection := Collection as TWellCollection;
  PumpingRateObserver := FObserverList[WelPumpingRatePosition];
  PumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidatePumpingRateData;
end;

function TWellItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TWellItem.CreateFormulaObjects;
begin
  FPumpingRate := CreateFormulaObject(dso3D);
end;

destructor TWellItem.Destroy;
begin
  PumpingRate := '0';
  inherited;
end;

function TWellItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRate;
    else Assert(False);
  end;
end;

procedure TWellItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FPumpingRate);
  List.Add(FObserverList[WelPumpingRatePosition]);
end;

function TWellItem.GetPumpingRate: string;
begin
  Result := FPumpingRate.Formula;
  ResetItemObserver(WelPumpingRatePosition);
end;

procedure TWellItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TWellItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TWellItem;
begin
  result := (AnotherItem is TWellItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TWellItem(AnotherItem);
    result := (Item.PumpingRate = PumpingRate)
  end;
end;

procedure TWellItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TWellItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    WelPumpingRatePosition: PumpingRate := Value;
    else Assert(False);
  end;
end;

procedure TWellItem.SetPumpingRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, WelPumpingRatePosition, FPumpingRate);
end;

{ TWellCollection }

procedure TWellCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TWellStorage.Create(AModel));
end;

procedure TWellCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod);
var
  WellStorage: TWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  LocalScreenObject: TScreenObject;
begin
  Assert(BoundaryFunctionIndex = 0);
  Assert(Expression <> nil);

  WellStorage := BoundaryStorage as TWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    try
      Expression.Evaluate;
      with WellStorage.WellArray[Index] do
      begin
        PumpingRate := Expression.DoubleResult;
        PumpingRateAnnotation := ACell.Annotation;
        PumpingRatePest := PestName;
        PumpingRatePestSeriesName := PestSeriesName;
        PumpingRatePestSeriesMethod := PestSeriesMethod;
      end;
    except on E: EMathError do
      begin
        with WellStorage.WellArray[Index] do
        begin
          PumpingRate := 0;
          PumpingRateAnnotation := StrWellFormulaError;
          PumpingRatePest := PestName;
          PumpingRatePestSeriesName := PestSeriesName;
          PumpingRatePestSeriesMethod := PestSeriesMethod;
        end;
        LocalScreenObject := ScreenObject as TScreenObject;

        frmErrorsAndWarnings.AddError(AModel, StrWellFormulaError,
          Format(StrObject0sLayerError,
          [LocalScreenObject.Name, ACell.Layer+1, ACell.Row+1,
          ACell.Column+1, E.Message]), LocalScreenObject);
      end;
    end;
  end;
end;

procedure TWellCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  WellStorage: TWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  WellStorage := BoundaryStorage as TWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with WellStorage.WellArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

function TWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMfWelTimeListLink;
end;

function TWellCollection.AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
var
  Boundary: TMfWellBoundary;
  Item: TWellItem;
  ScreenObject: TScreenObject;
begin
  result := '';
  if FormulaIndex = 0 then
  begin
    Boundary := BoundaryGroup as TMfWellBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    Item := Items[ItemIndex] as TWellItem;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.PumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.PumpingRate
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.PumpingRate
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.PumpingRate;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.PumpingRate;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.PumpingRate
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.PumpingRate
              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TWellCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

procedure TWellCollection.InvalidatePumpingRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TMfWelTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    Link := TimeListLink.GetLink(PhastModel) as TMfWelTimeListLink;
    Link.FPumpingRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TMfWelTimeListLink;
      Link.FPumpingRateData.Invalidate;
    end;
  end;
end;

class function TWellCollection.ItemClass: TBoundaryItemClass;
begin
  result := TWellItem;
end;

procedure TWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TWellStorage).FWellArray, BoundaryCount);
  inherited;
end;

{ TWellParamItem }

class function TWellParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TWellCollection;
end;

{ TWell_Cell }

procedure TWell_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TWell_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TWell_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TWell_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TWell_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TWell_Cell.GetMvrIndex: Integer;
begin
  result := Values.MvrIndex;
end;

function TWell_Cell.GetMvrUsed: Boolean;
begin
  result := Values.MvrUsed;
end;

function TWell_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRatePest;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TWell_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRatePestSeriesMethod;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TWell_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRatePestSeries;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TWell_Cell.GetPumpingParameterName: string;
begin
  result := Values.PumpingParameterName;
end;

function TWell_Cell.GetPumpingParameterValue: double;
begin
  result := Values.PumpingParameterValue;
end;

function TWell_Cell.GetPumpingRate: double;
begin
  result := Values.PumpingRate;
end;

function TWell_Cell.GetPumpingRateAnnotation: string;
begin
  result := Values.PumpingRateAnnotation;
end;

function TWell_Cell.GetPumpingRatePest: string;
begin
  result := Values.PumpingRatePest;
end;

function TWell_Cell.GetPumpingRatePestSeriesMethod: TPestParamMethod;
begin
  result := Values.PumpingRatePestSeriesMethod;
end;

function TWell_Cell.GetPumpingRatePestSeriesName: string;
begin
  result := Values.PumpingRatePestSeriesName;
end;

function TWell_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    WelPumpingRatePosition: result := PumpingRateAnnotation;
    else Assert(False);
  end;
end;

function TWell_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    WelPumpingRatePosition: result := PumpingRate;
    else Assert(False);
  end;
end;

function TWell_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TWell_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TWell_Cell.GetTimeSeriesName: string;
begin
  result := Values.TimeSeriesName;
end;

function TWell_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  WEL_Cell: TWell_Cell;
begin
  result := AnotherCell is TWell_Cell;
  if result then
  begin
    WEL_Cell := TWell_Cell(AnotherCell);
    result :=
      (PumpingRate = WEL_Cell.PumpingRate)
      and (IFace = WEL_Cell.IFace)
      and (Values.Cell = WEL_Cell.Values.Cell);
  end;
end;

procedure TWell_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TWell_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TWell_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TWell_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TWell_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TMfWellBoundary }

procedure TMfWellBoundary.Assign(Source: TPersistent);
var
  SourceWell: TMfWellBoundary;
begin
  if Source is TMfWellBoundary then
  begin
    SourceWell := TMfWellBoundary(Source);
    RelativeTabFileName := SourceWell.RelativeTabFileName;
    PestPumpingRateFormula := SourceWell.PestPumpingRateFormula;
    PestPumpingRateMethod := SourceWell.PestPumpingRateMethod;
  end;
  inherited;
end;

procedure TMfWellBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TWell_Cell;
  BoundaryValues: TWellRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TWellStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
  MvrUsed: Boolean;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TWellStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcWel);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TWell_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.WellArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.WellArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.WellArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.WellArray[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed;
        BoundaryValues.MvrIndex := BoundaryIndex;
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.PumpingRate :=
            BoundaryValues.PumpingRate * FCurrentParameter.Value;
          BoundaryValues.PumpingRateAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.PumpingRateAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.PumpingParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.PumpingParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.PumpingParameterName := '';
          BoundaryValues.PumpingParameterValue := 1;
        end;
        Cell := TWell_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TMfWellBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TWellCollection;
end;

function TMfWellBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestWel_';
end;

constructor TMfWellBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestPumpingRateFormula := '';
  FPestPumpingRateMethod := DefaultBoundaryMethod(WelPumpingRatePosition);
end;

procedure TMfWellBoundary.CreateFormulaObjects;
begin
  FPestPumpingRateFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TMfWellBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestPumpingRateObserver);
  end;
end;

class function TMfWellBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

destructor TMfWellBoundary.Destroy;
begin
  PestPumpingRateFormula := '';

  inherited;
end;

procedure TMfWellBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TWellStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  LocalModel: TCustomModel;
//  TimeSeriesList: TTimeSeriesList;
//  BoundaryList: TList;
//  TimeSeries: TTimeSeries;
//  StartTime: Double;
//  StressPeriods: TModflowStressPeriods;
//  EndTime: Double;
//  TimeCount: Integer;
//  ItemIndex: Integer;
//  SeriesIndex: Integer;
//  InitialTime: Double;
begin
  FCurrentParameter := nil;
//  EvaluateArrayBoundaries;
  EvaluateListBoundaries(AModel);
  LocalModel := AModel as TCustomModel;
  FMaxTabCells := 0;
  if LocalModel.ModflowPackages.WelPackage.UseTabFilesInThisModel then
  begin
    if (Values.Count > 0) and (Values.BoundaryCount[AModel] > 0) then
    begin
      BoundaryStorage := Values.Boundaries[0, AModel] as TWellStorage;
      FMaxTabCells := Length(BoundaryStorage.WellArray);
      BoundaryStorage.CacheData;
    end;
  end;
//  else
//  begin
    for ValueIndex := 0 to Values.Count - 1 do
    begin
      if ValueIndex < Values.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TWellStorage;
        AssignCells(BoundaryStorage, ValueTimeList, AModel);
      end;
    end;
//  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
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
//      TimeSeries.SeriesCount := Length(BoundaryStorage.WellArray);
//      TimeSeries.TimeCount := TimeCount;
//      TimeSeries.ParameterName := FCurrentParameter.ParameterName;
//      TimeSeries.ObjectName := (ScreenObject as TScreenObject).Name;
//      for SeriesIndex := 0 to Length(BoundaryStorage.WellArray) - 1 do
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
//          for SeriesIndex := 0 to Length(BoundaryStorage.WellArray) - 1 do
//          begin
//            if ItemIndex > 0 then
//            begin
//              TimeSeries.Values[SeriesIndex,TimeCount] := NoData;
//            end
//            else
//            begin
//              TimeSeries.Values[SeriesIndex,TimeCount] :=
//                BoundaryStorage.WellArray[SeriesIndex].PumpingRate;
//            end;
//          end;
//          Inc(TimeCount);
//        end;
//        TimeSeries.Times[TimeCount] := BoundaryStorage.StartingTime - InitialTime;
//        for SeriesIndex := 0 to Length(BoundaryStorage.WellArray) - 1 do
//        begin
//          TimeSeries.Values[SeriesIndex,TimeCount] :=
//            BoundaryStorage.WellArray[SeriesIndex].PumpingRate;
//          BoundaryStorage.WellArray[SeriesIndex].TimeSeriesName :=
//            TimeSeries.SeriesNames[SeriesIndex];
//        end;
//        StartTime := BoundaryStorage.EndingTime;
//        Inc(TimeCount);
//      end;
//      BoundaryStorage := BoundaryList.Last;
//      if BoundaryStorage.EndingTime <= EndTime then
//      begin
//        TimeSeries.Times[TimeCount] := EndTime - InitialTime;
//        for SeriesIndex := 0 to Length(BoundaryStorage.WellArray) - 1 do
//        begin
//          TimeSeries.Values[SeriesIndex,TimeCount] :=
//            BoundaryStorage.WellArray[SeriesIndex].PumpingRate;
//        end;
//      end;
//    end;

    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      if ValueIndex < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TWellStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

function TMfWellBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        result := PestPumpingRateFormula;
      end;
    else
      Assert(False);
  end;
end;

function TMfWellBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        result := PestPumpingRateMethod;
      end;
    else
      result := PestPumpingRateMethod;
      Assert(False);
  end;
end;

function TMfWellBoundary.GetPestPumpingRateFormula: string;
begin
  Result := FPestPumpingRateFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(WelPumpingRatePosition);
  end;
end;

function TMfWellBoundary.GetPestPumpingRateObserver: TObserver;
begin
  if FPestPumpingRateObserver = nil then
  begin
    CreateObserver('PestPumpingRate_', FPestPumpingRateObserver, nil);
    FPestPumpingRateObserver.OnUpToDateSet := InvalidatePumpingRateData;
  end;
  result := FPestPumpingRateObserver;
end;

procedure TMfWellBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestPumpingRateFormula then
  begin
    if WelPumpingRatePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[WelPumpingRatePosition]);
    end;
  end;
end;

function TMfWellBoundary.GetRelativeTabFileName: string;
var
  ModelFileName: string;
begin
  if ParentModel <> nil then
  begin
    ModelFileName := (ParentModel as TPhastModel).ModelFileName;
    FRelativeTabFileName := ExtractRelativePath(ModelFileName, FTabFileName);
  end
  else
  begin
    FRelativeTabFileName := FTabFileName;
  end;
  result := FRelativeTabFileName;
end;

function TMfWellBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestWell_Used_', FUsedObserver, nil);
//    FUsedObserver.OnUpToDateSet := HandleChangedValue;
  end;
  result := FUsedObserver;
end;

procedure TMfWellBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TMfWellBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    (ParentModel as TPhastModel).InvalidateMfWellPumpage(self);
  end;
end;

procedure TMfWellBoundary.InvalidatePumpingRateData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
//  if ParentModel = nil then
//  begin
//    Exit;
//  end;
//  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    if PhastModel.Clearing then
    begin
      Exit;
    end;
    PhastModel.InvalidateMfWellPumpage(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfWellPumpage(self);
    end;
  end;
end;

class function TMfWellBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TWellParamItem;
end;

function TMfWellBoundary.ParameterType: TParameterType;
begin
  result := ptQ;
end;

procedure TMfWellBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        PestPumpingRateFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TMfWellBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        PestPumpingRateMethod := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TMfWellBoundary.SetPestPumpingRateFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, WelPumpingRatePosition, FPestPumpingRateFormula);
end;

procedure TMfWellBoundary.SetPestPumpingRateMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestPumpingRateMethod, Value);
end;

procedure TMfWellBoundary.SetRelativeTabFileName(const Value: string);
begin
  if FRelativeTabFileName <> Value then
  begin
    InvalidateModel;
    FRelativeTabFileName := Value;
    FTabFileName := ExpandFileName(FRelativeTabFileName);
  end;
end;

procedure TMfWellBoundary.SetTabFileLines(const Value: Integer);
begin
  FTabFileLines := Value;
end;

procedure TMfWellBoundary.SetTabFileName(const Value: string);
begin
  if FTabFileName <> Value then
  begin
    InvalidateModel;
    FTabFileName := Value;
  end;
end;

procedure TMfWellBoundary.UpdateTimes(Times: TRealList; StartTestTime,
  EndTestTime: double; var StartRangeExtended, EndRangeExtended: boolean;
    AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  ParamIndex: Integer;
  Param: TModflowParamItem;
begin
  LocalModel := AModel as TCustomModel;
  if LocalModel.ModflowPackages.WelPackage.UseTabFilesInThisModel then
  begin
    // tab files only apply to non parameter boundaries.
    for ParamIndex := 0 to Parameters.Count - 1 do
    begin
      Param := Parameters[ParamIndex];
      AddBoundaryTimes(Param.Param, Times, StartTestTime, EndTestTime,
        StartRangeExtended, EndRangeExtended);
    end;
  end
  else
  begin
    inherited;
  end;


end;

{ TWellRecord }

procedure TWellRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, PumpingRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, PumpingParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(PumpingRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(PumpingRatePest));
  WriteCompInt(Comp, Strings.IndexOf(PumpingRatePestSeriesName));
  WriteCompInt(Comp, Ord(PumpingRatePestSeriesMethod));
  WriteCompInt(Comp, Strings.IndexOf(TimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(PumpingParameterName));
  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(PumpingRateAnnotation);
  Strings.Add(PumpingRatePest);
  Strings.Add(PumpingRatePestSeriesName);
  Strings.Add(TimeSeriesName);
  Strings.Add(PumpingParameterName);
end;

procedure TWellRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  PumpingRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  PumpingParameterValue := ReadCompReal(Decomp);
  PumpingRateAnnotation := Annotations[ReadCompInt(Decomp)];
  PumpingRatePest := Annotations[ReadCompInt(Decomp)];
  PumpingRatePestSeriesName := Annotations[ReadCompInt(Decomp)];
  PumpingRatePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  PumpingParameterName := Annotations[ReadCompInt(Decomp)];
  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);
end;

{ TWellStorage }

procedure TWellStorage.Clear;
begin
  SetLength(FWellArray, 0);
  FCleared := True;
end;

procedure TWellStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FWellArray);
    for Index := 0 to Count - 1 do
    begin
      FWellArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FWellArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TWellStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FWellArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FWellArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TWellStorage.GetWellArray: TWellArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FWellArray;
end;

{ TMfWelTimeListLink }

procedure TMfWelTimeListLink.CreateTimeLists;
begin
  inherited;
  FPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPumpingRateData.NonParamDescription := StrPumpingRate;
  FPumpingRateData.ParamDescription := StrPumpingRateMultip;
  if Model <> nil then
  begin
    FPumpingRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FPumpingRateData);
end;

destructor TMfWelTimeListLink.Destroy;
begin
  FPumpingRateData.Free;
  inherited;
end;

end.
