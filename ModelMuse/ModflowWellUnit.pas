unit ModflowWellUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit, RealListUnit, System.Generics.Collections,
  Modflow6DynamicTimeSeriesInterfaceUnit, Modflow6TimeSeriesInterfaceUnit;

type
  {
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
    MvrUsed: Boolean;
    MvrIndex: Integer;
    PumpingParameterName: string;
    PumpingParameterValue: double;
    PumpingRateTimeSeriesName: string;
    // GWT Concentrations
    GwtConcentrations: TGwtCellData;
    procedure Assign(const Item: TWellRecord);
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

  TWellCollection = class;

  TWelGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TWellCollection);
  end;

  // @name represents a MODFLOW well for one time interval.
  // @name is stored by @link(TWellCollection).
  TWellItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(PumpingRate).
    FPumpingRate: IFormulaObject;
    FGwtConcentrations: TWelGwtConcCollection;
    // See @link(PumpingRate).
    procedure SetPumpingRate(const Value: string);
    function GetPumpingRate: string;
    procedure SetGwtConcentrations(const Value: TWelGwtConcCollection);
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
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
  published
    // @name is the formula used to set the pumping rate
    // or the pumping rate multiplier of this boundary.
    property PumpingRate: string read GetPumpingRate write SetPumpingRate;
    property GwtConcentrations: TWelGwtConcCollection read FGwtConcentrations
      write SetGwtConcentrations;
  end;

  TMfWelTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the pumping rates for a series of
    // Wells over a series of time intervals.
    FPumpingRateData: TModflowTimeList;
    FConcList: TModflowTimeLists;
    procedure AddGwtTimeLists(SpeciesIndex: Integer);
    procedure RemoveGwtTimeLists(SpeciesIndex: Integer);
  protected
    procedure CreateTimeLists; override;
    procedure UpdateGwtTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Well boundaries
  // for a series of time intervals.
  TWellCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidatePumpingRateData(Sender: TObject);
    procedure InvalidateGwtConcentrations(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
  end;

  // Each @name stores a @link(TWellCollection).
  // @classname is stored by @link(TModflowParameters).
  TWellParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TWell_Cell = class(TValueCell)
  private
    FValues: TWellRecord;
    StressPeriod: integer;
    function GetPumpingRate: double;
    function GetPumpingRateAnnotation: string;
    function GetMvrUsed: Boolean;
    function GetMvrIndex: Integer;
    function GetPumpingParameterName: string;
    function GetPumpingParameterValue: double;
    function GetPumpingRatePest: string;
    function GetPumpingRatePestSeriesMethod: TPestParamMethod;
    function GetPumpingRatePestSeriesName: string;
    function GetPumpingRateTimeSeriesName: string;
    procedure SetPumpingRateTimeSeriesName(const Value: string);
    function GetGwtConcentrations: TGwtCellData;
  protected
    property Values: TWellRecord read FValues;
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
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    property PumpingRate: double read GetPumpingRate;
    property PumpingRateAnnotation: string read GetPumpingRateAnnotation;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property PumpingParameterName: string read GetPumpingParameterName;
    property PumpingParameterValue: double read GetPumpingParameterValue;
    // PEST parameters
    property PumpingRatePest: string read GetPumpingRatePest;
    property PumpingRatePestSeries: string read GetPumpingRatePestSeriesName;
    property PumpingRatePestSeriesMethod: TPestParamMethod
      read GetPumpingRatePestSeriesMethod;
    // Time Series
    property PumpingRateTimeSeriesName: string read GetPumpingRateTimeSeriesName
      write SetPumpingRateTimeSeriesName;
    // GWT
    property GwtConcentrations: TGwtCellData read GetGwtConcentrations;
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
    FPestPumpingRateFormula: IFormulaObject;
    FPestPumpingRateObserver: TObserver;
    FUsedObserver: TObserver;
    FPestConcentrationFormulas: TWelGwtConcCollection;
    FPestConcentrationMethods: TGwtPestMethodCollection;
    FConcentrationObservers: TObserverList;
    procedure SetTabFileName(const Value: string);
    function GetRelativeTabFileName: string;
    procedure SetRelativeTabFileName(const Value: string);
    procedure SetTabFileLines(const Value: Integer);
    function GetPestPumpingRateFormula: string;
    procedure SetPestPumpingRateFormula(const Value: string);
    procedure SetPestPumpingRateMethod(const Value: TPestParamMethod);
    function GetPestPumpingRateObserver: TObserver;
    procedure InvalidatePumpingRateData(Sender: TObject);
    procedure InvalidateConcData(Sender: TObject);
    procedure SetPestConcentrationFormulas(const Value: TWelGwtConcCollection);
    procedure SetPestConcentrationMethods(const Value: TGwtPestMethodCollection);
    function GetConcentrationObserver(const Index: Integer): TObserver;
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
    property ConcentrationObserver[const Index: Integer]: TObserver
      read GetConcentrationObserver;
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
      write SetPestPumpingRateFormula;
    property PestPumpingRateMethod: TPestParamMethod read FPestPumpingRateMethod
      write SetPestPumpingRateMethod;
    property PestConcentrationFormulas: TWelGwtConcCollection
      read FPestConcentrationFormulas write SetPestConcentrationFormulas;
    property PestConcentrationMethods: TGwtPestMethodCollection
      read FPestConcentrationMethods write SetPestConcentrationMethods;
  end;

resourcestring
  StrWellFormulaError = 'Pumping rate set to zero because of a math error';
  StrWellConcentrationS = 'Well Concentration set to zero because of a math ' +
  'error';

const
  WelPumpingRatePosition = 0;
  WelStartConcentration = 1;

implementation

uses ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  frmGoPhastUnit, GIS_Functions, frmErrorsAndWarningsUnit,
  ModflowMvrUnit;

resourcestring
  StrPumpingRateMultip = ' pumping rate multiplier';
  StrWellPumpingRate = 'Well pumping rate';

{ TWellItem }

procedure TWellItem.Assign(Source: TPersistent);
var
  WellSource: TWellItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TWellItem then
  begin
    WellSource := TWellItem(Source);
    PumpingRate := WellSource.PumpingRate;
    GwtConcentrations := WellSource.GwtConcentrations;
  end;
  inherited;
end;

procedure TWellItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TWellCollection;
  PumpingRateObserver: TObserver;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TWellCollection;
  PumpingRateObserver := FObserverList[WelPumpingRatePosition];
  PumpingRateObserver.OnUpToDateSet := ParentCollection.InvalidatePumpingRateData;

  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    GwtConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateGwtConcentrations;
  end;
end;

function TWellItem.BoundaryFormulaCount: integer;
begin
  result := 1;
  if GwtConcentrations <> nil then
  begin
    if (Model <> nil) and Model.GwtUsed then
    begin
      GwtConcentrations.Count := (Model as TCustomModel).MobileComponents.Count;
    end;
    if frmGoPhast.PhastModel.GwtUsed then
    begin
      result := result + GwtConcentrations.Count;
    end;
  end;
end;

constructor TWellItem.Create(Collection: TCollection);
var
  WelCol: TWellCollection;
begin
  WelCol := Collection as TWellCollection;
  FGwtConcentrations := TWelGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    WelCol);
  inherited;
end;

procedure TWellItem.CreateFormulaObjects;
begin
  FPumpingRate := CreateFormulaObject(dso3D);
end;

destructor TWellItem.Destroy;
var
  Index: Integer;
begin
  PumpingRate := '0';
  for Index := 0 to FGwtConcentrations.Count - 1 do
  begin
    FGwtConcentrations[Index].Value := '0';
  end;
  FGwtConcentrations.Free;
  inherited;
end;

function TWellItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TGwtConcStringValueItem;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRate;
    else
      begin
        Dec(Index, 1);
        while GwtConcentrations.Count <= Index do
        begin
          GwtConcentrations.Add;
        end;
        Item := GwtConcentrations[Index];
        result := Item.Value;
      end;
  end;
end;

procedure TWellItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Item: TGwtConcStringValueItem;
  ConcIndex: Integer;
begin
  if (Sender = FPumpingRate as TObject) then
  begin
    List.Add(FObserverList[WelPumpingRatePosition]);
  end;
  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    Item := GwtConcentrations.Items[ConcIndex];
    if Item.ValueObject as TObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
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
    PhastModel.InvalidateMfWellConc(self);
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
      and (Item.GwtConcentrations.IsSame(GwtConcentrations));
  end;
end;

procedure TWellItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPumpingRate,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TWellItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TGwtConcStringValueItem;
begin
  inherited;
  case Index of
    WelPumpingRatePosition: PumpingRate := Value;
    else
      begin
        Dec(Index, 1);
        while Index >= GwtConcentrations.Count do
        begin
          GwtConcentrations.Add;
        end;
        Item := GwtConcentrations[Index];
        Item.Value := Value;
      end;
  end;
end;

procedure TWellItem.SetGwtConcentrations(const Value: TWelGwtConcCollection);
begin
  FGwtConcentrations.Assign(Value);
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

procedure TWellCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  WellStorage: TWellStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  LocalScreenObject: TScreenObject;
  ConcIndex: Integer;
  AllowedIndicies: Set of Byte;
  SpeciesIndex: Byte;
  LocalModel: TCustomModel;
  ErrorMessage: string;
  Expression: TExpression;
  ACellList: TObject;
  BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer;
  Variables, DataSets: TList;
  AModel: TBaseModel;
  AScreenObject: TObject;
  PestName: string;
  PestSeriesName: string;
  PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string;
  DynamicTimeSeries: IDynamicTimeSeries;
  Location: TTimeSeriesLocation;
  TimeSeries: IMf6TimeSeries;
begin
  Expression := CellAssignmentData.Expression;
  ACellList := CellAssignmentData.ACellList;
  BoundaryStorage := CellAssignmentData.BoundaryStorage;
  BoundaryFunctionIndex := CellAssignmentData.BoundaryFunctionIndex;
  Variables := CellAssignmentData.Variables;
  DataSets := CellAssignmentData.DataSets;
  AModel := CellAssignmentData.AModel;
  AScreenObject := CellAssignmentData.AScreenObject;
  PestName := CellAssignmentData.PestName;
  PestSeriesName := CellAssignmentData.PestSeriesName;
  PestSeriesMethod := CellAssignmentData.PestSeriesMethod;
  TimeSeriesName := CellAssignmentData.TimeSeriesName;
  DynamicTimeSeries := CellAssignmentData.DynamicTimeSeries;

  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);
  AllowedIndicies := [0];
  LocalModel := AModel as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      Include(AllowedIndicies, WelStartConcentration + SpeciesIndex);
    end;
  end;

  Assert(BoundaryFunctionIndex in AllowedIndicies);
  Assert(Expression <> nil);

  WellStorage := BoundaryStorage as TWellStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];

    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // Handle dynamic time series here.

    if DynamicTimeSeries <> nil then
    begin
      Location.Layer := ACell.Layer;
      Location.Row := ACell.Row;
      Location.Column := ACell.Column;
      TimeSeries := DynamicTimeSeries.StaticTimeSeries[Location];
      BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeries.SeriesName);
      TimeSeriesName := TimeSeries.SeriesName;
    end;

    // 2. update locations
    try
      Expression.Evaluate;
      case BoundaryFunctionIndex of
        WelPumpingRatePosition:
          begin
            with WellStorage.WellArray[Index] do
            begin
              PumpingRate := Expression.DoubleResult;
              PumpingRateAnnotation := ACell.Annotation;
              PumpingRatePest := PestName;
              PumpingRatePestSeriesName := PestSeriesName;
              PumpingRatePestSeriesMethod := PestSeriesMethod;
              PumpingRateTimeSeriesName := TimeSeriesName;
            end;
          end;
        else
          begin
            ConcIndex := BoundaryFunctionIndex - WelStartConcentration;
            with WellStorage.WellArray[Index] do
            begin
              GwtConcentrations.Values[ConcIndex] := Expression.DoubleResult;
              GwtConcentrations.ValueAnnotations[ConcIndex] := ACell.Annotation;
              GwtConcentrations.ValuePestNames[ConcIndex] := PestName;
              GwtConcentrations.ValuePestSeriesNames[ConcIndex] := PestSeriesName;
              GwtConcentrations.ValuePestSeriesMethods[ConcIndex] := PestSeriesMethod;
              GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := TimeSeriesName;
            end;
          end;
      end;
    except
      on E: EMathError do
      begin
        case BoundaryFunctionIndex of
          WelPumpingRatePosition:
            begin
              with WellStorage.WellArray[Index] do
              begin
                ErrorMessage := StrWellFormulaError;
                PumpingRate := 0;
                PumpingRateAnnotation := ErrorMessage;
                PumpingRatePest := PestName;
                PumpingRatePestSeriesName := PestSeriesName;
                PumpingRatePestSeriesMethod := PestSeriesMethod;
                PumpingRateTimeSeriesName := TimeSeriesName;
              end;
            end;
          else
            begin
              ConcIndex := BoundaryFunctionIndex - WelStartConcentration;
              with WellStorage.WellArray[Index] do
              begin
                GwtConcentrations.Values[ConcIndex] := 0;
                GwtConcentrations.ValueAnnotations[ConcIndex] := StrWellConcentrationS;
                GwtConcentrations.ValuePestNames[ConcIndex] := PestName;
                GwtConcentrations.ValuePestSeriesNames[ConcIndex] := PestSeriesName;
                GwtConcentrations.ValuePestSeriesMethods[ConcIndex] := PestSeriesMethod;
                GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := TimeSeriesName;
              end;
            end;
        end;
        LocalScreenObject := ScreenObject as TScreenObject;

        frmErrorsAndWarnings.AddError(AModel, ErrorMessage,
          Format(StrObject0sLayerError,
          [LocalScreenObject.Name, ACell.Layer+1, ACell.Row+1,
          ACell.Column+1, E.Message]), LocalScreenObject);
      end;
      on E: ERbwParserError do
      begin
        case BoundaryFunctionIndex of
          WelPumpingRatePosition:
            begin
              with WellStorage.WellArray[Index] do
              begin
                ErrorMessage := StrWellFormulaError;
                PumpingRate := 0;
                PumpingRateAnnotation := ErrorMessage;
                PumpingRatePest := PestName;
                PumpingRatePestSeriesName := PestSeriesName;
                PumpingRatePestSeriesMethod := PestSeriesMethod;
                PumpingRateTimeSeriesName := TimeSeriesName;
              end;
            end;
          else
            begin
              ConcIndex := BoundaryFunctionIndex - WelStartConcentration;
              with WellStorage.WellArray[Index] do
              begin
                GwtConcentrations.Values[ConcIndex] := 0;
                GwtConcentrations.ValueAnnotations[ConcIndex] := StrWellConcentrationS;
                GwtConcentrations.ValuePestNames[ConcIndex] := PestName;
                GwtConcentrations.ValuePestSeriesNames[ConcIndex] := PestSeriesName;
                GwtConcentrations.ValuePestSeriesMethods[ConcIndex] := PestSeriesMethod;
                GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := TimeSeriesName;
              end;
            end;
        end;
        LocalScreenObject := ScreenObject as TScreenObject;

        frmErrorsAndWarnings.AddError(AModel, ErrorMessage,
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

class function TWellCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
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
  Item := Items[ItemIndex] as TWellItem;
  if FormulaIndex = 0 then
  begin
    Boundary := BoundaryGroup as TMfWellBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
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
    result := Item.BoundaryFormula[FormulaIndex];
  end;
end;

procedure TWellCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
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
    for Index := 0 to Link.FConcList.Count - 1 do
    begin
      TimeList := Link.FConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMfWelTimeListLink;
        for Index := 0 to Link.FConcList.Count - 1 do
        begin
          TimeList := Link.FConcList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
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
    PhastModel.InvalidateMfWellConc(self);
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
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TMfWelTimeListLink;
        Link.FPumpingRateData.Invalidate;
      end;
    end;
  end;
end;

class function TWellCollection.ItemClass: TBoundaryItemClass;
begin
  result := TWellItem;
end;

procedure TWellCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  Index: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TWellStorage).FWellArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for Index := 0 to BoundaryCount - 1 do
    begin
      SetLength(TWellStorage(Boundaries[ItemIndex, AModel]).FWellArray[Index].GwtConcentrations.Values,
        LocalModel.MobileComponents.Count);
      SetLength(TWellStorage(Boundaries[ItemIndex, AModel]).FWellArray[Index].GwtConcentrations.ValueAnnotations,
        LocalModel.MobileComponents.Count);
      SetLength(TWellStorage(Boundaries[ItemIndex, AModel]).FWellArray[Index].GwtConcentrations.ValuePestNames,
        LocalModel.MobileComponents.Count);
      SetLength(TWellStorage(Boundaries[ItemIndex, AModel]).FWellArray[Index].GwtConcentrations.ValuePestSeriesNames,
        LocalModel.MobileComponents.Count);
      SetLength(TWellStorage(Boundaries[ItemIndex, AModel]).FWellArray[Index].GwtConcentrations.ValuePestSeriesMethods,
        LocalModel.MobileComponents.Count);
      SetLength(TWellStorage(Boundaries[ItemIndex, AModel]).FWellArray[Index].GwtConcentrations.ValueTimeSeriesNames,
        LocalModel.MobileComponents.Count);
    end;
  end;
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

//function TWell_Cell.GetConcentration(const Index: Integer): double;
//begin
//  result := Values.GwtConcentrations.Concentrations[Index];
//end;
//
//function TWell_Cell.GetConcentrationAnnotation(const Index: Integer): string;
//begin
//  result := Values.GwtConcentrations.ConcentrationAnnotations[Index];
//end;
//
//function TWell_Cell.GetConcentrationPestName(const Index: Integer): string;
//begin
//  result := FValues.GwtConcentrations.ConcentrationPestNames[Index];
//end;
//
//function TWell_Cell.GetConcentrationPestSeriesMethod(
//  const Index: Integer): TPestParamMethod;
//begin
//  result := FValues.GwtConcentrations.ConcentrationPestSeriesMethods[Index];
//end;
//
//function TWell_Cell.GetConcentrationPestSeriesName(
//  const Index: Integer): string;
//begin
//  result := FValues.GwtConcentrations.ConcentrationPestSeriesNames[Index];
//end;
//
//function TWell_Cell.GetConcentrationTimeSeriesName(
//  const Index: Integer): string;
//begin
//  result := FValues.GwtConcentrations.ConcentrationTimeSeriesNames[Index];
//end;

function TWell_Cell.GetGwtConcentrations: TGwtCellData;
begin
  result := FValues.GwtConcentrations;
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

function TWell_Cell.GetMf6TimeSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRateTimeSeriesName;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        result := FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex];
      end;
  end;
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
var
  ConcIndex: Integer;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRatePest;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        result := FValues.GwtConcentrations.ValuePestNames[ConcIndex];
      end;
  end;
end;

function TWell_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRatePestSeriesMethod;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesMethods[ConcIndex];
      end;
  end;
end;

function TWell_Cell.GetPestSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    WelPumpingRatePosition: result := PumpingRatePestSeries;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesNames[ConcIndex];
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

function TWell_Cell.GetPumpingRateTimeSeriesName: string;
begin
  result := Values.PumpingRateTimeSeriesName;
end;

function TWell_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
var
  ConcIndex: Integer;
begin
//  result := '';
  case Index of
    WelPumpingRatePosition: result := PumpingRateAnnotation;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        result := FValues.GwtConcentrations.ValueAnnotations[ConcIndex];
      end;
  end;
end;

function TWell_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  ConcIndex: Integer;
begin
//  result := 0;
  case Index of
    WelPumpingRatePosition: result := PumpingRate;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        result := FValues.GwtConcentrations.Values[ConcIndex];
      end;
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
  FValues.Cell.Column := Value;
end;

procedure TWell_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TWell_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
var
  ConcIndex: Integer;
begin
  case Index of
    WelPumpingRatePosition:
      PumpingRateTimeSeriesName := Value;
    else
      begin
        ConcIndex := Index - WelStartConcentration;
        FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := Value;
      end;
  end;
end;

procedure TWell_Cell.SetPumpingRateTimeSeriesName(const Value: string);
begin
  FValues.PumpingRateTimeSeriesName := Value;
end;

procedure TWell_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
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
    PestConcentrationFormulas := SourceWell.PestConcentrationFormulas;
    PestConcentrationMethods := SourceWell.PestConcentrationMethods;
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
        Cell.FValues := BoundaryValues;
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
  FPestConcentrationFormulas:= TWelGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestConcentrationFormulas.UsedForPestSeries := True;
  FPestConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FConcentrationObservers := TObserverList.Create;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestPumpingRateFormula := '';
  FPestPumpingRateMethod := DefaultBoundaryMethod(WelPumpingRatePosition);
end;

procedure TMfWellBoundary.CreateFormulaObjects;
var
  LocalModel: TPhastModel;
  ConcIndex: Integer;
begin
  FPestPumpingRateFormula := CreateFormulaObjectBlocks(dso3D);
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestConcentrationFormulas.Add;
    end;
  end;
end;

procedure TMfWellBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestPumpingRateObserver);
    for Index := 0 to FPestConcentrationFormulas.Count - 1 do
    begin
      FObserverList.Add(ConcentrationObserver[Index]);
    end;
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
      end;
  end;
end;

destructor TMfWellBoundary.Destroy;
var
  Index: Integer;
begin
  PestPumpingRateFormula := '';

  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    FPestConcentrationFormulas[Index].Value := '';
  end;

  inherited;
  FPestConcentrationMethods.Free;
  FPestConcentrationFormulas.Free;
  FConcentrationObservers.Free;
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
begin
  FCurrentParameter := nil;

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

  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TWellStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;

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

function TMfWellBoundary.GetConcentrationObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FConcentrationObservers.Count do
  begin
    CreateObserver(Format('WelConc_%d', [Index+1]), AObserver, nil);
    FConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateConcData;
  end;
  result := FConcentrationObservers[Index];
end;

function TMfWellBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ConcIndex: Integer;
begin
  result := '';
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        result := PestPumpingRateFormula;
      end;
    else
      begin
        ConcIndex := FormulaIndex - WelStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        result := PestConcentrationFormulas[ConcIndex].Value;
      end;
  end;
end;

function TMfWellBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        result := PestPumpingRateMethod;
      end;
    else
      begin
        ConcIndex := FormulaIndex - WelStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        result := FPestConcentrationMethods[ConcIndex].PestParamMethod;
      end;
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
var
  Index: Integer;
begin
  if Sender = FPestPumpingRateFormula as TObject then
  begin
    if WelPumpingRatePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[WelPumpingRatePosition]);
    end;
  end;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    if FPestConcentrationFormulas[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[WelStartConcentration + Index]);
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
  end;
  result := FUsedObserver;
end;

procedure TMfWellBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TMfWellBoundary.InvalidateConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfWellConc(self);
end;

procedure TMfWellBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfWellPumpage(self);
    Model.InvalidateMfWellConc(self);
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
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfWellPumpage(self);
      end;
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
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        PestPumpingRateFormula := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - WelStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        PestConcentrationFormulas[ConcIndex].Value := Value;
      end;
  end;
end;

procedure TMfWellBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    WelPumpingRatePosition:
      begin
        PestPumpingRateMethod := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - WelStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        FPestConcentrationMethods[ConcIndex].PestParamMethod := Value;
      end;
  end;
end;

procedure TMfWellBoundary.SetPestConcentrationFormulas(
  const Value: TWelGwtConcCollection);
begin
  FPestConcentrationFormulas.Assign(Value);
end;

procedure TMfWellBoundary.SetPestConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestConcentrationMethods.Assign(Value);
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

procedure TWellRecord.Assign(const Item: TWellRecord);
begin
  self := Item;
  GwtConcentrations.Assign(Item.GwtConcentrations);
end;

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
  WriteCompInt(Comp, Strings.IndexOf(PumpingParameterName));
  WriteCompInt(Comp, Strings.IndexOf(PumpingRateTimeSeriesName));

  GwtConcentrations.Cache(Comp, Strings);

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TWellRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(PumpingRateAnnotation);
  Strings.Add(PumpingRatePest);
  Strings.Add(PumpingRatePestSeriesName);
  Strings.Add(PumpingParameterName);
  Strings.Add(PumpingRateTimeSeriesName);
  GwtConcentrations.RecordStrings(Strings);
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
  PumpingParameterName := Annotations[ReadCompInt(Decomp)];
  PumpingRateTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  GwtConcentrations.Restore(Decomp,Annotations);

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
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  inherited;
  FConcList := TModflowTimeLists.Create;

  FPumpingRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPumpingRateData.NonParamDescription := StrPumpingRate;
  FPumpingRateData.ParamDescription := StrPumpingRateMultip;
  if Model <> nil then
  begin
    FPumpingRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfWellPumpage;
  end;
  AddTimeList(FPumpingRateData);

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
  end;

end;

destructor TMfWelTimeListLink.Destroy;
begin
  FConcList.Free;
  FPumpingRateData.Free;
  inherited;
end;

procedure TMfWelTimeListLink.RemoveGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
begin
  ConcTimeList := FConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FConcList.Delete(SpeciesIndex);
end;

procedure TMfWelTimeListLink.UpdateGwtTimeLists;
var
  LocalModel: TCustomModel;
  SpeciesIndex: Integer;
begin
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := FConcList.Count to
      LocalModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
    for SpeciesIndex := LocalModel.MobileComponents.Count to
      FConcList.Count - 1 do
    begin
      RemoveGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

procedure TMfWelTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name;
  ConcTimeList.ParamDescription := ConcTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMfWellConc;
  end;
  AddTimeList(ConcTimeList);
  FConcList.Add(ConcTimeList);
end;

{ TWelGwtConcCollection }

constructor TWelGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TWellCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

end.
