unit ModflowRchUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes,
  ModflowTransientListParameterUnit;

type
  {
    @name stores the location, time and recharge rate for a cell.
  }
  TRchRecord = record
    Cell: TCellLocation;
    RechargeRate: double;
    StartingTime: double;
    EndingTime: double;
    RechargeRateAnnotation: string;
    RechargeParameterName: string;
    RechargeParameterValue: double;
    // PEST
    RechargePest: string;
    RechargePestSeries: string;
    RechargePestMethod: TPestParamMethod;
    RechargeTimeSeriesName: string;
    // GWT Concentrations
    GwtConcentrations: TGwtCellData;
    procedure Assign(const Item: TRchRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TRchLayerRecord = record
    Cell: TCellLocation;
    RechargeLayer: integer;
    StartingTime: double;
    EndingTime: double;
    RechargeLayerAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TRchRecord)s.
  TRchArray = array of TRchRecord;
  TRchLayerArray = array of TRchLayerRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of recharge cells.
  TRchStorage = class(TCustomBoundaryStorage)
  private
    FRchArray: TRchArray;
    function GetRchArray: TRchArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RchArray: TRchArray read GetRchArray;
  end;

  TRchLayerStorage = class(TCustomBoundaryStorage)
  private
    FRchLayerArray: TRchLayerArray;
    function GetRchLayerArray: TRchLayerArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RchLayerArray: TRchLayerArray read GetRchLayerArray;
  end;

  TRchCollection = class;

  TRchGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TRchCollection);
  end;

  // @name represents a MODFLOW recharge for one time interval.
  // @name is stored by @link(TRchCollection).
  TRchItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RechargeRate).
    FRechargeRate: TFormulaObject;
    FGwtConcentrations: TRchGwtConcCollection;
    // See @link(RechargeRate).
    procedure SetRechargeRate(const Value: string);
    function GetRechargeRate: string;
    procedure SetGwtConcentrations(const Value: TRchGwtConcCollection);
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
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property RechargeRate: string read GetRechargeRate write SetRechargeRate;
    property GwtConcentrations: TRchGwtConcCollection read FGwtConcentrations
      write SetGwtConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
  end;

  // @name represents a MODFLOW recharge layer for one time interval.
  // @name is stored by @link(TRchLayerCollection).
  TRchLayerItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RechargeLayer).
    FRechargeLayer: TFormulaObject;
    // See @link(RechargeLayer).
    procedure SetRechargeLayer(const Value: string);
    function GetRechargeLayer: string;
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
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the recharge rate
    // or the recharge rate multiplier of this boundary.
    property RechargeLayer: string read GetRechargeLayer write SetRechargeLayer;
  end;

  TRchTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the recharge rates for a series of
    // cells over a series of time intervals.
    FRechargeRateData: TModflowTimeList;
    FConcList: TModflowTimeLists;
  protected
    procedure CreateTimeLists; override;
    property RechargeRateData: TModflowTimeList read FRechargeRateData;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Recharge boundaries
  // for a series of time intervals.
  TRchCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateRechargeData(Sender: TObject);
    procedure InvalidateGwtConcentrations(Sender: TObject);
  protected
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRchStorage.RchArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  TRchLayerTimeListLink = class(TTimeListsModelLink)
    // @name is used to compute the recharge layers for a series of
    // cells over a series of time intervals.
    FRechargeLayerData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TRchLayerCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateRechLayerData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRchStorage.RchArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;


  // Each @name stores a @link(TRchCollection).
  // @classname is stored by @link(TModflowParameters).
  TRchParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TRechargeCell = class(TValueCell);

  TRch_Cell = class(TRechargeCell)
  private
    FValues: TRchRecord;
    FStressPeriod: integer;
    function GetRechargeRate: double;
    function GetRechargeRateAnnotation: string;
//    function GetTimeSeriesName: string;
    function GetRechargeParameterName: string;
    function GetRechargeParameterValue: double;
    function GetRechargePest: string;
    function GetRechargePestMethod: TPestParamMethod;
    function GetRechargePestSeries: string;
    function GetRechargeTimeSeriesName: string;
    procedure SetRechargeTimeSeriesName(const Value: string);
    function GetConcentration(const Index: Integer): double;
    function GetConcentrationAnnotation(const Index: Integer): string;
    function GetConcentrationPestName(const Index: Integer): string;
    function GetConcentrationPestSeriesMethod(
      const Index: Integer): TPestParamMethod;
    function GetConcentrationPestSeriesName(const Index: Integer): string;
    function GetConcentrationTimeSeriesName(const Index: Integer): string;
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
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TRchRecord read FValues write FValues;
    property RechargeRate: double read GetRechargeRate;
    property RechargeRateAnnotation: string read GetRechargeRateAnnotation;
//    property TimeSeriesName: string read GetTimeSeriesName;
    property RechargeParameterName: string read GetRechargeParameterName;
    property RechargeParameterValue: double read GetRechargeParameterValue;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;

    property RechargePest: string read GetRechargePest;
    property RechargePestSeries: string read GetRechargePestSeries;
    property RechargePestMethod: TPestParamMethod read GetRechargePestMethod;
    property RechargeTimeSeriesName: string read GetRechargeTimeSeriesName
      write SetRechargeTimeSeriesName;
    // GWT
    property Concentrations[const Index: Integer]: double
      read GetConcentration;
    property ConcentrationAnnotations[const Index: Integer]: string
      read GetConcentrationAnnotation;
    property ConcentrationPestNames[const Index: Integer]: string
      read GetConcentrationPestName;
    property ConcentrationPestSeriesNames[const Index: Integer]: string
      read GetConcentrationPestSeriesName;
    property ConcentrationPestSeriesMethods[const Index: Integer]: TPestParamMethod
      read GetConcentrationPestSeriesMethod;
    property ConcentrationTimeSeriesNames[const Index: Integer]: string
      read GetConcentrationTimeSeriesName;
  end;

  TRechargeLayerCell = class(TRechargeCell)
  private
    Values: TRchLayerRecord;
    StressPeriod: integer;
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
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;


  // @name represents the MODFLOW Recharge boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TRchCollection)
  TRchBoundary = class(TModflowParamBoundary)
  private
    FRechargeLayers: TRchLayerCollection;
    FCurrentParameter: TModflowTransientListParameter;
    FPestRechargeMethod: TPestParamMethod;
    FPestRechargeFormula: TFormulaObject;
    FPestRechargeObserver: TObserver;
    FUsedObserver: TObserver;
    FPestConcentrationMethods: TPestMethodCollection;
    FPestConcentrationFormulas: TRchGwtConcCollection;
    FConcentrationObservers: TObserverList;
    procedure SetRechargeLayers(const Value: TRchLayerCollection);
    function GetTimeVaryingRechargeLayers: boolean;
    procedure AssignRechargeLayerCells(BoundaryStorage: TRchLayerStorage;
      ValueTimeList: TList);
    function GetPestRechargeFormula: string;
    procedure SetPestRechargeFormula(const Value: string);
    procedure SetPestRechargeMethod(const Value: TPestParamMethod);
    procedure InvalidateRechargeData(Sender: TObject);
    procedure InvalidateConcData(Sender: TObject);
    function GetPestRechargeObserver: TObserver;
    procedure SetPestConcentrationFormulas(const Value: TRchGwtConcCollection);
    procedure SetPestConcentrationMethods(const Value: TPestMethodCollection);
    function GetConcentrationObserver(const Index: Integer): TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRch_Cell)s for that stress period.
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
    property PestRechargeObserver: TObserver read GetPestRechargeObserver;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property ConcentrationObserver[const Index: Integer]: TObserver
      read GetConcentrationObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // @link(TRchStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW Recharge parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TRchStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    procedure EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject); override;
    function NonParameterColumns: integer; override;
    property TimeVaryingRechargeLayers: boolean
      read GetTimeVaryingRechargeLayers;
    procedure GetRechargeLayerCells(LayerTimeList: TList; AModel: TBaseModel);
    procedure InvalidateDisplay; override;
    procedure Clear; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property RechargeLayers: TRchLayerCollection read FRechargeLayers
      write SetRechargeLayers;
    property Interp;
    property PestRechargeFormula: string read GetPestRechargeFormula
      write SetPestRechargeFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRechargeMethod: TPestParamMethod read FPestRechargeMethod
      write SetPestRechargeMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestConcentrationFormulas: TRchGwtConcCollection
      read FPestConcentrationFormulas write SetPestConcentrationFormulas
      {$IFNDEF GWT}
      Stored False
      {$ENDIF}
      ;
    property PestConcentrationMethods: TPestMethodCollection
      read FPestConcentrationMethods write SetPestConcentrationMethods
      {$IFNDEF GWT}
      Stored False
      {$ENDIF}
      ;
  end;

const
  RechPosition = 0;
  RchStartConcentration = 1;

implementation

uses RbwParser, ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  frmGoPhastUnit, TempFiles,
  AbstractGridUnit, ModflowIrregularMeshUnit, ModflowTimeSeriesUnit,
  ModflowParameterUnit, ModelMuseUtilities, CustomModflowWriterUnit,
  System.Generics.Collections;

resourcestring
  StrRechargeLayer = 'Recharge layer';
  StrRechargeRate = 'Recharge rate';
  StrRechargeRateMulti = ' recharge rate multiplier';

const
  StrAssignedFromTheCe = 'assigned from the cell''s layer';
  LayerPosition = 0;

{ TRchItem }

procedure TRchItem.Assign(Source: TPersistent);
var
  RchItem: TRchItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TRchItem then
  begin
    RchItem := TRchItem(Source);
    RechargeRate := RchItem.RechargeRate;
    GwtConcentrations := RchItem.GwtConcentrations;
  end;
  inherited;
end;

procedure TRchItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRchCollection;
  RechObserver: TObserver;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TRchCollection;
  RechObserver := FObserverList[RechPosition];
  RechObserver.OnUpToDateSet := ParentCollection.InvalidateRechargeData;

  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    GwtConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateGwtConcentrations;
  end;
end;

function TRchItem.BoundaryFormulaCount: integer;
begin
  result := 1;
  if GwtConcentrations <> nil then
  begin
    result := result + GwtConcentrations.Count;
  end;
end;

constructor TRchItem.Create(Collection: TCollection);
var
  RchCol: TRchCollection;
begin
  RchCol := Collection as TRchCollection;
  FGwtConcentrations := TRchGwtConcCollection.Create(Model, ScreenObject,
    RchCol);
  inherited;
end;

procedure TRchItem.CreateFormulaObjects;
begin
  inherited;
  FRechargeRate := CreateFormulaObject(dsoTop);
end;

destructor TRchItem.Destroy;
var
  Index: Integer;
begin
  RechargeRate := '0';
  for Index := 0 to FGwtConcentrations.Count - 1 do
  begin
    FGwtConcentrations[Index].Value := '0';
  end;
  FGwtConcentrations.Free;
  inherited;
end;

function TRchItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TGwtConcStringValueItem;
begin
  case Index of
    RechPosition: result := RechargeRate;
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

procedure TRchItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Item: TGwtConcStringValueItem;
  ConcIndex: Integer;
begin
  Assert(Sender = FRechargeRate);
  List.Add(FObserverList[RechPosition]);
  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    Item := GwtConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
end;

function TRchItem.GetRechargeRate: string;
begin
  Result := FRechargeRate.Formula;
  ResetItemObserver(RechPosition);
end;

procedure TRchItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfRchRate(self);
    PhastModel.InvalidateMfRchConc(self);
  end;
end;

function TRchItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRchItem;
begin
  result := (AnotherItem is TRchItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRchItem(AnotherItem);
    result := (Item.RechargeRate = RechargeRate)
      and (Item.GwtConcentrations.IsSame(GwtConcentrations));
  end;
end;

procedure TRchItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRechargeRate,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TRchItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TGwtConcStringValueItem;
begin
  case Index of
    RechPosition: RechargeRate := Value;
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

procedure TRchItem.SetGwtConcentrations(const Value: TRchGwtConcCollection);
begin
  FGwtConcentrations.Assign(Value);
end;

procedure TRchItem.SetRechargeRate(const Value: string);
begin
  UpdateFormulaBlocks(Value, RechPosition, FRechargeRate);
end;

{ TRchCollection }

procedure TRchCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TRchStorage.Create(AModel));
end;

procedure TRchCollection.AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  RechargeRateArray: TDataArray;
  Boundary: TRchStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
  LocalRechargePestSeries: string;
  LocalRechargePestMethod: TPestParamMethod;
  RechargePestItems: TStringList;
  LocalRechargePest: string;
  RechargeTimeItems: TStringList;
  LocalRechargeTimeSeries: string;
  SpeciesIndex: Integer;
  ConcentrationArray: TDataArray;
  LocalConcentrationPestSeries: string;
  LocalConcentrationPestMethod: TPestParamMethod;
  ConcentrationPestItems: TStringList;
  LocalConcentrationPest: string;
  ConcentrationTimeItems: TStringList;
  LocalConcentrationTimeSeries: string;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  Boundary := Boundaries[ItemIndex, AModel] as TRchStorage;
  // Note that TRchCollection is also used in UZF where RechPosition
  // is the same as UzfInfiltrationBoundaryPosition.
  RechargeRateArray := DataSets[RechPosition];
  RechargeRateArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);

  LocalRechargePestSeries := PestSeries[RechPosition];
  LocalRechargePestMethod := PestMethods[RechPosition];
  RechargePestItems := PestItemNames[RechPosition];
  LocalRechargePest := RechargePestItems[ItemIndex];
  RechargeTimeItems := TimeSeriesNames[RechPosition];
  LocalRechargeTimeSeries := RechargeTimeItems[ItemIndex];

  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      if LocalModel.IsLayerSimulated(LayerIndex) then
      begin
        for RowIndex := RowMin to RowMax do
        begin
          for ColIndex := ColMin to ColMax do
          begin
            if RechargeRateArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.RchArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                RechargeRate := RechargeRateArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                RechargeRateAnnotation := RechargeRateArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                RechargePest := LocalRechargePest;
                RechargePestSeries := LocalRechargePestSeries;
                RechargePestMethod := LocalRechargePestMethod;
                RechargeTimeSeriesName := LocalRechargeTimeSeries;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  RechargeRateArray.CacheData;

  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      ConcentrationArray := DataSets[RchStartConcentration+SpeciesIndex];
      ConcentrationArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
        LayerMax, RowMax, ColMax);

      LocalConcentrationPestSeries := PestSeries[RchStartConcentration+SpeciesIndex];
      LocalConcentrationPestMethod := PestMethods[RchStartConcentration+SpeciesIndex];
      ConcentrationPestItems := PestItemNames[RchStartConcentration+SpeciesIndex];
      LocalConcentrationPest := ConcentrationPestItems[ItemIndex];
      ConcentrationTimeItems := TimeSeriesNames[RchStartConcentration+SpeciesIndex];
      LocalConcentrationTimeSeries := ConcentrationTimeItems[ItemIndex];

      BoundaryIndex := 0;
      if LayerMin >= 0 then
      begin
        for LayerIndex := LayerMin to LayerMax do
        begin
          if LocalModel.IsLayerSimulated(LayerIndex) then
          begin
            for RowIndex := RowMin to RowMax do
            begin
              for ColIndex := ColMin to ColMax do
              begin
                if ConcentrationArray.IsValue[LayerIndex, RowIndex, ColIndex] then
                begin
                  with Boundary.RchArray[BoundaryIndex] do
                  begin
                    GwtConcentrations.Concentrations[SpeciesIndex] := ConcentrationArray.
                      RealData[LayerIndex, RowIndex, ColIndex];
                    GwtConcentrations.ConcentrationAnnotations[SpeciesIndex] := ConcentrationArray.
                      Annotation[LayerIndex, RowIndex, ColIndex];
                    GwtConcentrations.ConcentrationPestNames[SpeciesIndex] := LocalConcentrationPest;
                    GwtConcentrations.ConcentrationPestSeriesNames[SpeciesIndex] := LocalConcentrationPestSeries;
                    GwtConcentrations.ConcentrationPestSeriesMethods[SpeciesIndex] := LocalConcentrationPestMethod;
                    GwtConcentrations.ConcentrationTimeSeriesNames[SpeciesIndex] := LocalConcentrationTimeSeries;
                  end;
                  Inc(BoundaryIndex);
                end;
              end;
            end;
          end;
        end;
      end;
      ConcentrationArray.CacheData;
    end;
  end;

  Boundary.CacheData;
end;

function TRchCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TRchTimeListLink;
end;

procedure TRchCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TRchItem;
  ScreenObject: TScreenObject;
  ALink: TRchTimeListLink;
  RechargeRateData: TModflowTimeList;
  DataArrayIndex: Integer;
  DataArray: TTransientRealSparseDataSet;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  ShouldRemove: Boolean;
  RowCount: Integer;
  ColumnCount: Integer;
  LayerCount: Integer;
  LocalModel: TCustomModel;
  RechargeMethod: TPestParamMethod;
  PestRechargeSeriesName: string;
  RechargeItems: TStringList;
  ItemFormula: string;
  TimeSeriesItems: TStringList;
  SpeciesCount: Integer;
  SpeciesIndex: Integer;
  ConcentrationSeriesName: string;
  ConcentrationMethod: TPestParamMethod;
  ConcentrationItems: TStringList;
  ConcentrationTimeSeriesItems: TStringList;
  ConcPestItemList: TList<TStringList>;
  ConcTimeSeriesItemList: TList<TStringList>;
  ConcentrationData: TModflowTimeList;
  ConcDataArray: TTransientRealSparseDataSet;
begin
  ConcPestItemList := TList<TStringList>.Create;
  ConcTimeSeriesItemList := TList<TStringList>.Create;
  try
    LocalModel := AModel as TCustomModel;
    ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
    SetLength(BoundaryValues, Count);
    SpeciesCount := LocalModel.MobileComponents.Count;

    // Note that TRchCollection is also used in UZF where RechPosition
    // is the same as UzfInfiltrationBoundaryPosition.
    PestRechargeSeriesName := BoundaryGroup.PestBoundaryFormula[RechPosition];
    PestSeries.Add(PestRechargeSeriesName);
    RechargeMethod := BoundaryGroup.PestBoundaryMethod[RechPosition];
    PestMethods.Add(RechargeMethod);

    RechargeItems := TStringList.Create;
    PestItemNames.Add(RechargeItems);

    TimeSeriesItems := TStringList.Create;
    TimeSeriesNames.Add(TimeSeriesItems);

    if LocalModel.GwtUsed then
    begin
      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        ConcentrationSeriesName := BoundaryGroup.PestBoundaryFormula[RchStartConcentration + SpeciesIndex];
        PestSeries.Add(ConcentrationSeriesName);
        ConcentrationMethod := BoundaryGroup.PestBoundaryMethod[RchStartConcentration + SpeciesIndex];
        PestMethods.Add(ConcentrationMethod);

        ConcentrationItems := TStringList.Create;
        PestItemNames.Add(ConcentrationItems);
        ConcPestItemList.Add(ConcentrationItems);

        ConcentrationTimeSeriesItems := TStringList.Create;
        TimeSeriesNames.Add(ConcentrationTimeSeriesItems);
        ConcTimeSeriesItemList.Add(ConcentrationTimeSeriesItems);
      end;
    end;

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TRchItem;
      BoundaryValues[Index].Time := Item.StartTime;

      ItemFormula := Item.RechargeRate;
      AssignBoundaryFormula(AModel, PestRechargeSeriesName, RechargeMethod,
        RechargeItems, TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
    end;
    ALink := TimeListLink.GetLink(AModel) as TRchTimeListLink;
    RechargeRateData := ALink.FRechargeRateData;
    RechargeRateData.Initialize(BoundaryValues, ScreenObject, lctUse);
    Assert(RechargeRateData.Count = Count);

    if LocalModel.GwtUsed then
    begin
      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        for Index := 0 to Count - 1 do
        begin
          Item := Items[Index] as TRchItem;
          BoundaryValues[Index].Time := Item.StartTime;

          ConcentrationSeriesName := BoundaryGroup.PestBoundaryFormula[RchStartConcentration + SpeciesIndex];
          ConcentrationMethod := BoundaryGroup.PestBoundaryMethod[RchStartConcentration + SpeciesIndex];
          ConcentrationItems := ConcPestItemList[SpeciesIndex];
          ConcentrationTimeSeriesItems := ConcTimeSeriesItemList[SpeciesIndex];
          ItemFormula := Item.GwtConcentrations[SpeciesIndex].Value;
          AssignBoundaryFormula(AModel, ConcentrationSeriesName, ConcentrationMethod,
            ConcentrationItems, ConcentrationTimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
        end;

        ConcentrationData := ALink.FConcList[SpeciesIndex];
        ConcentrationData.Initialize(BoundaryValues, ScreenObject, lctUse);
        Assert(ConcentrationData.Count = Count);
      end;
    end;

    if PackageAssignmentMethod(AModel) = umAdd then
    begin
      RowCount := LocalModel.RowCount;
      ColumnCount := LocalModel.ColumnCount;
      LayerCount := LocalModel.LayerCount;
      for DataArrayIndex := 0 to RechargeRateData.Count - 1 do
      begin
        DataArray := RechargeRateData[DataArrayIndex] as TTransientRealSparseDataSet;
        for RowIndex := 0 to RowCount - 1 do
        begin
          for ColIndex := 0 to ColumnCount - 1 do
          begin
            ShouldRemove := False;
            for LayerIndex := LayerCount -1 downto 0 do
            begin
              if ShouldRemove then
              begin
                DataArray.RemoveValue(LayerIndex, RowIndex, ColIndex);
                if LocalModel.GwtUsed then
                begin
                  for SpeciesIndex := 0 to SpeciesCount - 1 do
                  begin
                    ConcentrationData := ALink.FConcList[SpeciesIndex];
                    ConcDataArray := ConcentrationData[DataArrayIndex] as TTransientRealSparseDataSet;
                    ConcDataArray.RemoveValue(LayerIndex, RowIndex, ColIndex);
                  end;
                end;
              end
              else
              begin
                ShouldRemove := DataArray.IsValue[LayerIndex, RowIndex, ColIndex];
              end;
            end;
          end;
        end;
      end;
    end;

    ClearBoundaries(AModel);
    SetBoundaryCapacity(RechargeRateData.Count, AModel);
    for TimeIndex := 0 to RechargeRateData.Count - 1 do
    begin
      AddBoundary(TRchStorage.Create(AModel));
    end;
    ListOfTimeLists.Add(RechargeRateData);
    if LocalModel.GwtUsed then
    begin
      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        ConcentrationData := ALink.FConcList[SpeciesIndex];
        ListOfTimeLists.Add(ConcentrationData);
      end;
    end;
  finally
    ConcTimeSeriesItemList.Free;
    ConcPestItemList.Free;
  end;
end;

procedure TRchCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TRchTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRchTimeListLink;
    for Index := 0 to Link.FConcList.Count - 1 do
    begin
      TimeList := Link.FConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRchTimeListLink;
      for Index := 0 to Link.FConcList.Count - 1 do
      begin
        TimeList := Link.FConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TRchCollection.InvalidateRechargeData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRchTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRchTimeListLink;
    Link.FRechargeRateData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRchTimeListLink;
      Link.FRechargeRateData.Invalidate;
    end;
  end;
end;

class function TRchCollection.ItemClass: TBoundaryItemClass;
begin
  result := TRchItem;
end;

function TRchCollection.PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.RchPackage.AssignmentMethod;
end;

procedure TRchCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  Index: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TRchStorage).FRchArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for Index := 0 to BoundaryCount - 1 do
    begin
      SetLength(TRchStorage(Boundaries[ItemIndex, AModel]).FRchArray[Index].GwtConcentrations.Concentrations,
        LocalModel.MobileComponents.Count);
      SetLength(TRchStorage(Boundaries[ItemIndex, AModel]).FRchArray[Index].GwtConcentrations.ConcentrationAnnotations,
        LocalModel.MobileComponents.Count);
      SetLength(TRchStorage(Boundaries[ItemIndex, AModel]).FRchArray[Index].GwtConcentrations.ConcentrationPestNames,
        LocalModel.MobileComponents.Count);
      SetLength(TRchStorage(Boundaries[ItemIndex, AModel]).FRchArray[Index].GwtConcentrations.ConcentrationPestSeriesNames,
        LocalModel.MobileComponents.Count);
      SetLength(TRchStorage(Boundaries[ItemIndex, AModel]).FRchArray[Index].GwtConcentrations.ConcentrationPestSeriesMethods,
        LocalModel.MobileComponents.Count);
      SetLength(TRchStorage(Boundaries[ItemIndex, AModel]).FRchArray[Index].GwtConcentrations.ConcentrationTimeSeriesNames,
        LocalModel.MobileComponents.Count);
    end;
  end;
  inherited;
end;

{ TRchParamItem }

class function TRchParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TRchCollection;
end;

{ TRch_Cell }

procedure TRch_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRch_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRch_Cell.GetConcentration(const Index: Integer): double;
begin
  result := FValues.GwtConcentrations.Concentrations[Index];
end;

function TRch_Cell.GetConcentrationAnnotation(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ConcentrationAnnotations[Index];
end;

function TRch_Cell.GetConcentrationPestName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ConcentrationPestNames[Index];
end;

function TRch_Cell.GetConcentrationPestSeriesMethod(
  const Index: Integer): TPestParamMethod;
begin
  result := FValues.GwtConcentrations.ConcentrationPestSeriesMethods[Index];
end;

function TRch_Cell.GetConcentrationPestSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ConcentrationPestSeriesNames[Index];
end;

function TRch_Cell.GetConcentrationTimeSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ConcentrationTimeSeriesNames[Index];
end;

function TRch_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    RechPosition: result := StrAssignedFromTheCe;
    else Assert(False);
  end;
end;

function TRch_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    RechPosition: result := (AModel as TCustomModel).
      DataSetLayerToModflowLayer(Layer);
    else Assert(False);
  end;
end;

function TRch_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRch_Cell.GetMf6TimeSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    RechPosition:
      begin
        result := RechargeTimeSeriesName;
      end;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        result := FValues.GwtConcentrations.ConcentrationTimeSeriesNames[ConcIndex];
      end;
  end;
end;

function TRch_Cell.GetPestName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    RechPosition:
      begin
        result := RechargePest;
      end;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        result := FValues.GwtConcentrations.ConcentrationPestNames[ConcIndex];
      end;
  end;
end;

function TRch_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case Index of
    RechPosition:
      begin
        result := RechargePestMethod;
      end;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        result := FValues.GwtConcentrations.ConcentrationPestSeriesMethods[ConcIndex];
      end;
  end;
end;

function TRch_Cell.GetPestSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    RechPosition:
      begin
        result := RechargePestSeries;
      end;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        result := FValues.GwtConcentrations.ConcentrationPestSeriesNames[ConcIndex];
      end;
  end;
end;

function TRch_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
var
  ConcIndex: Integer;
begin
  result := '';
  case Index of
    RechPosition: result := RechargeRateAnnotation;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        result := FValues.GwtConcentrations.ConcentrationAnnotations[ConcIndex];
      end;
  end;
end;

function TRch_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  ConcIndex: Integer;
begin
  case Index of
    RechPosition: result := RechargeRate;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        result := FValues.GwtConcentrations.Concentrations[ConcIndex];
      end;
  end;
end;

function TRch_Cell.GetRechargeParameterName: string;
begin
  result := Values.RechargeParameterName;
end;

function TRch_Cell.GetRechargeParameterValue: double;
begin
  result := Values.RechargeParameterValue;
end;

function TRch_Cell.GetRechargePest: string;
begin
  result := Values.RechargePest;
end;

function TRch_Cell.GetRechargePestMethod: TPestParamMethod;
begin
  result := Values.RechargePestMethod;
end;

function TRch_Cell.GetRechargePestSeries: string;
begin
  result := Values.RechargePestSeries;
end;

function TRch_Cell.GetRechargeRate: double;
begin
  result := Values.RechargeRate;
end;

function TRch_Cell.GetRechargeRateAnnotation: string;
begin
  result := Values.RechargeRateAnnotation;
end;

function TRch_Cell.GetRechargeTimeSeriesName: string;
begin
  result := Values.RechargeTimeSeriesName;
end;

function TRch_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRch_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

//function TRch_Cell.GetTimeSeriesName: string;
//begin
//  result := Values.TimeSeriesName;
//end;

function TRch_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Rch_Cell: TRch_Cell;
begin
  result := AnotherCell is TRch_Cell;
  if result then
  begin
    Rch_Cell := TRch_Cell(AnotherCell);
    result := (RechargeRate = Rch_Cell.RechargeRate)
      and (Values.Cell = Rch_Cell.Values.Cell)
  end;
end;

procedure TRch_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRch_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TRch_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TRch_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TRch_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
var
  ConcIndex: Integer;
begin
  case Index of
    RechPosition:
      begin
        RechargeTimeSeriesName := Value;
      end;
    else
      begin
        ConcIndex := Index - RchStartConcentration;
        FValues.GwtConcentrations.ConcentrationTimeSeriesNames[ConcIndex] := Value;
      end;
  end;
end;

procedure TRch_Cell.SetRechargeTimeSeriesName(const Value: string);
begin
  FValues.RechargeTimeSeriesName := Value;
end;

procedure TRch_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TRchBoundary }

procedure TRchBoundary.Assign(Source: TPersistent);
var
  SourceBoundary: TRchBoundary;
begin
  if Source is TRchBoundary then
  begin
    SourceBoundary := TRchBoundary(Source);
    RechargeLayers := SourceBoundary.RechargeLayers;

    PestRechargeFormula := SourceBoundary.PestRechargeFormula;
    PestRechargeMethod := SourceBoundary.PestRechargeMethod;
    PestConcentrationFormulas := SourceBoundary.PestConcentrationFormulas;
    PestConcentrationMethods := SourceBoundary.PestConcentrationMethods;
  end;
  inherited;
end;

procedure TRchBoundary.AssignRechargeLayerCells(
  BoundaryStorage: TRchLayerStorage; ValueTimeList: TList);
var
  Cell: TRechargeLayerCell;
  BoundaryValues: TRchLayerRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRchLayerStorage;
begin
  LocalBoundaryStorage := BoundaryStorage;// as TRchStorage;
  for TimeIndex := 0 to
    (ParentModel as TCustomModel).ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRechargeLayerCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := (ParentModel as TCustomModel).
      ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to
        Length(LocalBoundaryStorage.RchLayerArray) - 1 do
      begin
//        Cells.Cached := False;
        BoundaryValues := LocalBoundaryStorage.RchLayerArray[BoundaryIndex];
        Cell := TRechargeLayerCell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;


procedure TRchBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TRch_Cell;
  BoundaryValues: TRchRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRchStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TRchStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRch_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.RchArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.RchArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RchArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RchArray[BoundaryIndex];
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.RechargeRate :=
            BoundaryValues.RechargeRate * FCurrentParameter.Value;
          BoundaryValues.RechargeRateAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.RechargeRateAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.RechargeParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.RechargeParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.RechargeParameterName := '';
          BoundaryValues.RechargeParameterValue := 1;
        end;
        Cell := TRch_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObject;
//        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TRchBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TRchCollection;
end;

function TRchBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestRch_';
end;

procedure TRchBoundary.Clear;
begin
  inherited;
  RechargeLayers.Clear;
end;

constructor TRchBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FPestConcentrationFormulas:= TRchGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestConcentrationMethods := TPestMethodCollection.Create(Model);
  FConcentrationObservers := TObserverList.Create;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestRechargeFormula := '';
  FPestRechargeMethod := DefaultBoundaryMethod(RechPosition);

  FRechargeLayers := TRchLayerCollection.Create(self, Model, ScreenObject);
end;

procedure TRchBoundary.CreateFormulaObjects;
var
  LocalModel: TPhastModel;
  ConcIndex: Integer;
begin
  FPestRechargeFormula := CreateFormulaObjectBlocks(dsoTop);
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestConcentrationFormulas.Add;
    end;
  end;
end;

procedure TRchBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestRechargeObserver);
    for Index := 0 to FPestConcentrationFormulas.Count - 1 do
    begin
      FObserverList.Add(ConcentrationObserver[Index]);
    end;
  end;
end;

class function TRchBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    RechPosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := inherited;
//        Assert(False);
      end;
  end;
end;

destructor TRchBoundary.Destroy;
var
  Index: Integer;
begin
  PestRechargeFormula := '';
  FRechargeLayers.Free;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    FPestConcentrationFormulas[Index].Value := '';
  end;
  inherited;
  FPestConcentrationMethods.Free;
  FPestConcentrationFormulas.Free;
  FConcentrationObservers.Free;
end;

procedure TRchBoundary.EvaluateArrayBoundaries(AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  if (ParentModel as TCustomModel).
    ModflowPackages.RchPackage.TimeVaryingLayers then
  begin
    RechargeLayers.EvaluateArrayBoundaries(AModel, Writer);
  end;
end;

procedure TRchBoundary.GetRechargeLayerCells(LayerTimeList: TList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TRchLayerStorage;
begin
  if not (ParentModel as TCustomModel).ModflowPackages.
    RchPackage.TimeVaryingLayers then
  begin
    Exit;
  end;
  for ValueIndex := 0 to RechargeLayers.Count - 1 do
  begin
    if ValueIndex < RechargeLayers.BoundaryCount[AModel] then
    begin
      BoundaryStorage := RechargeLayers.Boundaries[ValueIndex, AModel]
        as TRchLayerStorage;
      AssignRechargeLayerCells(BoundaryStorage, LayerTimeList);
    end;
  end;
end;

procedure TRchBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TRchStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  Model: TCustomModel;
  BoundaryList: TList;
  ItemIndex: Integer;
  ArrayIndex: Integer;
begin
  FCurrentParameter := nil;
  EvaluateArrayBoundaries(AModel, Writer);
  Model := ParentModel as TCustomModel;
  if Model.ModflowTransientParameters.CountParam(ParameterType) = 0 then
  begin
    for ValueIndex := 0 to Values.Count - 1 do
    begin
      if ValueIndex < Values.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TRchStorage;
        AssignCells(BoundaryStorage, ValueTimeList, AModel);
      end;
    end;
  end
  else
  begin
    for ParamIndex := 0 to Parameters.Count - 1 do
    begin
      Param := Parameters[ParamIndex];
      ParamName := Param.Param.ParamName;
      if Model.ModelSelection = msModflow2015 then
      begin
        FCurrentParameter := Model.ModflowTransientParameters.GetParamByName(ParamName);
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

      if FCurrentParameter <> nil then
      begin
        BoundaryList := Param.Param.BoundaryList[AModel];
        for ItemIndex := 0 to BoundaryList.Count - 1 do
        begin
          BoundaryStorage := BoundaryList[ItemIndex];
          for ArrayIndex := 0 to Length(BoundaryStorage.RchArray) - 1 do
          begin
            BoundaryStorage.RchArray[ArrayIndex].RechargeParameterName := FCurrentParameter.ParameterName;
            BoundaryStorage.RchArray[ArrayIndex].RechargeParameterValue := FCurrentParameter.Value;
          end;
        end;
      end;

      for ValueIndex := 0 to Param.Param.Count - 1 do
      begin
        if ValueIndex < Param.Param.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TRchStorage;
          AssignCells(BoundaryStorage, Times, AModel);
        end;
      end;
    end;
  end;
  ClearBoundaries(AModel);
end;

function TRchBoundary.GetConcentrationObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FConcentrationObservers.Count do
  begin
    CreateObserver(Format('RchConc_%d', [Index+1]), AObserver, nil);
    FConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateConcData;
  end;
  result := FConcentrationObservers[Index];
end;

function TRchBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ConcIndex: Integer;
begin
  result := '';
  case FormulaIndex of
    RechPosition:
      begin
        result := PestRechargeFormula;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RchStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        result := PestConcentrationFormulas[ConcIndex].Value;
      end;
  end;
end;

function TRchBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RechPosition:
      begin
        result := PestRechargeMethod;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RchStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        result := FPestConcentrationMethods[ConcIndex].PestParamMethod;
      end;
  end;
end;

function TRchBoundary.GetPestRechargeFormula: string;
begin
  Result := FPestRechargeFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RechPosition);
  end;
end;

function TRchBoundary.GetPestRechargeObserver: TObserver;
begin
  if FPestRechargeObserver = nil then
  begin
    CreateObserver('PestRecharge_', FPestRechargeObserver, nil);
    FPestRechargeObserver.OnUpToDateSet := InvalidateRechargeData;
  end;
  result := FPestRechargeObserver;
end;

procedure TRchBoundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: Integer;
begin
  if Sender = FPestRechargeFormula then
  begin
    if RechPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[RechPosition]);
    end;
  end;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    if FPestConcentrationFormulas[Index].ValueObject = Sender then
    begin
      List.Add(FObserverList[RchStartConcentration + Index]);
    end;
  end;
end;

function TRchBoundary.GetTimeVaryingRechargeLayers: boolean;
begin
  if ParentModel = nil then
  begin
    result := frmGoPhast.PhastModel.ModflowPackages.
      RchPackage.TimeVaryingLayers;
  end
  else
  begin
    result := (ParentModel as TCustomModel).ModflowPackages.
      RchPackage.TimeVaryingLayers;
  end;
end;

function TRchBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestRecharge_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TRchBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TRchBoundary.InvalidateConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfRchConc(self);
end;

procedure TRchBoundary.InvalidateDisplay;
var
  Model: TCustomModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    Model.InvalidateMfRchRate(self);
    Model.InvalidateMfRchLayer(self);
    Model.InvalidateMfRchConc(self);
  end;
end;

procedure TRchBoundary.InvalidateRechargeData(Sender: TObject);
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
    PhastModel.InvalidateMfRchRate(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfRchRate(self);
    end;
  end;
end;

class function TRchBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TRchParamItem;
end;

function TRchBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
  if TimeVaryingRechargeLayers then
  begin
    result := result + RechargeLayers.TimeListCount(frmGoPhast.PhastModel);
  end;
end;

function TRchBoundary.ParameterType: TParameterType;
begin
  result := ptRCH;
end;

procedure TRchBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RechPosition:
      begin
        PestRechargeFormula := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RchStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        PestConcentrationFormulas[ConcIndex].Value := Value;
      end;
  end;
end;

procedure TRchBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RechPosition:
      begin
        PestRechargeMethod := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RchStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        FPestConcentrationMethods[ConcIndex].PestParamMethod := Value;
      end;
  end;
end;

procedure TRchBoundary.SetPestConcentrationFormulas(
  const Value: TRchGwtConcCollection);
begin
  FPestConcentrationFormulas.Assign(Value);
end;

procedure TRchBoundary.SetPestConcentrationMethods(
  const Value: TPestMethodCollection);
begin
  FPestConcentrationMethods.Assign(Value);
end;

procedure TRchBoundary.SetPestRechargeFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RechPosition, FPestRechargeFormula);
end;

procedure TRchBoundary.SetPestRechargeMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRechargeMethod, Value);
end;

procedure TRchBoundary.SetRechargeLayers(const Value: TRchLayerCollection);
begin
  FRechargeLayers.Assign(Value);
end;

function TRchBoundary.Used: boolean;
var
  Model: TCustomModel;
  ParamIndex: Integer;
  Param: TModflowTransientListParameter;
begin
  if ParentModel <> nil then
  begin
    Model := ParentModel as TCustomModel;
    result := Model.ModflowPackages.RchPackage.TimeVaryingLayers
      and RechargeLayers.Used;
  end
  else
  begin
    result := RechargeLayers.Used;
  end;
  if result then Exit;
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
    Model := ParentModel as TCustomModel;
    if csLoading in Model.ComponentState then
    begin
      Exit;
    end;
    for ParamIndex := 0 to Model.ModflowTransientParameters.Count - 1 do
    begin
      Param := Model.ModflowTransientParameters[ParamIndex];
      if Param.ParameterType = ptRCH then
      begin
        result := Parameters.Used;
        Exit;
      end;
    end;
    result := Values.Used;
  end;
end;

{ TRchLayerItem }

procedure TRchLayerItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TRchLayerItem then
  begin
    RechargeLayer := TRchLayerItem(Source).RechargeLayer;
  end;
  inherited;
end;

procedure TRchLayerItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRchLayerCollection;
  RechLayerObserver: TObserver;
begin
  ParentCollection := Collection as TRchLayerCollection;
  RechLayerObserver := FObserverList[LayerPosition];
  RechLayerObserver.OnUpToDateSet := ParentCollection.InvalidateRechLayerData;
end;

function TRchLayerItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TRchLayerItem.CreateFormulaObjects;
begin
  FRechargeLayer := CreateFormulaObject(dsoTop);
end;

destructor TRchLayerItem.Destroy;
begin
  RechargeLayer := '0';
  inherited;
end;

function TRchLayerItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    LayerPosition: result := RechargeLayer;
    else Assert(False);
  end;
end;

procedure TRchLayerItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FRechargeLayer);
  List.Add(FObserverList[LayerPosition]);
end;

function TRchLayerItem.GetRechargeLayer: string;
begin
  Result := FRechargeLayer.Formula;
  ResetItemObserver(LayerPosition);
end;

function TRchLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRchLayerItem;
begin
  result := (AnotherItem is TRchItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRchLayerItem(AnotherItem);
    result := (Item.RechargeLayer = RechargeLayer)
  end;
end;

procedure TRchLayerItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRechargeLayer,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TRchLayerItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    LayerPosition: RechargeLayer := Value;
    else Assert(False);
  end;
end;

procedure TRchLayerItem.SetRechargeLayer(const Value: string);
begin
  UpdateFormulaBlocks(Value, LayerPosition, FRechargeLayer);
end;

{ TRchLayerCollection }

procedure TRchLayerCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TRchLayerStorage.Create(AModel));
end;

procedure TRchLayerCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  RechargeLayerArray: TDataArray;
  Boundary: TRchLayerStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
begin
  BoundaryIndex := 0;
  RechargeLayerArray := DataSets[LayerPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TRchLayerStorage;
  RechargeLayerArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
  if LayerMin >= 0 then
  begin
    for LayerIndex := LayerMin to LayerMax do
    begin
      for RowIndex := RowMin to RowMax do
      begin
        for ColIndex := ColMin to ColMax do
        begin
          if RechargeLayerArray.IsValue[LayerIndex, RowIndex, ColIndex] then
          begin
            with Boundary.RchLayerArray[BoundaryIndex] do
            begin
              Cell.Layer := LayerIndex;
              Cell.Row := RowIndex;
              Cell.Column := ColIndex;
//              Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
              RechargeLayer := RechargeLayerArray.
                IntegerData[LayerIndex, RowIndex, ColIndex];
              RechargeLayerAnnotation := RechargeLayerArray.
                Annotation[LayerIndex, RowIndex, ColIndex];
            end;
            Inc(BoundaryIndex);
          end;
        end;
      end;
    end;
  end;
  RechargeLayerArray.CacheData;
  Boundary.CacheData;
end;

function TRchLayerCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TRchLayerTimeListLink;
end;

procedure TRchLayerCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TRchLayerItem;
  Boundary: TRchBoundary;
  ScreenObject: TScreenObject;
  ALink: TRchLayerTimeListLink;
  RechargeLayerData: TModflowTimeList;
begin
  Boundary := BoundaryGroup as TRchBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TRchLayerItem;
    BoundaryValues[Index].Time := Item.StartTime;
    BoundaryValues[Index].Formula := Item.RechargeLayer;
  end;
  ALink := TimeListLink.GetLink(AModel) as TRchLayerTimeListLink;
  RechargeLayerData := ALink.FRechargeLayerData;
  RechargeLayerData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(RechargeLayerData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(RechargeLayerData.Count, AModel);
  for TimeIndex := 0 to RechargeLayerData.Count - 1 do
  begin
    AddBoundary(TRchLayerStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(RechargeLayerData);
end;

procedure TRchLayerCollection.InvalidateRechLayerData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRchLayerTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRchLayerTimeListLink;
    Link.FRechargeLayerData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRchLayerTimeListLink;
      Link.FRechargeLayerData.Invalidate;
    end;
  end;
end;

class function TRchLayerCollection.ItemClass: TBoundaryItemClass;
begin
  result := TRchLayerItem;
end;

procedure TRchLayerCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TRchLayerStorage).FRchLayerArray,
    BoundaryCount);
  inherited;
end;

{ TRechargeLayerCell }

procedure TRechargeLayerCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRechargeLayerCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRechargeLayerCell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    LayerPosition: result := Values.RechargeLayerAnnotation;
    else Assert(False);
  end;
end;

function TRechargeLayerCell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  case Index of
    LayerPosition: result := Layer + 1;
    else Assert(False);
  end;
end;

function TRechargeLayerCell.GetLayer: integer;
begin
  // 1 is subtractred from RechargeLayer in order to compensate
  // for 1 being added to the layer in TModflowRCH_Writer.WriteRechargeLayer.
  result := Values.RechargeLayer-1;
end;

function TRechargeLayerCell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TRechargeLayerCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  Assert(False);
end;

function TRechargeLayerCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRechargeLayerCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TRechargeLayerCell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Rch_Cell: TRechargeLayerCell;
begin
  result := AnotherCell is TRechargeLayerCell;
  if result then
  begin
    Rch_Cell := TRechargeLayerCell(AnotherCell);
    result := (Values.Cell = Rch_Cell.Values.Cell);
  end;
end;

procedure TRechargeLayerCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRechargeLayerCell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TRechargeLayerCell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TRechargeLayerCell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TRechargeLayerCell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TRchStorage }

procedure TRchStorage.Clear;
begin
  SetLength(FRchArray, 0);
  FCleared := True;
end;

procedure TRchStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRchArray);
    for Index := 0 to Count - 1 do
    begin
      FRchArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRchArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TRchStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRchArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRchArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TRchStorage.GetRchArray: TRchArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRchArray;
end;

{ TRchLayerStorage }

procedure TRchLayerStorage.Clear;
begin
  SetLength(FRchLayerArray, 0);
  FCleared := True;
end;

procedure TRchLayerStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRchLayerArray);
    for Index := 0 to Count - 1 do
    begin
      FRchLayerArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRchLayerArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TRchLayerStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRchLayerArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRchLayerArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TRchLayerStorage.GetRchLayerArray: TRchLayerArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRchLayerArray;
end;

{ TRchRecord }

procedure TRchRecord.Assign(const Item: TRchRecord);
begin
  self := Item;
  GwtConcentrations.Assign(Item.GwtConcentrations);
end;

procedure TRchRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, RechargeRate);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, RechargeParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(RechargeRateAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RechargeParameterName));

  WriteCompInt(Comp, Strings.IndexOf(RechargePest));
  WriteCompInt(Comp, Strings.IndexOf(RechargePestSeries));
  WriteCompInt(Comp, Strings.IndexOf(RechargeTimeSeriesName));

  WriteCompInt(Comp, Ord(RechargePestMethod));

  GwtConcentrations.Cache(Comp, Strings);
end;

procedure TRchRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RechargeRateAnnotation);
  Strings.Add(RechargeParameterName);
  Strings.Add(RechargePest);
  Strings.Add(RechargePestSeries);
  Strings.Add(RechargeTimeSeriesName);

  GwtConcentrations.RecordStrings(Strings);
end;

procedure TRchRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  RechargeRate := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  RechargeParameterValue := ReadCompReal(Decomp);
  RechargeRateAnnotation := Annotations[ReadCompInt(Decomp)];
  RechargeParameterName := Annotations[ReadCompInt(Decomp)];
  RechargePest := Annotations[ReadCompInt(Decomp)];
  RechargePestSeries := Annotations[ReadCompInt(Decomp)];
  RechargeTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RechargePestMethod := TPestParamMethod(ReadCompInt(Decomp));

  GwtConcentrations.Restore(Decomp,Annotations);
end;

{ TRchLayerRecord }

procedure TRchLayerRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, RechargeLayer);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(RechargeLayerAnnotation));
end;

procedure TRchLayerRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(RechargeLayerAnnotation);
end;

procedure TRchLayerRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  RechargeLayer := ReadCompInt(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  RechargeLayerAnnotation := Annotations[ReadCompInt(Decomp)];
end;

{ TRchLayerTimeListLink }

procedure TRchLayerTimeListLink.CreateTimeLists;
begin
  inherited;
  FRechargeLayerData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRechargeLayerData.NonParamDescription := StrRechargeLayer;
  FRechargeLayerData.ParamDescription := ' ' + LowerCase(StrRechargeLayer);
  FRechargeLayerData.DataType := rdtInteger;
  AddTimeList(FRechargeLayerData);
  if Model <> nil then
  begin
    FRechargeLayerData.OnInvalidate := (Model as TCustomModel).InvalidateMfRchLayer;
  end;
end;

destructor TRchLayerTimeListLink.Destroy;
begin
  FRechargeLayerData.Free;
  inherited;
end;

{ TRchTimeListLink }

procedure TRchTimeListLink.CreateTimeLists;
var
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
  ConcTimeList: TModflowTimeList;
  LocalModel: TCustomModel;
begin
  inherited;
  FConcList := TModflowTimeLists.Create;

  FRechargeRateData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRechargeRateData.NonParamDescription := StrRechargeRate;
  FRechargeRateData.ParamDescription := StrRechargeRateMulti;
  AddTimeList(FRechargeRateData);
  if Model <> nil then
  begin
    FRechargeRateData.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
  end;

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      ConcTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
      ConcTimeList.NonParamDescription := PhastModel.MobileComponents[SpeciesIndex].Name;
      ConcTimeList.ParamDescription :=  ConcTimeList.NonParamDescription;
      if Model <> nil then
      begin
        LocalModel := Model as TCustomModel;
        ConcTimeList.OnInvalidate := LocalModel.InvalidateMfRchConc;
      end;
      AddTimeList(ConcTimeList);
      FConcList.Add(ConcTimeList);
    end;
  end;
end;

destructor TRchTimeListLink.Destroy;
begin
  FConcList.Free;
  FRechargeRateData.Free;
  inherited;
end;

{ TRchGwtConcCollection }

constructor TRchGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TRchCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

end.
