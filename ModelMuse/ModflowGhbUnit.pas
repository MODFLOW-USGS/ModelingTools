unit ModflowGhbUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, ModflowCellUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit, Modflow6DynamicTimeSeriesInterfaceUnit,
  System.Math;

type
  TGhbRecord = record
    Cell: TCellLocation;
    Conductance: double;
    BoundaryHead: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    BoundaryHeadAnnotation: string;
//    TimeSeriesName: string;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    ConductanceParameterName: string;
    ConductanceParameterValue: double;
    // PEST
    ConductancePest: string;
    BoundaryHeadPest: string;
    ConductancePestSeriesName: string;
    BoundaryHeadPestSeriesName: string;
    ConductancePestSeriesMethod: TPestParamMethod;
    BoundaryHeadPestSeriesMethod: TPestParamMethod;
    // MODFLOW 6 TimeSeries
    ConductanceTimeSeriesName: string;
    BoundaryHeadTimeSeriesName: string;
    // GWT Concentrations
    GwtConcentrations: TGwtCellData;
    procedure Assign(const Item: TGhbRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TGhbArray = array of TGhbRecord;

  TGhbStorage = class(TCustomBoundaryStorage)
  private
    FGhbArray: TGhbArray;
    function GetGhbArray: TGhbArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property GhbArray: TGhbArray read GetGhbArray;
  end;

  TGhbCollection = class;

  TGhbGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TGhbCollection);
  end;

  // @name represents a MODFLOW General Head boundary for one time interval.
  // @name is stored by @link(TGhbCollection).
  TGhbItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(BoundaryHead).
    FBoundaryHead: IFormulaObject;
    // See @link(Conductance).
    FConductance: IFormulaObject;
    FGwtConcentrations: TGhbGwtConcCollection;
    // See @link(BoundaryHead).
    procedure SetBoundaryHead(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetBoundaryHead: string;
    function GetConductance: string;
    procedure SetGwtConcentrations(const Value: TGhbGwtConcCollection);
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
    function GetConductanceIndex: Integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the boundary head
    // of this boundary.
    property BoundaryHead: string read GetBoundaryHead write SetBoundaryHead;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
    property GwtConcentrations: TGhbGwtConcCollection read FGwtConcentrations
      write SetGwtConcentrations;
  end;

  TGhbTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Boundary Heads for a series of
    // General Head Boundaries over a series of time intervals.
    FBoundaryHeadData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // General Head Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    FConcList: TModflowTimeLists;
    procedure AddGwtTimeLists(SpeciesIndex: Integer);
    procedure RemoveGwtTimeLists(SpeciesIndex: Integer);
  protected
    procedure UpdateGwtTimeLists; override;
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW General Head boundaries
  // for a series of time intervals.
  TGhbCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateHeadData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateGwtConcentrations(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name calls inherited @name and then sets the length of
    // the @link(TGhbStorage.GhbArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
  end;

  // Each @name stores a @link(TGhbCollection).
  // @classname is stored by @link(TModflowParameters).
  TGhbParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TGhb_Cell = class(TValueCell)
  private
    FValues: TGhbRecord;
    FStressPeriod: integer;
    function GetBoundaryHead: double;
    function GetConductance: double;
    function GetBoundaryHeadAnnotation: string;
    function GetConductanceAnnotation: string;
    function GetMvrUsed: Boolean;
    function GetMvrIndex: Integer;
    function GetConductanceParameterName: string;
    function GetConductanceParameterValue: double;
    function GetBoundaryHeadPest: string;
    function GetBoundaryHeadPestSeries: string;
    function GetBoundaryHeadPestSeriesMethod: TPestParamMethod;
    function GetConductancePest: string;
    function GetConductancePestSeries: string;
    function GetConductancePestSeriesMethod: TPestParamMethod;
    function GetBoundaryHeadTimeSeriesName: string;
    function GetConductanceTimeSeriesName: string;
    procedure SetBoundaryHeadTimeSeriesName(const Value: string);
    procedure SetConductanceTimeSeriesName(const Value: string);
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
    function GetPestName(Index: Integer): string; override;
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
    function GetMf6TimeSeriesName(Index: Integer): string; override;
    procedure SetMf6TimeSeriesName(Index: Integer; const Value: string); override;
  public
    property Conductance: double read GetConductance;
    property BoundaryHead: double read GetBoundaryHead;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property BoundaryHeadAnnotation: string read GetBoundaryHeadAnnotation;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property ConductanceParameterName: string read GetConductanceParameterName;
    property ConductanceParameterValue: double read GetConductanceParameterValue;
    // PEST parameters
    property BoundaryHeadPest: string read GetBoundaryHeadPest;
    property ConductancePest: string read GetConductancePest;
    property BoundaryHeadPestSeries: string read GetBoundaryHeadPestSeries;
    property ConductancePestSeries: string read GetConductancePestSeries;
    property BoundaryHeadPestSeriesMethod: TPestParamMethod
      read GetBoundaryHeadPestSeriesMethod;
    property ConductancePestSeriesMethod: TPestParamMethod
      read GetConductancePestSeriesMethod;
    property ConductanceTimeSeriesName: string read GetConductanceTimeSeriesName
      write SetConductanceTimeSeriesName;
    property BoundaryHeadTimeSeriesName: string
      read GetBoundaryHeadTimeSeriesName write SetBoundaryHeadTimeSeriesName;
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

  // @name represents the MODFLOW General-Head boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TGhbItem.Conductance
  // TGhbItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TGhbCollection)
  TGhbBoundary = class(TSpecificModflowBoundary)
  private
    FPestConductanceMethod: TPestParamMethod;
    FPestConductanceFormula: IFormulaObject;
    FPestHeadMethod: TPestParamMethod;
    FPestHeadFormula: IFormulaObject;
    FUsedObserver: TObserver;
    FPestConductanceObserver: TObserver;
    FPestHeadObserver: TObserver;
    FPestConcentrationFormulas: TGhbGwtConcCollection;
    FPestConcentrationMethods: TGwtPestMethodCollection;
    FConcentrationObservers: TObserverList;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
    function GetPestConductanceFormula: string;
    function GetPestConductanceObserver: TObserver;
    function GetPestHeadFormula: string;
    function GetPestHeadObserver: TObserver;
    procedure SetPestConductanceFormula(const Value: string);
    procedure SetPestConductanceMethod(const Value: TPestParamMethod);
    procedure SetPestHeadFormula(const Value: string);
    procedure SetPestHeadMethod(const Value: TPestParamMethod);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateHeadData(Sender: TObject);
    procedure InvalidateConcData(Sender: TObject);
    procedure SetPestConcentrationFormulas(const Value: TGhbGwtConcCollection);
    procedure SetPestConcentrationMethods(const Value: TGwtPestMethodCollection);
    function GetConcentrationObserver(const Index: Integer): TObserver;
//    function GetPConcentrationFormulas(const Index: Integer): string;
//    procedure SetPConcentrationFormulas(const Index: Integer;
//      const Value: string);
//    procedure SetInterp(const Value: TMf6InterpolationMethods);
  protected
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TGhb_Cell)s for that stress period.
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
    property PestHeadObserver: TObserver read GetPestHeadObserver;
    property ConcentrationObserver[const Index: Integer]: TObserver
      read GetConcentrationObserver;
    property PestConductanceObserver: TObserver read GetPestConductanceObserver;
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
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TGhbStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW GHB parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TGhbStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
//    property PConcentrationFormulas[const Index: Integer]: string
//      read GetPConcentrationFormulas write SetPConcentrationFormulas;
  published
    property Interp;
    property PestHeadFormula: string read GetPestHeadFormula
      write SetPestHeadFormula;
    property PestConductanceFormula: string read GetPestConductanceFormula
      write SetPestConductanceFormula;
    property PestHeadMethod: TPestParamMethod read FPestHeadMethod
      write SetPestHeadMethod;
    property PestConductanceMethod: TPestParamMethod
      read FPestConductanceMethod write SetPestConductanceMethod;
    property PestConcentrationFormulas: TGhbGwtConcCollection
      read FPestConcentrationFormulas write SetPestConcentrationFormulas;
    property PestConcentrationMethods: TGwtPestMethodCollection
      read FPestConcentrationMethods write SetPestConcentrationMethods;
  end;

const
  GhbHeadPosition = 0;
  GhbConductancePosition = 1;
  GhbStartConcentration = 2;

resourcestring
  StrBoundaryHeadSetTo = 'GHB Boundary head set to zero because of a math error';
  StrConductanceSetToZ = 'GHB Conductance set to zero because of a math error';
  StrConcentrationSetTo = 'GHB Concentration set to zero because of a math error';

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit,
  frmGoPhastUnit, GIS_Functions, ModflowMvrUnit,
  frmErrorsAndWarningsUnit, CellLocationUnit;

resourcestring
  StrConductance = 'Conductance';
  StrConductanceMultipl = ' conductance multiplier';

{ TGhbItem }

procedure TGhbItem.Assign(Source: TPersistent);
var
  Ghb: TGhbItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TGhbItem then
  begin
    Ghb := TGhbItem(Source);
    BoundaryHead := Ghb.BoundaryHead;
    Conductance := Ghb.Conductance;
    GwtConcentrations := Ghb.GwtConcentrations;
  end;
  inherited;
end;

procedure TGhbItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TGhbCollection;
  HeadObserver: TObserver;
  ConductanceObserver: TObserver;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TGhbCollection;
  HeadObserver := FObserverList[GhbHeadPosition];
  HeadObserver.OnUpToDateSet := ParentCollection.InvalidateHeadData;
  ConductanceObserver := FObserverList[GhbConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;

  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    GwtConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateGwtConcentrations; 
  end;
end;

function TGhbItem.BoundaryFormulaCount: integer;
begin
  result := 2;
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

constructor TGhbItem.Create(Collection: TCollection);
var
  GhbCol: TGhbCollection;
begin
  GhbCol := Collection as TGhbCollection;
  FGwtConcentrations := TGhbGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    GhbCol);
  inherited;
end;

procedure TGhbItem.CreateFormulaObjects;
begin
  FBoundaryHead := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
end;

destructor TGhbItem.Destroy;
var
  Index: Integer;
begin
  BoundaryHead := '0';
  Conductance := '0';

  for Index := 0 to FGwtConcentrations.Count - 1 do
  begin
    FGwtConcentrations[Index].Value := '0';
  end;
  FGwtConcentrations.Free;
  inherited;
end;

function TGhbItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TGwtConcStringValueItem;
begin
  case Index of
    GhbHeadPosition: result := BoundaryHead;
    GhbConductancePosition: result := Conductance;
    else
      begin
        Dec(Index, 2);
        while GwtConcentrations.Count <= Index do
        begin
          GwtConcentrations.Add;
        end;
        Item := GwtConcentrations[Index];
        result := Item.Value;
      end;
  end;
end;

function TGhbItem.GetBoundaryHead: string;
begin
  FBoundaryHead.ScreenObject := ScreenObjectI;
  try
    Result := FBoundaryHead.Formula;
  finally
    FBoundaryHead.ScreenObject := nil;
  end;
  ResetItemObserver(GhbHeadPosition);
end;

function TGhbItem.GetConductance: string;
begin
  FConductance.ScreenObject := ScreenObjectI;
  try
    Result := FConductance.Formula;
  finally
    FConductance.ScreenObject := ScreenObjectI;
  end;
  ResetItemObserver(GhbConductancePosition);
end;

function TGhbItem.GetConductanceIndex: Integer;
begin
  Result := GhbConductancePosition;
end;

procedure TGhbItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  Item: TGwtConcStringValueItem;
  ConcIndex: Integer;
begin
  if Sender = FConductance as TObject then
  begin
    List.Add(FObserverList[GhbConductancePosition]);
  end;
  if Sender = FBoundaryHead as TObject then
  begin
    List.Add(FObserverList[GhbHeadPosition]);
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

procedure TGhbItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfGhbConductance(self);
    PhastModel.InvalidateMfGhbBoundaryHead(self);
    PhastModel.InvalidateMfGhbConc(self);
  end;
end;

function TGhbItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TGhbItem;
begin
  result := (AnotherItem is TGhbItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TGhbItem(AnotherItem);
    result := (Item.BoundaryHead = BoundaryHead)
      and (Item.Conductance = Conductance)
      and (Item.GwtConcentrations.IsSame(GwtConcentrations));
  end;
end;

procedure TGhbItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBoundaryHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TGhbItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TGwtConcStringValueItem;
begin
  inherited;
  case Index of
    GhbHeadPosition: BoundaryHead := Value;
    GhbConductancePosition: Conductance := Value;
    else
      begin
        Dec(Index, 2);
        while Index >= GwtConcentrations.Count do
        begin
          GwtConcentrations.Add;
        end;
        Item := GwtConcentrations[Index];
        Item.Value := Value;
      end;
  end;
end;

procedure TGhbItem.SetBoundaryHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, GhbHeadPosition, FBoundaryHead);
end;

procedure TGhbItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, GhbConductancePosition, FConductance);
end;

procedure TGhbItem.SetGwtConcentrations(const Value: TGhbGwtConcCollection);
begin
  FGwtConcentrations.Assign(Value);
end;

{ TGhbCollection }

class function TGhbCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TGhbTimeListLink;
end;

procedure TGhbCollection.TestIfObservationsPresent(
  var EndOfLastStressPeriod: Double; var StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
var
  Boundary: TGhbBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TGhbBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

procedure TGhbCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TGhbStorage.Create(AModel));
end;

function TGhbCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TGhbBoundary;
  ScreenObject: TScreenObject;
  Item: TGhbItem;
begin
  Item := Items[ItemIndex] as TGhbItem;
  if FormulaIndex = GhbConductancePosition then
  begin
    Boundary := BoundaryGroup as TGhbBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    case Boundary.FormulaInterpretation of
      fiSpecific:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.Conductance;
          end
          else if ScreenObject.Closed then
          begin
            result := '(' + Item.Conductance
              + ') * ' + StrObjectIntersectArea;
          end
          else
          begin
            result := '(' + Item.Conductance
              + ') * ' + StrObjectSectionIntersectLength;
          end;
        end;
      fiDirect:
        begin
          result := Item.Conductance;
        end;
      fiTotal:
        begin
          if ScreenObject.ScreenObjectLength = 0 then
          begin
            result := Item.Conductance;
          end
          else if ScreenObject.Closed then
          begin
            result := '((' + Item.Conductance
              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
          end
          else
          begin
            result := '((' + Item.Conductance
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

procedure TGhbCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  GhbStorage: TGhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  ConcIndex: Integer;
  AllowedIndicies: Set of Byte;
  SpeciesIndex: Byte;
  LocalModel: TCustomModel;
  ErrorMessage: string;
  LocalScreenObject: TScreenObject;
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

  AllowedIndicies := [GhbHeadPosition,GhbConductancePosition];
  LocalModel := AModel as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      Include(AllowedIndicies, GhbStartConcentration + SpeciesIndex);
    end;
  end;

  Assert(BoundaryFunctionIndex in AllowedIndicies);
  Assert(Expression <> nil);

  GhbStorage := BoundaryStorage as TGhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    AssignDynamicTimeSeries(TimeSeriesName, DynamicTimeSeries, ACell);

    try
      Expression.Evaluate;
      with GhbStorage.GhbArray[Index] do
      begin
        case BoundaryFunctionIndex of
          GhbHeadPosition:
            begin
              BoundaryHead := Expression.DoubleResult;
              BoundaryHeadAnnotation := ACell.Annotation;
              BoundaryHeadPest := PestName;
              BoundaryHeadPestSeriesName := PestSeriesName;
              BoundaryHeadPestSeriesMethod := PestSeriesMethod;
              BoundaryHeadTimeSeriesName := TimeSeriesName;
            end;
          GhbConductancePosition:
            begin
              Conductance := Expression.DoubleResult;
              ConductanceAnnotation := ACell.Annotation;
              ConductancePest := PestName;
              ConductancePestSeriesName := PestSeriesName;
              ConductancePestSeriesMethod := PestSeriesMethod;
              ConductanceTimeSeriesName := TimeSeriesName;
            end;
          else
            begin
              ConcIndex := BoundaryFunctionIndex - GhbStartConcentration;
              GwtConcentrations.Values[ConcIndex] := Expression.DoubleResult;
              GwtConcentrations.ValueAnnotations[ConcIndex] := ACell.Annotation;;
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
        with GhbStorage.GhbArray[Index] do
        begin
          case BoundaryFunctionIndex of
            GhbHeadPosition:
              begin
                BoundaryHead := 0;
                ErrorMessage := StrBoundaryHeadSetTo;
                BoundaryHeadAnnotation := ErrorMessage;
                BoundaryHeadPest := PestName;
                BoundaryHeadPestSeriesName := PestSeriesName;
                BoundaryHeadPestSeriesMethod := PestSeriesMethod;
                BoundaryHeadTimeSeriesName := TimeSeriesName;
              end;
            GhbConductancePosition:
              begin
                Conductance := 0;
                ErrorMessage := StrConductanceSetToZ;
                ConductanceAnnotation := ErrorMessage;
                ConductancePest := PestName;
                ConductancePestSeriesName := PestSeriesName;
                ConductancePestSeriesMethod := PestSeriesMethod;
                ConductanceTimeSeriesName := TimeSeriesName;
              end;
            else
              begin
                ConcIndex := BoundaryFunctionIndex - GhbStartConcentration;
                GwtConcentrations.Values[ConcIndex] := 0;
                ErrorMessage := StrConcentrationSetTo;
                GwtConcentrations.ValueAnnotations[ConcIndex] := ErrorMessage;
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
        with GhbStorage.GhbArray[Index] do
        begin
          case BoundaryFunctionIndex of
            GhbHeadPosition:
              begin
                BoundaryHead := 0;
                ErrorMessage := StrBoundaryHeadSetTo;
                BoundaryHeadAnnotation := ErrorMessage;
                BoundaryHeadPest := PestName;
                BoundaryHeadPestSeriesName := PestSeriesName;
                BoundaryHeadPestSeriesMethod := PestSeriesMethod;
                BoundaryHeadTimeSeriesName := TimeSeriesName;
              end;
            GhbConductancePosition:
              begin
                Conductance := 0;
                ErrorMessage := StrConductanceSetToZ;
                ConductanceAnnotation := ErrorMessage;
                ConductancePest := PestName;
                ConductancePestSeriesName := PestSeriesName;
                ConductancePestSeriesMethod := PestSeriesMethod;
                ConductanceTimeSeriesName := TimeSeriesName;
              end;
            else
              begin
                ConcIndex := BoundaryFunctionIndex - GhbStartConcentration;
                GwtConcentrations.Values[ConcIndex] := 0;
                ErrorMessage := StrConductanceSetToZ;
                GwtConcentrations.ValueAnnotations[ConcIndex] := ErrorMessage;
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

procedure TGhbCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  GhbStorage: TGhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  GhbStorage := BoundaryStorage as TGhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with GhbStorage.GhbArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TGhbCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  Index: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TGhbStorage).FGhbArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for Index := 0 to BoundaryCount - 1 do
    begin
      SetLength(TGhbStorage(Boundaries[ItemIndex, AModel]).FGhbArray[Index].GwtConcentrations.Values,
        LocalModel.MobileComponents.Count);
      SetLength(TGhbStorage(Boundaries[ItemIndex, AModel]).FGhbArray[Index].GwtConcentrations.ValueAnnotations,
        LocalModel.MobileComponents.Count);
      SetLength(TGhbStorage(Boundaries[ItemIndex, AModel]).FGhbArray[Index].GwtConcentrations.ValuePestNames,
        LocalModel.MobileComponents.Count);
      SetLength(TGhbStorage(Boundaries[ItemIndex, AModel]).FGhbArray[Index].GwtConcentrations.ValuePestSeriesNames,
        LocalModel.MobileComponents.Count);
      SetLength(TGhbStorage(Boundaries[ItemIndex, AModel]).FGhbArray[Index].GwtConcentrations.ValuePestSeriesMethods,
        LocalModel.MobileComponents.Count);
      SetLength(TGhbStorage(Boundaries[ItemIndex, AModel]).FGhbArray[Index].GwtConcentrations.ValueTimeSeriesNames,
        LocalModel.MobileComponents.Count);
    end;
  end;
  inherited;
end;

procedure TGhbCollection.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TGhbTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TGhbTimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TGhbTimeListLink;
        Link.FConductanceData.Invalidate;
      end;
    end;
  end;
end;

procedure TGhbCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TGhbTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TGhbTimeListLink;
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
        Link := TimeListLink.GetLink(ChildModel) as TGhbTimeListLink;
        for Index := 0 to Link.FConcList.Count - 1 do
        begin
          TimeList := Link.FConcList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TGhbCollection.InvalidateHeadData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TGhbTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TGhbTimeListLink;
    Link.FBoundaryHeadData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TGhbTimeListLink;
        Link.FBoundaryHeadData.Invalidate;
      end;
    end;
  end;
end;

procedure TGhbCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfGhbConductance(self);
    PhastModel.InvalidateMfGhbBoundaryHead(self);
    PhastModel.InvalidateMfGhbConc(self);
  end;
end;

class function TGhbCollection.ItemClass: TBoundaryItemClass;
begin
  result := TGhbItem;
end;

{ TGhbParamItem }

class function TGhbParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TGhbCollection;
end;

{ TGhb_Cell }

procedure TGhb_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  FValues.Cache(Comp, Strings);
  WriteCompInt(Comp, FStressPeriod);
end;

function TGhb_Cell.GetBoundaryHead: double;
begin
  result := FValues.BoundaryHead;
end;

function TGhb_Cell.GetBoundaryHeadAnnotation: string;
begin
  result := FValues.BoundaryHeadAnnotation;
end;

function TGhb_Cell.GetBoundaryHeadPest: string;
begin
  result := FValues.BoundaryHeadPest;
end;

function TGhb_Cell.GetBoundaryHeadPestSeries: string;
begin
  result := FValues.BoundaryHeadPestSeriesName;
end;

function TGhb_Cell.GetBoundaryHeadPestSeriesMethod: TPestParamMethod;
begin
  result := FValues.BoundaryHeadPestSeriesMethod;
end;

function TGhb_Cell.GetBoundaryHeadTimeSeriesName: string;
begin
  result := FValues.BoundaryHeadTimeSeriesName;
end;

function TGhb_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TGhb_Cell.GetConcentration(const Index: Integer): double;
begin
  result := FValues.GwtConcentrations.Values[Index];
end;

function TGhb_Cell.GetConcentrationAnnotation(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValueAnnotations[Index];
end;

function TGhb_Cell.GetConcentrationPestName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValuePestNames[Index];
end;

function TGhb_Cell.GetConcentrationPestSeriesMethod(
  const Index: Integer): TPestParamMethod;
begin
  result := FValues.GwtConcentrations.ValuePestSeriesMethods[Index];
end;

function TGhb_Cell.GetConcentrationPestSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValuePestSeriesNames[Index];
end;

function TGhb_Cell.GetConcentrationTimeSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValueTimeSeriesNames[Index];
end;

function TGhb_Cell.GetConductance: double;
begin
  result := FValues.Conductance;
end;

function TGhb_Cell.GetConductanceAnnotation: string;
begin
  result := FValues.ConductanceAnnotation;
end;

function TGhb_Cell.GetConductanceParameterName: string;
begin
  result := FValues.ConductanceParameterName;
end;

function TGhb_Cell.GetConductanceParameterValue: double;
begin
  result := FValues.ConductanceParameterValue;
end;

function TGhb_Cell.GetConductancePest: string;
begin
  result := FValues.ConductancePest;
end;

function TGhb_Cell.GetConductancePestSeries: string;
begin
  result := FValues.ConductancePestSeriesName;
end;

function TGhb_Cell.GetConductancePestSeriesMethod: TPestParamMethod;
begin
  result := FValues.ConductancePestSeriesMethod;
end;

function TGhb_Cell.GetConductanceTimeSeriesName: string;
begin
  result := FValues.ConductanceTimeSeriesName;
end;

function TGhb_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TGhb_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TGhb_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TGhb_Cell.GetMf6TimeSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        result := BoundaryHeadTimeSeriesName;
      end;
    GhbConductancePosition:
      begin
        result := ConductanceTimeSeriesName;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        result := FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex];
      end;
  end;
end;

function TGhb_Cell.GetMvrIndex: Integer;
begin
  result := FValues.MvrIndex;
end;

function TGhb_Cell.GetMvrUsed: Boolean;
begin
  result := FValues.MvrUsed;
end;

function TGhb_Cell.GetPestName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        result := BoundaryHeadPest;
      end;
    GhbConductancePosition:
      begin
        result := ConductancePest;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        result := FValues.GwtConcentrations.ValuePestNames[ConcIndex];
      end;
  end;
end;

function TGhb_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        result := BoundaryHeadPestSeriesMethod;
      end;
    GhbConductancePosition:
      begin
        result := ConductancePestSeriesMethod;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesMethods[ConcIndex];
      end;
  end;
end;

function TGhb_Cell.GetPestSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        result := BoundaryHeadPestSeries;
      end;
    GhbConductancePosition:
      begin
        result := ConductancePestSeries;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesNames[ConcIndex];
      end;
  end;
end;

function TGhb_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        result := BoundaryHeadAnnotation;
      end;
    GhbConductancePosition:
      begin
        result := ConductanceAnnotation;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        result := FValues.GwtConcentrations.ValueAnnotations[ConcIndex];
      end;
  end;
end;

function TGhb_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        result := BoundaryHead;
      end;
    GhbConductancePosition:
      begin
        result := Conductance;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        result := FValues.GwtConcentrations.Values[ConcIndex];
      end;
  end;
end;

function TGhb_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TGhb_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

//function TGhb_Cell.GetTimeSeriesName: string;
//begin
//  result := FValues.TimeSeriesName;
//end;

function TGhb_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  GHB_Cell: TGhb_Cell;
begin
  result := AnotherCell is TGhb_Cell;
  if result then
  begin
    GHB_Cell := TGhb_Cell(AnotherCell);
    result :=
      (Conductance = GHB_Cell.Conductance)
      and (BoundaryHead = GHB_Cell.BoundaryHead)
      and (IFace = GHB_Cell.IFace)
      and (FValues.Cell = GHB_Cell.FValues.Cell);
  end;
end;

procedure TGhb_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  FValues.RecordStrings(Strings);
end;

procedure TGhb_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  FValues.Restore(Decomp, Annotations);
  FStressPeriod := ReadCompInt(Decomp);
end;

procedure TGhb_Cell.SetBoundaryHeadTimeSeriesName(const Value: string);
begin
  FValues.BoundaryHeadTimeSeriesName := Value;
end;

procedure TGhb_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TGhb_Cell.SetConductanceTimeSeriesName(const Value: string);
begin
  FValues.ConductanceTimeSeriesName := Value;
end;

procedure TGhb_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TGhb_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
var
  ConcIndex: Integer;
begin
  case Index of
    GhbHeadPosition:
      begin
        BoundaryHeadTimeSeriesName := Value;
      end;
    GhbConductancePosition:
      begin
        ConductanceTimeSeriesName := Value;
      end;
    else
      begin
        ConcIndex := Index - GhbStartConcentration;
        FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := Value;
      end;
  end;
end;

procedure TGhb_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TGhbBoundary }

procedure TGhbBoundary.Assign(Source: TPersistent);
var
  SourceGhb: TGhbBoundary;
begin
  if Source is TGhbBoundary then
  begin
    SourceGhb := TGhbBoundary(Source);
    PestHeadFormula := SourceGhb.PestHeadFormula;
    PestConductanceFormula := SourceGhb.PestConductanceFormula;
    PestHeadMethod := SourceGhb.PestHeadMethod;
    PestConductanceMethod := SourceGhb.PestConductanceMethod;
    PestConcentrationFormulas := SourceGhb.PestConcentrationFormulas;
    PestConcentrationMethods := SourceGhb.PestConcentrationMethods;
  end;
  inherited;
end;

procedure TGhbBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TGhb_Cell;
  BoundaryValues: TGhbRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TGhbStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
  MvrUsed: Boolean;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TGhbStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcGhb);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TGhb_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.GhbArray) then
      begin
        Cells.Capacity := Cells.Count + Max(Length(LocalBoundaryStorage.GhbArray), Cells.Count div 4);
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.GhbArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.GhbArray[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed;
        BoundaryValues.MvrIndex := BoundaryIndex;
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.Conductance :=
            BoundaryValues.Conductance * FCurrentParameter.Value;
          BoundaryValues.ConductanceAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.ConductanceAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.ConductanceParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.ConductanceParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.ConductanceParameterName := '';
          BoundaryValues.ConductanceParameterValue := 1;
        end;
        Cell := TGhb_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
        Cells.Add(Cell);
        Cell.FStressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TGhbBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TGhbCollection;
end;

function TGhbBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestGhb_';
end;

constructor TGhbBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FPestConcentrationFormulas:= TGhbGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestConcentrationFormulas.UsedForPestSeries := True;
  FPestConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FConcentrationObservers := TObserverList.Create;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestHeadFormula := '';
  PestConductanceFormula := '';
  FPestHeadMethod := DefaultBoundaryMethod(GhbHeadPosition);
  FPestConductanceMethod := DefaultBoundaryMethod(GhbConductancePosition);
end;

procedure TGhbBoundary.CreateFormulaObjects;
var
  LocalModel: TPhastModel;
  ConcIndex: Integer;
begin
  FPestHeadFormula := CreateFormulaObjectBlocks(dso3D);
  FPestConductanceFormula := CreateFormulaObjectBlocks(dso3D);
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestConcentrationFormulas.Add;
    end;
  end;
end;

procedure TGhbBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestHeadObserver);
    FObserverList.Add(PestConductanceObserver);
    for Index := 0 to FPestConcentrationFormulas.Count - 1 do
    begin
      FObserverList.Add(ConcentrationObserver[Index]);
    end;
  end;
end;

class function TGhbBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    GhbHeadPosition:
      begin
        result := ppmAdd;
      end;
    GhbConductancePosition:
      begin
        result := ppmMultiply;
      end;
    else
      begin
        result := ppmMultiply;
      end;
  end;
end;

destructor TGhbBoundary.Destroy;
var
  Index: Integer;
begin
  PestHeadFormula := '';
  PestConductanceFormula := '';

  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    FPestConcentrationFormulas[Index].Value := '';
  end;

  inherited;

  FPestConcentrationMethods.Free;
  FPestConcentrationFormulas.Free;
  FConcentrationObservers.Free;
end;

procedure TGhbBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TGhbStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  EndOfLastStressPeriod: Double;
  StartOfFirstStressPeriod: Double;
  ObservationsPresent: Boolean;
  PriorTime: Double;
  ValueCount: Integer;
  Item: TCustomModflowBoundaryItem;
  LocalModel: TCustomModel;
begin
  FCurrentParameter := nil;
  EvaluateListBoundaries(AModel);
  TestIfObservationsPresent(EndOfLastStressPeriod, StartOfFirstStressPeriod,
    ObservationsPresent);
  PriorTime := StartOfFirstStressPeriod;
  ValueCount := 0;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    Item := Values[ValueIndex] as TCustomModflowBoundaryItem;
    if ObservationsPresent then
    begin
      if PriorTime <= Item.StartTime then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TGhbStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TGhbStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
    if ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TGhbStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
    end;
  end;
  LocalModel := AModel as TCustomModel;
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

    PriorTime := StartOfFirstStressPeriod;
    ValueCount := 0;
    for ValueIndex := 0 to Param.Param.Count - 1 do
    begin
      Item := Param.Param[ValueIndex] as TCustomModflowBoundaryItem;
      if ObservationsPresent then
      begin
        if PriorTime < Item.StartTime then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TGhbStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TGhbStorage;
        AssignCells(BoundaryStorage, Times, AModel);
        Inc(ValueCount);
      end;
      if ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TGhbStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

function TGhbBoundary.GetConcentrationObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FConcentrationObservers.Count do
  begin
    CreateObserver(Format('GhbConc_%d', [Index+1]), AObserver, nil);
    FConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateConcData;
  end;
  result := FConcentrationObservers[Index];
end;

function TGhbBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ConcIndex: Integer;
begin
//  result := '';
  case FormulaIndex of
    GhbHeadPosition:
      begin
        result := PestHeadFormula;
      end;
    GhbConductancePosition:
      begin
        result := PestConductanceFormula;
      end;
    else
      begin
        ConcIndex := FormulaIndex - GhbStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        result := PestConcentrationFormulas[ConcIndex].Value;
      end;
  end;
end;

function TGhbBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
//  result := PestConductanceMethod;
  case FormulaIndex of
    GhbHeadPosition:
      begin
        result := PestHeadMethod;
      end;
    GhbConductancePosition:
      begin
        result := PestConductanceMethod;
      end;
    else
      begin
        ConcIndex := FormulaIndex - GhbStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        result := FPestConcentrationMethods[ConcIndex].PestParamMethod;
      end;
  end;
end;

function TGhbBoundary.GetPestConductanceFormula: string;
begin
  Result := FPestConductanceFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(GhbConductancePosition);
  end;
end;

function TGhbBoundary.GetPestConductanceObserver: TObserver;
begin
  if FPestConductanceObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestConductanceObserver, nil);
    FPestConductanceObserver.OnUpToDateSet := InvalidateConductanceData;
  end;
  result := FPestConductanceObserver;
end;

function TGhbBoundary.GetPestHeadFormula: string;
begin
  Result := FPestHeadFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(GhbHeadPosition);
  end;

end;

function TGhbBoundary.GetPestHeadObserver: TObserver;
begin
  if FPestHeadObserver = nil then
  begin
    CreateObserver('PestGhbHead_', FPestHeadObserver, nil);
    FPestHeadObserver.OnUpToDateSet := InvalidateHeadData;
  end;
  result := FPestHeadObserver;
end;

procedure TGhbBoundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: Integer;
begin
  if Sender = FPestHeadFormula as TObject then
  begin
    if GhbHeadPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[GhbHeadPosition]);
    end;
  end;
  if Sender = FPestConductanceFormula as TObject then
  begin
    if GhbConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[GhbConductancePosition]);
    end;
  end;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    if FPestConcentrationFormulas[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[GhbStartConcentration + Index]);
    end;
  end;

end;

function TGhbBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestGhb_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TGhbBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TGhbBoundary.InvalidateConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfGhbConc(self);

end;

procedure TGhbBoundary.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfGhbConductance(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateMfGhbConductance(self);
    end;
  end;
end;

procedure TGhbBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfGhbConductance(self);
    Model.InvalidateMfGhbBoundaryHead(self);
    Model.InvalidateMfGhbConc(self);
  end;
end;

procedure TGhbBoundary.InvalidateHeadData(Sender: TObject);
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfGhbBoundaryHead(self);

  for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
  begin
    ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
    if ChildModel <> nil then
    begin
      ChildModel.InvalidateMfGhbBoundaryHead(self);
    end;
  end;
end;

class function TGhbBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TGhbParamItem;
end;

function TGhbBoundary.ParameterType: TParameterType;
begin
  result := ptGHB;
end;

procedure TGhbBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    GhbHeadPosition:
      begin
        PestHeadFormula := Value;
      end;
    GhbConductancePosition:
      begin
        PestConductanceFormula := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - GhbStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        PestConcentrationFormulas[ConcIndex].Value := Value;
      end;
  end;
end;

procedure TGhbBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    GhbHeadPosition:
      begin
        PestHeadMethod := Value;
      end;
    GhbConductancePosition:
      begin
        PestConductanceMethod := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - GhbStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        FPestConcentrationMethods[ConcIndex].PestParamMethod := Value;
      end;
  end;
end;

procedure TGhbBoundary.SetPestConcentrationFormulas(const Value: TGhbGwtConcCollection);
begin
  FPestConcentrationFormulas.Assign(Value);
end;

procedure TGhbBoundary.SetPestConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestConcentrationMethods.Assign(Value);
end;

procedure TGhbBoundary.SetPestConductanceFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, GhbConductancePosition, FPestConductanceFormula);
end;

procedure TGhbBoundary.SetPestConductanceMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestConductanceMethod, Value);
end;

procedure TGhbBoundary.SetPestHeadFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, GhbHeadPosition, FPestHeadFormula);
end;

procedure TGhbBoundary.SetPestHeadMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestHeadMethod, Value);
end;

//procedure TGhbBoundary.SetInterp(const Value: TMf6InterpolationMethods);
//begin
//  if FInterp <> Value then
//  begin
//    InvalidateModel;
//    FInterp := Value;
//  end;
//end;

procedure TGhbBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  LocalPhastModel: TPhastModel;
  LocalScreenObject: TScreenObject;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalPhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(LocalPhastModel <> nil);
  ObservationsPresent := LocalPhastModel.GbobIsSelected
    and (LocalPhastModel.GhbObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

{ TGhbRecord }

procedure TGhbRecord.Assign(const Item: TGhbRecord);
begin
  self := Item;
  GwtConcentrations.Assign(Item.GwtConcentrations);
end;

procedure TGhbRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, BoundaryHead);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, ConductanceParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryHeadAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ConductanceParameterName));
  WriteCompInt(Comp, Strings.IndexOf(ConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryHeadPest));
  WriteCompInt(Comp, Strings.IndexOf(ConductancePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryHeadPestSeriesName));
  WriteCompInt(Comp, Ord(ConductancePestSeriesMethod));
  WriteCompInt(Comp, Ord(BoundaryHeadPestSeriesMethod));

  WriteCompInt(Comp, Strings.IndexOf(ConductanceTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryHeadTimeSeriesName));

  GwtConcentrations.Cache(Comp, Strings);

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TGhbRecord.RecordStrings(Strings: TStringList);
//var
//  Index: Integer;
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(BoundaryHeadAnnotation);
  Strings.Add(ConductanceParameterName);
  Strings.Add(ConductancePest);
  Strings.Add(BoundaryHeadPest);
  Strings.Add(ConductancePestSeriesName);
  Strings.Add(BoundaryHeadPestSeriesName);
  Strings.Add(ConductanceTimeSeriesName);
  Strings.Add(BoundaryHeadTimeSeriesName);

  GwtConcentrations.RecordStrings(Strings);
end;

procedure TGhbRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  BoundaryHead := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceParameterValue := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  BoundaryHeadAnnotation := Annotations[ReadCompInt(Decomp)];
  ConductanceParameterName := Annotations[ReadCompInt(Decomp)];
  ConductancePest := Annotations[ReadCompInt(Decomp)];
  BoundaryHeadPest := Annotations[ReadCompInt(Decomp)];
  ConductancePestSeriesName := Annotations[ReadCompInt(Decomp)];
  BoundaryHeadPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductancePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  BoundaryHeadPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

  ConductanceTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  BoundaryHeadTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  GwtConcentrations.Restore(Decomp,Annotations);

  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);
end;

{ TGhbStorage }


procedure TGhbStorage.Clear;
begin
  SetLength(FGhbArray, 0);
  FCleared := True;
end;

procedure TGhbStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FGhbArray);
    for Index := 0 to Count - 1 do
    begin
      FGhbArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FGhbArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TGhbStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FGhbArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FGhbArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TGhbStorage.GetGhbArray: TGhbArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FGhbArray;
end;

{ TGhbTimeListLink }

procedure TGhbTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  inherited;
  FConcList := TModflowTimeLists.Create;

  FBoundaryHeadData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBoundaryHeadData.NonParamDescription := StrBoundaryHead;
  FBoundaryHeadData.ParamDescription := ' ' + LowerCase(StrBoundaryHead);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;
  AddTimeList(FBoundaryHeadData);
  AddTimeList(FConductanceData);

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfGhbConductance;
    FBoundaryHeadData.OnInvalidate := LocalModel.InvalidateMfGhbBoundaryHead;
  end;

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

destructor TGhbTimeListLink.Destroy;
begin
  FConcList.Free;
  FBoundaryHeadData.Free;
  FConductanceData.Free;
  inherited;
end;

procedure TGhbTimeListLink.RemoveGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
begin
  ConcTimeList := FConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FConcList.Delete(SpeciesIndex);
end;

procedure TGhbTimeListLink.UpdateGwtTimeLists;
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

procedure TGhbTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
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
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMfGhbConc;
  end;
  AddTimeList(ConcTimeList);
  FConcList.Add(ConcTimeList);
end;

{ TGhbGwtConcCollection }

constructor TGhbGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TGhbCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

end.
