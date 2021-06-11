unit SutraBoundaryUnit;

interface

uses
  GoPhastTypes, Classes, OrderedCollectionUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, Generics.Collections, RbwParser, DataSetUnit,
  SysUtils, SubscriptionUnit, SutraOptionsUnit;

type
  TObservationFormat = (ofOBS, ofOBC);
  TObservationFormats = set of TObservationFormat;

  TSutraBoundaryType = (sbtFluidSource, sbtMassEnergySource, sbtSpecPress,
    sbtSpecConcTemp);

  TSutraBoundaryValue = record
    Time: double;
    Formula: string;
    UsedFormula: string;
//    LakeInteraction: TLakeBoundaryInteraction;
  end;

  TSutraBoundaryValueArray = array of TSutraBoundaryValue;

  TSutraTimeList = class(TCustomTimeList)
  private
    FScreenObject: TObject;
    FDescription: string;
  protected
    procedure CheckSameModel(const Data: TDataArray); override;
  public
    constructor Create(Model: TBaseModel; ScreenObject: TObject);
    procedure Initialize(BoundaryValues: TSutraBoundaryValueArray); reintroduce;
    property Description: string read FDescription write FDescription;
  end;

  {
  Merge @link(TSutraTimeList)s as follows.

  1. Get a list of @link(TSutraTimeList)s in the same order as the
  @link(TScreenObject)s that define them.

  2. Get a combined list of all the times in the @link(TSutraTimeList)s

  3. Merge each @link(TSutraTimeList) in turn, into the combined list.
  For each data set in each @link(TSutraTimeList), apply from its
  start time up until but not including
  the next start time included in the @link(TSutraTimeList).

  3a. With specified pressures and specified temperature/concentration,
  use the value in the last @link(TSutraTimeList).

  3b. With specified flux, add the flux to the existing flux and compute
  a weighted average of the injection concentrations or temperatures.

  3c. With solute/energy sources, add the solute/energy source to
  the existing solute/energy sources.

  Merging @link(TSutraTimeList)s is done in
  @link(TSutraBoundaryWriter.UpdateMergeLists).

  @name can be used for both export and display.
  }
  TSutraMergedTimeList = class(TCustomTimeList)
  private
//    FOnGetUseList: TOnGetUseList;
    FOnInitialize: TNotifyEvent;
    procedure SetOnInitialize(const Value: TNotifyEvent);
  public
    procedure Initialize; override;
//    property OnGetUseList: TOnGetUseList read FOnGetUseList
//      write FOnGetUseList;
    property OnInitialize: TNotifyEvent read FOnInitialize
      write SetOnInitialize;
    property UpToDate: boolean read GetUpToDate write SetUpToDate;
  end;

  TSutraBoundary = class(TModflowBoundary)
  private
    FLakeInteraction: TLakeBoundaryInteraction;
    FUseBCTime: Boolean;
    FPestBoundaryValueMethod: TPestParamMethod;
    FPestAssociatedValueMethod: TPestParamMethod;
    procedure SetLakeInteraction(const Value: TLakeBoundaryInteraction);
    procedure SetUseBCTime(const Value: Boolean);
    function GetPestAssociatedValueFormula: string;
    function GetPestBoundaryValueFormula: string;
    procedure SetPestAssociatedValueFormula(const Value: string);
    procedure SetPestAssociatedValueMethod(const Value: TPestParamMethod);
    procedure SetPestBoundaryValueFormula(const Value: string);
    procedure SetPestBoundaryValueMethod(const Value: TPestParamMethod);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
//    procedure ResetObserversUptodate;
    procedure Changed;
    procedure Loaded;
  published
    property LakeInteraction: TLakeBoundaryInteraction read FLakeInteraction
      write SetLakeInteraction default lbiUseDefaults;
    property UseBCTime: Boolean read FUseBCTime write SetUseBCTime stored True;

    property PestBoundaryValueFormula: string read GetPestBoundaryValueFormula
      write SetPestBoundaryValueFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestBoundaryValueMethod: TPestParamMethod read FPestBoundaryValueMethod
      write SetPestBoundaryValueMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestAssociatedValueFormula: string read GetPestAssociatedValueFormula
      write SetPestAssociatedValueFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestAssociatedValueMethod: TPestParamMethod
      read FPestAssociatedValueMethod write SetPestAssociatedValueMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;

  end;

  TSutraBoundaryList = TList<TSutraBoundary>;

  TSutraObservations = class(TGoPhastPersistent)
  private
    FObservationName: AnsiString;
    FScheduleName: AnsiString;
    FTimes: TRealCollection;
    FObservationFormat: TObservationFormat;
    FExportScheduleName: AnsiString;
    procedure SetObservationName(Value: AnsiString);
    procedure SetScheduleName(const Value: AnsiString);
    procedure SetTimes(const Value: TRealCollection);
    procedure SetObservationFormat(const Value: TObservationFormat);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    function Used: boolean;
    property ExportScheduleName: AnsiString read FExportScheduleName
      write FExportScheduleName;
  published
    // OBSNAM
    property ObservationName: AnsiString read FObservationName
      write SetObservationName;

    // OBSSCH
    property ScheduleName: AnsiString read FScheduleName write SetScheduleName;
    property Times: TRealCollection read FTimes write SetTimes;
    // OBSFMT
    property ObservationFormat: TObservationFormat read FObservationFormat
      write SetObservationFormat;
  end;

  TSutraLake = class(TFormulaProperty)
  private
    const
    InitialStagePosition = 0;
    InitialConcentrationOrTemperaturePosition = 1;
    FractionRechargeDivertedPosition = 2;
    FractionDischargeDivertedPosition = 3;
    var
    FInitialStage: TFormulaObject;
    FInitialConcentrationOrTemperature: TFormulaObject;
    FFractionRechargeDiverted: TFormulaObject;
    FFractionDischargeDiverted: TFormulaObject;
    FInitialStageObserver: TObserver;
    FFracDisDivObserver: TObserver;
    FFracRechDivObserver: TObserver;
    FInitialUObserver: TObserver;
    FUsed: boolean;
    FUsedObserver: TObserver;
    function GetFractionDischargeDiverted: string;
    function GetFractionRechargeDiverted: string;
    function GetInitialConcentrationOrTemperature: string;
    function GetInitialStage: string;
    procedure SetFractionDischargeDiverted(const Value: string);
    procedure SetFractionRechargeDiverted(const Value: string);
    procedure SetInitialConcentrationOrTemperature(const Value: string);
    procedure SetInitialStage(const Value: string);
    procedure CreateFormulaObjects;
    function GetInitialStageObserver: TObserver;
    function GetFracDisDivObserver: TObserver;
    function GetFracRechDivObserver: TObserver;
    function GetInitialUObserver: TObserver;
    function GetUsedObserver: TObserver;
    procedure SetUsed(const Value: boolean);
    procedure HandleChangedValue(Observer: TObserver);
  protected
    property InitialStageObserver: TObserver read GetInitialStageObserver;
    property InitialUObserver: TObserver read GetInitialUObserver;
    property FracRechDivObserver: TObserver read GetFracRechDivObserver;
    property FracDisDivObserver: TObserver read GetFracDisDivObserver;
    property UsedObserver: TObserver read GetUsedObserver;
    procedure CreateObservers;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    function Used: boolean; override;
    function GetBoundaryFormula(Index: integer): string;
    procedure SetBoundaryFormula(Index: integer; const Value: string);
    function BoundaryFormulaCount: integer;
  published
    // STGI
    property InitialStage: string read GetInitialStage write SetInitialStage;
    // UWI
    property InitialConcentrationOrTemperature: string
      read GetInitialConcentrationOrTemperature
      write SetInitialConcentrationOrTemperature;
    // FRRO
    property FractionRechargeDiverted: string read GetFractionRechargeDiverted
      write SetFractionRechargeDiverted;
    // FDRO
    property FractionDischargeDiverted: string read GetFractionDischargeDiverted
      write SetFractionDischargeDiverted;
    property IsUsed: boolean read FUsed write SetUsed;
  end;

  TSutraLakeList = TList<TSutraLake>;

  TSutraObsList = TList<TSutraObservations>;

  TCustomSutraBoundaryItem = class(TCustomBoundaryItem)
  private
    FUFormulaObject: TFormulaObject;
    FUsedFormulaObject: TFormulaObject;
    FUsed: Boolean;
    procedure SetUFormula(const Value: string);
    function GetUFormula: string;
    procedure SetUsed(const Value: Boolean);
    function GetUsedFormula: string;
    procedure SetUsedFormula(const Value: string);
  protected
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject; override;
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
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
  published
    // UINC, QUINC, UBC
    property UFormula: string read GetUFormula write SetUFormula;
    // @name is retained only for backwards compatibility. Use
    // @link(UsedFormula) instead.
    property Used: Boolean read FUsed write SetUsed Stored False;
    // @name is used to determine whether or not a @classname is used
    // at a particular node.
    property UsedFormula: string read GetUsedFormula write SetUsedFormula;
  end;

  TAbstractSutraBoundaryCollection = class(TCustomMF_ListBoundColl)
  private
    FScheduleName: AnsiString;
    procedure SetScheduleName(const Value: AnsiString);
  protected
    procedure Changed; virtual; abstract;
  published
    // BCSSCH
    property ScheduleName: AnsiString read FScheduleName write SetScheduleName;

  end;

  TCustomSutraBoundaryCollection = class(TAbstractSutraBoundaryCollection)
  protected
    procedure UChangeHandler(Sender: TObject); virtual;
    procedure UsedChangeHandler(Sender: TObject); virtual;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
//    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    function CanInvalidate: boolean;
    procedure Changed; override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

  TUTimeLink = class(TTimeListsModelLink)
  private
    // @name is only used to record the name of the data to be edited.
    FUTimeList: TModflowTimeList;
  public
    Destructor Destroy; override;
  end;

  TCustomSutraAssociatedBoundaryItem = class(TCustomSutraBoundaryItem)
  private
    FPQFormulaObject: TFormulaObject;
    procedure SetPQFormula(const Value: string);
    function GetPQFormula: string;
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
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    // QINC, PBC
    property PQFormula: string read GetPQFormula write SetPQFormula;
  end;

  TPQ_UTimeLink = class(TUTimeLink)
  private
    // @name is only used to record the name of the data to be edited.
    FPQTimeList: TModflowTimeList;
  public
    Destructor Destroy; override;
  end;

  TCustomAssociatedSutraBoundaryCollection = class(TCustomSutraBoundaryCollection)
  protected
    procedure PQChangeHandler(Sender: TObject); virtual;
    procedure Changed; override;
  end;

  TSutraFluidBoundaryItem = class(TCustomSutraAssociatedBoundaryItem)
  private
    function GetFluidSource: string;
    procedure SetFluidSource(const AValue: string);
  public
    property FluidSource: string read GetFluidSource write SetFluidSource;
  end;

  TSutraFluidTimeLink = class(TPQ_UTimeLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSutraFluidBoundaryCollection = class(TCustomAssociatedSutraBoundaryCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
    procedure PQChangeHandler(Sender: TObject); override;
    procedure UChangeHandler(Sender: TObject); override;
    procedure UsedChangeHandler(Sender: TObject); override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TSutraFluidBoundary = class(TSutraBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
  end;

  TSutraMassEnergySourceSinkItem = class(TCustomSutraBoundaryItem)
  private
    function GetSoluteEnergy: string;
    procedure SetSoluteEnergy(const AValue: string);
  published
    property SoluteEnergy: string read GetSoluteEnergy write SetSoluteEnergy;
  end;

  TSutraMassEnergyTimeLink = class(TUTimeLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSutraMassEnergySourceSinkCollection = class(TCustomSutraBoundaryCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
    procedure UChangeHandler(Sender: TObject); override;
    procedure UsedChangeHandler(Sender: TObject); override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TSutraMassEnergySourceSinkBoundary = class(TSutraBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
  end;

  TSutraSpecifiedPressureBoundaryItem = class(TCustomSutraAssociatedBoundaryItem)
  private
    function GetPressure: string;
    procedure SetPressure(const AValue: string);
  public
    property Pressure: string read GetPressure write SetPressure;
  end;

  TSutraSpecifiedPressureTimeLink = class(TPQ_UTimeLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSutraSpecifiedPressureCollection = class(TCustomAssociatedSutraBoundaryCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
    procedure PQChangeHandler(Sender: TObject); override;
    procedure UChangeHandler(Sender: TObject); override;
    procedure UsedChangeHandler(Sender: TObject); override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TSutraSpecifiedPressureBoundary = class(TSutraBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
  end;

  TSutraSpecifiedConcTempItem = class(TCustomSutraBoundaryItem)
  private
    function GetConcTemp: string;
    procedure SetConcTemp(const AValue: string);
  published
    property ConcTemp: string read GetConcTemp write SetConcTemp;
  end;

  TSutraSpecifiedConcTempTimeLink = class(TUTimeLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSutraSpecifiedConcTempCollection = class(TCustomSutraBoundaryCollection)
  protected
    class function ItemClass: TBoundaryItemClass; override;
    procedure UChangeHandler(Sender: TObject); override;
    procedure UsedChangeHandler(Sender: TObject); override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TSutraSpecifiedConcTempBoundary = class(TSutraBoundary)
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass;
      override;
  public
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, frmProgressUnit,
  ScreenObjectUnit, SutraMeshUnit, frmFormulaErrorsUnit;

const
  UFormulaPosition = 0;
  UsedFormulaPosition = 1;
  PQFormulaPosition = 2;

//  FractionRechargeDivertedPosition = 0;

  InitialStagePosition = 0;
  InitialConcentrationOrTemperaturePosition = 1;
  FractionRechargeDivertedPosition = 2;
  FractionDischargeDivertedPosition = 3;


{ TCustomSutraBoundaryCollection }

procedure TCustomSutraBoundaryCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod);
begin
  inherited;
  // this needs to be changed?
  Assert(False);
end;

procedure TCustomSutraBoundaryCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
begin
  inherited;
  // this needs to be changed.
  Assert(False);
end;

function TCustomSutraBoundaryCollection.CanInvalidate: boolean;
begin
  result := (Model <> nil) and (ScreenObject <> nil)
    and (ScreenObject as TScreenObject).CanInvalidateModel;
end;

procedure TCustomSutraBoundaryCollection.Changed;
begin
  UChangeHandler(Self);
  UsedChangeHandler(Self);

end;

//function TCustomSutraBoundaryCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
//begin
//  result := nil;
//  // this needs to be changed.
//  Assert(False);
//end;

//procedure TCustomSutraBoundaryCollection.SetBoundaryName(Value: AnsiString);
//const
//  MaxLength = 40;
//begin
//  if Length(Value) > MaxLength then
//  begin
//    SetLength(Value, MaxLength);
//  end;
//  if FBoundaryName <> Value then
//  begin
//    FBoundaryName := Value;
//
//    InvalidateModel;
//  end;
//end;


procedure TCustomSutraBoundaryCollection.AddSpecificBoundary(
  AModel: TBaseModel);
begin
  inherited;
  // this needs to be changed.
  Assert(False);
end;

function TCustomSutraBoundaryCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
begin
  // this needs to be changed.
  Assert(False);
end;

procedure TCustomSutraBoundaryCollection.Assign(Source: TPersistent);
var
  SourceBoundary: TCustomSutraBoundaryCollection;
begin
  if Source is TCustomSutraBoundaryCollection then
  begin
    SourceBoundary := TCustomSutraBoundaryCollection(Source);
    ScheduleName := SourceBoundary.ScheduleName;
//    BoundaryName := SourceBoundary.BoundaryName;
  end;
  inherited;
end;

procedure TCustomSutraBoundaryCollection.UChangeHandler(Sender: TObject);
begin
  InvalidateModel;
end;

procedure TCustomSutraBoundaryCollection.UsedChangeHandler(Sender: TObject);
begin
  InvalidateModel;
end;

procedure TSutraObservations.Assign(Source: TPersistent);
var
  SourceCollection: TSutraObservations;
begin
  if Source is TSutraObservations then
  begin
    SourceCollection := TSutraObservations(Source);
    ObservationName := SourceCollection.ObservationName;
    ScheduleName := SourceCollection.ScheduleName;
    Times := SourceCollection.Times;
    ObservationFormat := SourceCollection.ObservationFormat;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraObservations.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(InvalidateModelEvent);
  FTimes:= TRealCollection.Create(InvalidateModelEvent);
end;

destructor TSutraObservations.Destroy;
begin
  FTimes.Free;
  inherited;
end;

procedure TSutraObservations.SetObservationName(Value: AnsiString);
const
  MaxLength = 40;
begin
  if Length(Value) > MaxLength then
  begin
    SetLength(Value, MaxLength);
  end;
  if FObservationName <> Value then
  begin
    FObservationName := Value;

    InvalidateModel;
  end;
end;


procedure TSutraObservations.SetObservationFormat(
  const Value: TObservationFormat);
begin
  if FObservationFormat <> Value then
  begin
    FObservationFormat := Value;
    InvalidateModel;
  end;
end;

procedure TSutraObservations.SetScheduleName(const Value: AnsiString);
begin
  if FScheduleName <> Value then
  begin
    FScheduleName := Value;
    InvalidateModel;
  end;
end;

procedure TSutraObservations.SetTimes(const Value: TRealCollection);
begin
  FTimes.Assign(Value);
end;

function TSutraObservations.Used: boolean;
begin
  result := Times.Count > 0;
end;

{ TCustomSutraAssociatedBoundaryItem }

procedure TCustomSutraAssociatedBoundaryItem.Assign(Source: TPersistent);
begin
  if Source is TCustomSutraAssociatedBoundaryItem then
  begin
    PQFormula := TCustomSutraAssociatedBoundaryItem(Source).PQFormula;
  end;
  inherited;
end;

procedure TCustomSutraAssociatedBoundaryItem.AssignObserverEvents(
  Collection: TCollection);
var
  ParentCollection: TCustomAssociatedSutraBoundaryCollection;
  PQObserver: TObserver;
begin
  ParentCollection := Collection as TCustomAssociatedSutraBoundaryCollection;
  PQObserver := FObserverList[PQFormulaPosition];
  PQObserver.OnUpToDateSet := ParentCollection.PQChangeHandler;
  inherited;
end;

function TCustomSutraAssociatedBoundaryItem.BoundaryFormulaCount: integer;
begin
  result := inherited + 1;
end;

procedure TCustomSutraAssociatedBoundaryItem.CreateFormulaObjects;
begin
  FPQFormulaObject := CreateFormulaObject(dso3D);
  inherited;
end;

destructor TCustomSutraAssociatedBoundaryItem.Destroy;
begin
  PQFormula := '0';
  inherited;
end;

function TCustomSutraAssociatedBoundaryItem.GetBoundaryFormula(
  Index: integer): string;
begin
  case Index of
    PQFormulaPosition: result := PQFormula;
    else result := inherited;
  end;
end;

function TCustomSutraAssociatedBoundaryItem.GetPQFormula: string;
begin
  Result := FPQFormulaObject.Formula;
  ResetItemObserver(PQFormulaPosition);
end;

procedure TCustomSutraAssociatedBoundaryItem.GetPropertyObserver(
  Sender: TObject; List: TList);
begin
  if Sender = FPQFormulaObject then
  begin
    List.Add(FObserverList[PQFormulaPosition]);
  end
  else
  begin
    inherited;
  end;
end;

procedure TCustomSutraAssociatedBoundaryItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TCustomSutraAssociatedBoundaryItem.IsSame(AnotherItem: TOrderedItem): Boolean;
var
  Item: TCustomSutraAssociatedBoundaryItem;
begin
  result := (AnotherItem is TCustomSutraAssociatedBoundaryItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCustomSutraAssociatedBoundaryItem(AnotherItem);
    result := (Item.PQFormula = PQFormula)
  end;
end;

procedure TCustomSutraAssociatedBoundaryItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FPQFormulaObject,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  inherited;
end;

procedure TCustomSutraAssociatedBoundaryItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    PQFormulaPosition: PQFormula := Value;
    else inherited SetBoundaryFormula(Index, Value);
  end;
end;

procedure TCustomSutraAssociatedBoundaryItem.SetPQFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, PQFormulaPosition, FPQFormulaObject);
end;

{ TSutraFluidBoundaryItem }

function TSutraFluidBoundaryItem.GetFluidSource: string;
begin
  result := PQFormula;
end;

procedure TSutraFluidBoundaryItem.SetFluidSource(const AValue: string);
begin
  PQFormula := AValue;
end;

{ TSutraMassSourceSinkItem }

function TSutraMassEnergySourceSinkItem.GetSoluteEnergy: string;
begin
  result := UFormula;
end;

procedure TSutraMassEnergySourceSinkItem.SetSoluteEnergy(const AValue: string);
begin
  UFormula := AValue;
end;

{ TSutraSpecifiedPressureBoundaryItem }

function TSutraSpecifiedPressureBoundaryItem.GetPressure: string;
begin
  result := PQFormula;
end;

procedure TSutraSpecifiedPressureBoundaryItem.SetPressure(const AValue: string);
begin
  PQFormula := AValue;
end;

{ TSutraSpecifiedConcTempItem }

function TSutraSpecifiedConcTempItem.GetConcTemp: string;
begin
  result := UFormula;
end;

procedure TSutraSpecifiedConcTempItem.SetConcTemp(const AValue: string);
begin
  UFormula := AValue;
end;

{ TCustomSutraBoundaryItem }

procedure TCustomSutraBoundaryItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomSutraBoundaryItem then
  begin
    UFormula := TCustomSutraBoundaryItem(Source).UFormula;
    UsedFormula := TCustomSutraBoundaryItem(Source).UsedFormula;
  end;
  inherited;
end;

procedure TCustomSutraBoundaryItem.AssignObserverEvents(
  Collection: TCollection);
var
  ParentCollection: TCustomSutraBoundaryCollection;
  PumpingRateObserver: TObserver;
  UsedObserver: TObserver;
begin
  ParentCollection := Collection as TCustomSutraBoundaryCollection;
  PumpingRateObserver := FObserverList[UFormulaPosition];
  PumpingRateObserver.OnUpToDateSet := ParentCollection.UChangeHandler;

  UsedObserver := FObserverList[UsedFormulaPosition];
  UsedObserver.OnUpToDateSet := ParentCollection.UsedChangeHandler;

end;

function TCustomSutraBoundaryItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

constructor TCustomSutraBoundaryItem.Create(Collection: TCollection);
begin
  inherited;
  BoundaryFormula[UsedFormulaPosition] := 'False';
end;

function TCustomSutraBoundaryItem.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := frmGoPhast.PhastModel.FormulaManager.Add;
  case Orientation of
    dsoTop:
      begin
        result.Parser := frmGoPhast.PhastModel.rpTopFormulaCompilerNodes;
      end;
    dso3D:
      begin
        result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompilerNodes;
      end;
    else Assert(False);
  end;
  result.AddSubscriptionEvents(
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCustomSutraBoundaryItem.CreateFormulaObjects;
begin
  inherited;
  FUFormulaObject := CreateFormulaObject(dso3D);
  FUsedFormulaObject := CreateFormulaObject(dso3D);
end;

destructor TCustomSutraBoundaryItem.Destroy;
begin
  UFormula := '0';
  UsedFormula := 'False';
  inherited;
end;

function TCustomSutraBoundaryItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UFormulaPosition: result := UFormula;
    UsedFormulaPosition: result := UsedFormula;
    else Assert(False);
  end;
end;

procedure TCustomSutraBoundaryItem.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
  if Sender = FUFormulaObject then
  begin
    List.Add(FObserverList[UFormulaPosition]);
  end
  else if Sender = FUsedFormulaObject then
  begin
    List.Add(FObserverList[UsedFormulaPosition]);
  end
  else
  begin
    Assert(False)
  end;
end;

function TCustomSutraBoundaryItem.GetUFormula: string;
begin
  Result := FUFormulaObject.Formula;
  ResetItemObserver(UFormulaPosition);
end;

function TCustomSutraBoundaryItem.GetUsedFormula: string;
begin
  Result := FUsedFormulaObject.Formula;
  ResetItemObserver(UsedFormulaPosition);
end;

procedure TCustomSutraBoundaryItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.Invalidate(self);
//    PhastModel.InvalidateMfWellPumpage(self);
  end;
end;

function TCustomSutraBoundaryItem.IsSame(AnotherItem: TOrderedItem): Boolean;
var
  Item: TCustomSutraBoundaryItem;
begin
  result := (AnotherItem is TCustomSutraBoundaryItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCustomSutraBoundaryItem(AnotherItem);
    result := (Item.UFormula = UFormula)
      and (Item.UsedFormula = UsedFormula);
  end;
end;

procedure TCustomSutraBoundaryItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FUFormulaObject,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUsedFormulaObject,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCustomSutraBoundaryItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  inherited;
  case Index of
    UFormulaPosition: UFormula := Value;
    UsedFormulaPosition: UsedFormula := Value;
    else Assert(False);
  end;
end;

procedure TCustomSutraBoundaryItem.SetUFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, UFormulaPosition, FUFormulaObject);
end;

procedure TCustomSutraBoundaryItem.SetUsed(const Value: Boolean);
begin
  if FUsed <> Value then
  begin
    FUsed := Value;
    InvalidateModel;

    if FUsed then
    begin
      UsedFormula := 'True';
    end
    else
    begin
      UsedFormula := 'False';
    end;
  end;
end;

procedure TCustomSutraBoundaryItem.SetUsedFormula(const Value: string);
begin
  UpdateFormulaNodes(Value, UsedFormulaPosition, FUsedFormulaObject);
end;

{ TCustomAssociatedSutraBoundaryCollection }

procedure TCustomAssociatedSutraBoundaryCollection.Changed;
begin
  inherited;
  PQChangeHandler(Self);
end;

procedure TCustomAssociatedSutraBoundaryCollection.PQChangeHandler(
  Sender: TObject);
begin
  InvalidateModel;
end;

{ TSutraFluidBoundaryCollection }

constructor TSutraFluidBoundaryCollection.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

end;

function TSutraFluidBoundaryCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSutraFluidTimeLink;
end;

class function TSutraFluidBoundaryCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSutraFluidBoundaryItem;
end;

procedure TSutraFluidBoundaryCollection.PQChangeHandler(Sender: TObject);
begin
  inherited;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraFluidFlux(Sender);
  end;
end;

procedure TSutraFluidBoundaryCollection.UChangeHandler(Sender: TObject);
begin
  inherited;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraFluidFluxU(Sender);
  end;
end;

procedure TSutraFluidBoundaryCollection.UsedChangeHandler(Sender: TObject);
begin
  inherited;
  UChangeHandler(Sender);
  PQChangeHandler(Sender);
end;

{ TSutraMassEnergySourceSinkCollection }

constructor TSutraMassEnergySourceSinkCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
end;

function TSutraMassEnergySourceSinkCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSutraMassEnergyTimeLink;
end;

class function TSutraMassEnergySourceSinkCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSutraMassEnergySourceSinkItem;
end;

procedure TSutraMassEnergySourceSinkCollection.UChangeHandler(Sender: TObject);
begin
  inherited;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraUFlux(Sender);
  end;
end;

procedure TSutraMassEnergySourceSinkCollection.UsedChangeHandler(
  Sender: TObject);
begin
  inherited;
  UChangeHandler(Sender);
end;

{ TSutraSpecifiedPressureCollection }

constructor TSutraSpecifiedPressureCollection.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

end;

function TSutraSpecifiedPressureCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSutraSpecifiedPressureTimeLink;
end;

class function TSutraSpecifiedPressureCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSutraSpecifiedPressureBoundaryItem;
end;

procedure TSutraSpecifiedPressureCollection.PQChangeHandler(Sender: TObject);
begin
  inherited;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraSpecPressure(Sender);
  end;
end;

procedure TSutraSpecifiedPressureCollection.UChangeHandler(Sender: TObject);
begin
  inherited;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraSpecPressureU(Sender);
  end;
end;

procedure TSutraSpecifiedPressureCollection.UsedChangeHandler(Sender: TObject);
begin
  inherited;
  UChangeHandler(Sender);
  PQChangeHandler(Sender);
end;

{ TSutraSpecifiedConcTempCollection }

constructor TSutraSpecifiedConcTempCollection.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

end;

function TSutraSpecifiedConcTempCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSutraSpecifiedConcTempTimeLink;
end;

class function TSutraSpecifiedConcTempCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSutraSpecifiedConcTempItem;
end;

procedure TSutraSpecifiedConcTempCollection.UChangeHandler(Sender: TObject);
begin
  inherited;
  if CanInvalidate then
  begin
    (Model as TCustomModel).InvalidateSutraSpecifiedU(Sender);
  end;
end;

procedure TSutraSpecifiedConcTempCollection.UsedChangeHandler(Sender: TObject);
begin
  inherited;
  UChangeHandler(Sender);
end;

{ TSutraFluidBoundary }

procedure TSutraFluidBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  inherited;
  Assert(False);
end;

class function TSutraFluidBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSutraFluidBoundaryCollection;
end;

procedure TSutraFluidBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  Assert(False);
end;

{ TSutraMassEnergySourceSinkBoundary }

procedure TSutraMassEnergySourceSinkBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  inherited;
  Assert(False);
end;

class function TSutraMassEnergySourceSinkBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSutraMassEnergySourceSinkCollection;
end;

procedure TSutraMassEnergySourceSinkBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  Assert(False);
end;

{ TSutraSpecifiedPressureBoundary }

procedure TSutraSpecifiedPressureBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  inherited;
  Assert(False);
end;

class function TSutraSpecifiedPressureBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSutraSpecifiedPressureCollection;
end;

procedure TSutraSpecifiedPressureBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  Assert(False);
end;

{ TSutraSpecifiedConcTempBoundary }

procedure TSutraSpecifiedConcTempBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
begin
  inherited;
  Assert(False);
end;

class function TSutraSpecifiedConcTempBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSutraSpecifiedConcTempCollection;
end;

procedure TSutraSpecifiedConcTempBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
begin
  inherited;
  Assert(False);
end;

{ TSutraBoundary }

{ TSutraTimeList }

procedure TSutraTimeList.CheckSameModel(const Data: TDataArray);
begin
  if Data <> nil then
  begin
    Assert(Model = Data.Model);
  end;
end;

constructor TSutraTimeList.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited Create(Model);
  FScreenObject := ScreenObject;
end;

procedure TSutraTimeList.Initialize(BoundaryValues: TSutraBoundaryValueArray);
var
  LocalScreenObject: TScreenObject;
  LocalModel: TCustomModel;
  Mesh: TSutraMesh3D;
  StoredUpToDate: Boolean;
  Index: Integer;
  Time: Double;
  Formula: string;
  DataArray: TDataArray;
  UsedFormula: string;
begin
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  if UpToDate then
    Exit;

  LocalScreenObject := FScreenObject as TScreenObject;
  Assert(LocalScreenObject <> nil);
  LocalModel := Model as TCustomModel;
  Assert(LocalModel <> nil);

  Mesh := LocalModel.SutraMesh;
  StoredUpToDate := LocalModel.UpToDate;
  try
    Clear;

    for Index := 0 to Length(BoundaryValues) - 1 do
    begin
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      Time := BoundaryValues[Index].Time;
      Formula := BoundaryValues[Index].Formula;
      UsedFormula := BoundaryValues[Index].UsedFormula;
      DataArray := nil;
      case DataType of
        rdtDouble:
          begin
            DataArray := TTransientRealSparseDataSet.Create(LocalModel);
            DataArray.DataType := rdtDouble;
          end;
        else Assert(False);
      end;
      DataArray.Name := ValidName(Description) + '_' + IntToStr(Index+1);
      Add(Time, DataArray);
      DataArray.UseLgrEdgeCells := lctUse;
      DataArray.EvaluatedAt := eaNodes;
      DataArray.Orientation := dso3D;
      LocalModel.UpdateDataArrayDimensions(DataArray);

      try
        LocalScreenObject.AssignValuesToSutraDataSet(Mesh, DataArray,
          Formula, LocalModel, UsedFormula);
      except on E: ErbwParserError do
        begin
          frmFormulaErrors.AddFormulaError(LocalScreenObject.Name, Name,
            Formula, E.Message);
          Formula := '0';
          UsedFormula := 'True';
          BoundaryValues[Index].Formula := Formula;
          BoundaryValues[Index].UsedFormula := UsedFormula;
          LocalScreenObject.AssignValuesToSutraDataSet(Mesh, DataArray,
            Formula, LocalModel, UsedFormula);
        end;
      end;
      LocalModel.DataArrayManager.CacheDataArrays;
      DataArray.UpToDate := True;
      DataArray.CacheData;
    end;
    SetUpToDate(True);
  finally
    LocalModel.UpToDate := StoredUpToDate;
  end
end;

{ TSutraMergedTimeList }

procedure TSutraMergedTimeList.Initialize;
var
  index: Integer;
  DataArray: TDataArray;
begin
  if Assigned(OnInitialize) then
  begin
    OnInitialize(Self);
    for index := 0 to Count - 1 do
    begin
      DataArray := Items[index];
      DataArray.UpToDate := True;
      DataArray.CacheData;
    end;
    SetUpToDate(True);
  end;
end;

procedure TSutraMergedTimeList.SetOnInitialize(const Value: TNotifyEvent);
begin
  if Addr(FOnInitialize) <> Addr(Value) then
  begin
    FOnInitialize := Value;
    Invalidate;
  end;
end;

{ TSutraLake }

procedure TSutraLake.Assign(Source: TPersistent);
var
  SourceLake: TSutraLake;
begin
  if Source is TSutraLake then
  begin
    SourceLake := TSutraLake(Source);
    InitialStage := SourceLake.InitialStage;
    InitialConcentrationOrTemperature := SourceLake.InitialConcentrationOrTemperature;
    FractionRechargeDiverted := SourceLake.FractionRechargeDiverted;
    FractionDischargeDiverted := SourceLake.FractionDischargeDiverted;
    IsUsed := SourceLake.IsUsed;
  end
  else
  begin
    inherited;
  end;
end;

function TSutraLake.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

constructor TSutraLake.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateObservers;

  InitialStage := '0';
  InitialConcentrationOrTemperature := '0';
  FractionRechargeDiverted := '0';
  FractionDischargeDiverted := '0';
end;

procedure TSutraLake.CreateFormulaObjects;
begin
  FInitialStage := CreateFormulaObjectNodes(dsoTop);
  FInitialConcentrationOrTemperature := CreateFormulaObjectNodes(dsoTop);
  FFractionRechargeDiverted := CreateFormulaObjectNodes(dsoTop);
  FFractionDischargeDiverted := CreateFormulaObjectNodes(dsoTop);
end;

procedure TSutraLake.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(InitialStageObserver);
    FObserverList.Add(InitialUObserver);
    FObserverList.Add(FracRechDivObserver);
    FObserverList.Add(FracDisDivObserver);
  end;
end;

destructor TSutraLake.Destroy;
begin
  InitialStage := '0';
  InitialConcentrationOrTemperature := '0';
  FractionRechargeDiverted := '0';
  FractionDischargeDiverted := '0';
  FUsedObserver.Free;
  inherited;
end;

function TSutraLake.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    InitialStagePosition:
      result := InitialStage;
    InitialConcentrationOrTemperaturePosition:
      result := InitialConcentrationOrTemperature;
    FractionRechargeDivertedPosition:
      result := FractionRechargeDiverted;
    FractionDischargeDivertedPosition:
      result := FractionDischargeDiverted;
    else Assert(False);
  end;
end;

function TSutraLake.GetFracDisDivObserver: TObserver;
var
  Observer: TObserver;
begin
  if FFracDisDivObserver = nil then
  begin
    Observer := nil;
    CreateObserver('SutraLakeFracDisDiv_', FFracDisDivObserver, Observer);
  end;
  result := FFracDisDivObserver;
end;

function TSutraLake.GetFracRechDivObserver: TObserver;
var
  Observer: TObserver;
begin
  if FFracRechDivObserver = nil then
  begin
    Observer := nil;
    CreateObserver('SutraLakeFracRechDiv_', FFracRechDivObserver, Observer);
  end;
  result := FFracRechDivObserver;
end;

function TSutraLake.GetFractionDischargeDiverted: string;
begin
  Result := FFractionDischargeDiverted.Formula;
  ResetBoundaryObserver(FractionDischargeDivertedPosition);
end;

function TSutraLake.GetFractionRechargeDiverted: string;
begin
  Result := FFractionRechargeDiverted.Formula;
  ResetBoundaryObserver(FractionRechargeDivertedPosition);
end;

function TSutraLake.GetInitialConcentrationOrTemperature: string;
begin
  Result := FInitialConcentrationOrTemperature.Formula;
  ResetBoundaryObserver(InitialConcentrationOrTemperaturePosition);
end;

function TSutraLake.GetInitialStage: string;
begin
  Result := FInitialStage.Formula;
  ResetBoundaryObserver(InitialStagePosition);
end;

function TSutraLake.GetInitialStageObserver: TObserver;
var
  Observer: TObserver;
begin
  if FInitialStageObserver = nil then
  begin
    Observer := nil;
    CreateObserver('SutraLakeInitialStage_', FInitialStageObserver, Observer);
  end;
  result := FInitialStageObserver;
end;

function TSutraLake.GetInitialUObserver: TObserver;
var
  Observer: TObserver;
begin
  if FInitialUObserver = nil then
  begin
    Observer := nil;
    CreateObserver('SutraLakeInitialU_', FInitialUObserver, Observer);
  end;
  result := FInitialUObserver;
end;

function TSutraLake.GetUsedObserver: TObserver;
var
  Observer: TObserver;
begin
  if FUsedObserver = nil then
  begin
    Observer := nil;
    CreateObserver('SutraLakeUsed_', FUsedObserver, Observer);
  end;
  result := FUsedObserver;
end;

procedure TSutraLake.HandleChangedValue(Observer: TObserver);
var
  Model: TPhastModel;
//  ChildIndex: Integer;
begin
  Model := ParentModel as TPhastModel;
  if not (csDestroying in Model.ComponentState)
    and not Model.Clearing then
  begin
    Observer.UpToDate := True;
    Observer.UpToDate := False;
    Observer.UpToDate := True;
  end;
end;

//procedure TSutraLake.ResetObserversUptodate;
//var
//  index: Integer;
//  AnObserver: TObserver;
//begin
//  for index := 0 to FObserverList.Count - 1 do
//  begin
//    AnObserver := FObserverList[index];
//    AnObserver.UpToDate := True;
//  end;
//end;

procedure TSutraLake.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    InitialStagePosition:
      InitialStage := Value;
    InitialConcentrationOrTemperaturePosition:
      InitialConcentrationOrTemperature := Value;
    FractionRechargeDivertedPosition:
      FractionRechargeDiverted := Value;
    FractionDischargeDivertedPosition:
      FractionDischargeDiverted := Value;
    else Assert(False);
  end;
end;

procedure TSutraLake.SetFractionDischargeDiverted(const Value: string);
begin
  UpdateFormulaNodes(Value, FractionDischargeDivertedPosition, FFractionDischargeDiverted);
end;

procedure TSutraLake.SetFractionRechargeDiverted(const Value: string);
begin
  UpdateFormulaNodes(Value, FractionRechargeDivertedPosition, FFractionRechargeDiverted);
end;

procedure TSutraLake.SetInitialConcentrationOrTemperature(const Value: string);
begin
  UpdateFormulaNodes(Value, InitialConcentrationOrTemperaturePosition, FInitialConcentrationOrTemperature);
end;

procedure TSutraLake.SetInitialStage(const Value: string);
begin
  UpdateFormulaNodes(Value, InitialStagePosition, FInitialStage);
end;

procedure TSutraLake.SetUsed(const Value: boolean);
var
  LocalScreenObject: TScreenObject;
begin
  if FUsed <> Value then
  begin
    LocalScreenObject := ScreenObject as TScreenObject;
    if ScreenObject <> nil then
    begin
      if LocalScreenObject.CanInvalidateModel then
      begin
        HandleChangedValue(UsedObserver);
      end;
    end;
    FUsed := Value;
    InvalidateModel;
  end;
end;

function TSutraLake.Used: boolean;
begin
  result := IsUsed;
end;

{ TSutraBoundary }

procedure TSutraBoundary.Assign(Source: TPersistent);

var
  SutraSource: TSutraBoundary;
begin
  if Source is TSutraBoundary then
  begin
    SutraSource := TSutraBoundary(Source);
    LakeInteraction := SutraSource.LakeInteraction;
    UseBCTime := SutraSource.UseBCTime;

    PestBoundaryValueFormula := SutraSource.PestBoundaryValueFormula;
    PestBoundaryValueMethod := SutraSource.PestBoundaryValueMethod;
    PestAssociatedValueFormula := SutraSource.PestAssociatedValueFormula;
    PestAssociatedValueMethod := SutraSource.PestAssociatedValueMethod;
  end;
  inherited;
end;

procedure TSutraBoundary.Changed;
begin
  if Used then
  begin
    (Values as TAbstractSutraBoundaryCollection).Changed;
  end;
end;

constructor TSutraBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FLakeInteraction := lbiUseDefaults;
  FUseBCTime := False;
end;

function TSutraBoundary.GetPestAssociatedValueFormula: string;
begin
  result := '';
end;

function TSutraBoundary.GetPestBoundaryValueFormula: string;
begin
  result := '';
end;

procedure TSutraBoundary.Loaded;
var
  SutraBoundaryCollection: TCustomSutraBoundaryCollection;
  Index: Integer;
  AnItem: TCustomSutraBoundaryItem;
begin
  SutraBoundaryCollection := Values as TCustomSutraBoundaryCollection;
  for Index := 0 to SutraBoundaryCollection.Count - 1 do
  begin
    AnItem := SutraBoundaryCollection[Index] as TCustomSutraBoundaryItem;
    if (AnItem.UsedFormula = '') or (AnItem.UsedFormula = '0') then
    begin
      AnItem.UsedFormula := 'False';
    end;
  end;
end;

procedure TSutraBoundary.SetLakeInteraction(
  const Value: TLakeBoundaryInteraction);
begin
  if FLakeInteraction <> Value then
  begin
    FLakeInteraction := Value;
    InvalidateModel;
  end;
end;

procedure TSutraBoundary.SetPestAssociatedValueFormula(const Value: string);
begin

end;

procedure TSutraBoundary.SetPestAssociatedValueMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestAssociatedValueMethod, Value);
end;

procedure TSutraBoundary.SetPestBoundaryValueFormula(const Value: string);
begin

end;

procedure TSutraBoundary.SetPestBoundaryValueMethod(
  const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestBoundaryValueMethod, Value);
end;

procedure TSutraBoundary.SetUseBCTime(const Value: Boolean);
begin
  if FUseBCTime <> Value then
  begin
    FUseBCTime := Value;
    InvalidateModel;
  end;
end;

{ TAbstractSutraBoundaryCollection }

procedure TAbstractSutraBoundaryCollection.SetScheduleName(
  const Value: AnsiString);
begin
  if FScheduleName <> Value then
  begin
    FScheduleName := Value;
    InvalidateModel;
  end;
end;

{ TUTimeLink }

destructor TUTimeLink.Destroy;
begin
  FUTimeList.Free;
  inherited;
end;

{ TPQ_UTimeLink }

destructor TPQ_UTimeLink.Destroy;
begin
  FPQTimeList.Free;
  inherited;
end;

{ TSutraFluidTimeLink }

procedure TSutraFluidTimeLink.CreateTimeLists;
begin
//  inherited;
  FPQTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPQTimeList.NonParamDescription := 'Fluid flow';
  FPQTimeList.ParamDescription := 'Fluid flow';
  AddTimeList(FPQTimeList);
//  if Model <> nil then
//  begin
//    FPQTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FUTimeList.NonParamDescription := 'Associated concentration or temperature';
  FUTimeList.ParamDescription := 'Associated concentration or temperature';
  AddTimeList(FUTimeList);

end;

{ TSutraMassEnergyTimeLink }

procedure TSutraMassEnergyTimeLink.CreateTimeLists;
begin
//  inherited;
  FUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FUTimeList.NonParamDescription := 'Mass or energy flow';
  FUTimeList.ParamDescription := 'Mass or energy flow';
  AddTimeList(FUTimeList);

end;

{ TSutraSpecifiedPressureTimeLink }

procedure TSutraSpecifiedPressureTimeLink.CreateTimeLists;
begin
//  inherited;
  FPQTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FPQTimeList.NonParamDescription := 'Specified pressure or head';
  FPQTimeList.ParamDescription := 'Specified pressure or head';
  AddTimeList(FPQTimeList);
//  if Model <> nil then
//  begin
//    FPQTimeList.OnInvalidate := (Model as TCustomModel).InvalidateMfRchRate;
//  end;
  FUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FUTimeList.NonParamDescription := 'Associated concentration or temperature';
  FUTimeList.ParamDescription := 'Associated concentration or temperature';
  AddTimeList(FUTimeList);

end;

{ TSutraSpecifiedConcTempTimeLink }

procedure TSutraSpecifiedConcTempTimeLink.CreateTimeLists;
begin
//  inherited;
  FUTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FUTimeList.NonParamDescription := 'Specified concentration or temperature';
  FUTimeList.ParamDescription := 'Specified concentration or temperature';
  AddTimeList(FUTimeList);

end;

end.
