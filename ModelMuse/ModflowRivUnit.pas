unit ModflowRivUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit;

type
  TRivRecord = record
    Cell: TCellLocation;
    // With MODFLOW 6, Conductance includes the effect of the parameter,
    // if any. In a template, the appropriate formula would be
    // the parameter name times the conductance divided by the parameter
    // value.
    // For example,
    //
    // if ConductanceParameterName <> '' then
    // begin
    //   WriteString(ConductanceParameterName);
    //   WriteString('*');
    //   WriteFloat(Conductance/ConductanceParameterValue);
    // end;
    Conductance: double;
    RiverStage: double;
    RiverBottom: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    RiverStageAnnotation: string;
    RiverBottomAnnotation: string;

    // PEST
    ConductancePest: string;
    RiverStagePest: string;
    RiverBottomPest: string;
    ConductancePestSeriesName: string;
    RiverStagePestSeriesName: string;
    RiverBottomPestSeriesName: string;
    ConductancePestSeriesMethod: TPestParamMethod;
    RiverStagePestSeriesMethod: TPestParamMethod;
    RiverBottomPestSeriesMethod: TPestParamMethod;
    // MVR
    MvrUsed: Boolean;
    MvrIndex: Integer;
    // Parameters
    ConductanceParameterName: string;
    ConductanceParameterValue: double;
    // Time Series
    ConductanceTimeSeriesName: string;
    RiverStageTimeSeriesName: string;
    RiverBottomTimeSeriesName: string;
    // GWT Concentrations
    GwtConcentrations: TGwtCellData;
//    Concentrations: array of double;
//    ConcentrationAnnotations: array of string;
//    ConcentrationPestNames: array of string;
//    ConcentrationPestSeriesNames: array of string;
//    ConcentrationPestSeriesMethods: array of TPestParamMethod;
//    ConcentrationTimeSeriesNames: array of string;
    procedure Assign(const Item: TRivRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TRivArray = array of TRivRecord;

  TRivStorage = class(TCustomBoundaryStorage)
  private
    FRivArray: TRivArray;
    function GetRivArray: TRivArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property RivArray: TRivArray read GetRivArray;
  end;

  TRivCollection = class;

  TRivGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TRivCollection);
  end;

  // @name represents a MODFLOW River boundary for one time interval.
  // @name is stored by @link(TRivCollection).
  TRivItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(RiverBottom).
    FRiverBottom: TFormulaObject;
    // See @link(RiverStage).
    FRiverStage: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    FGwtConcentrations: TRivGwtConcCollection;
    // See @link(RiverBottom).
    procedure SetRiverBottom(const Value: string);
    // See @link(RiverStage).
    procedure SetRiverStage(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetRiverBottom: string;
    function GetRiverStage: string;
    procedure SetGwtConcentrations(const Value: TRivGwtConcCollection);
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
    // @name is the formula used to set the river stage
    // of this boundary.
    property RiverBottom: string read GetRiverBottom write SetRiverBottom;
    // @name is the formula used to set the river stage
    // of this boundary.
    property RiverStage: string read GetRiverStage write SetRiverStage;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
    property GwtConcentrations: TRivGwtConcCollection read FGwtConcentrations
      write SetGwtConcentrations
      {$IFNDEF GWT}
      stored False
      {$ENDIF}
      ;
  end;

  TRivTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the River Bottoms for a series of
    // River Boundaries over a series of time intervals.
    FRiverBottomData: TModflowTimeList;
    // @name is used to compute the River Stages for a series of
    // River Boundaries over a series of time intervals.
    FRiverStageData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // River Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    FConcList: TModflowTimeLists;
    procedure AddGwtTimeLists(SpeciesIndex: Integer);
  protected
    procedure CreateTimeLists; override;
    procedure UpdateGwtTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW River boundaries
  // for a series of time intervals.
  TRivCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateRiverBottomData(Sender: TObject);
    procedure InvalidateGwtConcentrations(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TRivStorage.RivArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure InvalidateModel; override;
  end;

  // Each @name stores a @link(TRivCollection).
  // @classname is stored by @link(TModflowParameters).
  TRivParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TRiv_Cell = class(TValueCell)
  private
    FValues: TRivRecord;
    StressPeriod: integer;
    function GetRiverStage: double;
    function GetConductance: double;
    function GetRiverBottom: double;
    function GetConductanceAnnotation: string;
    function GetRiverBottomAnnotation: string;
    function GetRiverStageAnnotation: string;
    function GetMvrUsed: Boolean;
    function GetMvrIndex: Integer;
    function GetConductanceParameterName: string;
    function GetConductanceParameterValue: double;
    function GetConductancePest: string;
    function GetConductancePestSeriesMethod: TPestParamMethod;
    function GetConductancePestSeriesName: string;
    function GetRiverBottomPest: string;
    function GetRiverBottomPestSeriesMethod: TPestParamMethod;
    function GetRiverBottomPestSeriesName: string;
    function GetRiverStagePest: string;
    function GetRiverStagePestSeriesMethod: TPestParamMethod;
    function GetRiverStagePestSeriesName: string;
    function GetConductanceTimeSeriesName: string;
    function GetRiverBottomTimeSeriesName: string;
    function GetRiverStageTimeSeriesName: string;
    procedure SetConductanceTimeSeriesName(const Value: string);
    procedure SetRiverBottomTimeSeriesName(const Value: string);
    procedure SetRiverStageTimeSeriesName(const Value: string);
    function GetConcentration(const Index: Integer): double;
    function GetConcentrationAnnotation(const Index: Integer): string;
    function GetConcentrationPestName(const Index: Integer): string;
    function GetConcentrationPestSeriesMethod(
      const Index: Integer): TPestParamMethod;
    function GetConcentrationPestSeriesName(const Index: Integer): string;
    function GetConcentrationTimeSeriesName(const Index: Integer): string;
  protected
    property Values: TRivRecord read FValues;
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
    // With MODFLOW 6, Conductance includes the effect of the parameter,
    // if any. In a template, the appropriate formula would be
    // the parameter name times the conductance divided by the parameter
    // value.
    // For example,
    //
    // if ConductanceParameterName <> '' then
    // begin
    //   WriteString(ConductanceParameterName);
    //   WriteString('*');
    //   WriteFloat(Conductance/ConductanceParameterValue);
    // end;
    property Conductance: double read GetConductance;
    property RiverBottom: double read GetRiverBottom;
    property RiverStage: double read GetRiverStage;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property RiverBottomAnnotation: string read GetRiverBottomAnnotation;
    property RiverStageAnnotation: string read GetRiverStageAnnotation;
//    property TimeSeriesName: string read GetTimeSeriesName;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property ConductanceParameterName: string read GetConductanceParameterName;
    property ConductanceParameterValue: double read GetConductanceParameterValue;
    // PEST properties
    property ConductancePest: string read GetConductancePest;
    property RiverStagePest: string read GetRiverStagePest;
    property RiverBottomPest: string read GetRiverBottomPest;

    property ConductancePestSeries: string read GetConductancePestSeriesName;
    property RiverStagePestSeries: string read GetRiverStagePestSeriesName;
    property RiverBottomPestSeries: string read GetRiverBottomPestSeriesName;

    property ConductancePestSeriesMethod: TPestParamMethod
      read GetConductancePestSeriesMethod;
    property RiverStagePestSeriesMethod: TPestParamMethod
      read GetRiverStagePestSeriesMethod;
    property RiverBottomPestSeriesMethod: TPestParamMethod
      read GetRiverBottomPestSeriesMethod;

    property ConductanceTimeSeriesName: string read GetConductanceTimeSeriesName
      write SetConductanceTimeSeriesName;
    property RiverStageTimeSeriesName: string read GetRiverStageTimeSeriesName
      write SetRiverStageTimeSeriesName;
    property RiverBottomTimeSeriesName: string read GetRiverBottomTimeSeriesName
      write SetRiverBottomTimeSeriesName;
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

  // @name represents the MODFLOW River boundaries associated with
  // a single @link(TScreenObject).
  //
  // @link(TSpecificModflowBoundary.FormulaInterpretation) determines whether the @Link(TRivItem.Conductance
  // TRivItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TRivCollection)
  TRivBoundary = class(TSpecificModflowBoundary)
  private
    FCurrentParameter: TModflowTransientListParameter;
    FPestConductanceMethod: TPestParamMethod;
    FPestRiverBottomMethod: TPestParamMethod;
    FPestRiverStageMethod: TPestParamMethod;

    FPestCondFormula: TFormulaObject;
    FPestRiverStageFormula: TFormulaObject;
    FPestRiverBottomFormula: TFormulaObject;
    FPestConductanceObserver: TObserver;
    FPestRiverBottomObserver: TObserver;
    FUsedObserver: TObserver;
    FPestRiverStageObserver: TObserver;
    FPestConcentrationMethods: TGwtPestMethodCollection;
    FPestConcentrationFormulas: TRivGwtConcCollection;
    FConcentrationObservers: TObserverList;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
    function GetPestConductanceFormula: string;
    function GetPestRiverBottomFormula: string;
    function GetPestRiverStageFormula: string;
    procedure SetPestConductanceFormula(const Value: string);
    procedure SetPestConductanceMethod(const Value: TPestParamMethod);
    procedure SetPestRiverBottomFormula(const Value: string);
    procedure SetPestRiverBottomMethod(const Value: TPestParamMethod);
    procedure SetPestRiverStageFormula(const Value: string);
    procedure SetPestRiverStageMethod(const Value: TPestParamMethod);
    function GetPestConductanceObserver: TObserver;
    function GetPestRiverBottomObserver: TObserver;
    function GetPestRiverStageObserver: TObserver;
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateRiverBottomData(Sender: TObject);
    procedure InvalidateConcData(Sender: TObject);
    procedure SetPestConcentrationFormulas(const Value: TRivGwtConcCollection);
    procedure SetPestConcentrationMethods(const Value: TGwtPestMethodCollection);
    function GetConcentrationObserver(const Index: Integer): TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TRiv_Cell)s for that stress period.
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
    property PestRiverStageObserver: TObserver read GetPestRiverStageObserver;
    property PestRiverBottomObserver: TObserver read GetPestRiverBottomObserver;
    property PestConductanceObserver: TObserver read GetPestConductanceObserver;
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
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TRivStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW RIV parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TRivStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property Interp;
    property PestRiverBottomFormula: string read GetPestRiverBottomFormula
      write SetPestRiverBottomFormula;
    property PestRiverStageFormula: string read GetPestRiverStageFormula
      write SetPestRiverStageFormula;
    property PestConductanceFormula: string read GetPestConductanceFormula
      write SetPestConductanceFormula;
    property PestRiverBottomMethod: TPestParamMethod read FPestRiverBottomMethod
      write SetPestRiverBottomMethod;
    property PestRiverStageMethod: TPestParamMethod read FPestRiverStageMethod
      write SetPestRiverStageMethod;
    property PestConductanceMethod: TPestParamMethod
      read FPestConductanceMethod write SetPestConductanceMethod;
    property PestConcentrationFormulas: TRivGwtConcCollection
      read FPestConcentrationFormulas write SetPestConcentrationFormulas
      {$IFNDEF GWT}
      Stored False
      {$ENDIF}
      ;
    property PestConcentrationMethods: TGwtPestMethodCollection
      read FPestConcentrationMethods write SetPestConcentrationMethods
      {$IFNDEF GWT}
      Stored False
      {$ENDIF}
      ;
  end;

const
  RivStagePosition = 0;
  RivConductancePosition = 1;
  RivBottomPosition = 2;
  RivStartConcentration = 3;

resourcestring
  StrRIVStageSetToZer = 'RIV stage set to zero because of a math error';
  StrRIVConductanceSet = 'RIV conductance set to zero because of a math erro' +
  'r';
  StrRIVBottomSetToZe = 'RIV bottom set to zero because of a math error';
  StrRIVConcentrationSe = 'RIV Concentration set to zero because of a math e' +
  'rror';


implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, ModflowTimeSeriesUnit, ModflowMvrUnit,
  frmErrorsAndWarningsUnit;

{ TRivItem }

procedure TRivItem.Assign(Source: TPersistent);
var
  Riv: TRivItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TRivItem then
  begin
    Riv := TRivItem(Source);
    RiverStage := Riv.RiverStage;
    Conductance := Riv.Conductance;
    RiverBottom := Riv.RiverBottom;
    GwtConcentrations := Riv.GwtConcentrations;
  end;
  inherited;
end;

procedure TRivItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRivCollection;
  StageObserver: TObserver;
  ConductanceObserver: TObserver;
  BottomObserver: TObserver;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TRivCollection;
  StageObserver := FObserverList[RivStagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStageData;
  ConductanceObserver := FObserverList[RivConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
  BottomObserver := FObserverList[RivBottomPosition];
  BottomObserver.OnUpToDateSet := ParentCollection.InvalidateRiverBottomData;
  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    GwtConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateGwtConcentrations;
  end;
end;

function TRivItem.BoundaryFormulaCount: integer;
begin
  result := 3;
  if GwtConcentrations <> nil then
  begin
    result := result + GwtConcentrations.Count;
  end;
end;

constructor TRivItem.Create(Collection: TCollection);
var
  RivCol: TRivCollection;
begin
  RivCol := Collection as TRivCollection;
  FGwtConcentrations := TRivGwtConcCollection.Create(Model, ScreenObject,
    RivCol);
  inherited;
end;

procedure TRivItem.CreateFormulaObjects;
begin
  FRiverStage := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FRiverBottom := CreateFormulaObject(dso3D);
end;

destructor TRivItem.Destroy;
begin
  FGwtConcentrations.Free;
  RiverBottom := '0';
  RiverStage := '0';
  Conductance := '0';
  inherited;
end;

function TRivItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TGwtConcStringValueItem;
begin
  case Index of
    RivStagePosition: result := RiverStage;
    RivConductancePosition: result := Conductance;
    RivBottomPosition: result := RiverBottom;
    else
      begin
        Dec(Index, 3);
        while GwtConcentrations.Count <= Index do
        begin
          GwtConcentrations.Add;
        end;
        Item := GwtConcentrations[Index];
        result := Item.Value;
      end;
  end;
end;

function TRivItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(RivConductancePosition);
end;

function TRivItem.GetConductanceIndex: Integer;
begin
  result := RivConductancePosition;
end;

procedure TRivItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  ConcIndex: Integer;
  Item: TGwtConcStringValueItem;
begin
  if Sender = FRiverStage then
  begin
    List.Add( FObserverList[RivStagePosition]);
  end;
  if Sender = FConductance then
  begin
    List.Add( FObserverList[RivConductancePosition]);
  end;
  if Sender = FRiverBottom then
  begin
    List.Add( FObserverList[RivBottomPosition]);
  end;
  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    Item := GwtConcentrations.Items[ConcIndex];
    if Item.ValueObject = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
end;

function TRivItem.GetRiverBottom: string;
begin
  Result := FRiverBottom.Formula;
  ResetItemObserver(RivBottomPosition);
end;

function TRivItem.GetRiverStage: string;
begin
  Result := FRiverStage.Formula;
  ResetItemObserver(RivStagePosition);
end;

procedure TRivItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfRivConductance(self);
    PhastModel.InvalidateMfRivStage(self);
    PhastModel.InvalidateMfRivBottom(self);
    PhastModel.InvalidateMfRivConc(self);
  end;
end;

function TRivItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TRivItem;
begin
  result := (AnotherItem is TRivItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TRivItem(AnotherItem);
    result := (Item.RiverStage = RiverStage)
      and (Item.Conductance = Conductance)
      and (Item.RiverBottom = RiverBottom)
      and (Item.GwtConcentrations.IsSame(GwtConcentrations));
  end;
end;

procedure TRivItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FRiverBottom,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRiverStage,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TRivItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TGwtConcStringValueItem;
begin
  inherited;
  case Index of
    RivStagePosition: RiverStage := Value;
    RivConductancePosition: Conductance := Value;
    RivBottomPosition: RiverBottom := Value;
    else
      begin
        Dec(Index, 3);
        while Index >= GwtConcentrations.Count do
        begin
          GwtConcentrations.Add;
        end;
        Item := GwtConcentrations[Index];
        Item.Value := Value;
      end;
  end;
end;

procedure TRivItem.SetRiverBottom(const Value: string);
begin
  UpdateFormulaBlocks(Value, RivBottomPosition, FRiverBottom);
end;

procedure TRivItem.SetRiverStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, RivStagePosition, FRiverStage);
end;

procedure TRivItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, RivConductancePosition, FConductance);
end;

procedure TRivItem.SetGwtConcentrations(const Value: TRivGwtConcCollection);
begin
  FGwtConcentrations.Assign(Value);
end;

{ TRivCollection }

function TRivCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TRivTimeListLink;
end;

procedure TRivCollection.TestIfObservationsPresent(
  var EndOfLastStressPeriod: Double; var StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
var
  Boundary: TRivBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TRivBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

procedure TRivCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TRivStorage.Create(AModel));
end;

function TRivCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TRivBoundary;
  ScreenObject: TScreenObject;
  Item: TRivItem;
begin
  Item := Items[ItemIndex] as TRivItem;
  if FormulaIndex = RivConductancePosition then
  begin
    Boundary := BoundaryGroup as TRivBoundary;
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

procedure TRivCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  RivStorage: TRivStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  ConcIndex: Integer;
  AllowedIndicies: Set of Byte;
  SpeciesIndex: Byte;
  LocalModel: TCustomModel;
  ErrorMessage: string;
  LocalScreenObject: TScreenObject;
begin
  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);

  AllowedIndicies := [RivStagePosition,RivConductancePosition, RivBottomPosition];
  LocalModel := AModel as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      Include(AllowedIndicies, RivStartConcentration + SpeciesIndex);
    end;
  end;

  Assert(BoundaryFunctionIndex in AllowedIndicies);
  Assert(Expression <> nil);

  RivStorage := BoundaryStorage as TRivStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    try
      Expression.Evaluate;
      with RivStorage.RivArray[Index] do
      begin
        case BoundaryFunctionIndex of
          RivStagePosition:
            begin
              RiverStage := Expression.DoubleResult;
              RiverStageAnnotation := ACell.Annotation;
              RiverStagePest := PestName;
              RiverStagePestSeriesName := PestSeriesName;
              RiverStagePestSeriesMethod := PestSeriesMethod;
              RiverStageTimeSeriesName := TimeSeriesName;
            end;
          RivConductancePosition:
            begin
              Conductance := Expression.DoubleResult;
              ConductanceAnnotation := ACell.Annotation;
              ConductancePest := PestName;
              ConductancePestSeriesName := PestSeriesName;
              ConductancePestSeriesMethod := PestSeriesMethod;
              ConductanceTimeSeriesName := TimeSeriesName;
            end;
          RivBottomPosition:
            begin
              RiverBottom := Expression.DoubleResult;
              RiverBottomAnnotation := ACell.Annotation;
              RiverBottomPest := PestName;
              RiverBottomPestSeriesName := PestSeriesName;
              RiverBottomPestSeriesMethod := PestSeriesMethod;
              RiverBottomTimeSeriesName := TimeSeriesName;
            end;
          else
            begin
              ConcIndex := BoundaryFunctionIndex - RivStartConcentration;
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
        with RivStorage.RivArray[Index] do
        begin
          case BoundaryFunctionIndex of
            RivStagePosition:
              begin
                ErrorMessage :=  StrRIVStageSetToZer;
                RiverStage := 0;
                RiverStageAnnotation := ErrorMessage;
                RiverStagePest := PestName;
                RiverStagePestSeriesName := PestSeriesName;
                RiverStagePestSeriesMethod := PestSeriesMethod;
                RiverStageTimeSeriesName := TimeSeriesName;
              end;
            RivConductancePosition:
              begin
                ErrorMessage :=  StrRIVConductanceSet;
                Conductance := 0;
                ConductanceAnnotation := ErrorMessage;
                ConductancePest := PestName;
                ConductancePestSeriesName := PestSeriesName;
                ConductancePestSeriesMethod := PestSeriesMethod;
                ConductanceTimeSeriesName := TimeSeriesName;
              end;
            RivBottomPosition:
              begin
                ErrorMessage :=  StrRIVBottomSetToZe;
                RiverBottom := 0;
                RiverBottomAnnotation := ErrorMessage;
                RiverBottomPest := PestName;
                RiverBottomPestSeriesName := PestSeriesName;
                RiverBottomPestSeriesMethod := PestSeriesMethod;
                RiverBottomTimeSeriesName := TimeSeriesName;
              end;
            else
              begin
                ErrorMessage := StrRIVConcentrationSe;
                ConcIndex := BoundaryFunctionIndex - RivStartConcentration;
                GwtConcentrations.Values[ConcIndex] := 0;
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
        with RivStorage.RivArray[Index] do
        begin
          case BoundaryFunctionIndex of
            RivStagePosition:
              begin
                ErrorMessage :=  StrRIVStageSetToZer;
                RiverStage := 0;
                RiverStageAnnotation := ErrorMessage;
                RiverStagePest := PestName;
                RiverStagePestSeriesName := PestSeriesName;
                RiverStagePestSeriesMethod := PestSeriesMethod;
                RiverStageTimeSeriesName := TimeSeriesName;
              end;
            RivConductancePosition:
              begin
                ErrorMessage :=  StrRIVConductanceSet;;
                Conductance := 0;
                ConductanceAnnotation := ErrorMessage;
                ConductancePest := PestName;
                ConductancePestSeriesName := PestSeriesName;
                ConductancePestSeriesMethod := PestSeriesMethod;
                ConductanceTimeSeriesName := TimeSeriesName;
              end;
            RivBottomPosition:
              begin
                ErrorMessage :=  StrRIVBottomSetToZe;
                RiverBottom := 0;
                RiverBottomAnnotation := ErrorMessage;
                RiverBottomPest := PestName;
                RiverBottomPestSeriesName := PestSeriesName;
                RiverBottomPestSeriesMethod := PestSeriesMethod;
                RiverBottomTimeSeriesName := TimeSeriesName;
              end;
            else
              begin
                ErrorMessage := StrRIVConcentrationSe;
                ConcIndex := BoundaryFunctionIndex - RivStartConcentration;
                GwtConcentrations.Values[ConcIndex] := 0;
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

procedure TRivCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  RivStorage: TRivStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  RivStorage := BoundaryStorage as TRivStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with RivStorage.RivArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TRivCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  Index: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TRivStorage).FRivArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for Index := 0 to BoundaryCount - 1 do
    begin
      SetLength(TRivStorage(Boundaries[ItemIndex, AModel]).FRivArray[Index].GwtConcentrations.Values,
        LocalModel.MobileComponents.Count);
      SetLength(TRivStorage(Boundaries[ItemIndex, AModel]).FRivArray[Index].GwtConcentrations.ValueAnnotations,
        LocalModel.MobileComponents.Count);
      SetLength(TRivStorage(Boundaries[ItemIndex, AModel]).FRivArray[Index].GwtConcentrations.ValuePestNames,
        LocalModel.MobileComponents.Count);
      SetLength(TRivStorage(Boundaries[ItemIndex, AModel]).FRivArray[Index].GwtConcentrations.ValuePestSeriesNames,
        LocalModel.MobileComponents.Count);
      SetLength(TRivStorage(Boundaries[ItemIndex, AModel]).FRivArray[Index].GwtConcentrations.ValuePestSeriesMethods,
        LocalModel.MobileComponents.Count);
      SetLength(TRivStorage(Boundaries[ItemIndex, AModel]).FRivArray[Index].GwtConcentrations.ValueTimeSeriesNames,
        LocalModel.MobileComponents.Count);
    end;
  end;
  inherited;
end;

procedure TRivCollection.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRivTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRivTimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRivTimeListLink;
      Link.FConductanceData.Invalidate;
    end;
  end;
end;

procedure TRivCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TRivTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRivTimeListLink;
    for Index := 0 to Link.FConcList.Count - 1 do
    begin
      TimeList := Link.FConcList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRivTimeListLink;
      for Index := 0 to Link.FConcList.Count - 1 do
      begin
        TimeList := Link.FConcList[Index];
        TimeList.Invalidate;
      end;
    end;
  end;
end;

procedure TRivCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfRivConductance(self);
    PhastModel.InvalidateMfRivStage(self);
    PhastModel.InvalidateMfRivBottom(self);
    PhastModel.InvalidateMfRivConc(self);
  end;
end;

procedure TRivCollection.InvalidateRiverBottomData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRivTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRivTimeListLink;
    Link.FRiverBottomData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRivTimeListLink;
      Link.FRiverBottomData.Invalidate;
    end;
  end;
end;

procedure TRivCollection.InvalidateStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TRivTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TRivTimeListLink;
    Link.FRiverStageData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TRivTimeListLink;
      Link.FRiverStageData.Invalidate;
    end;
  end;
end;

class function TRivCollection.ItemClass: TBoundaryItemClass;
begin
  result := TRivItem;
end;

{ TRivParamItem }

class function TRivParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TRivCollection;
end;

{ TRiv_Cell }

function TRiv_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
var
  ConcIndex: Integer;
begin
  result := '';
  case Index of
    RivStagePosition: result := RiverStageAnnotation;
    RivConductancePosition: result := ConductanceAnnotation;
    RivBottomPosition: result := RiverBottomAnnotation;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        result := FValues.GwtConcentrations.ValueAnnotations[ConcIndex];
      end;
  end;
end;

function TRiv_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  ConcIndex: Integer;
begin
  case Index of
    RivStagePosition: result := RiverStage;
    RivConductancePosition: result := Conductance;
    RivBottomPosition: result := RiverBottom;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        result := FValues.GwtConcentrations.Values[ConcIndex];
      end;
  end;
end;

function TRiv_Cell.GetRiverBottom: double;
begin
  result := Values.RiverBottom;
end;

function TRiv_Cell.GetRiverBottomAnnotation: string;
begin
  result := Values.RiverBottomAnnotation;
end;

function TRiv_Cell.GetRiverBottomPest: string;
begin
  result := Values.RiverBottomPest;
end;

function TRiv_Cell.GetRiverBottomPestSeriesMethod: TPestParamMethod;
begin
  result := Values.RiverBottomPestSeriesMethod;
end;

function TRiv_Cell.GetRiverBottomPestSeriesName: string;
begin
  result := Values.RiverBottomPestSeriesName;
end;

function TRiv_Cell.GetRiverBottomTimeSeriesName: string;
begin
  result := Values.RiverBottomTimeSeriesName;
end;

function TRiv_Cell.GetRiverStage: double;
begin
  result := Values.RiverStage;
end;

function TRiv_Cell.GetRiverStageAnnotation: string;
begin
  result := Values.RiverStageAnnotation;
end;

function TRiv_Cell.GetRiverStagePest: string;
begin
  result := Values.RiverStagePest;
end;

function TRiv_Cell.GetRiverStagePestSeriesMethod: TPestParamMethod;
begin
  result := Values.RiverStagePestSeriesMethod;
end;

function TRiv_Cell.GetRiverStagePestSeriesName: string;
begin
  result := Values.RiverStagePestSeriesName;
end;

function TRiv_Cell.GetRiverStageTimeSeriesName: string;
begin
  result := Values.RiverStageTimeSeriesName;
end;

procedure TRiv_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TRiv_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TRiv_Cell.GetConcentration(const Index: Integer): double;
begin
  result := FValues.GwtConcentrations.Values[Index];
end;

function TRiv_Cell.GetConcentrationAnnotation(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValueAnnotations[Index];
end;

function TRiv_Cell.GetConcentrationPestName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValuePestNames[Index];
end;

function TRiv_Cell.GetConcentrationPestSeriesMethod(
  const Index: Integer): TPestParamMethod;
begin
  result := FValues.GwtConcentrations.ValuePestSeriesMethods[Index];
end;

function TRiv_Cell.GetConcentrationPestSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValuePestSeriesNames[Index];
end;

function TRiv_Cell.GetConcentrationTimeSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValueTimeSeriesNames[Index];
end;

function TRiv_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TRiv_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TRiv_Cell.GetConductanceParameterName: string;
begin
  result := Values.ConductanceParameterName;
end;

function TRiv_Cell.GetConductanceParameterValue: double;
begin
  result := Values.ConductanceParameterValue;
end;

function TRiv_Cell.GetConductancePest: string;
begin
  result := Values.ConductancePest;
end;

function TRiv_Cell.GetConductancePestSeriesMethod: TPestParamMethod;
begin
  result := Values.ConductancePestSeriesMethod;
end;

function TRiv_Cell.GetConductancePestSeriesName: string;
begin
  result := Values.ConductancePestSeriesName;
end;

function TRiv_Cell.GetConductanceTimeSeriesName: string;
begin
  result := Values.ConductanceTimeSeriesName;
end;

function TRiv_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TRiv_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TRiv_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TRiv_Cell.GetMf6TimeSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    RivStagePosition: result := RiverStageTimeSeriesName;
    RivConductancePosition: result := ConductanceTimeSeriesName;
    RivBottomPosition: result := RiverBottomTimeSeriesName;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        result := FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex];
      end;
  end;
end;

function TRiv_Cell.GetMvrIndex: Integer;
begin
  result := Values.MvrIndex;
end;

function TRiv_Cell.GetMvrUsed: Boolean;
begin
  result := Values.MvrUsed;
end;

function TRiv_Cell.GetPestName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    RivStagePosition: result := RiverStagePest;
    RivConductancePosition: result := ConductancePest;
    RivBottomPosition: result := RiverBottomPest;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        result := FValues.GwtConcentrations.ValuePestNames[ConcIndex];
      end;
  end;
end;

function TRiv_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case Index of
    RivStagePosition: result := RiverStagePestSeriesMethod;
    RivConductancePosition: result := ConductancePestSeriesMethod;
    RivBottomPosition: result := RiverBottomPestSeriesMethod;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesMethods[ConcIndex];
      end;
  end;
end;

function TRiv_Cell.GetPestSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    RivStagePosition: result := RiverStagePestSeries;
    RivConductancePosition: result := ConductancePestSeries;
    RivBottomPosition: result := RiverBottomPestSeries;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesNames[ConcIndex];
      end;
  end;
end;

function TRiv_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRiv_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

//function TRiv_Cell.GetTimeSeriesName: string;
//begin
//  result := Values.TimeSeriesName;
//end;

function TRiv_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  RIV_Cell: TRiv_Cell;
begin
  result := AnotherCell is TRiv_Cell;
  if result then
  begin
    RIV_Cell := TRiv_Cell(AnotherCell);
    result :=
      (Conductance = RIV_Cell.Conductance)
      and (RiverBottom = RIV_Cell.RiverBottom)
      and (RiverStage = RIV_Cell.RiverStage)
      and (IFace = RIV_Cell.IFace)
      and (Values.Cell = RIV_Cell.Values.Cell);
  end;
end;

procedure TRiv_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TRiv_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TRiv_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TRiv_Cell.SetConductanceTimeSeriesName(const Value: string);
begin
  FValues.ConductanceTimeSeriesName := Value;
end;

procedure TRiv_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TRiv_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
var
  ConcIndex: Integer;
begin
  inherited;
  case Index of
    RivStagePosition:
      RiverStageTimeSeriesName := Value;
    RivConductancePosition:
      ConductanceTimeSeriesName := Value;
    RivBottomPosition:
      RiverBottomTimeSeriesName := Value;
    else
      begin
        ConcIndex := Index - RivStartConcentration;
        FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := Value;
      end;
  end;
end;

procedure TRiv_Cell.SetRiverBottomTimeSeriesName(const Value: string);
begin
  FValues.RiverBottomTimeSeriesName := Value;
end;

procedure TRiv_Cell.SetRiverStageTimeSeriesName(const Value: string);
begin
  FValues.RiverStageTimeSeriesName := Value;
end;

procedure TRiv_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TRivBoundary }

procedure TRivBoundary.Assign(Source: TPersistent);
var
  SourceRiv: TRivBoundary;
begin
  if Source is TRivBoundary then
  begin
    SourceRiv := TRivBoundary(Source);

    PestRiverBottomFormula := SourceRiv.PestRiverBottomFormula;
    PestRiverStageFormula := SourceRiv.PestRiverStageFormula;
    PestConductanceFormula := SourceRiv.PestConductanceFormula;
    PestRiverBottomMethod := SourceRiv.PestRiverBottomMethod;
    PestRiverStageMethod := SourceRiv.PestRiverStageMethod;
    PestConductanceMethod := SourceRiv.PestConductanceMethod;
    PestConcentrationFormulas := SourceRiv.PestConcentrationFormulas;
    PestConcentrationMethods := SourceRiv.PestConcentrationMethods;
  end;
  inherited;
end;

procedure TRivBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TRiv_Cell;
  BoundaryValues: TRivRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TRivStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
  MvrUsed: Boolean;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TRivStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcRiv);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TRiv_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Length(LocalBoundaryStorage.RivArray) then
      begin
        Cells.Capacity := Length(LocalBoundaryStorage.RivArray);
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.RivArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.RivArray[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed;
        BoundaryValues.MvrIndex := BoundaryIndex;
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.Conductance :=
            BoundaryValues.Conductance * FCurrentParameter.Value;
//          BoundaryValues.ConductanceAnnotation :=
//            BoundaryValues.ConductanceAnnotation
//            + ' multiplied by the parameter value for "'+ FCurrentParameter.ParameterName + '."';
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
        Cell := TRiv_Cell.Create;
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

class function TRivBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TRivCollection;
end;

function TRivBoundary.BoundaryObserverPrefix: string;
begin
  result := 'Pest_Riv';
end;

constructor TRivBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FPestConcentrationFormulas:= TRivGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestConcentrationFormulas.UsedForPestSeries := True;
  FPestConcentrationMethods := TGwtPestMethodCollection.Create(Model);
  FConcentrationObservers := TObserverList.Create;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestRiverStageFormula := '';
  PestRiverBottomFormula := '';
  PestConductanceFormula := '';
  FPestRiverStageMethod := DefaultBoundaryMethod(RivStagePosition);
  FPestRiverBottomMethod := DefaultBoundaryMethod(RivBottomPosition);
  FPestConductanceMethod := DefaultBoundaryMethod(RivConductancePosition);

end;

procedure TRivBoundary.CreateFormulaObjects;
var
  LocalModel: TPhastModel;
  ConcIndex: Integer;
begin
  FPestRiverStageFormula := CreateFormulaObjectBlocks(dso3D);
  FPestCondFormula := CreateFormulaObjectBlocks(dso3D);
  FPestRiverBottomFormula := CreateFormulaObjectBlocks(dso3D);
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestConcentrationFormulas.Add;
    end;
  end;
end;

procedure TRivBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestRiverStageObserver);
    FObserverList.Add(PestConductanceObserver);
    FObserverList.Add(PestRiverBottomObserver);
    for Index := 0 to FPestConcentrationFormulas.Count - 1 do
    begin
      FObserverList.Add(ConcentrationObserver[Index]);
    end;
  end;
end;

class function TRivBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    RivStagePosition:
      begin
        result := ppmAdd;
      end;
    RivConductancePosition:
      begin
        result := ppmMultiply;
      end;
    RivBottomPosition:
      begin
        result := ppmAdd;
      end;
    else
      begin
        result := ppmMultiply;
      end;
  end;
end;

destructor TRivBoundary.Destroy;
var
  Index: Integer;
begin
  PestRiverStageFormula := '';
  PestRiverBottomFormula := '';
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

procedure TRivBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TRivStorage;
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
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TRivStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TRivStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
    if {(ValueIndex = Values.Count - 1) and} ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TRivStorage;
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
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TRivStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TRivStorage;
        AssignCells(BoundaryStorage, Times, AModel);
        Inc(ValueCount);
      end;
      if {(ValueIndex = Param.Param.Count - 1) and} ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TRivStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

function TRivBoundary.GetConcentrationObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FConcentrationObservers.Count do
  begin
    CreateObserver(Format('RivConc_%d', [Index+1]), AObserver, nil);
    FConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateConcData;
  end;
  result := FConcentrationObservers[Index];
end;

function TRivBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ConcIndex: Integer;
begin
//  result := '';
  case FormulaIndex of
    RivStagePosition:
      begin
        result := PestRiverStageFormula;
      end;
    RivConductancePosition:
      begin
        result := PestConductanceFormula;
      end;
    RivBottomPosition:
      begin
        result := PestRiverBottomFormula;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RivStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        result := PestConcentrationFormulas[ConcIndex].Value;
      end;
  end;
end;

function TRivBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RivStagePosition:
      begin
        result := PestRiverStageMethod;
      end;
    RivConductancePosition:
      begin
        result := PestConductanceMethod;
      end;
    RivBottomPosition:
      begin
        result := PestRiverBottomMethod;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RivStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        result := FPestConcentrationMethods[ConcIndex].PestParamMethod;
      end;
  end;
end;

function TRivBoundary.GetPestConductanceFormula: string;
begin
  Result := FPestCondFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RivConductancePosition);
  end;
end;

function TRivBoundary.GetPestConductanceObserver: TObserver;
begin
  if FPestConductanceObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestConductanceObserver, nil);
    FPestConductanceObserver.OnUpToDateSet := InvalidateConductanceData;
  end;
  result := FPestConductanceObserver;
end;

function TRivBoundary.GetPestRiverBottomFormula: string;
begin
  Result := FPestRiverBottomFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RivBottomPosition);
  end;
end;

function TRivBoundary.GetPestRiverBottomObserver: TObserver;
begin
  if FPestRiverBottomObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestRiverBottomObserver, nil);
    FPestRiverBottomObserver.OnUpToDateSet := InvalidateRiverBottomData;
  end;
  result := FPestRiverBottomObserver;
end;

function TRivBoundary.GetPestRiverStageFormula: string;
begin
  Result := FPestRiverStageFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(RivStagePosition);
  end;
end;

function TRivBoundary.GetPestRiverStageObserver: TObserver;
begin
  if FPestRiverStageObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestRiverStageObserver, nil);
    FPestRiverStageObserver.OnUpToDateSet := InvalidateStageData;
  end;
  result := FPestRiverStageObserver;
end;

procedure TRivBoundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: Integer;
begin
  if Sender = FPestRiverStageFormula then
  begin
    if RivStagePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[RivStagePosition]);
    end;
  end;
  if Sender = FPestCondFormula then
  begin
    if RivConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[RivConductancePosition]);
    end;
  end;
  if Sender = FPestRiverBottomFormula then
  begin
    if RivBottomPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[RivBottomPosition]);
    end;
  end;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    if FPestConcentrationFormulas[Index].ValueObject = Sender then
    begin
      List.Add(FObserverList[RivStartConcentration + Index]);
    end;
  end;
end;

function TRivBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestRiv_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TRivBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TRivBoundary.InvalidateConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfRivConc(self);
end;

procedure TRivBoundary.InvalidateConductanceData(Sender: TObject);
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
    PhastModel.InvalidateMfRivConductance(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfRivConductance(self);
    end;
  end;
end;

procedure TRivBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfRivConductance(self);
    Model.InvalidateMfRivStage(self);
    Model.InvalidateMfRivBottom(self);
    Model.InvalidateMfRivConc(self);
  end;
end;

procedure TRivBoundary.InvalidateRiverBottomData(Sender: TObject);
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
    PhastModel.InvalidateMfRivBottom(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfRivBottom(self);
    end;
  end;
end;

procedure TRivBoundary.InvalidateStageData(Sender: TObject);
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
    PhastModel.InvalidateMfRivStage(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfRivStage(self);
    end;
  end;
end;

class function TRivBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TRivParamItem;
end;

function TRivBoundary.ParameterType: TParameterType;
begin
  result := ptRIV;
end;

procedure TRivBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RivStagePosition:
      begin
        PestRiverStageFormula := Value;
      end;
    RivConductancePosition:
      begin
        PestConductanceFormula := Value;
      end;
    RivBottomPosition:
      begin
        PestRiverBottomFormula := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RivStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        PestConcentrationFormulas[ConcIndex].Value := Value;
      end;
  end;
end;

procedure TRivBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    RivStagePosition:
      begin
        PestRiverStageMethod := Value;
      end;
    RivConductancePosition:
      begin
        PestConductanceMethod := Value;
      end;
    RivBottomPosition:
      begin
        PestRiverBottomMethod := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - RivStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        FPestConcentrationMethods[ConcIndex].PestParamMethod := Value;
      end;
  end;
end;

procedure TRivBoundary.SetPestConcentrationFormulas(
  const Value: TRivGwtConcCollection);
begin
  FPestConcentrationFormulas.Assign(Value);
end;

procedure TRivBoundary.SetPestConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestConcentrationMethods.Assign(Value);
end;

procedure TRivBoundary.SetPestConductanceFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RivConductancePosition, FPestCondFormula);
end;

procedure TRivBoundary.SetPestConductanceMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestConductanceMethod, Value);
end;

procedure TRivBoundary.SetPestRiverBottomFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RivBottomPosition, FPestRiverBottomFormula);
end;

procedure TRivBoundary.SetPestRiverBottomMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRiverBottomMethod, Value);
end;

procedure TRivBoundary.SetPestRiverStageFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, RivStagePosition, FPestRiverStageFormula);
end;

procedure TRivBoundary.SetPestRiverStageMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRiverStageMethod, Value);
end;

//procedure TRivBoundary.SetInterp(const Value: TMf6InterpolationMethods);
//begin
//  if FInterp <> Value then
//  begin
//    InvalidateModel;
//    FInterp := Value;
//  end;
//end;

procedure TRivBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
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
  ObservationsPresent := LocalPhastModel.RvobIsSelected
    and (LocalPhastModel.RiverObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

{ TRivRecord }

procedure TRivRecord.Assign(const Item: TRivRecord);
begin
  self := Item;
  GwtConcentrations.Assign(Item.GwtConcentrations);
//  SetLength(Concentrations, Length(Concentrations));
//  SetLength(ConcentrationAnnotations, Length(ConcentrationAnnotations));
//  SetLength(ConcentrationPestNames, Length(ConcentrationPestNames));
//  SetLength(ConcentrationPestSeriesNames, Length(ConcentrationPestSeriesNames));
//  SetLength(ConcentrationPestSeriesMethods, Length(ConcentrationPestSeriesMethods));
//  SetLength(ConcentrationTimeSeriesNames, Length(ConcentrationTimeSeriesNames));
end;

procedure TRivRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
//var
//  Index: Integer;
//  Count: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, RiverStage);
  WriteCompReal(Comp, RiverBottom);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, ConductanceParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RiverStageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RiverBottomAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(ConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(RiverStagePest));
  WriteCompInt(Comp, Strings.IndexOf(RiverBottomPest));

  WriteCompInt(Comp, Strings.IndexOf(ConductancePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RiverStagePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RiverBottomPestSeriesName));

  WriteCompInt(Comp, Ord(ConductancePestSeriesMethod));
  WriteCompInt(Comp, Ord(RiverStagePestSeriesMethod));
  WriteCompInt(Comp, Ord(RiverBottomPestSeriesMethod));

  WriteCompInt(Comp, Strings.IndexOf(ConductanceParameterName));

  WriteCompInt(Comp, Strings.IndexOf(ConductanceTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RiverStageTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(RiverBottomTimeSeriesName));

  GwtConcentrations.Cache(Comp, Strings);
//  Count := Length(Concentrations);
//  WriteCompInt(Comp, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    WriteCompReal(Comp, Concentrations[Index]);
//  end;
//  for Index := 0 to Count - 1 do
//  begin
//    WriteCompInt(Comp, Strings.IndexOf(ConcentrationAnnotations[Index]));
//  end;
//  for Index := 0 to Count - 1 do
//  begin
//    WriteCompInt(Comp, Strings.IndexOf(ConcentrationPestNames[Index]));
//  end;
//  for Index := 0 to Count - 1 do
//  begin
//    WriteCompInt(Comp, Strings.IndexOf(ConcentrationPestSeriesNames[Index]));
//  end;
//  for Index := 0 to Count - 1 do
//  begin
//    WriteCompInt(Comp, Ord(ConcentrationPestSeriesMethods[Index]));
//  end;
//  for Index := 0 to Count - 1 do
//  begin
//    WriteCompInt(Comp, Strings.IndexOf(ConcentrationTimeSeriesNames[Index]));
//  end;

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TRivRecord.RecordStrings(Strings: TStringList);
//var
//  Index: Integer;
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(RiverStageAnnotation);
  Strings.Add(RiverBottomAnnotation);
  Strings.Add(ConductancePest);
  Strings.Add(RiverStagePest);
  Strings.Add(RiverBottomPest);

  Strings.Add(ConductancePestSeriesName);
  Strings.Add(RiverStagePestSeriesName);
  Strings.Add(RiverBottomPestSeriesName);

//  Strings.Add(TimeSeriesName);
  Strings.Add(ConductanceParameterName);

  Strings.Add(ConductanceTimeSeriesName);
  Strings.Add(RiverStageTimeSeriesName);
  Strings.Add(RiverBottomTimeSeriesName);

  GwtConcentrations.RecordStrings(Strings);

//  for Index := 0 to Length(ConcentrationAnnotations) - 1 do
//  begin
//    Strings.Add(ConcentrationAnnotations[Index]);
//  end;
//  for Index := 0 to Length(ConcentrationPestNames) - 1 do
//  begin
//    Strings.Add(ConcentrationPestNames[Index]);
//  end;
//  for Index := 0 to Length(ConcentrationPestSeriesNames) - 1 do
//  begin
//    Strings.Add(ConcentrationPestSeriesNames[Index]);
//  end;
//  for Index := 0 to Length(ConcentrationTimeSeriesNames) - 1 do
//  begin
//    Strings.Add(ConcentrationTimeSeriesNames[Index]);
//  end;
end;

procedure TRivRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
//var
//  Count: Integer;
//  Index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  RiverStage := ReadCompReal(Decomp);
  RiverBottom := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceParameterValue := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  RiverStageAnnotation := Annotations[ReadCompInt(Decomp)];
  RiverBottomAnnotation := Annotations[ReadCompInt(Decomp)];

  ConductancePest := Annotations[ReadCompInt(Decomp)];
  RiverStagePest := Annotations[ReadCompInt(Decomp)];
  RiverBottomPest := Annotations[ReadCompInt(Decomp)];

  ConductancePestSeriesName := Annotations[ReadCompInt(Decomp)];
  RiverStagePestSeriesName := Annotations[ReadCompInt(Decomp)];
  RiverBottomPestSeriesName := Annotations[ReadCompInt(Decomp)];

  ConductancePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RiverStagePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  RiverBottomPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

//  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductanceParameterName := Annotations[ReadCompInt(Decomp)];

  ConductanceTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RiverStageTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  RiverBottomTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  GwtConcentrations.Restore(Decomp,Annotations);

//  Count := ReadCompInt(Decomp);
//  SetLength(Concentrations, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    Concentrations[Index] := ReadCompReal(Decomp);
//  end;
//  SetLength(ConcentrationAnnotations, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    ConcentrationAnnotations[Index] := Annotations[ReadCompInt(Decomp)];
//  end;
//  SetLength(ConcentrationPestNames, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    ConcentrationPestNames[Index] := Annotations[ReadCompInt(Decomp)];
//  end;
//  SetLength(ConcentrationPestSeriesNames, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    ConcentrationPestSeriesNames[Index] := Annotations[ReadCompInt(Decomp)];
//  end;
//  SetLength(ConcentrationPestSeriesMethods, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    ConcentrationPestSeriesMethods[Index] := TPestParamMethod(ReadCompInt(Decomp));
//  end;
//  SetLength(ConcentrationTimeSeriesNames, Count);
//  for Index := 0 to Count - 1 do
//  begin
//    ConcentrationTimeSeriesNames[Index] := Annotations[ReadCompInt(Decomp)];
//  end;

  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);
//  ConductanceAnnotation := ReadCompString(Decomp, Annotations);
//  RiverStageAnnotation := ReadCompString(Decomp, Annotations);
//  RiverBottomAnnotation := ReadCompString(Decomp, Annotations);
{
    ConductanceParameterName: string;
    ConductanceParameterValue: double;
}
end;

{ TRivStorage }

procedure TRivStorage.Clear;
begin
  SetLength(FRivArray, 0);
  FCleared := True;
end;

procedure TRivStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FRivArray);
    for Index := 0 to Count - 1 do
    begin
      FRivArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FRivArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TRivStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FRivArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FRivArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TRivStorage.GetRivArray: TRivArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FRivArray;
end;

{ TRivTimeListLink }

procedure TRivTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  inherited;
  FConcList := TModflowTimeLists.Create;

  FRiverBottomData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRiverStageData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRiverStageData.NonParamDescription := StrRiverStage;
  FRiverStageData.ParamDescription := ' ' + LowerCase(StrRiverStage);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;
  FRiverBottomData.NonParamDescription := StrRiverBottom;
  FRiverBottomData.ParamDescription := ' ' + LowerCase(StrRiverBottom);
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FRiverStageData.OnInvalidate := LocalModel.InvalidateMfRivStage;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfRivConductance;
    FRiverBottomData.OnInvalidate := LocalModel.InvalidateMfRivBottom;
  end;
  AddTimeList(FRiverStageData);
  AddTimeList(FConductanceData);
  AddTimeList(FRiverBottomData);

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

destructor TRivTimeListLink.Destroy;
begin
  FConcList.Free;
  FRiverStageData.Free;
  FConductanceData.Free;
  FRiverBottomData.Free;
  inherited;
end;

procedure TRivTimeListLink.UpdateGwtTimeLists;
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
  end;
end;

procedure TRivTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
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
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMfRivConc;
  end;
  AddTimeList(ConcTimeList);
  FConcList.Add(ConcTimeList);
end;

{ TRivGwtConcCollection }

constructor TRivGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TRivCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

end.
