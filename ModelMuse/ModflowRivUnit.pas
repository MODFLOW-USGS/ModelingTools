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

    ConductancePest: string;
    RiverStagePest: string;
    RiverBottomPest: string;

    ConductancePestSeriesName: string;
    RiverStagePestSeriesName: string;
    RiverBottomPestSeriesName: string;

    ConductancePestSeriesMethod: TPestParamMethod;
    RiverStagePestSeriesMethod: TPestParamMethod;
    RiverBottomPestSeriesMethod: TPestParamMethod;

    TimeSeriesName: string;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    ConductanceParameterName: string;
    ConductanceParameterValue: double;
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
    // See @link(RiverBottom).
    procedure SetRiverBottom(const Value: string);
    // See @link(RiverStage).
    procedure SetRiverStage(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetRiverBottom: string;
    function GetRiverStage: string;
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
  protected
    procedure CreateTimeLists; override;
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
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod); override;
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
  end;

  // Each @name stores a @link(TRivCollection).
  // @classname is stored by @link(TModflowParameters).
  TRivParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TRiv_Cell = class(TValueCell)
  private
    Values: TRivRecord;
    StressPeriod: integer;
    function GetRiverStage: double;
    function GetConductance: double;
    function GetRiverBottom: double;
    function GetConductanceAnnotation: string;
    function GetRiverBottomAnnotation: string;
    function GetRiverStageAnnotation: string;
    function GetTimeSeriesName: string;
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
    property TimeSeriesName: string read GetTimeSeriesName;
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

    property ConductancePestSeriesMethod: TPestParamMethod read GetConductancePestSeriesMethod;
    property RiverStagePestSeriesMethod: TPestParamMethod read GetRiverStagePestSeriesMethod;
    property RiverBottomPestSeriesMethod: TPestParamMethod read GetRiverBottomPestSeriesMethod;

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
//    FInterp: TMf6InterpolationMethods;
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
//    procedure SetInterp(const Value: TMf6InterpolationMethods);
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
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property Interp;
    property PestRiverBottomFormula: string read GetPestRiverBottomFormula
      write SetPestRiverBottomFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRiverStageFormula: string read GetPestRiverStageFormula
      write SetPestRiverStageFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestConductanceFormula: string read GetPestConductanceFormula
      write SetPestConductanceFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRiverBottomMethod: TPestParamMethod read FPestRiverBottomMethod
      write SetPestRiverBottomMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestRiverStageMethod: TPestParamMethod read FPestRiverStageMethod
      write SetPestRiverStageMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestConductanceMethod: TPestParamMethod
      read FPestConductanceMethod write SetPestConductanceMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles, 
  frmGoPhastUnit, GIS_Functions, ModflowTimeSeriesUnit, ModflowMvrUnit;

const
  StagePosition = 0;
  ConductancePosition = 1;
  BottomPosition = 2;

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
  end;
  inherited;
end;

procedure TRivItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TRivCollection;
  StageObserver: TObserver;
  ConductanceObserver: TObserver;
  BottomObserver: TObserver;
begin
  ParentCollection := Collection as TRivCollection;
  StageObserver := FObserverList[StagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStageData;
  ConductanceObserver := FObserverList[ConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
  BottomObserver := FObserverList[BottomPosition];
  BottomObserver.OnUpToDateSet := ParentCollection.InvalidateRiverBottomData;
end;

function TRivItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

procedure TRivItem.CreateFormulaObjects;
begin
  FRiverStage := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FRiverBottom := CreateFormulaObject(dso3D);
end;

destructor TRivItem.Destroy;
begin
  RiverBottom := '0';
  RiverStage := '0';
  Conductance := '0';
  inherited;
end;

function TRivItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StagePosition: result := RiverStage;
    ConductancePosition: result := Conductance;
    BottomPosition: result := RiverBottom;
    else Assert(False);
  end;
end;

function TRivItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

function TRivItem.GetConductanceIndex: Integer;
begin
  result := ConductancePosition;
end;

procedure TRivItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FRiverStage then
  begin
    List.Add( FObserverList[StagePosition]);
  end;
  if Sender = FConductance then
  begin
    List.Add( FObserverList[ConductancePosition]);
  end;
  if Sender = FRiverBottom then
  begin
    List.Add( FObserverList[BottomPosition]);
  end;
end;

function TRivItem.GetRiverBottom: string;
begin
  Result := FRiverBottom.Formula;
  ResetItemObserver(BottomPosition);
end;

function TRivItem.GetRiverStage: string;
begin
  Result := FRiverStage.Formula;
  ResetItemObserver(StagePosition);
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
      and (Item.RiverBottom = RiverBottom);
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
begin
  inherited;
  case Index of
    StagePosition: RiverStage := Value;
    ConductancePosition: Conductance := Value;
    BottomPosition: RiverBottom := Value;
    else Assert(False);
  end;
end;

procedure TRivItem.SetRiverBottom(const Value: string);
begin
  UpdateFormulaBlocks(Value, BottomPosition, FRiverBottom);
end;

procedure TRivItem.SetRiverStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, StagePosition, FRiverStage);
end;

procedure TRivItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConductancePosition, FConductance);
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
  if FormulaIndex = ConductancePosition then
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
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod);
var
  RivStorage: TRivStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in
    [StagePosition,ConductancePosition, BottomPosition]);
  Assert(Expression <> nil);

  RivStorage := BoundaryStorage as TRivStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with RivStorage.RivArray[Index] do
    begin
      case BoundaryFunctionIndex of
        StagePosition:
          begin
            RiverStage := Expression.DoubleResult;
            RiverStageAnnotation := ACell.Annotation;
            RiverStagePest := PestName;
            RiverStagePestSeriesName := PestSeriesName;
            RiverStagePestSeriesMethod := PestSeriesMethod;
          end;
        ConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
            ConductancePest := PestName;
            ConductancePestSeriesName := PestSeriesName;
            ConductancePestSeriesMethod := PestSeriesMethod;
          end;
        BottomPosition:
          begin
            RiverBottom := Expression.DoubleResult;
            RiverBottomAnnotation := ACell.Annotation;
            RiverBottomPest := PestName;
            RiverBottomPestSeriesName := PestSeriesName;
            RiverBottomPestSeriesMethod := PestSeriesMethod;
          end;
        else
          Assert(False);
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
begin
  SetLength((Boundaries[ItemIndex, AModel] as TRivStorage).FRivArray, BoundaryCount);
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
begin
  result := '';
  case Index of
    StagePosition: result := RiverStageAnnotation;
    ConductancePosition: result := ConductanceAnnotation;
    BottomPosition: result := RiverBottomAnnotation;
    else Assert(False);
  end;
end;

function TRiv_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    StagePosition: result := RiverStage;
    ConductancePosition: result := Conductance;
    BottomPosition: result := RiverBottom;
    else Assert(False);
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

function TRiv_Cell.GetMvrIndex: Integer;
begin
  result := Values.MvrIndex;
end;

function TRiv_Cell.GetMvrUsed: Boolean;
begin
  result := Values.MvrUsed;
end;

function TRiv_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TRiv_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TRiv_Cell.GetTimeSeriesName: string;
begin
  result := Values.TimeSeriesName;
end;

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
  Values.Cell.Column := Value;
end;

procedure TRiv_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TRiv_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TRivBoundary }

procedure TRivBoundary.Assign(Source: TPersistent);
//var
//  SourceRiv: TRivBoundary;
begin
//  if Source is TRivBoundary then
//  begin
//    SourceRiv := TRivBoundary(Source);
//    Interp := SourceRiv.Interp;
//  end;
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
        Cell.Values := BoundaryValues;
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
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestRiverStageFormula := '';
  PestRiverBottomFormula := '';
  PestConductanceFormula := '';
  FPestRiverStageMethod := DefaultBoundaryMethod(StagePosition);
  FPestRiverBottomMethod := DefaultBoundaryMethod(BottomPosition);
  FPestConductanceMethod := DefaultBoundaryMethod(ConductancePosition);

end;

procedure TRivBoundary.CreateFormulaObjects;
begin
  FPestRiverStageFormula := CreateFormulaObjectBlocks(dso3D);
  FPestCondFormula := CreateFormulaObjectBlocks(dso3D);
  FPestRiverBottomFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TRivBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestRiverStageObserver);
    FObserverList.Add(PestConductanceObserver);
    FObserverList.Add(PestRiverBottomObserver);
  end;
end;

class function TRivBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    StagePosition:
      begin
        result := ppmAdd;
      end;
    ConductancePosition:
      begin
        result := ppmMultiply;
      end;
    BottomPosition:
      begin
        result := ppmAdd;
      end;
    else
      result := inherited;
      Assert(False);
  end;
end;

destructor TRivBoundary.Destroy;
begin
  PestRiverStageFormula := '';
  PestRiverBottomFormula := '';
  PestConductanceFormula := '';

  inherited;
end;

procedure TRivBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
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

function TRivBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    StagePosition:
      begin
        result := PestRiverStageFormula;
      end;
    ConductancePosition:
      begin
        result := PestConductanceFormula;
      end;
    BottomPosition:
      begin
        result := PestRiverSBottomFormula;
      end;
    else
      Assert(False);
  end;
end;

function TRivBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    StagePosition:
      begin
        result := PestRiverStageMethod;
      end;
    ConductancePosition:
      begin
        result := PestConductanceMethod;
      end;
    BottomPosition:
      begin
        result := PestRiverBottomMethod;
      end;
    else
      Assert(False);
  end;
end;

function TRivBoundary.GetPestConductanceFormula: string;
begin
  Result := FPestCondFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ConductancePosition);
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
    ResetItemObserver(BottomPosition);
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
    ResetItemObserver(StagePosition);
  end;
end;

function TRivBoundary.GetPestRiverStageObserver: TObserver;
begin
  if FPestRiverStageObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestRiverStageObserver, nil);
    FPestRiverStageObserver.OnUpToDateSet := InvalidateStageBottomData;
  end;
  result := FPestRiverStageObserver;
end;

procedure TRivBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestRiverStageFormula then
  begin
    if StagePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[StagePosition]);
    end;
  end;
  if Sender = FPestCondFormula then
  begin
    if ConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[ConductancePosition]);
    end;
  end;
  if Sender = FPestRiverBottomFormula then
  begin
    if BottomPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[BottomPosition]);
    end;
  end;
end;

function TRivBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestRiv_Used_', FUsedObserver, nil);
//    FUsedObserver.OnUpToDateSet := HandleChangedValue;
  end;
  result := FUsedObserver;
end;

procedure TRivBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
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
begin
  case FormulaIndex of
    StagePosition:
      begin
        PestRiverStageFormula := Value;
      end;
    ConductancePosition:
      begin
        PestConductanceFormula := Value;
      end;
    BottomPosition:
      begin
        PestRiverBottomFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TRivBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    StagePosition:
      begin
        PestRiverStageMethod := Value;
      end;
    ConductancePosition:
      begin
        PestConductanceMethod := Value;
      end;
    BottomPosition:
      begin
        PestRiverBottomMethod := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TRivBoundary.SetPestConductanceFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConductancePosition, FPestCondFormula);
end;

procedure TRivBoundary.SetPestConductanceMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestConductanceMethod, Value);
end;

procedure TRivBoundary.SetPestRiverBottomFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, BottomPosition, FPestRiverBottomFormula);
end;

procedure TRivBoundary.SetPestRiverBottomMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestRiverBottomMethod, Value);
end;

procedure TRivBoundary.SetPestRiverStageFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, StagePosition, FPestRiverStageFormula);
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

procedure TRivRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
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

  WriteCompInt(Comp, Strings.IndexOf(TimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ConductanceParameterName));
  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
//  WriteCompString(Comp, ConductanceAnnotation);
//  WriteCompString(Comp, RiverStageAnnotation);
//  WriteCompString(Comp, RiverBottomAnnotation);
end;

procedure TRivRecord.RecordStrings(Strings: TStringList);
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

  Strings.Add(TimeSeriesName);
  Strings.Add(ConductanceParameterName);
end;

procedure TRivRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
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

  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductanceParameterName := Annotations[ReadCompInt(Decomp)];
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
begin
  inherited;
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
end;

destructor TRivTimeListLink.Destroy;
begin
  FRiverStageData.Free;
  FConductanceData.Free;
  FRiverBottomData.Free;
  inherited;
end;

end.
