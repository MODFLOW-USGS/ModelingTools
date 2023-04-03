unit ModflowDrnUnit;

interface

uses ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, RbwParser, GoPhastTypes, ModflowTransientListParameterUnit;

type
  TDrnRecord = record
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
    Elevation: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    ElevationAnnotation: string;
//    TimeSeriesName: string;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    ConductanceParameterName: string;
    ConductanceParameterValue: double;
    ElevationPest: string;
    ConductancePest: string;
    ElevationPestSeries: string;
    ConductancePestSeries: string;
    ElevationPestSeriesMethod: TPestParamMethod;
    ConductancePestSeriesMethod: TPestParamMethod;
    ElevTimeSeriesName: string;
    ConductanceTimeSeriesName: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TDrnArray = array of TDrnRecord;

  TDrnStorage = class(TCustomBoundaryStorage)
  private
    FDrnArray: TDrnArray;
    function GetDrnArray: TDrnArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property DrnArray: TDrnArray read GetDrnArray;
  end;

  // @name represents a MODFLOW Drain boundary for one time interval.
  // @name is stored by @link(TDrnCollection).
  TDrnItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(Elevation).
    procedure SetElevation(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetElevation: string;
  protected
    // See @link(Elevation).
    FElevation: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
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
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the elevation
    // of this boundary.
    property Elevation: string read GetElevation write SetElevation;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
  end;

  TDrnTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to perform notifications of the Elevations for a series of
    // Drain Boundaries over a series of time intervals.
    FElevationData: TModflowTimeList;
    // @name is used to perform notifications of the Conductances
    // for a series of
    // Drain Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Drain boundaries
  // for a series of time intervals.
  TDrnCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateElevationData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    // the @link(TDrnStorage.DrnArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
      AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TDrnCollection).
  // @classname is stored by @link(TModflowParameters).
  TDrnParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TDrn_Cell = class(TValueCell)
  private
    Values: TDrnRecord;
//    StressPeriod: integer;
    function GetElevation: double;
    function GetConductance: double;
    function GetConductanceAnnotation: string;
    function GetElevationAnnotation: string;
//    function GetTimeSeriesName: string;
    function GetMvrUsed: Boolean;
    function GetMvrIndex: Integer;
    function GetConductanceParameterName: string;
    function GetConductanceParameterValue: double;
    function GetConductancePest: string;
    function GetElevationPest: string;
    function GetConductancePestSeries: string;
    function GetElevationPestSeries: string;
    function GetConductancePestSeriesMethod: TPestParamMethod;
    function GetElevationPestSeriesMethod: TPestParamMethod;
    function GetConductanceTimeSeriesName: string;
    function GetElevTimeSeriesName: string;
    procedure SetConductanceTimeSeriesName(const Value: string);
    procedure SetElevTimeSeriesName(const Value: string);
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
    property Elevation: double read GetElevation;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property ElevationAnnotation: string read GetElevationAnnotation;
//    property TimeSeriesName: string read GetTimeSeriesName;
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    // MODFLOW-2000 style parameter
    property ConductanceParameterName: string read GetConductanceParameterName;
    property ConductanceParameterValue: double read GetConductanceParameterValue;
    // PEST parameters
    property ElevationPest: string read GetElevationPest;
    property ConductancePest: string read GetConductancePest;
    property ElevationPestSeries: string read GetElevationPestSeries;
    property ConductancePestSeries: string read GetConductancePestSeries;
    property ElevationPestSeriesMethod: TPestParamMethod
      read GetElevationPestSeriesMethod;
    property ConductancePestSeriesMethod: TPestParamMethod
      read GetConductancePestSeriesMethod;
    property ElevTimeSeriesName: string read GetElevTimeSeriesName
      write SetElevTimeSeriesName;
    property ConductanceTimeSeriesName: string read GetConductanceTimeSeriesName
      write SetConductanceTimeSeriesName;
  end;

  // @name represents the MODFLOW Drain boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TDrnItem.Conductance
  // TDrnItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TDrnCollection)
  TDrnBoundary = class(TSpecificModflowBoundary)
  private
    FCurrentParameter: TModflowTransientListParameter;
    FPestElevFormula: TFormulaObject;
    FPestCondFormula: TFormulaObject;
    FUsedObserver: TObserver;
    FPestElevationObserver: TObserver;
    FPestConductanceObserver: TObserver;
    FPestConductanceMethod: TPestParamMethod;
    FPestElevMethod: TPestParamMethod;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
    function GetPestElevFormula: string;
    procedure SetPestElevFormula(const Value: string);
    function GetPestConductanceFormula: string;
    procedure SetPestConductanceFormula(const Value: string);
    function GetPestConductanceObserver: TObserver;
    function GetPestElevationObserver: TObserver;
    procedure SetPestConductanceMethod(const Value: TPestParamMethod);
    procedure SetPestElevMethod(const Value: TPestParamMethod);
    procedure InvalidateElevationData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TDrn_Cell)s for that stress period.
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
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    property PestElevationObserver: TObserver read GetPestElevationObserver;
    property PestConductanceObserver: TObserver read GetPestConductanceObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TDrnStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW DRN parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TDrnStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property Interp;
    property PestElevFormula: string read GetPestElevFormula
      write SetPestElevFormula;
    property PestElevMethod: TPestParamMethod read FPestElevMethod
      write SetPestElevMethod;
    property PestConductanceFormula: string read GetPestConductanceFormula
      write SetPestConductanceFormula;
    property PestConductanceMethod: TPestParamMethod
      read FPestConductanceMethod write SetPestConductanceMethod;
  end;

const
  DrnElevationPosition = 0;
  DrnConductancePosition = 1;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit,
  frmGoPhastUnit, GIS_Functions, ModflowMvrUnit;

{ TDrnItem }

procedure TDrnItem.Assign(Source: TPersistent);
var
  Drn: TDrnItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TDrnItem then
  begin
    Drn := TDrnItem(Source);
    Elevation := Drn.Elevation;
    Conductance := Drn.Conductance;
  end;
  inherited;
end;

procedure TDrnItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FElevation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TDrnItem.CreateFormulaObjects;
begin
  FElevation := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
end;

destructor TDrnItem.Destroy;
begin
  Elevation := '0';
  Conductance := '0';
  inherited;
end;

procedure TDrnItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TDrnCollection;
  ElevationObserver: TObserver;
  ConductanceObserver: TObserver;
begin
  ParentCollection := Collection as TDrnCollection;
  ElevationObserver := FObserverList[DrnElevationPosition];
  ElevationObserver.OnUpToDateSet := ParentCollection.InvalidateElevationData;
  ConductanceObserver := FObserverList[DrnConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
end;

procedure TDrnItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[DrnConductancePosition]);
  end;
  if Sender = FElevation then
  begin
    List.Add(FObserverList[DrnElevationPosition]);
  end;
end;

function TDrnItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

function TDrnItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    DrnElevationPosition: result := Elevation;
    DrnConductancePosition: result := Conductance;
    else Assert(False);
  end;
end;

function TDrnItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(DrnConductancePosition);
end;

function TDrnItem.GetConductanceIndex: Integer;
begin
  result := DrnConductancePosition;
end;

function TDrnItem.GetElevation: string;
begin
  Result := FElevation.Formula;
  ResetItemObserver(DrnElevationPosition);
end;

procedure TDrnItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfDrnConductance(self);
    PhastModel.InvalidateMfDrnElevation(self);
  end;
end;

function TDrnItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TDrnItem;
begin
  result := (AnotherItem is TDrnItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TDrnItem(AnotherItem);
    result := (Item.Elevation = Elevation)
      and (Item.Conductance = Conductance);
  end;
end;

procedure TDrnItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    DrnElevationPosition: Elevation := Value;
    DrnConductancePosition: Conductance := Value;
    else Assert(False);
  end;
end;

procedure TDrnItem.SetElevation(const Value: string);
begin
  UpdateFormulaBlocks(Value, DrnElevationPosition, FElevation);
end;

procedure TDrnItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, DrnConductancePosition, FConductance);
end;

{ TDrnCollection }

class function TDrnCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TDrnTimeListLink;
end;

procedure TDrnCollection.TestIfObservationsPresent(
  var EndOfLastStressPeriod: Double; var StartOfFirstStressPeriod: Double;
  var ObservationsPresent: Boolean);
var
  Boundary: TDrnBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TDrnBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

procedure TDrnCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TDrnStorage.Create(AModel));
end;

function TDrnCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TDrnBoundary;
  ScreenObject: TScreenObject;
  Item: TDrnItem;
begin
  Item := Items[ItemIndex] as TDrnItem;
  if FormulaIndex = DrnConductancePosition then
  begin
    Boundary := BoundaryGroup as TDrnBoundary;
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

procedure TDrnCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  DrnStorage: TDrnStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  BoundaryGroup.Mf6TimeSeriesNames.Add(TimeSeriesName);
  { DONE -cPEST : Handle PestSeriesName }
  Assert(BoundaryFunctionIndex in [DrnElevationPosition, DrnConductancePosition]);
  Assert(Expression <> nil);

  DrnStorage := BoundaryStorage as TDrnStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with DrnStorage.DrnArray[Index] do
    begin
      case BoundaryFunctionIndex of
        DrnElevationPosition:
          begin
            Elevation := Expression.DoubleResult;
            ElevationAnnotation := ACell.Annotation;
            ElevationPest := PestName;
            ElevationPestSeries := PestSeriesName;
            ElevationPestSeriesMethod := PestSeriesMethod;
            ElevTimeSeriesName := TimeSeriesName;
          end;
        DrnConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
            ConductancePest := PestName;
            ConductancePestSeries := PestSeriesName;
            ConductancePestSeriesMethod := PestSeriesMethod;
            ConductanceTimeSeriesName := TimeSeriesName;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TDrnCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  DrnStorage: TDrnStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  DrnStorage := BoundaryStorage as TDrnStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with DrnStorage.DrnArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TDrnCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TDrnStorage).FDrnArray, BoundaryCount);
  inherited;
end;

procedure TDrnCollection.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TDrnTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TDrnTimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TDrnTimeListLink;
        Link.FConductanceData.Invalidate;
      end;
    end;
  end;
end;

procedure TDrnCollection.InvalidateElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TDrnTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TDrnTimeListLink;
    Link.FElevationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TDrnTimeListLink;
        Link.FElevationData.Invalidate;
      end;
    end;
  end;
end;

class function TDrnCollection.ItemClass: TBoundaryItemClass;
begin
  result := TDrnItem;
end;

{ TDrnParamItem }

class function TDrnParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TDrnCollection;
end;

{ TDrn_Cell }

function TDrn_Cell.GetElevation: double;
begin
  result := Values.Elevation;
end;

function TDrn_Cell.GetElevationAnnotation: string;
begin
  result := Values.ElevationAnnotation;
end;

function TDrn_Cell.GetElevationPest: string;
begin
  result := Values.ElevationPest;
end;

function TDrn_Cell.GetElevationPestSeriesMethod: TPestParamMethod;
begin
  result := Values.ElevationPestSeriesMethod;
end;

function TDrn_Cell.GetElevTimeSeriesName: string;
begin
  result := Values.ElevTimeSeriesName;
end;

function TDrn_Cell.GetElevationPestSeries: string;
begin
  result := Values.ElevationPestSeries;
end;

function TDrn_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TDrn_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

procedure TDrn_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
//  WriteCompInt(Comp, StressPeriod);
end;

function TDrn_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TDrn_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TDrn_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TDrn_Cell.GetConductanceParameterName: string;
begin
  result := Values.ConductanceParameterName;
end;

function TDrn_Cell.GetConductanceParameterValue: double;
begin
  result := Values.ConductanceParameterValue;
end;

function TDrn_Cell.GetConductancePest: string;
begin
  result := Values.ConductancePest;
end;

function TDrn_Cell.GetConductancePestSeriesMethod: TPestParamMethod;
begin
  result := Values.ConductancePestSeriesMethod;
end;

function TDrn_Cell.GetConductanceTimeSeriesName: string;
begin
  result := Values.ConductanceTimeSeriesName;
end;

function TDrn_Cell.GetConductancePestSeries: string;
begin
  result := Values.ConductancePestSeries;
end;

function TDrn_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TDrn_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  case Index of
    DrnElevationPosition: result := ElevTimeSeriesName;
    DrnConductancePosition: result := ConductanceTimeSeriesName;
    else
      begin
        result := Inherited;
        Assert(False);
      end;
  end;
end;

function TDrn_Cell.GetMvrIndex: Integer;
begin
  result := Values.MvrIndex;
end;

function TDrn_Cell.GetMvrUsed: Boolean;
begin
  result := Values.MvrUsed;
end;

function TDrn_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    DrnElevationPosition: result := ElevationPest;
    DrnConductancePosition: result := ConductancePest;
    else
      begin
        result := Inherited;
        Assert(False);
      end;
  end;
end;

function TDrn_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    DrnElevationPosition: result := ElevationPestSeriesMethod;
    DrnConductancePosition: result := ConductancePestSeriesMethod;
    else
      begin
        result := Inherited;
        Assert(False);
      end;
  end;
end;

function TDrn_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    DrnElevationPosition: result := ElevationPestSeries;
    DrnConductancePosition: result := ConductancePestSeries;
    else
      begin
        result := Inherited;
        Assert(False);
      end;
  end;
end;

function TDrn_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    DrnElevationPosition: result := ElevationAnnotation;
    DrnConductancePosition: result := ConductanceAnnotation;
    else Assert(False);
  end;
end;

function TDrn_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    DrnElevationPosition: result := Elevation;
    DrnConductancePosition: result := Conductance;
    else Assert(False);
  end;
end;

function TDrn_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TDrn_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

//function TDrn_Cell.GetTimeSeriesName: string;
//begin
//  result := Values.TimeSeriesName;
//end;

function TDrn_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  DRN_Cell: TDrn_Cell;
begin
  result := AnotherCell is TDrn_Cell;
  if result then
  begin
    DRN_Cell := TDrn_Cell(AnotherCell);
    result :=
      (Conductance = DRN_Cell.Conductance)
      and (Elevation = DRN_Cell.Elevation)
      and (IFace = DRN_Cell.IFace)
      and (Values.Cell = DRN_Cell.Values.Cell);
  end;
end;

procedure TDrn_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TDrn_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
//  StressPeriod := ReadCompInt(Decomp);
end;

procedure TDrn_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TDrn_Cell.SetConductanceTimeSeriesName(const Value: string);
begin
  Values.ConductanceTimeSeriesName := Value;
end;

procedure TDrn_Cell.SetElevTimeSeriesName(const Value: string);
begin
  Values.ElevTimeSeriesName := Value;
end;

procedure TDrn_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TDrn_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
begin
  case Index of
    DrnElevationPosition:
      ElevTimeSeriesName := Value;
    DrnConductancePosition:
      ConductanceTimeSeriesName := Value;
    else
      begin
        Inherited;
        Assert(False);
      end;
  end;
end;

procedure TDrn_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TDrnBoundary }
procedure TDrnBoundary.Assign(Source: TPersistent);
var
  SourceDrn: TDrnBoundary;
begin
  if Source is TDrnBoundary then
  begin
    SourceDrn := TDrnBoundary(Source);
    PestElevFormula := SourceDrn.PestElevFormula;
    PestConductanceFormula := SourceDrn.PestConductanceFormula;
    PestElevMethod := SourceDrn.PestElevMethod;
    PestConductanceMethod := SourceDrn.PestConductanceMethod;
  end;
  inherited;
end;

procedure TDrnBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TDrn_Cell;
  BoundaryValues: TDrnRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TDrnStorage;
  LocalModel: TCustomModel;
  LocalScreenObject: TScreenObject;
  MvrUsed: Boolean;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TDrnStorage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcDrn);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TDrn_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.DrnArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.DrnArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.DrnArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.DrnArray[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed;
        BoundaryValues.MvrIndex := BoundaryIndex;
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.Conductance :=
            BoundaryValues.Conductance * FCurrentParameter.Value;
          BoundaryValues.ConductanceAnnotation := Format(Str0sMultipliedByT,
            [BoundaryValues.ConductanceAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.ConductanceParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.ConductanceParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.ConductanceParameterName := '';
          BoundaryValues.ConductanceParameterValue := 1;
        end;
        Cell := TDrn_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Cell.IFace := LocalScreenObject.IFace;
        Cells.Add(Cell);
//        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := LocalScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TDrnBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TDrnCollection;
end;

function TDrnBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestDrn_';
end;

constructor TDrnBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestElevFormula := '';
  PestConductanceFormula := '';
  FPestElevMethod := DefaultBoundaryMethod(DrnElevationPosition);
  FPestConductanceMethod := DefaultBoundaryMethod(DrnConductancePosition);
end;

procedure TDrnBoundary.CreateFormulaObjects;
begin
  FPestElevFormula := CreateFormulaObjectBlocks(dso3D);
  FPestCondFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TDrnBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestElevationObserver);
    FObserverList.Add(PestConductanceObserver);
  end;
end;

class function TDrnBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    DrnElevationPosition:
      begin
        result := ppmAdd;
      end;
    DrnConductancePosition:
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

destructor TDrnBoundary.Destroy;
begin
  PestElevFormula := '';
  PestConductanceFormula := '';

  inherited;
end;

procedure TDrnBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TDrnStorage;
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
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TDrnStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TDrnStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
    if ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TDrnStorage;
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
      FCurrentParameter := LocalModel.ModflowTransientParameters.
        GetParamByName(ParamName);
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
            BoundaryStorage := Param.Param.
              Boundaries[ValueCount, AModel] as TDrnStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param
          .Boundaries[ValueCount, AModel] as TDrnStorage;
        AssignCells(BoundaryStorage, Times, AModel);
        Inc(ValueCount);
      end;
      if ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.
              Boundaries[ValueCount, AModel] as TDrnStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

function TDrnBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    DrnElevationPosition:
      begin
        result := PestElevFormula;
      end;
    DrnConductancePosition:
      begin
        result := PestConductanceFormula;
      end;
    else
      Assert(False);
  end;
end;

function TDrnBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    DrnElevationPosition:
      begin
        result := PestElevMethod;
      end;
    DrnConductancePosition:
      begin
        result := PestConductanceMethod;
      end;
    else
      result := PestConductanceMethod;
      Assert(False);
  end;
end;

function TDrnBoundary.GetPestConductanceFormula: string;
begin
  Result := FPestCondFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DrnConductancePosition);
  end;
end;

function TDrnBoundary.GetPestConductanceObserver: TObserver;
begin
  if FPestConductanceObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestConductanceObserver, nil);
    FPestConductanceObserver.OnUpToDateSet := InvalidateConductanceData;
  end;
  result := FPestConductanceObserver;
end;

function TDrnBoundary.GetPestElevationObserver: TObserver;
begin
  if FPestElevationObserver = nil then
  begin
    CreateObserver('PestElevation_', FPestElevationObserver, nil);
    FPestElevationObserver.OnUpToDateSet := InvalidateElevationData;
  end;
  result := FPestElevationObserver;
end;

function TDrnBoundary.GetPestElevFormula: string;
begin
  Result := FPestElevFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(DrnElevationPosition);
  end;
end;

procedure TDrnBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestElevFormula then
  begin
    if DrnElevationPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DrnElevationPosition]);
    end;
  end;
  if Sender = FPestCondFormula then
  begin
    if DrnConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[DrnConductancePosition]);
    end;
  end;
end;

function TDrnBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestDRN_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TDrnBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TDrnBoundary.InvalidateConductanceData(Sender: TObject);
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
    PhastModel.InvalidateMfDrnConductance(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfDrnConductance(self);
      end;
    end;
  end;
end;

procedure TDrnBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfDrnConductance(self);
    Model.InvalidateMfDrnElevation(self);
  end;
end;

procedure TDrnBoundary.InvalidateElevationData(Sender: TObject);
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
    PhastModel.InvalidateMfDrnElevation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfDrnElevation(self);
      end;
    end;
  end;
end;

class function TDrnBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TDrnParamItem;
end;

function TDrnBoundary.ParameterType: TParameterType;
begin
  result := ptDRN;
end;

procedure TDrnBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    DrnElevationPosition:
      begin
        PestElevFormula := Value;
      end;
    DrnConductancePosition:
      begin
        PestConductanceFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TDrnBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    DrnElevationPosition:
      begin
        PestElevMethod := Value;
      end;
    DrnConductancePosition:
      begin
        PestConductanceMethod := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TDrnBoundary.SetPestConductanceFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, DrnConductancePosition, FPestCondFormula);
end;

procedure TDrnBoundary.SetPestConductanceMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestConductanceMethod, Value);
end;

procedure TDrnBoundary.SetPestElevFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, DrnElevationPosition, FPestElevFormula);
end;

procedure TDrnBoundary.SetPestElevMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestElevMethod, Value);
end;

procedure TDrnBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  LocalScreenObject: TScreenObject;
  LocalPhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalPhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(LocalPhastModel <> nil);
  ObservationsPresent := LocalPhastModel.DrobIsSelected
    and (LocalPhastModel.DrainObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

{ TDrnRecord }

procedure TDrnRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, Elevation);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, ConductanceParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ElevationAnnotation));
//  WriteCompInt(Comp, Strings.IndexOf(TimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ConductanceParameterName));
  WriteCompInt(Comp, Strings.IndexOf(ElevationPest));
  WriteCompInt(Comp, Strings.IndexOf(ConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(ElevationPestSeries));
  WriteCompInt(Comp, Strings.IndexOf(ConductancePestSeries));
  WriteCompInt(Comp, Strings.IndexOf(ElevTimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ConductanceTimeSeriesName));
  WriteCompInt(Comp, Ord(ElevationPestSeriesMethod));
  WriteCompInt(Comp, Ord(ConductancePestSeriesMethod));
  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TDrnRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(ElevationAnnotation);
//  Strings.Add(TimeSeriesName);
  Strings.Add(ConductanceParameterName);
  Strings.Add(ElevationPest);
  Strings.Add(ConductancePest);
  Strings.Add(ElevationPestSeries);
  Strings.Add(ConductancePestSeries);
  Strings.Add(ElevTimeSeriesName);
  Strings.Add(ConductanceTimeSeriesName);
end;

procedure TDrnRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  Elevation := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceParameterValue := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  ElevationAnnotation := Annotations[ReadCompInt(Decomp)];
//  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductanceParameterName := Annotations[ReadCompInt(Decomp)];
  ElevationPest := Annotations[ReadCompInt(Decomp)];
  ConductancePest := Annotations[ReadCompInt(Decomp)];
  ElevationPestSeries := Annotations[ReadCompInt(Decomp)];
  ConductancePestSeries := Annotations[ReadCompInt(Decomp)];
  ElevTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductanceTimeSeriesName := Annotations[ReadCompInt(Decomp)];

  ElevationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  ConductancePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);
end;

{ TDrnStorage }

procedure TDrnStorage.Clear;
begin
  SetLength(FDrnArray, 0);
  FCleared := True;
end;

procedure TDrnStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FDrnArray);
    for Index := 0 to Count - 1 do
    begin
      FDrnArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FDrnArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TDrnStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FDrnArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FDrnArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TDrnStorage.GetDrnArray: TDrnArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FDrnArray;
end;

{ TDrnimeListLink }

procedure TDrnTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FElevationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FElevationData.NonParamDescription := StrElevation;
  FElevationData.ParamDescription := ' ' + LowerCase(StrElevation);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FElevationData.OnInvalidate := LocalModel.InvalidateMfDrnElevation;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfDrnConductance;
  end;
  AddTimeList(FElevationData);
  AddTimeList(FConductanceData);
end;

destructor TDrnTimeListLink.Destroy;
begin
  FElevationData.Free;
  FConductanceData.Free;
  inherited;
end;

end.
