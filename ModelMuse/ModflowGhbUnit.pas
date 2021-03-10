unit ModflowGhbUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit;

type
  TGhbRecord = record
    Cell: TCellLocation;
    Conductance: double;
    BoundaryHead: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    BoundaryHeadAnnotation: string;
    TimeSeriesName: string;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    ConductanceParameterName: string;
    ConductanceParameterValue: double;
    ConductancePest: string;
    BoundaryHeadPest: string;
    ConductancePestSeriesName: string;
    BoundaryHeadPestSeriesName: string;
    ConductancePestSeriesMethod: TPestParamMethod;
    BoundaryHeadPestSeriesMethod: TPestParamMethod;
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

  // @name represents a MODFLOW General Head boundary for one time interval.
  // @name is stored by @link(TGhbCollection).
  TGhbItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(BoundaryHead).
    FBoundaryHead: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    // See @link(BoundaryHead).
    procedure SetBoundaryHead(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetBoundaryHead: string;
    function GetConductance: string;
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
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the boundary head
    // of this boundary.
    property BoundaryHead: string read GetBoundaryHead write SetBoundaryHead;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
  end;

  TGhbTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Boundary Heads for a series of
    // General Head Boundaries over a series of time intervals.
    FBoundaryHeadData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // General Head Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW General Head boundaries
  // for a series of time intervals.
  TGhbCollection = class(TCustomMF_ListBoundColl)
  private
    // @name is used to compute the Boundary Heads for a series of
    // General Head Boundaries over a series of time intervals.
//    FBoundaryHeadData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // General Head Boundaries over a series of time intervals.
//    FConductanceData: TModflowTimeList;
    procedure InvalidateHeadData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod); override;
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
    function GetTimeSeriesName: string;
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
    property Conductance: double read GetConductance;
    property BoundaryHead: double read GetBoundaryHead;
    property TimeSeriesName: string read GetTimeSeriesName;
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
    FPestHeadMethod: TPestParamMethod;
    FPestHeadFormula: TFormulaObject;
    FPestConductanceFormula: TFormulaObject;
    FUsedObserver: TObserver;
    FPestConductanceObserver: TObserver;
    FPestHeadObserver: TObserver;
//    FInterp: TMf6InterpolationMethods;
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
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property Interp;
    property PestHeadFormula: string read GetPestHeadFormula
      write SetPestHeadFormula
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
    property PestHeadMethod: TPestParamMethod read FPestHeadMethod
      write SetPestHeadMethod
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

resourcestring
  StrConductance = 'Conductance';
  StrConductanceMultipl = ' conductance multiplier';

const
  HeadPosition = 0;
  ConductancePosition = 1;

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
  end;
  inherited;
end;

procedure TGhbItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TGhbCollection;
  HeadObserver: TObserver;
  ConductanceObserver: TObserver;
begin
  ParentCollection := Collection as TGhbCollection;
  HeadObserver := FObserverList[HeadPosition];
  HeadObserver.OnUpToDateSet := ParentCollection.InvalidateHeadData;
  ConductanceObserver := FObserverList[ConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
end;

function TGhbItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

procedure TGhbItem.CreateFormulaObjects;
begin
  FBoundaryHead := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
end;

destructor TGhbItem.Destroy;
begin
  BoundaryHead := '0';
  Conductance := '0';
  inherited;
end;

function TGhbItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    HeadPosition: result := BoundaryHead;
    ConductancePosition: result := Conductance;
    else Assert(False);
  end;
end;

function TGhbItem.GetBoundaryHead: string;
begin
  Result := FBoundaryHead.Formula;
  ResetItemObserver(HeadPosition);
end;

function TGhbItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(ConductancePosition);
end;

function TGhbItem.GetConductanceIndex: Integer;
begin
  Result := ConductancePosition;
end;

procedure TGhbItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[ConductancePosition]);
  end;
  if Sender = FBoundaryHead then
  begin
    List.Add(FObserverList[HeadPosition]);
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
      and (Item.Conductance = Conductance);
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
begin
  inherited;
  case Index of
    HeadPosition: BoundaryHead := Value;
    ConductancePosition: Conductance := Value;
    else Assert(False);
  end;
end;

procedure TGhbItem.SetBoundaryHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, HeadPosition, FBoundaryHead);
end;

procedure TGhbItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConductancePosition, FConductance);
end;

{ TGhbCollection }

function TGhbCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
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
  if FormulaIndex = ConductancePosition then
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

procedure TGhbCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod);
var
  GhbStorage: TGhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex in [HeadPosition,ConductancePosition]);
  Assert(Expression <> nil);

  GhbStorage := BoundaryStorage as TGhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with GhbStorage.GhbArray[Index] do
    begin
      case BoundaryFunctionIndex of
        HeadPosition:
          begin
            BoundaryHead := Expression.DoubleResult;
            BoundaryHeadAnnotation := ACell.Annotation;
            BoundaryHeadPest := PestName;
            BoundaryHeadPestSeriesName := PestSeriesName;
            BoundaryHeadPestSeriesMethod := PestSeriesMethod;
          end;
        ConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
            ConductancePest := PestName;
            ConductancePestSeriesName := PestSeriesName;
            ConductancePestSeriesMethod := PestSeriesMethod;
          end;
        else
          Assert(False);
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
begin
  SetLength((Boundaries[ItemIndex, AModel] as TGhbStorage).FGhbArray, BoundaryCount);
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
      Link := TimeListLink.GetLink(ChildModel) as TGhbTimeListLink;
      Link.FConductanceData.Invalidate;
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
      Link := TimeListLink.GetLink(ChildModel) as TGhbTimeListLink;
      Link.FBoundaryHeadData.Invalidate;
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

function TGhb_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
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

function TGhb_Cell.GetMvrIndex: Integer;
begin
  result := FValues.MvrIndex;
end;

function TGhb_Cell.GetMvrUsed: Boolean;
begin
  result := FValues.MvrUsed;
end;

function TGhb_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  case Index of
    HeadPosition:
      begin
        result := BoundaryHeadAnnotation;
      end;
    ConductancePosition:
      begin
        result := ConductanceAnnotation;
      end;
    else Assert(False);
  end;
end;

function TGhb_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    HeadPosition:
      begin
        result := BoundaryHead;
      end;
    ConductancePosition:
      begin
        result := Conductance;
      end;
    else Assert(False);
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

function TGhb_Cell.GetTimeSeriesName: string;
begin
  result := FValues.TimeSeriesName;
end;

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

procedure TGhb_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TGhb_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
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
//  if Source is TGhbBoundary then
//  begin
//    SourceGhb := TGhbBoundary(Source);
//    Interp := SourceGhb.Interp;
//  end;
  if Source is TGhbBoundary then
  begin
    SourceGhb := TGhbBoundary(Source);
    PestHeadFormula := SourceGhb.PestHeadFormula;
    PestConductanceFormula := SourceGhb.PestConductanceFormula;
    PestHeadMethod := SourceGhb.PestHeadMethod;
    PestConductanceMethod := SourceGhb.PestConductanceMethod;
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
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.GhbArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.GhbArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.GhbArray[BoundaryIndex];
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
        Cell := TGhb_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := LocalScreenObject.IFace;
        Cells.Add(Cell);
        Cell.FStressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        Cell.ScreenObject := LocalScreenObject;
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
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestHeadFormula := '';
  PestConductanceFormula := '';
  FPestHeadMethod := DefaultBoundaryMethod(HeadPosition);
  FPestConductanceMethod := DefaultBoundaryMethod(ConductancePosition);
end;

procedure TGhbBoundary.CreateFormulaObjects;
begin
  FPestHeadFormula := CreateFormulaObjectBlocks(dso3D);
  FPestConductanceFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TGhbBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestHeadObserver);
    FObserverList.Add(PestConductanceObserver);
  end;
end;

class function TGhbBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    HeadPosition:
      begin
        result := ppmAdd;
      end;
    ConductancePosition:
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

destructor TGhbBoundary.Destroy;
begin
  PestHeadFormula := '';
  PestConductanceFormula := '';

  inherited;
end;

procedure TGhbBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
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
    if {(ValueIndex = Values.Count - 1) and} ObservationsPresent then
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
//      TimeSeries.SeriesCount := Length(BoundaryStorage.GhbArray);
//      TimeSeries.TimeCount := TimeCount;
//      TimeSeries.ParameterName := FCurrentParameter.ParameterName;
//      TimeSeries.ObjectName := (ScreenObject as TScreenObject).Name;
//      for SeriesIndex := 0 to Length(BoundaryStorage.GhbArray) - 1 do
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
//          for SeriesIndex := 0 to Length(BoundaryStorage.GhbArray) - 1 do
//          begin
//            if ItemIndex > 0 then
//            begin
//              TimeSeries.Values[SeriesIndex,TimeCount] := NoData;
//            end
//            else
//            begin
//              TimeSeries.Values[SeriesIndex,TimeCount] :=
//                BoundaryStorage.GhbArray[SeriesIndex].Conductance;
//            end;
//          end;
//          Inc(TimeCount);
//        end;
//        TimeSeries.Times[TimeCount] := BoundaryStorage.StartingTime - InitialTime;
//        for SeriesIndex := 0 to Length(BoundaryStorage.GhbArray) - 1 do
//        begin
//          TimeSeries.Values[SeriesIndex,TimeCount] :=
//            BoundaryStorage.GhbArray[SeriesIndex].Conductance;
//          BoundaryStorage.GhbArray[SeriesIndex].TimeSeriesName :=
//            TimeSeries.SeriesNames[SeriesIndex];
//        end;
//        StartTime := BoundaryStorage.EndingTime;
//        Inc(TimeCount);
//      end;
//      BoundaryStorage := BoundaryList.Last;
//      if BoundaryStorage.EndingTime <= EndTime then
//      begin
//        TimeSeries.Times[TimeCount] := EndTime - InitialTime;
//        for SeriesIndex := 0 to Length(BoundaryStorage.GhbArray) - 1 do
//        begin
//          TimeSeries.Values[SeriesIndex,TimeCount] :=
//            BoundaryStorage.GhbArray[SeriesIndex].Conductance;
//        end;
//      end;
//    end;


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
      if {(ValueIndex = Param.Param.Count - 1) and} ObservationsPresent then
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

function TGhbBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    HeadPosition:
      begin
        result := PestHeadFormula;
      end;
    ConductancePosition:
      begin
        result := PestConductanceFormula;
      end;
    else
      Assert(False);
  end;
end;

function TGhbBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := PestConductanceMethod;
  case FormulaIndex of
    HeadPosition:
      begin
        result := PestHeadMethod;
      end;
    ConductancePosition:
      begin
        result := PestConductanceMethod;
      end;
    else
      Assert(False);
  end;
end;

function TGhbBoundary.GetPestConductanceFormula: string;
begin
  Result := FPestConductanceFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ConductancePosition);
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
    ResetItemObserver(HeadPosition);
  end;

end;

function TGhbBoundary.GetPestHeadObserver: TObserver;
begin
  if FPestHeadObserver = nil then
  begin
    CreateObserver('PestConductance_', FPestHeadObserver, nil);
    FPestHeadObserver.OnUpToDateSet := InvalidateHeadData;
  end;
  result := FPestHeadObserver;
end;

procedure TGhbBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestHeadFormula then
  begin
    if HeadPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[HeadPosition]);
    end;
  end;
  if Sender = FPestConductanceFormula then
  begin
    if ConductancePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[ConductancePosition]);
    end;
  end;
end;

function TGhbBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestDRN_Used_', FUsedObserver, nil);
//    FUsedObserver.OnUpToDateSet := HandleChangedValue;
  end;
  result := FUsedObserver;
end;

procedure TGhbBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
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
    ChildModel.InvalidateMfGhbConductance(self);
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
    ChildModel.InvalidateMfGhbBoundaryHead(self);
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
begin
  case FormulaIndex of
    HeadPosition:
      begin
        PestHeadFormula := Value;
      end;
    ConductancePosition:
      begin
        PestConductanceFormula := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TGhbBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    HeadPosition:
      begin
        PestHeadMethod := Value;
      end;
    ConductancePosition:
      begin
        PestConductanceMethod := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TGhbBoundary.SetPestConductanceFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, ConductancePosition, FPestConductanceFormula);
end;

procedure TGhbBoundary.SetPestConductanceMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestConductanceMethod, Value);
end;

procedure TGhbBoundary.SetPestHeadFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, HeadPosition, FPestHeadFormula);
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
  WriteCompInt(Comp, Strings.IndexOf(TimeSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(ConductanceParameterName));
  WriteCompInt(Comp, Strings.IndexOf(ConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryHeadPest));
  WriteCompInt(Comp, Strings.IndexOf(ConductancePestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryHeadPestSeriesName));
  WriteCompInt(Comp, Ord(ConductancePestSeriesMethod));
  WriteCompInt(Comp, Ord(BoundaryHeadPestSeriesMethod));
  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
end;

procedure TGhbRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(BoundaryHeadAnnotation);
  Strings.Add(TimeSeriesName);
  Strings.Add(ConductanceParameterName);
  Strings.Add(ConductancePest);
  Strings.Add(BoundaryHeadPest);
  Strings.Add(ConductancePestSeriesName);
  Strings.Add(BoundaryHeadPestSeriesName);
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
  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductanceParameterName := Annotations[ReadCompInt(Decomp)];
  ConductancePest := Annotations[ReadCompInt(Decomp)];
  BoundaryHeadPest := Annotations[ReadCompInt(Decomp)];
  ConductancePestSeriesName := Annotations[ReadCompInt(Decomp)];
  BoundaryHeadPestSeriesName := Annotations[ReadCompInt(Decomp)];
  ConductancePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  BoundaryHeadPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
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
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
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
begin
  inherited;
  FBoundaryHeadData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBoundaryHeadData.NonParamDescription := StrBoundaryHead;
  FBoundaryHeadData.ParamDescription := ' ' + LowerCase(StrBoundaryHead);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfGhbConductance;
    FBoundaryHeadData.OnInvalidate := LocalModel.InvalidateMfGhbBoundaryHead;
  end;
  AddTimeList(FBoundaryHeadData);
  AddTimeList(FConductanceData);
end;

destructor TGhbTimeListLink.Destroy;
begin
  FBoundaryHeadData.Free;
  FConductanceData.Free;
  inherited;
end;

end.
