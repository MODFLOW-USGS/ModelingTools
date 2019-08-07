unit ModflowDrtUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, GoPhastTypes,
  ModflowDrnUnit, ModflowGridUnit, FormulaManagerUnit, SubscriptionUnit,
  SparseDataSets, RbwParser;

type
  TReturnChoice = (rtNone, rtObject, rtLocation, rtCell);

  TDrtRecord = record
    Cell: TCellLocation;
    Conductance: double;
    Elevation: double;
    ReturnFraction: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    ElevationAnnotation: string;
    ReturnFractionAnnotation: string;
    ReturnCell: TCellLocation;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TDrtArray = array of TDrtRecord;

  TDrtStorage = class(TCustomBoundaryStorage)
  private
    FDrtArray: TDrtArray;
    function GetDrtArray: TDrtArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property DrtArray: TDrtArray read GetDrtArray;
  end;

  // @name represents a MODFLOW Drain boundary for one time interval.
  // @name is stored by @link(TDrnCollection).
  TDrtItem = class(TDrnItem)
  private
    FReturnFraction: TFormulaObject;
    procedure SetReturnFraction(const Value: string);
    function GetReturnFraction: string;
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
    function GetConductanceIndex: Integer; override;
  public
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the fraction of extracted flow
    // returned to the model
    property ReturnFraction: string read GetReturnFraction write SetReturnFraction;
  end;

  TDrtTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Elevations for a series of
    // Drain Return Boundaries over a series of time intervals.
    FElevationData: TModflowTimeList;
    // @name is used to compute the Conductances for a series of
    // Drain Return Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    // @name is used to compute the return fractions for a series of
    // Drain Return Boundaries over a series of time intervals.
    FReturnFractionData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Drain boundaries
  // for a series of time intervals.
  TDrtCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateElevationData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateReturnData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TDrtStorage.DrtArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  end;

  // Each @name stores a @link(TDrtCollection).
  // @classname is stored by @link(TModflowParameters).
  TDrtParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TDrt_Cell = class(TValueCell)
  private
    Values: TDrtRecord;
    StressPeriod: integer;
    function GetElevation: double;
    function GetConductance: double;
    function GetReturnFraction: double;
    function GetReturnCell: TCellLocation;
    function GetConductanceAnnotation: string;
    function GetElevationAnnotation: string;
    function GetReturnFractionAnnotation: string;
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
    property Conductance: double read GetConductance;
    property Elevation: double read GetElevation;
    property ReturnFraction: double read GetReturnFraction;
    property ReturnCell: TCellLocation read GetReturnCell;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property ElevationAnnotation: string read GetElevationAnnotation;
    property ReturnFractionAnnotation: string read GetReturnFractionAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;


  TReturnCell = Class(TGoPhastPersistent)
  private
    FLay: integer;
    FCol: integer;
    FRow: integer;
    procedure SetCol(const Value: integer);
    procedure SetLay(const Value: integer);
    procedure SetRow(const Value: integer);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(OtherReturnCell: TReturnCell): boolean;
  published
    property Col: integer read FCol write SetCol;
    property Row: integer read FRow write SetRow;
    property Lay: integer read FLay write SetLay;
  End;

  TReturnLocation = Class(TGoPhastPersistent)
  private
    FZ: real;
    FX: real;
    FY: real;
    procedure SetX(const Value: real);
    procedure SetY(const Value: real);
    procedure SetZ(const Value: real);
  public
    procedure Assign(Source: TPersistent); override;
    function IsSame(OtherReturnLocation: TReturnLocation): boolean;
  published
    property X: real read FX write SetX;
    property Y: real read FY write SetY;
    property Z: real read FZ write SetZ;
  End;

  TReturnObject = class(TGoPhastPersistent)
  private
    FObjectName: string;
    FScreenObject: TObject;
    procedure SetObjectName(const Value: string);
    function GetObjectName: string;
    procedure SetScreenObject(const Value: TObject);
    function GetScreenObject: TObject;
    function ValidScreenObject(AScreenObject: TObject): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TObject read GetScreenObject
      write SetScreenObject;
    function IsSame(OtherReturnObject: TReturnObject): boolean;
  published
    property ObjectName: string read GetObjectName write SetObjectName;
  end;

  TDrainReturn = class(TGoPhastPersistent)
  private
    FReturnObject: TReturnObject;
    FReturnCell: TReturnCell;
    FReturnLocation: TReturnLocation;
    FReturnChoice: TReturnChoice;
    procedure SetReturnCell(const Value: TReturnCell);
    procedure SetReturnChoice(const Value: TReturnChoice);
    procedure SetReturnLocation(const Value: TReturnLocation);
    procedure SetReturnObject(const Value: TReturnObject);
    function StoreReturnCell: boolean;
    function StoreReturnLocation: boolean;
    function StoreReturnObject: boolean;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    Destructor Destroy; override;
    // @name returns the cell where the drain returns extracted water
    // The cell numbers in the @link(TCellLocation) will be 1 based.
    // If the @link(TCellLocation.Layer TCellLocation.Layer) = 0
    // no water is returned to the model.
    function ReturnCellLocation(AModel: TBaseModel): TCellLocation;
    function IsSame(OtherDrainReturn: TDrainReturn): boolean;
  published
    property ReturnChoice: TReturnChoice read FReturnChoice
      write SetReturnChoice;
    property ReturnCell: TReturnCell read FReturnCell
      write SetReturnCell stored StoreReturnCell;
    property ReturnLocation: TReturnLocation read FReturnLocation
      write SetReturnLocation stored StoreReturnLocation;
    property ReturnObject: TReturnObject read FReturnObject
      write SetReturnObject stored StoreReturnObject;
  end;

  // @name represents the MODFLOW Drain boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TDrnItem.Conductance
  // TDrtItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TDrtCollection)
  TDrtBoundary = class(TSpecificModflowBoundary)
  private
    FDrainReturn: TDrainReturn;
    procedure SetDrainReturn(const Value: TDrainReturn);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TDrt_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    // See @link(TModflowParamBoundary.ModflowParamItemClass
    // TModflowParamBoundary.ModflowParamItemClass).
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;
  public
    // @name copies @link(Values) and @link(Parameters) from the Source
    // @classname to this @classname.
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TDrtStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW DRT parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TDrtStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    Destructor Destroy; override;
    procedure InvalidateDisplay; override;
  published
    property DrainReturn: TDrainReturn read FDrainReturn write SetDrainReturn;
  end;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, FastGEO, GIS_Functions,
  TempFiles, frmGoPhastUnit;

resourcestring
  StrReturnFraction = 'Return fraction';

const
  ElevationPosition = 0;
  ConductancePosition = 1;
  ReturnPosition = 2;

{ TDrtItem }

procedure TDrtItem.Assign(Source: TPersistent);
var
  Drt: TDrtItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TDrtItem then
  begin
    Drt := TDrtItem(Source);
    ReturnFraction := Drt.ReturnFraction;
  end;
  inherited;
end;

procedure TDrtItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TDrtCollection;
  ElevationObserver: TObserver;
  ConductanceObserver: TObserver;
  ReturnObserver: TObserver;
begin
  ParentCollection := Collection as TDrtCollection;
  ElevationObserver := FObserverList[ElevationPosition];
  ElevationObserver.OnUpToDateSet := ParentCollection.InvalidateElevationData;
  ConductanceObserver := FObserverList[ConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
  ReturnObserver := FObserverList[ReturnPosition];
  ReturnObserver.OnUpToDateSet := ParentCollection.InvalidateReturnData;
end;

function TDrtItem.BoundaryFormulaCount: integer;
begin
  result := 3;
end;

procedure TDrtItem.CreateFormulaObjects;
begin
  inherited;
  FReturnFraction := CreateFormulaObject(dso3D);
end;

destructor TDrtItem.Destroy;
begin
  ReturnFraction := '0';
  inherited;
end;

function TDrtItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    ElevationPosition: result := Elevation;
    ConductancePosition: result := Conductance;
    ReturnPosition: result := ReturnFraction;
    else Assert(False);
  end;
end;

function TDrtItem.GetConductanceIndex: Integer;
begin
  Result := ConductancePosition;
end;

procedure TDrtItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[ConductancePosition]);
  end;
  if Sender = FElevation then
  begin
    List.Add(FObserverList[ElevationPosition]);
  end;
  if Sender = FReturnFraction then
  begin
    List.Add(FObserverList[ReturnPosition]);
  end;
end;

function TDrtItem.GetReturnFraction: string;
begin
  Result := FReturnFraction.Formula;
  ResetItemObserver(ReturnPosition);
end;

procedure TDrtItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfDrtConductance(self);
    PhastModel.InvalidateMfDrtElevation(self);
    PhastModel.InvalidateMfDrtReturnFraction(self);
  end;
end;

function TDrtItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TDrtItem;
begin
  result := (AnotherItem is TDrtItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TDrtItem(AnotherItem);
    result := (Item.ReturnFraction = ReturnFraction);
  end;
end;

procedure TDrtItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FReturnFraction,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  inherited;
end;

procedure TDrtItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    ElevationPosition: Elevation := Value;
    ConductancePosition: Conductance := Value;
    ReturnPosition: ReturnFraction := Value;
    else Assert(False);
  end;
end;

procedure TDrtItem.SetReturnFraction(const Value: string);
begin
  UpdateFormula(Value, ReturnPosition, FReturnFraction);
end;

{ TDrtCollection }

function TDrtCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TDrtTimeListLink;
end;

procedure TDrtCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TDrtStorage.Create(AModel));
end;

function TDrtCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TDrtBoundary;
  ScreenObject: TScreenObject;
  Item: TDrtItem;
begin
  Item := Items[ItemIndex] as TDrtItem;
  if FormulaIndex = ConductancePosition then
  begin
    Boundary := BoundaryGroup as TDrtBoundary;
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

procedure TDrtCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject);
var
  DrtStorage: TDrtStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  DrtBoundaryGroup: TDrtBoundary;
  ReturnLocation: TCellLocation;
begin
  Assert(BoundaryFunctionIndex in [ElevationPosition, ConductancePosition,
    ReturnPosition]);
  Assert(Expression <> nil);

  DrtBoundaryGroup := BoundaryGroup as TDrtBoundary;
  ReturnLocation := DrtBoundaryGroup.DrainReturn.ReturnCellLocation(AModel);

  DrtStorage := BoundaryStorage as TDrtStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);

    Expression.Evaluate;
    with DrtStorage.DrtArray[Index] do
    begin
      case BoundaryFunctionIndex of
        ElevationPosition:
          begin
            Elevation := Expression.DoubleResult;
            ElevationAnnotation := ACell.Annotation;
          end;
        ConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
            ReturnCell := ReturnLocation;
          end;
        ReturnPosition:
          begin
            ReturnFraction := Expression.DoubleResult;
            ReturnFractionAnnotation := ACell.Annotation;
          end
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TDrtCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  DrtStorage: TDrtStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  DrtStorage := BoundaryStorage as TDrtStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with DrtStorage.DrtArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TDrtCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TDrtStorage).FDrtArray, BoundaryCount);
  inherited;
end;

procedure TDrtCollection.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TDrtTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TDrtTimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TDrtTimeListLink;
      Link.FConductanceData.Invalidate;
    end;
  end;
end;

procedure TDrtCollection.InvalidateElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TDrtTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TDrtTimeListLink;
    Link.FElevationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TDrtTimeListLink;
      Link.FElevationData.Invalidate;
    end;
  end;
end;

procedure TDrtCollection.InvalidateReturnData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TDrtTimeListLink;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  if not (Sender as TObserver).UpToDate then
  begin
    PhastModel := frmGoPhast.PhastModel;
    Link := TimeListLink.GetLink(PhastModel) as TDrtTimeListLink;
    Link.FReturnFractionData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TDrtTimeListLink;
      Link.FReturnFractionData.Invalidate;
    end;
  end;
end;

class function TDrtCollection.ItemClass: TBoundaryItemClass;
begin
  result := TDrtItem;
end;

{ TDrtParamItem }

class function TDrtParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TDrtCollection;
end;

{ TDrt_Cell }

function TDrt_Cell.GetElevation: double;
begin
  result := Values.Elevation;
end;

function TDrt_Cell.GetElevationAnnotation: string;
begin
  result := Values.ElevationAnnotation;
end;

function TDrt_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TDrt_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

procedure TDrt_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TDrt_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TDrt_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TDrt_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TDrt_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TDrt_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    ElevationPosition: result := ElevationAnnotation;
    ConductancePosition: result := ConductanceAnnotation;
    ReturnPosition: result := ReturnFractionAnnotation;
    else Assert(False);
  end;
end;

function TDrt_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    ElevationPosition: result := Elevation;
    ConductancePosition: result := Conductance;
    ReturnPosition: result := ReturnFraction;
    else Assert(False);
  end;
end;

function TDrt_Cell.GetReturnCell: TCellLocation;
begin
  result := Values.ReturnCell;
end;

function TDrt_Cell.GetReturnFraction: double;
begin
  result := Values.ReturnFraction;
end;

function TDrt_Cell.GetReturnFractionAnnotation: string;
begin
  result := Values.ReturnFractionAnnotation;
end;

function TDrt_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TDrt_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TDrt_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  DRT_Cell: TDrt_Cell;
begin
  result := AnotherCell is TDrt_Cell;
  if result then
  begin
    DRT_Cell := TDrt_Cell(AnotherCell);
    result :=
      (Conductance = DRT_Cell.Conductance)
      and (Elevation = DRT_Cell.Elevation)
      and (ReturnFraction = DRT_Cell.ReturnFraction)
      and (ReturnCell = DRT_Cell.ReturnCell)
      and (IFace = DRT_Cell.IFace)
      and (Values.Cell = DRT_Cell.Values.Cell);
  end;
end;

procedure TDrt_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TDrt_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TDrt_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TDrt_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TDrt_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TDrtBoundary }

procedure TDrtBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TDrt_Cell;
  BoundaryValues: TDrtRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TDrtStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TDrtStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TDrt_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.DrtArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.DrtArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.DrtArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.DrtArray[BoundaryIndex];
        Cell := TDrt_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
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

class function TDrtBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TDrtCollection;
end;

procedure TDrtBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TDrtStorage;
  ParamIndex: Integer;
  Param: TModflowParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
begin
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TDrtStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    ParamName := Param.Param.ParamName;
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
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TDrtStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

procedure TDrtBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfDrtConductance(self);
    Model.InvalidateMfDrtElevation(self);
    Model.InvalidateMfDrtReturnFraction(self);
  end;
end;

class function TDrtBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TDrtParamItem;
end;

function TDrtBoundary.ParameterType: TParameterType;
begin
  result := ptDRT
end;

procedure TDrtBoundary.Assign(Source: TPersistent);
begin
  if Source is TDrtBoundary then
  begin
    DrainReturn :=
      TDrtBoundary(Source).DrainReturn;
  end;
  inherited;
end;

constructor TDrtBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  OnInvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    OnInvalidateModelEvent := nil;
  end
  else
  begin
    OnInvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(Model, ScreenObject);
  FDrainReturn := TDrainReturn.Create(OnInvalidateModelEvent);
end;

destructor TDrtBoundary.Destroy;
begin
  FDrainReturn.Free;
  inherited;
end;

procedure TDrtBoundary.SetDrainReturn(const Value: TDrainReturn);
begin
  FDrainReturn.Assign(Value);
end;

{ TReturnCell }

procedure TReturnCell.Assign(Source: TPersistent);
var
  RC: TReturnCell;
begin
  if Source is TReturnCell then
  begin
    RC := TReturnCell(Source);
    Col := RC.Col;
    Row := RC.Row;
    Lay := RC.Lay;
  end
  else
  begin
    inherited;
  end;
end;

function TReturnCell.IsSame(OtherReturnCell: TReturnCell): boolean;
begin
  result := (Col = OtherReturnCell.Col)
    and (Row = OtherReturnCell.Row)
    and (Lay = OtherReturnCell.Lay);
end;

procedure TReturnCell.SetCol(const Value: integer);
begin
  if FCol <> Value then
  begin
    FCol := Value;
    InvalidateModel;
  end;
end;

procedure TReturnCell.SetLay(const Value: integer);
begin
  if FLay <> Value then
  begin
    FLay := Value;
    InvalidateModel;
  end;
end;

procedure TReturnCell.SetRow(const Value: integer);
begin
  if FRow <> Value then
  begin
    FRow := Value;
    InvalidateModel;
  end;
end;

{ TReturnLocation }

procedure TReturnLocation.Assign(Source: TPersistent);
var
  RL: TReturnLocation;
begin
  if Source is TReturnLocation then
  begin
    RL := TReturnLocation(Source);
    X := RL.X;
    Y := RL.Y;
    Z := RL.Z;
  end
  else
  begin
    inherited;
  end;
end;

function TReturnLocation.IsSame(OtherReturnLocation: TReturnLocation): boolean;
begin
  result := (X = OtherReturnLocation.X)
    and (Y = OtherReturnLocation.Y)
    and (Z = OtherReturnLocation.Z);
end;

procedure TReturnLocation.SetX(const Value: real);
begin
  if FX <> Value then
  begin
    FX := Value;
    InvalidateModel;
  end;
end;

procedure TReturnLocation.SetY(const Value: real);
begin
  if FY <> Value then
  begin
    FY := Value;
    InvalidateModel;
  end;
end;

procedure TReturnLocation.SetZ(const Value: real);
begin
  if FZ <> Value then
  begin
    FZ := Value;
    InvalidateModel;
  end;
end;

{ TReturnObject }

procedure TReturnObject.Assign(Source: TPersistent);
var
  RO: TReturnObject;
begin
  if Source is TReturnObject then
  begin
    RO := TReturnObject(Source);
    ScreenObject := RO.ScreenObject;
    ObjectName := RO.ObjectName;
  end
  else
  begin
    inherited;
  end;
end;

function TReturnObject.GetObjectName: string;
var
  ScreenObject: TScreenObject;
begin
  if FScreenObject = nil then
  begin
    result := FObjectName;
  end
  else
  begin
    if ValidScreenObject(FScreenObject) then
    begin
      ScreenObject := FScreenObject as TScreenObject;
      result := ScreenObject.Name;
    end
    else
    begin
      result := '';
    end;
  end;
end;

function TReturnObject.ValidScreenObject(AScreenObject: TObject): boolean;
var
  ScreenObject: TScreenObject;
begin
  result := (AScreenObject <> nil);
  if result then
  begin
    ScreenObject := AScreenObject as TScreenObject;
    result :=  (ScreenObject.Count= 1)
      and (ScreenObject.ElevationCount = ecOne)
      and not ScreenObject.Deleted;
  end;
end;

function TReturnObject.GetScreenObject: TObject;
begin
  if ValidScreenObject(FScreenObject) then
  begin
    result := FScreenObject;
  end
  else
  begin
    result := nil;
  end;
end;

function TReturnObject.IsSame(OtherReturnObject: TReturnObject): boolean;
begin
  result := ObjectName = OtherReturnObject.ObjectName;
end;

procedure TReturnObject.SetObjectName(const Value: string);
begin
  if FObjectName <> Value then
  begin
    FObjectName := Value;
    InvalidateModel;
  end;
end;

procedure TReturnObject.SetScreenObject(const Value: TObject);
begin
  Assert((Value = nil) or (Value is TScreenObject));
  FScreenObject := Value;
  if FScreenObject = nil then
  begin
    ObjectName := '';
  end
  else
  begin
    ObjectName := TScreenObject(Value).Name;
  end;
end;

{ TDrainReturn }

procedure TDrainReturn.Assign(Source: TPersistent);
var
  DR: TDrainReturn;
begin
  if Source is TDrainReturn then
  begin
    DR := TDrainReturn(Source);
    ReturnChoice := DR.ReturnChoice;
    ReturnCell := DR.ReturnCell;
    ReturnLocation := DR.ReturnLocation;
    ReturnObject := DR.ReturnObject;
  end
  else
  begin
    inherited;
  end;
end;

procedure TDrainReturn.SetReturnCell(const Value: TReturnCell);
begin
  FReturnCell.Assign(Value);
end;

procedure TDrainReturn.SetReturnChoice(const Value: TReturnChoice);
begin
  if FReturnChoice <> Value then
  begin
    FReturnChoice := Value;
    InvalidateModel;
  end;
end;

procedure TDrainReturn.SetReturnLocation(const Value: TReturnLocation);
begin
  FReturnLocation.Assign(Value);
end;

procedure TDrainReturn.SetReturnObject(const Value: TReturnObject);
begin
  FReturnObject.Assign(Value);
end;

function TDrainReturn.StoreReturnCell: boolean;
begin
  result := ReturnChoice = rtCell;
end;

function TDrainReturn.StoreReturnLocation: boolean;
begin
  result := ReturnChoice = rtLocation;
end;

function TDrainReturn.StoreReturnObject: boolean;
begin
  result := ReturnChoice = rtObject;
end;

constructor TDrainReturn.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FReturnCell := TReturnCell.Create(InvalidateModelEvent);
  FReturnLocation := TReturnLocation.Create(InvalidateModelEvent);
  FReturnObject := TReturnObject.Create(InvalidateModelEvent);
end;

destructor TDrainReturn.Destroy;
begin
  FReturnCell.Free;
  FReturnLocation.Free;
  FReturnObject.Free;
  inherited;
end;

function TDrainReturn.IsSame(OtherDrainReturn: TDrainReturn): boolean;
begin
  result := ReturnChoice = OtherDrainReturn.ReturnChoice;
  if Result then
  begin
    case ReturnChoice of
      rtNone: ;
      rtObject: result := ReturnObject.IsSame(OtherDrainReturn.ReturnObject);
      rtLocation: result := ReturnLocation.IsSame(OtherDrainReturn.ReturnLocation);
      rtCell: result := ReturnCell.IsSame(OtherDrainReturn.ReturnCell);
      else Assert(False);
    end;
  end;
end;

function TDrainReturn.ReturnCellLocation(AModel: TBaseModel): TCellLocation;
var
  ScreenObject: TScreenObject;
  X, Y, Z: double;
  Model: TCustomModel;
  Grid: TModflowGrid;
begin
  case ReturnChoice of
    rtNone:
      begin
        result.Layer := 0;
        result.Row := 1;
        result.Column := 1;
      end;
    rtObject:
      begin
        ScreenObject := ReturnObject.ScreenObject as TScreenObject;
        if ScreenObject = nil then
        begin
          result.Layer := 0;
          result.Row := 1;
          result.Column := 1;
        end
        else
        begin
          result := ScreenObject.SingleCellLocation(AModel);
        end;
      end;
    rtLocation:
      begin
        Model := AModel as TCustomModel;
        Grid := Model.ModflowGrid;
        X := ReturnLocation.X;
        Y := ReturnLocation.Y;
        Z := ReturnLocation.Z;
        if (X < Grid.ColumnPosition[Grid.ColumnCount]) and
          (Y > Grid.RowPosition[Grid.RowCount]) then
        begin
          result.Column := Grid.GetContainingColumn(X);
          result.Row := Grid.GetContainingRow(Y);
          if (result.Column >= 0) and (result.Row >= 0) then
          begin
            GetLayerFromZ(Z, Result, Grid, Model);
          end
          else
          begin
            result.Layer := 0;
            result.Row := 1;
            result.Column := 1;
          end;
        end
        else
        begin
          result.Layer := 0;
          result.Row := 1;
          result.Column := 1;
        end;
      end;
    rtCell:
      begin
        result.Layer := ReturnCell.Lay;
        result.Row := ReturnCell.Row;
        result.Column := ReturnCell.Col;
      end;
    else Assert(False);
  end;
end;

{ TDrtRecord }

procedure TDrtRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, Elevation);
  WriteCompReal(Comp, ReturnFraction);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ReturnFractionAnnotation));
//  WriteCompString(Comp, ConductanceAnnotation);
//  WriteCompString(Comp, ElevationAnnotation);
//  WriteCompString(Comp, ReturnFractionAnnotation);
  WriteCompCell(Comp, ReturnCell);
end;

procedure TDrtRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(ElevationAnnotation);
  Strings.Add(ReturnFractionAnnotation);
end;

procedure TDrtRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  Elevation := ReadCompReal(Decomp);
  ReturnFraction := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  ElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  ReturnFractionAnnotation := Annotations[ReadCompInt(Decomp)];
//  ConductanceAnnotation := ReadCompString(Decomp, Annotations);
//  ElevationAnnotation := ReadCompString(Decomp, Annotations);
//  ReturnFractionAnnotation := ReadCompString(Decomp, Annotations);
  ReturnCell := ReadCompCell(Decomp);
end;

{ TDrtStorage }

procedure TDrtStorage.Clear;
begin
  SetLength(FDrtArray, 0);
  FCleared := True;
end;

procedure TDrtStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FDrtArray);
    for Index := 0 to Count - 1 do
    begin
      FDrtArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FDrtArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TDrtStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FDrtArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FDrtArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TDrtStorage.GetDrtArray: TDrtArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FDrtArray;
end;

{ TDrtTimeListLink }

procedure TDrtTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FElevationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReturnFractionData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FElevationData.NonParamDescription := StrElevation;
  FElevationData.ParamDescription := ' ' + LowerCase(StrElevation);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;
  FReturnFractionData.NonParamDescription := StrReturnFraction;
  FReturnFractionData.ParamDescription := ' ' + LowerCase(StrReturnFraction);
  FReturnFractionData.Max := 1;
  FReturnFractionData.Min := 0;
  FReturnFractionData.CheckMax := True;
  FReturnFractionData.CheckMin := True;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FElevationData.OnInvalidate := LocalModel.InvalidateMfDrtElevation;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfDrtConductance;
    FReturnFractionData.OnInvalidate := LocalModel.InvalidateMfDrtReturnFraction;
  end;
  AddTimeList(FElevationData);
  AddTimeList(FConductanceData);
  AddTimeList(FReturnFractionData);
end;

destructor TDrtTimeListLink.Destroy;
begin
  FElevationData.Free;
  FConductanceData.Free;
  FReturnFractionData.Free;
  inherited;
end;

end.
