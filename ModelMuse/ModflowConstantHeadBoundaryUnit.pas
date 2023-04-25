unit ModflowConstantHeadBoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, OrderedCollectionUnit,
  ModflowBoundaryUnit, ModflowCellUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  SubscriptionUnit, RbwParser, GoPhastTypes,
  ModflowTransientListParameterUnit, OrderedCollectionInterfaceUnit,
  Modflow6DynamicTimeSeriesInterfaceUnit;

type
  // @name stores data for one CHD cell in a time increment defined by
  // @link(StartingTime) and @link(EndingTime).
  // The @link(StartingTime) and @link(EndingTime) may or may not be
  // the starting and ending time of stress periods.
  // @member(Cell Cell is the cell to which this boundary applies.)
  // @member(StartingHead StartingHead is the specified head
  //   for this boundary at @link(StartingTime).)
  // @member(EndingHead EndingHead is the specified head
  //   for this boundary at @link(EndingTime). EndingHead is not used
  //   in MODFLOW 6.)
  // @member(StartingTime StartingTime is when this boundary
  //   first begins to apply.)
  // @member(EndingTime EndingTime is when this boundary ceases to apply.)
  // @member(StartAnnotation StartAnnotation tells how
  //  @link(StartingHead) was assigned.)
  // @member(EndAnnotation EndAnnotation tells how
  //  @link(EndingHead) was assigned.)
  // @member(HeadTimeSeriesName HeadTimeSeriesName is the name of a time
  //  series for MODFLOW 6.)
  TChdRecord = record
    Cell: TCellLocation;
    StartingHead: double;
    EndingHead: double;
    StartingTime: double;
    EndingTime: double;
    StartAnnotation: string;
    EndAnnotation: string;
//    TimeSeriesName: string;
    HeadParameterName: string;
    HeadParameterValue: double;
    // PEST
    StartHeadPest: string;
    EndHeadPest: string;
    StartHeadPestSeriesName: string;
    EndHeadPestSeriesName: string;
    HeadTimeSeriesName: string;
    StartHeadPestSeriesMethod: TPestParamMethod;
    EndHeadPestSeriesMethod: TPestParamMethod;
    // GWT Concentrations
    GwtConcentrations: TGwtCellData;
    procedure Assign(const Item: TChdRecord);
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TChdArray = array of TChdRecord;

  TChdStorage = class(TCustomBoundaryStorage)
  private
    FChdArray: TChdArray;
    function GetChdArray: TChdArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property ChdArray: TChdArray read GetChdArray;
  end;

  TChdTimeListLink = class(TTimeListsModelLink)
  private
    FStartData: TModflowTimeList;
    FEndData: TModflowTimeList;
    FConcList: TModflowTimeLists;
    procedure AddGwtTimeLists(SpeciesIndex: Integer);
    procedure RemoveGwtTimeLists(SpeciesIndex: Integer);
  protected
    procedure UpdateGwtTimeLists; override;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TChdCollection = class;

  TChdGwtConcCollection = class(TGwtConcStringCollection)
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TChdCollection);
  end;

  // @name represents MODFLOW Constant-Head boundaries
  // for a series of time intervals.
  TChdCollection = class(TCustomMF_ListBoundColl)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure InvalidateStartData(Sender: TObject);
    procedure InvalidateEndData(Sender: TObject);
    procedure InvalidateGwtConcentrations(Sender: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    // @name returns @link(TChdItem).
    class function ItemClass: TBoundaryItemClass; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignCellList(CellAssignmentData: TCellAssignmentData); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure InvalidateModel; override;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: IModelForTOrderedCollection; ScreenObject: TObject); override;
  end;

  // Each @name stores a @link(TChdCollection).
  // @classname is stored by @link(TModflowParameters).
  TChdParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;
  
  // @name represents a MODFLOW Constant-Head boundary for one time interval.
  // @name is stored by TChdCollection.
  TChdItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(EndHead).
    FEndHead: IFormulaObject;
    // See @link(StartHead).
    FStartHead: IFormulaObject;
    FGwtConcentrations: TChdGwtConcCollection;
    // See @link(EndHead).
    procedure SetEndHead(const Value: string);
    // See @link(StartHead).
    procedure SetStartHead(const Value: string);
    function GetEndHead: string;
    function GetStartHead: string;
    procedure SetGwtConcentrations(const Value: TChdGwtConcCollection);
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
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the ending head
    // or the ending head multiplier of this boundary.
    property EndHead: string read GetEndHead write SetEndHead;
    // @name is the formula used to set the starting head
    // or the starting head multiplier of this boundary.
    property StartHead: string read GetStartHead write SetStartHead;
    property GwtConcentrations: TChdGwtConcCollection read FGwtConcentrations
      write SetGwtConcentrations;
  end;

  TCHD_Cell = class(TValueCell)
  private
    FValues: TChdRecord;
    StressPeriod: integer;
    function GetEndingHead: double;
    function GetStartingHead: double;
    function GetEndingHeadAnnotation: string;
    function GetStartingHeadAnnotation: string;
    function GetHeadParameterName: string;
    function GetHeadParameterValue: double;
    function GetEndHeadPest: string;
    function GetEndHeadPestSeriesMethod: TPestParamMethod;
    function GetEndHeadPestSeriesName: string;
    function GetStartHeadPest: string;
    function GetStartHeadPestSeriesMethod: TPestParamMethod;
    function GetStartHeadPestSeriesName: string;
    function GetHeadTimeSeriesName: string;
    procedure SetHeadTimeSeriesName(const Value: string);
    function GetConcentration(const Index: Integer): double;
    function GetConcentrationAnnotation(const Index: Integer): string;
    function GetConcentrationPestName(const Index: Integer): string;
    function GetConcentrationPestSeriesMethod(
      const Index: Integer): TPestParamMethod;
    function GetConcentrationPestSeriesName(const Index: Integer): string;
    function GetConcentrationTimeSeriesName(const Index: Integer): string;
  protected
    property Values: TChdRecord read FValues;
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
    property StartingHead: double read GetStartingHead;
    property EndingHead: double read GetEndingHead;
    property StartingHeadAnnotation: string read GetStartingHeadAnnotation;
    property EndingHeadAnnotation: string read GetEndingHeadAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    property HeadParameterName: string read GetHeadParameterName;
    property HeadParameterValue: double read GetHeadParameterValue;
    //PEST properties
    property StartHeadPest: string read GetStartHeadPest;
    property EndHeadPest: string read GetEndHeadPest;
    property StartHeadPestSeriesName: string read GetStartHeadPestSeriesName;
    property EndHeadPestSeriesName: string read GetEndHeadPestSeriesName;
    property StartHeadPestSeriesMethod: TPestParamMethod
      read GetStartHeadPestSeriesMethod;
    property EndHeadPestSeriesMethod: TPestParamMethod
      read GetEndHeadPestSeriesMethod;
    property HeadTimeSeriesName: string read GetHeadTimeSeriesName
      write SetHeadTimeSeriesName;
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


  // @name represents the MODFLOW Constant-Head boundaries associated with
  // a single @link(TScreenObject).
  //See also TChdCollection in implementation section
  // @seealso(TModflowParameters)
  TChdBoundary = class(TModflowParamBoundary)
  private
    FCurrentParameter: TModflowTransientListParameter;
    FPestStartingHeadMethod: TPestParamMethod;
    FPestEndingHeadMethod: TPestParamMethod;
    FPestStartingHeadFormula: IFormulaObject;
    FPestEndingHeadFormula: IFormulaObject;
    FPestEndingObserver: TObserver;
    FPestStartingObserver: TObserver;
    FUsedObserver: TObserver;
    FPestConcentrationMethods: TGwtPestMethodCollection;
    FPestConcentrationFormulas: TChdGwtConcCollection;
    FConcentrationObservers: TObserverList;
    function GetPestEndingHeadObserver: TObserver;
    function GetPestStartingHeadObserver: TObserver;
    function GetPestEndingHeadFormula: string;
    function GetPestStartingHeadFormula: string;
    procedure SetPestEndingHeadFormula(const Value: string);
    procedure SetPestEndingHeadMethod(const Value: TPestParamMethod);
    procedure SetPestStartingHeadFormula(const Value: string);
    procedure SetPestStartingHeadMethod(const Value: TPestParamMethod);
    procedure InvalidateStartingHeadData(Sender: TObject);
    procedure InvalidateEndingHeadData(Sender: TObject);
    procedure InvalidateConcData(Sender: TObject);
    procedure SetPestConcentrationFormulas(const Value: TChdGwtConcCollection);
    procedure SetPestConcentrationMethods(const Value: TGwtPestMethodCollection);
    function GetConcentrationObserver(const Index: Integer): TObserver;
  protected
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel);  override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    class function ModflowParamItemClass: TModflowParamItemClass; override;
    function ParameterType: TParameterType; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    property PestStartingHeadObserver: TObserver read GetPestStartingHeadObserver;
    property PestEndingHeadObserver: TObserver read GetPestEndingHeadObserver;
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
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    procedure InvalidateDisplay; override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property Interp;
    property PestStartingHeadFormula: string read GetPestStartingHeadFormula
      write SetPestStartingHeadFormula;
    property PestEndingHeadFormula: string read GetPestEndingHeadFormula
      write SetPestEndingHeadFormula;
    property PestStartingHeadMethod: TPestParamMethod read FPestStartingHeadMethod
      write SetPestStartingHeadMethod;
    property PestEndingHeadMethod: TPestParamMethod
      read FPestEndingHeadMethod write SetPestEndingHeadMethod;
    property PestConcentrationFormulas: TChdGwtConcCollection
      read FPestConcentrationFormulas write SetPestConcentrationFormulas;
    property PestConcentrationMethods: TGwtPestMethodCollection
      read FPestConcentrationMethods write SetPestConcentrationMethods;
  end;

const
  ChdStartHeadPosition = 0;
  ChdEndHeadPosition = 1;
  ChdStartConcentration = 2;

resourcestring
  StrCHDStartingHeadSe = 'CHD Starting Head set to zero because of a math er' +
  'ror';
  StrCHDEndingHeadSet = 'CHD Ending Head set to zero because of a math error';
  StrCHDConcentrationSe = 'CHD Concentration set to zero because of a math e' +
  'rror';

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit,
  frmGoPhastUnit, GIS_Functions,
  frmErrorsAndWarningsUnit;

resourcestring
  FormatString =
    'Assigned by interpolation between the starting head of %0:f at '
    + 't = %1:f (%2:s) and the ending head of %3:f at t = %4:f (%5:s).';
  StrStartingHeadMulti = ' starting head multiplier';
  StrEndingHeadMultipl = ' ending head multiplier';
  StrNoValidCHDBoundar = 'No valid CHD boundary times defined.';
  StrNoValidCHDBoundar2 = 'No valid CHD boundary times are defined for the p' +
  'arameter ';


{ TChdItem }

procedure TChdItem.Assign(Source: TPersistent);
var
  Chd: TChdItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TChdItem then
  begin
    Chd := TChdItem(Source);
    StartHead := Chd.StartHead;
    EndHead := Chd.EndHead;
    GwtConcentrations := Chd.GwtConcentrations;
  end;
  inherited;
end;

procedure TChdItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FStartHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEndHead,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

constructor TChdItem.Create(Collection: TCollection);
var
  ChdCol: TChdCollection;
begin
  ChdCol := Collection as TChdCollection;
  FGwtConcentrations := TChdGwtConcCollection.Create(Model as TCustomModel, ScreenObject,
    ChdCol);
  inherited;
end;

procedure TChdItem.CreateFormulaObjects;
begin
  FStartHead := CreateFormulaObject(dso3D);
  FEndHead := CreateFormulaObject(dso3D);
end;

destructor TChdItem.Destroy;
begin
  FGwtConcentrations.Free;
  StartHead := '0';
  EndHead := '0';
  inherited;
end;

procedure TChdItem.AssignObserverEvents(Collection: TCollection);
var
  EndObserver: TObserver;
  StartObserver: TObserver;
  ParentCollection: TChdCollection;
  ConcIndex: Integer;
begin
  ParentCollection := Collection as TChdCollection;
  StartObserver := FObserverList[ChdStartHeadPosition];
  StartObserver.OnUpToDateSet := ParentCollection.InvalidateStartData;
  EndObserver := FObserverList[ChdEndHeadPosition];
  EndObserver.OnUpToDateSet := ParentCollection.InvalidateEndData;
  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    GwtConcentrations[ConcIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateGwtConcentrations;
  end;
end;

function TChdItem.BoundaryFormulaCount: integer;
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

function TChdItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TGwtConcStringValueItem;
begin
  case Index of
    ChdStartHeadPosition: result := StartHead;
    ChdEndHeadPosition: result := EndHead;
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

function TChdItem.GetEndHead: string;
begin
  Result := FEndHead.Formula;
  ResetItemObserver(ChdEndHeadPosition);
end;

procedure TChdItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  ConcIndex: Integer;
  Item: TGwtConcStringValueItem;
begin
  if Sender = FStartHead as TObject then
  begin
    List.Add(FObserverList[ChdStartHeadPosition]);
  end;
  if Sender = FEndHead as TObject then
  begin
    List.Add(FObserverList[ChdEndHeadPosition]);
  end;
  for ConcIndex := 0 to GwtConcentrations.Count - 1 do
  begin
    Item := GwtConcentrations.Items[ConcIndex];
    if (Item.ValueObject as TObject) = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;
end;

function TChdItem.GetStartHead: string;
begin
  Result := FStartHead.Formula;
  ResetItemObserver(ChdStartHeadPosition);
end;

procedure TChdItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfChdStartingHead(self);
    PhastModel.InvalidateMfChdEndingHead(self);
    PhastModel.InvalidateMfChdConc(self);
  end;
end;

function TChdItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TChdItem;
begin
  result := (AnotherItem is TChdItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TChdItem(AnotherItem);
    result := (Item.EndHead = EndHead)
      and (Item.StartHead = StartHead)
      and (Item.GwtConcentrations.IsSame(GwtConcentrations));
  end;
end;

{ TChdParamItem }
procedure TChdItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TGwtConcStringValueItem;
begin
  inherited;
  case Index of
    ChdStartHeadPosition: StartHead := Value;
    ChdEndHeadPosition: EndHead := Value;
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

procedure TChdItem.SetEndHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, ChdEndHeadPosition, FEndHead);
end;

procedure TChdItem.SetGwtConcentrations(const Value: TChdGwtConcCollection);
begin
  FGwtConcentrations.Assign(Value);
end;

procedure TChdItem.SetStartHead(const Value: string);
begin
  UpdateFormulaBlocks(Value, ChdStartHeadPosition, FStartHead);
end;

{ TChdParamItem }

class function TChdParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TChdCollection;
end;

{ TChdCollection }

class function TChdCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TChdTimeListLink;
end;

procedure TChdCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TChdStorage.Create(AModel));
end;

function TChdCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Item: TChdItem;
begin
  Item := Items[ItemIndex] as TChdItem;
  result := Item.BoundaryFormula[FormulaIndex];
end;

procedure TChdCollection.AssignCellList(CellAssignmentData: TCellAssignmentData);
var
  ChdStorage: TChdStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  ConcIndex: Integer;
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
  Assert(Expression <> nil);

  ChdStorage := BoundaryStorage as TChdStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    try
      Expression.Evaluate;
      with ChdStorage.ChdArray[Index] do
      begin
        case BoundaryFunctionIndex of
          0:
            begin
              StartingHead := Expression.DoubleResult;
              StartAnnotation := ACell.Annotation;
              StartHeadPest := PestName;
              StartHeadPestSeriesName := PestSeriesName;
              StartHeadPestSeriesMethod := PestSeriesMethod;
              HeadTimeSeriesName := TimeSeriesName;
            end;
          1:
            begin
              EndingHead := Expression.DoubleResult;
              EndAnnotation := ACell.Annotation;
              EndHeadPest := PestName;
              EndHeadPestSeriesName := PestSeriesName;
              EndHeadPestSeriesMethod := PestSeriesMethod;
            end;
          else
            begin
              ConcIndex := BoundaryFunctionIndex - ChdStartConcentration;
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
        with ChdStorage.ChdArray[Index] do
        begin
          case BoundaryFunctionIndex of
            0:
              begin
                ErrorMessage :=   StrCHDStartingHeadSe;
                StartingHead := 0;
                StartAnnotation := ErrorMessage;
                StartHeadPest := PestName;
                StartHeadPestSeriesName := PestSeriesName;
                StartHeadPestSeriesMethod := PestSeriesMethod;
                HeadTimeSeriesName := TimeSeriesName;
              end;
            1:
              begin
                ErrorMessage :=  StrCHDEndingHeadSet;
                EndingHead := 0;
                EndAnnotation := ErrorMessage;
                EndHeadPest := PestName;
                EndHeadPestSeriesName := PestSeriesName;
                EndHeadPestSeriesMethod := PestSeriesMethod;
              end;
            else
              begin
                ErrorMessage := StrCHDConcentrationSe;
                ConcIndex := BoundaryFunctionIndex - ChdStartConcentration;
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
        with ChdStorage.ChdArray[Index] do
        begin
          case BoundaryFunctionIndex of
            0:
              begin
                ErrorMessage :=   StrCHDStartingHeadSe;
                StartingHead := 0;
                StartAnnotation := ErrorMessage;
                StartHeadPest := PestName;
                StartHeadPestSeriesName := PestSeriesName;
                StartHeadPestSeriesMethod := PestSeriesMethod;
                HeadTimeSeriesName := TimeSeriesName;
              end;
            1:
              begin
                ErrorMessage :=  StrCHDEndingHeadSet;
                EndingHead := 0;
                EndAnnotation := ErrorMessage;
                EndHeadPest := PestName;
                EndHeadPestSeriesName := PestSeriesName;
                EndHeadPestSeriesMethod := PestSeriesMethod;
              end;
            else
              begin
                ErrorMessage := StrCHDConcentrationSe;
                ConcIndex := BoundaryFunctionIndex - ChdStartConcentration;
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

procedure TChdCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  ChdStorage: TChdStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  ChdStorage := BoundaryStorage as TChdStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    with ChdStorage.ChdArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

constructor TChdCollection.Create(Boundary: TModflowScreenObjectProperty; Model: IModelForTOrderedCollection;
  ScreenObject: TObject);
begin
  inherited;
  ListDuplicatesAllowed := False;
end;

procedure TChdCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  Index: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TChdStorage).FChdArray, BoundaryCount);
  LocalModel := Model as TCustomModel;
  if LocalModel.GwtUsed then
  begin
    for Index := 0 to BoundaryCount - 1 do
    begin
      SetLength(TChdStorage(Boundaries[ItemIndex, AModel]).FChdArray[Index].GwtConcentrations.Values,
        LocalModel.MobileComponents.Count);
      SetLength(TChdStorage(Boundaries[ItemIndex, AModel]).FChdArray[Index].GwtConcentrations.ValueAnnotations,
        LocalModel.MobileComponents.Count);
      SetLength(TChdStorage(Boundaries[ItemIndex, AModel]).FChdArray[Index].GwtConcentrations.ValuePestNames,
        LocalModel.MobileComponents.Count);
      SetLength(TChdStorage(Boundaries[ItemIndex, AModel]).FChdArray[Index].GwtConcentrations.ValuePestSeriesNames,
        LocalModel.MobileComponents.Count);
      SetLength(TChdStorage(Boundaries[ItemIndex, AModel]).FChdArray[Index].GwtConcentrations.ValuePestSeriesMethods,
        LocalModel.MobileComponents.Count);
      SetLength(TChdStorage(Boundaries[ItemIndex, AModel]).FChdArray[Index].GwtConcentrations.ValueTimeSeriesNames,
        LocalModel.MobileComponents.Count);
    end;
  end;
  inherited;
end;

procedure TChdCollection.InvalidateEndData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TChdTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TChdTimeListLink;
    Link.FEndData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TChdTimeListLink;
        Link.FEndData.Invalidate;
      end;
    end;
  end;
end;

procedure TChdCollection.InvalidateGwtConcentrations(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TChdTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TChdTimeListLink;
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
        Link := TimeListLink.GetLink(ChildModel) as TChdTimeListLink;
        for Index := 0 to Link.FConcList.Count - 1 do
        begin
          TimeList := Link.FConcList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

procedure TChdCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfChdStartingHead(self);
    PhastModel.InvalidateMfChdEndingHead(self);
    PhastModel.InvalidateMfChdConc(self);
  end;
end;

procedure TChdCollection.InvalidateStartData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TChdTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TChdTimeListLink;
    Link.FStartData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TChdTimeListLink;
        Link.FStartData.Invalidate;
      end;
    end;
  end;
end;

class function TChdCollection.ItemClass: TBoundaryItemClass;
begin
  result := TChdItem;
end;

{ TChdBoundary }

procedure TChdBoundary.Assign(Source: TPersistent);
var
  SourceChd: TChdBoundary;
begin
  if Source is TChdBoundary then
  begin
    SourceChd := TChdBoundary(Source);
//    Interp := SourceChd.Interp;
    PestStartingHeadFormula := SourceChd.PestStartingHeadFormula;
    PestEndingHeadFormula := SourceChd.PestEndingHeadFormula;
    PestStartingHeadMethod := SourceChd.PestStartingHeadMethod;
    PestEndingHeadMethod := SourceChd.PestEndingHeadMethod;
    PestConcentrationFormulas := SourceChd.PestConcentrationFormulas;
    PestConcentrationMethods := SourceChd.PestConcentrationMethods;
  end;
  inherited;
end;

procedure TChdBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TCHD_Cell;
  BoundaryValues: TChdRecord;
  BoundaryIndex: Integer;
  EndHeadFactor: Double;
  StartFormatString: string;
  StartHeadFactor: Double;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TChdStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TChdStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TCHD_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
//      Cells.CheckRestore;
      // The starting head for each cell will be
      // StartHeadFactor * StartingHead + (1-StartHeadFactor)*EndingHead
      // The ending head for each cell will be
      // EndHeadFactor * StartingHead + (1-EndHeadFactor)*EndingHead
      if (StressPeriod.StartTime = LocalBoundaryStorage.StartingTime)
        or (LocalModel.ModelSelection = msModflow2015) then
      begin
        StartHeadFactor := 1;
        StartFormatString := '';
      end
      else
      begin
        StartHeadFactor := 1 -
          (StressPeriod.StartTime - LocalBoundaryStorage.StartingTime)
          / (LocalBoundaryStorage.EndingTime
          - LocalBoundaryStorage.StartingTime);
      end;
      if (StressPeriod.EndTime = LocalBoundaryStorage.EndingTime)
        or (LocalModel.ModelSelection = msModflow2015) then
      begin
        EndHeadFactor := 0;
      end
      else
      begin
        EndHeadFactor := 1 -
          (StressPeriod.EndTime - LocalBoundaryStorage.StartingTime)
          / (LocalBoundaryStorage.EndingTime
          - LocalBoundaryStorage.StartingTime);
      end;
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.ChdArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.ChdArray)
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.ChdArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.ChdArray[BoundaryIndex];
        if FCurrentParameter <> nil then
        begin
          BoundaryValues.StartingHead :=
            BoundaryValues.StartingHead * FCurrentParameter.Value;
//          BoundaryValues.StartAnnotation :=
//            BoundaryValues.StartAnnotation
//            + ' multiplied by the parameter value for "'+ FCurrentParameter.ParameterName + '."';
          BoundaryValues.StartAnnotation := Format(Str0sMultipliedByT, [
            BoundaryValues.StartAnnotation, FCurrentParameter.ParameterName]);
          BoundaryValues.HeadParameterName := FCurrentParameter.ParameterName;
          BoundaryValues.HeadParameterValue := FCurrentParameter.Value;
        end
        else
        begin
          BoundaryValues.HeadParameterName := '';
          BoundaryValues.HeadParameterValue := 1;
        end;
        Cell := TCHD_Cell.Create;
        Cell.BoundaryIndex := BoundaryIndex;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
//        Cell.FValues.HeadParameterName := BoundaryValues.HeadParameterName;
//        Cell.FValues.HeadParameterValue := BoundaryValues.HeadParameterValue;
////        Cell.FValues.TimeSeriesName := BoundaryValues.TimeSeriesName;
//        Cell.FValues.StartHeadPest := BoundaryValues.StartHeadPest;
//        Cell.FValues.EndHeadPest := BoundaryValues.EndHeadPest;
//        Cell.FValues.StartHeadPestSeriesName := BoundaryValues.StartHeadPestSeriesName;
//        Cell.FValues.EndHeadPestSeriesName := BoundaryValues.EndHeadPestSeriesName;
//        Cell.FValues.StartHeadPestSeriesMethod := BoundaryValues.StartHeadPestSeriesMethod;
//        Cell.FValues.EndHeadPestSeriesMethod := BoundaryValues.EndHeadPestSeriesMethod;
//        Cell.FValues.HeadTimeSeriesName := BoundaryValues.HeadTimeSeriesName;
//        Cell.FValues.Cell := BoundaryValues.Cell;
        Cell.FValues.StartingHead :=
          StartHeadFactor * BoundaryValues.StartingHead
          + (1 - StartHeadFactor) * BoundaryValues.EndingHead;
        Cell.FValues.EndingHead :=
          EndHeadFactor * BoundaryValues.StartingHead
          + (1 - EndHeadFactor) * BoundaryValues.EndingHead;
        if StartHeadFactor = 1 then
        begin
          Cell.FValues.StartAnnotation := BoundaryValues.StartAnnotation;
        end
        else
        begin
          Cell.FValues.StartAnnotation := Format(FormatString,
            [BoundaryValues.StartingHead, BoundaryValues.StartingTime,
            BoundaryValues.StartAnnotation, BoundaryValues.EndingHead,
            BoundaryValues.EndingTime, BoundaryValues.EndAnnotation]);
        end;
        if EndHeadFactor = 0 then
        begin
          Cell.FValues.EndAnnotation := BoundaryValues.EndAnnotation;
        end
        else
        begin
          Cell.FValues.EndAnnotation := Format(FormatString,
            [BoundaryValues.StartingHead, BoundaryValues.StartingTime,
            BoundaryValues.StartAnnotation, BoundaryValues.EndingHead,
            BoundaryValues.EndingTime, BoundaryValues.EndAnnotation]);
        end;
        Cell.ScreenObject := ScreenObject;
        // don't move CHD cells away from the edge of child  model grids.
//        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TChdBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TChdCollection;
end;

function TChdBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestChd_';
end;

constructor TChdBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  Interp := mimLinearEnd;

  FPestConcentrationFormulas:= TChdGwtConcCollection.Create(Model, ScreenObject, nil);
  FPestConcentrationFormulas.UsedForPestSeries := True;
  FPestConcentrationMethods := TGwtPestMethodCollection.Create(Model as TCustomModel);
  FConcentrationObservers := TObserverList.Create;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestStartingHeadFormula := '';
  PestEndingHeadFormula := '';
  FPestStartingHeadMethod := DefaultBoundaryMethod(ChdStartHeadPosition);
  FPestEndingHeadMethod := DefaultBoundaryMethod(ChdEndHeadPosition);

end;

procedure TChdBoundary.CreateFormulaObjects;
var
  LocalModel: TPhastModel;
  ConcIndex: Integer;
begin
  FPestStartingHeadFormula := CreateFormulaObjectBlocks(dso3D);
  FPestEndingHeadFormula := CreateFormulaObjectBlocks(dso3D);
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and LocalModel.GwtUsed then
  begin
    for ConcIndex := 0 to LocalModel.MobileComponents.Count - 1 do
    begin
      FPestConcentrationFormulas.Add;
    end;
  end;
end;

procedure TChdBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestStartingHeadObserver);
    FObserverList.Add(PestEndingHeadObserver);
    for Index := 0 to FPestConcentrationFormulas.Count - 1 do
    begin
      FObserverList.Add(ConcentrationObserver[Index]);
    end;
  end;
end;

class function TChdBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    ChdStartHeadPosition:
      begin
        result := ppmAdd;
      end;
    ChdEndHeadPosition:
      begin
        result := ppmAdd;
      end;
    else
      result := inherited;
//      Assert(False);
  end;
end;

destructor TChdBoundary.Destroy;
var
  Index: Integer;
begin
  PestStartingHeadFormula := '';
  PestEndingHeadFormula := '';

  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    FPestConcentrationFormulas[Index].Value := '';
  end;
  inherited;
  FPestConcentrationMethods.Free;
  FPestConcentrationFormulas.Free;
  FConcentrationObservers.Free;
end;

procedure TChdBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
const
  NoData = 3.0E30;
var
  ValueIndex: Integer;
  BoundaryStorage: TChdStorage;
  ParamIndex: Integer;
  Param: TChdParamItem;
  Times: TList;
  Position: integer;
  ParamName: string;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  FCurrentParameter := nil;
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TChdStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex] as TChdParamItem;
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
        BoundaryStorage := Param.Param.Boundaries[ValueIndex, AModel] as TChdStorage;
        AssignCells(BoundaryStorage, Times, AModel);
      end;
    end;
  end;
end;

function TChdBoundary.GetConcentrationObserver(const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FConcentrationObservers.Count do
  begin
    CreateObserver(Format('ChdConc_%d', [Index+1]), AObserver, nil);
    FConcentrationObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateConcData;
  end;
  result := FConcentrationObservers[Index];
end;

function TChdBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
var
  ConcIndex: Integer;
begin
  result := '';
  case FormulaIndex of
    ChdStartHeadPosition:
      begin
        result := PestStartingHeadFormula;
      end;
    ChdEndHeadPosition:
      begin
        result := PestEndingHeadFormula;
      end;
    else
      begin
        ConcIndex := FormulaIndex - ChdStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        result := PestConcentrationFormulas[ConcIndex].Value;
      end;
  end;
end;

function TChdBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    ChdStartHeadPosition:
      begin
        result := PestStartingHeadMethod;
      end;
    ChdEndHeadPosition:
      begin
        result := PestEndingHeadMethod;
      end;
    else
      begin
        ConcIndex := FormulaIndex - ChdStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        result := FPestConcentrationMethods[ConcIndex].PestParamMethod;
      end;
  end;
end;

function TChdBoundary.GetPestEndingHeadObserver: TObserver;
begin
  if FPestEndingObserver = nil then
  begin
    CreateObserver('PestEndingHead_', FPestEndingObserver, nil);
    FPestEndingObserver.OnUpToDateSet := InvalidateEndingHeadData;
  end;
  result := FPestEndingObserver;
end;

function TChdBoundary.GetPestStartingHeadObserver: TObserver;
begin
  if FPestStartingObserver = nil then
  begin
    CreateObserver('PestStartingHead_', FPestStartingObserver, nil);
    FPestStartingObserver.OnUpToDateSet := InvalidateStartingHeadData;
  end;
  result := FPestStartingObserver;
end;

function TChdBoundary.GetPestEndingHeadFormula: string;
begin
  Result := FPestEndingHeadFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ChdEndHeadPosition);
  end;
end;

function TChdBoundary.GetPestStartingHeadFormula: string;
begin
  Result := FPestStartingHeadFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(ChdStartHeadPosition);
  end;
end;

procedure TChdBoundary.GetPropertyObserver(Sender: TObject; List: TList);
var
  Index: Integer;
begin
  if Sender = FPestStartingHeadFormula as TObject then
  begin
    if ChdStartHeadPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[ChdStartHeadPosition]);
    end;
  end;
  if Sender = FPestEndingHeadFormula as TObject then
  begin
    if ChdEndHeadPosition < FObserverList.Count then
    begin
      List.Add(FObserverList[ChdEndHeadPosition]);
    end;
  end;
  for Index := 0 to FPestConcentrationFormulas.Count - 1 do
  begin
    if FPestConcentrationFormulas[Index].ValueObject as TObject = Sender then
    begin
      List.Add(FObserverList[ChdStartConcentration + Index]);
    end;
  end;
end;

function TChdBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestChd_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TChdBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

procedure TChdBoundary.InvalidateConcData(Sender: TObject);
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.Clearing then
  begin
    Exit;
  end;
  PhastModel.InvalidateMfChdConc(self);
end;

procedure TChdBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfChdStartingHead(self);
    Model.InvalidateMfChdEndingHead(self);
    Model.InvalidateMfChdConc(self);
  end;
end;

procedure TChdBoundary.InvalidateEndingHeadData(Sender: TObject);
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
    PhastModel.InvalidateMfChdEndingHead(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfChdEndingHead(self);
      end;
    end;
  end;
end;

procedure TChdBoundary.InvalidateStartingHeadData(Sender: TObject);
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
    PhastModel.InvalidateMfChdStartingHead(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfChdStartingHead(self);
      end;
    end;
  end;
end;

class function TChdBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TChdParamItem;
end;

function TChdBoundary.ParameterType: TParameterType;
begin
  result := ptCHD;
end;

procedure TChdBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    ChdStartHeadPosition:
      begin
        PestStartingHeadFormula := Value;
      end;
    ChdEndHeadPosition:
      begin
        PestEndingHeadFormula := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - ChdStartConcentration;
        while ConcIndex >= PestConcentrationFormulas.Count do
        begin
          PestConcentrationFormulas.Add;
        end;
        PestConcentrationFormulas[ConcIndex].Value := Value;
      end;
  end;
end;

procedure TChdBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
var
  ConcIndex: Integer;
begin
  case FormulaIndex of
    ChdStartHeadPosition:
      begin
        PestStartingHeadMethod := Value;
      end;
    ChdEndHeadPosition:
      begin
        PestEndingHeadMethod := Value;
      end;
    else
      begin
        ConcIndex := FormulaIndex - ChdStartConcentration;
        while ConcIndex >= FPestConcentrationMethods.Count do
        begin
          FPestConcentrationMethods.Add;
        end;
        FPestConcentrationMethods[ConcIndex].PestParamMethod := Value;
      end;
  end;
end;

procedure TChdBoundary.SetPestConcentrationFormulas(
  const Value: TChdGwtConcCollection);
begin
  FPestConcentrationFormulas.Assign(Value);
end;

procedure TChdBoundary.SetPestConcentrationMethods(
  const Value: TGwtPestMethodCollection);
begin
  FPestConcentrationMethods.Assign(Value);
end;

procedure TChdBoundary.SetPestEndingHeadFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, ChdEndHeadPosition, FPestEndingHeadFormula);
end;

procedure TChdBoundary.SetPestEndingHeadMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestEndingHeadMethod, Value);
end;

procedure TChdBoundary.SetPestStartingHeadFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, ChdStartHeadPosition, FPestStartingHeadFormula);
end;

procedure TChdBoundary.SetPestStartingHeadMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestStartingHeadMethod, Value);
end;

//procedure TChdBoundary.SetInterp(const Value: TMf6InterpolationMethods);
//begin
//  if FInterp <> Value then
//  begin
//    InvalidateModel;
//    FInterp := Value;
//  end;
//end;

{ TCHD_Cell }

procedure TCHD_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TCHD_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TCHD_Cell.GetConcentration(const Index: Integer): double;
begin
  result := FValues.GwtConcentrations.Values[Index];
end;

function TCHD_Cell.GetConcentrationAnnotation(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValueAnnotations[Index];
end;

function TCHD_Cell.GetConcentrationPestName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValuePestNames[Index];
end;

function TCHD_Cell.GetConcentrationPestSeriesMethod(
  const Index: Integer): TPestParamMethod;
begin
  result := FValues.GwtConcentrations.ValuePestSeriesMethods[Index];
end;

function TCHD_Cell.GetConcentrationPestSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValuePestSeriesNames[Index];
end;

function TCHD_Cell.GetConcentrationTimeSeriesName(const Index: Integer): string;
begin
  result := FValues.GwtConcentrations.ValueTimeSeriesNames[Index];
end;

function TCHD_Cell.GetEndHeadPest: string;
begin
  result := Values.EndHeadPest;
end;

function TCHD_Cell.GetEndHeadPestSeriesMethod: TPestParamMethod;
begin
  result := Values.EndHeadPestSeriesMethod;
end;

function TCHD_Cell.GetEndHeadPestSeriesName: string;
begin
  result := Values.EndHeadPestSeriesName;
end;

function TCHD_Cell.GetEndingHead: double;
begin
  result := Values.EndingHead;
end;

function TCHD_Cell.GetEndingHeadAnnotation: string;
begin
  result := Values.EndAnnotation;
end;

function TCHD_Cell.GetHeadParameterName: string;
begin
  result := Values.HeadParameterName;
end;

function TCHD_Cell.GetHeadParameterValue: double;
begin
  result := Values.HeadParameterValue;
end;

function TCHD_Cell.GetHeadTimeSeriesName: string;
begin
  result := Values.HeadTimeSeriesName;
end;

function TCHD_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TCHD_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TCHD_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TCHD_Cell.GetMf6TimeSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  if Index = ChdStartHeadPosition then
  begin
    result := HeadTimeSeriesName;
  end
  else if Index = ChdEndHeadPosition then
  begin
    result := HeadTimeSeriesName;
  end
  else
    begin
      ConcIndex := Index - ChdStartConcentration;
      result := FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex];
    end;
end;

function TCHD_Cell.GetPestName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    ChdStartHeadPosition: result := StartHeadPest;
    ChdEndHeadPosition: result := EndHeadPest;
    else
      begin
        ConcIndex := Index - ChdStartConcentration;
        result := FValues.GwtConcentrations.ValuePestNames[ConcIndex];
      end;
  end;
end;

function TCHD_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
var
  ConcIndex: Integer;
begin
  case Index of
    ChdStartHeadPosition: result := StartHeadPestSeriesMethod;
    ChdEndHeadPosition: result := EndHeadPestSeriesMethod;
    else
      begin
        ConcIndex := Index - ChdStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesMethods[ConcIndex];
      end;
  end;
end;

function TCHD_Cell.GetPestSeriesName(Index: Integer): string;
var
  ConcIndex: Integer;
begin
  case Index of
    ChdStartHeadPosition: result := StartHeadPestSeriesName;
    ChdEndHeadPosition: result := EndHeadPestSeriesName;
    else
      begin
        ConcIndex := Index - ChdStartConcentration;
        result := FValues.GwtConcentrations.ValuePestSeriesNames[ConcIndex];
      end;
  end;
end;

function TCHD_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
var
  ConcIndex: Integer;
begin
  case Index of
    ChdStartHeadPosition: result := StartingHeadAnnotation;
    ChdEndHeadPosition: result := EndingHeadAnnotation;
    else
      begin
        ConcIndex := Index - ChdStartConcentration;
        result := FValues.GwtConcentrations.ValueAnnotations[ConcIndex];
      end;
  end;
end;

function TCHD_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
var
  ConcIndex: Integer;
begin
  case Index of
    ChdStartHeadPosition: result := StartingHead;
    ChdEndHeadPosition: result := EndingHead;
    else
      begin
        ConcIndex := Index - ChdStartConcentration;
        result := FValues.GwtConcentrations.Values[ConcIndex];
      end;
  end;
end;

function TCHD_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TCHD_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TCHD_Cell.GetStartHeadPest: string;
begin
  result := Values.StartHeadPest;
end;

function TCHD_Cell.GetStartHeadPestSeriesMethod: TPestParamMethod;
begin
  result := Values.StartHeadPestSeriesMethod;
end;

function TCHD_Cell.GetStartHeadPestSeriesName: string;
begin
  result := Values.StartHeadPestSeriesName;
end;

function TCHD_Cell.GetStartingHead: double;
begin
  result := Values.StartingHead;
end;

function TCHD_Cell.GetStartingHeadAnnotation: string;
begin
  result := Values.StartAnnotation;
end;

//function TCHD_Cell.GetTimeSeriesName: string;
//begin
//  result := Values.TimeSeriesName;
//end;

function TCHD_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  CHD_Cell: TCHD_Cell;
begin
  result := AnotherCell is TCHD_Cell;
  if result then
  begin
    CHD_Cell := TCHD_Cell(AnotherCell);
    result :=
      (StartingHead = CHD_Cell.StartingHead)
      and (EndingHead = CHD_Cell.EndingHead)
      and (IFace = CHD_Cell.IFace)
      and (Values.Cell = CHD_Cell.Values.Cell);
//      and (EndingHead = StartingHead);
  end;
end;

procedure TCHD_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TCHD_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TCHD_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TCHD_Cell.SetHeadTimeSeriesName(const Value: string);
begin
  FValues.HeadTimeSeriesName := Value;
end;

procedure TCHD_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TCHD_Cell.SetMf6TimeSeriesName(Index: Integer; const Value: string);
var
  ConcIndex: Integer;
begin
  if Index = ChdStartHeadPosition then
  begin
    HeadTimeSeriesName := Value;
  end
  else
    begin
      ConcIndex := Index - ChdStartConcentration;
      FValues.GwtConcentrations.ValueTimeSeriesNames[ConcIndex] := Value;
    end;
end;

procedure TCHD_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TChdRecord }

procedure TChdRecord.Assign(const Item: TChdRecord);
begin
  self := Item;
  GwtConcentrations.Assign(Item.GwtConcentrations);
end;

procedure TChdRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingHead);
  WriteCompReal(Comp, EndingHead);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompReal(Comp, HeadParameterValue);
  WriteCompInt(Comp, Strings.IndexOf(StartAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EndAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(HeadParameterName));
  WriteCompInt(Comp, Strings.IndexOf(StartHeadPest));
  WriteCompInt(Comp, Strings.IndexOf(EndHeadPest));
  WriteCompInt(Comp, Strings.IndexOf(StartHeadPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(EndHeadPestSeriesName));
  WriteCompInt(Comp, Strings.IndexOf(HeadTimeSeriesName));
  WriteCompInt(Comp, Ord(StartHeadPestSeriesMethod));
  WriteCompInt(Comp, Ord(EndHeadPestSeriesMethod));

  GwtConcentrations.Cache(Comp, Strings);
end;

procedure TChdRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(StartAnnotation);
  Strings.Add(EndAnnotation);
  Strings.Add(HeadParameterName);
  Strings.Add(StartHeadPest);
  Strings.Add(EndHeadPest);
  Strings.Add(StartHeadPestSeriesName);
  Strings.Add(EndHeadPestSeriesName);
  Strings.Add(HeadTimeSeriesName);

  GwtConcentrations.RecordStrings(Strings);
end;

procedure TChdRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StartingHead := ReadCompReal(Decomp);
  EndingHead := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  HeadParameterValue := ReadCompReal(Decomp);
  StartAnnotation := Annotations[ReadCompInt(Decomp)];
  EndAnnotation := Annotations[ReadCompInt(Decomp)];
//  TimeSeriesName := Annotations[ReadCompInt(Decomp)];
  HeadParameterName := Annotations[ReadCompInt(Decomp)];
  StartHeadPest := Annotations[ReadCompInt(Decomp)];
  EndHeadPest := Annotations[ReadCompInt(Decomp)];
  StartHeadPestSeriesName := Annotations[ReadCompInt(Decomp)];
  EndHeadPestSeriesName := Annotations[ReadCompInt(Decomp)];
  HeadTimeSeriesName := Annotations[ReadCompInt(Decomp)];
  StartHeadPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  EndHeadPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));

  GwtConcentrations.Restore(Decomp,Annotations);
end;

{ TChdStorage }

procedure TChdStorage.Clear;
begin
  SetLength(FChdArray, 0);
  FCleared := True;
end;

procedure TChdStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Count := Length(FChdArray);
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    for Index := 0 to Count - 1 do
    begin
      FChdArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FChdArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TChdStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FChdArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FChdArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TChdStorage.GetChdArray: TChdArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FChdArray;

end;

{ TChdTimeListLink }

procedure TChdTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
  PhastModel: TPhastModel;
  SpeciesIndex: Integer;
begin
  FConcList := TModflowTimeLists.Create;

  FStartData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEndData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStartData.NonParamDescription := StrStartingHead;
  FStartData.ParamDescription := StrStartingHeadMulti;
  FEndData.NonParamDescription := StrEndingHead;
  FEndData.ParamDescription := StrEndingHeadMultipl;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FStartData.OnInvalidate := LocalModel.InvalidateMfChdStartingHead;
    FEndData.OnInvalidate := LocalModel.InvalidateMfChdEndingHead;
  end;
  AddTimeList(FStartData);
  AddTimeList(FEndData);

  PhastModel := frmGoPhast.PhastModel;
  if PhastModel.GwtUsed then
  begin
    for SpeciesIndex := 0 to PhastModel.MobileComponents.Count - 1 do
    begin
      AddGwtTimeLists(SpeciesIndex);
    end;
  end;
end;

destructor TChdTimeListLink.Destroy;
begin
  FConcList.Free;
  FStartData.Free;
  FEndData.Free;
  inherited;
end;

procedure TChdTimeListLink.RemoveGwtTimeLists(SpeciesIndex: Integer);
var
  ConcTimeList: TModflowTimeList;
//  LocalModel: TCustomModel;
begin
  ConcTimeList := FConcList[SpeciesIndex];
  RemoveTimeList(ConcTimeList);
  FConcList.Delete(SpeciesIndex);
end;

procedure TChdTimeListLink.UpdateGwtTimeLists;
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

procedure TChdTimeListLink.AddGwtTimeLists(SpeciesIndex: Integer);
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
    ConcTimeList.OnInvalidate := LocalModel.InvalidateMfChdConc;
  end;
  AddTimeList(ConcTimeList);
  FConcList.Add(ConcTimeList);
end;

{ TChdGwtConcCollection }

constructor TChdGwtConcCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TChdCollection);
begin
  inherited Create(Model, AScreenObject, ParentCollection);
end;

end.
