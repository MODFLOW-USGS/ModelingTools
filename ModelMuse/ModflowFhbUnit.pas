{
@name is used to define boundary conditions for the Flow and Head Boundary (FHB)
package. The data for it are defined somewhat differently than for other
MODFLOW boundary conditions. Instead of a start and end time only a time is
defined. MODFLOW will interpolate between whatever is defined for that time
and the value for the next defined time. If a time before the beginning
or after the end of the model is defined, ModelMuse will interpolate values
at the beginning or end of the model. If no value is defined for the beginning
of a model, ModelMuse will use the value for the first defined time as the
initial value. If different boundaries have different defined times, ModelMuse
will combine all the defined times and interpolate values as needed.
}
unit ModflowFhbUnit;

interface

uses
  Classes, ZLib, ModflowBoundaryUnit,
  FormulaManagerUnit, FormulaManagerInterfaceUnit,
  OrderedCollectionUnit,
  RbwParser, GoPhastTypes, ModflowCellUnit, SysUtils, SubscriptionUnit;

type
  TFhbRecord = record
    Cell: TCellLocation;
    BoundaryValue: double;
    BoundaryValueAnnotation: string;
    BoundaryValuePest: string;
    BoundaryValuePestSeriesName: string;
    BoundaryValuePestSeriesMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TFhbArray = array of TFhbRecord;

  TFhbStorage = class(TCustomBoundaryStorage)
  private
    FFhbArray: TFhbArray;
    function GetFhbArray: TFhbArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property FhbArray: TFhbArray read GetFhbArray;
  end;


  // @name represents either a specified flow or specified head boundary.
  TFhbItem = class(TCustomModflowBoundaryItem)
    // See @link(BoundaryValue).
    FBoundaryValue: IFormulaObject;
    function GetBoundaryValue: string;
    procedure SetBoundaryValue(const Value: string);
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
  published
    // @name represents either a specified head or a specified flow value
    property BoundaryValue: string read GetBoundaryValue write SetBoundaryValue;
    Destructor Destroy; override;
  end;

  TFhbHeadTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to perform notifications of the boundary values
    // for a series of FHB Boundaries over a series of time intervals.
    FBoundaryValueData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TFhbFlowTimeListLink = class(TFhbHeadTimeListLink)
    procedure CreateTimeLists; override;
  end;

  // @name represents MODFLOW Drain boundaries
  // for a series of time intervals.
  TFhbHeadCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateBoundaryValueData(Sender: TObject);
    function GetItem(Index: Integer): TFhbItem;
    procedure SetItem(Index: Integer; const Value: TFhbItem);
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

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TFhbStorage.FhbArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure DeleteItemsWithZeroDuration; override;
  public
    property Items[Index: Integer]: TFhbItem read GetItem write SetItem; default;
  end;

  TFhbFlowCollection = class(TFhbHeadCollection)
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
  end;

  TFhb_Cell = class(TValueCell)
  private
    Values: TFhbRecord;
    function GetBoundaryValue: double;
    function GetBoundaryValueAnnotation: string;
    function GetBoundaryValuePest: string;
    function GetBoundaryValuePestSeriesMethod: TPestParamMethod;
    function GetBoundaryValuePestSeriesName: string;
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
  public
    property BoundaryValue: double read GetBoundaryValue;
    property BoundaryValueAnnotation: string read GetBoundaryValueAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
    // PEST properties
    property BoundaryValuePest: string read GetBoundaryValuePest;
    property BoundaryValuePestSeriesName: string read GetBoundaryValuePestSeriesName;
    property BoundaryValuePestSeriesMethod: TPestParamMethod read GetBoundaryValuePestSeriesMethod;

  end;

  TFhbCellList = class(TValueCellList)
  private
    FStartTime: double;
    FEndTime: double;
  public
    property StartTime: Double read FStartTime write FStartTime;
    property EndTime: Double read FEndTime write FEndTime;
  end;

  // @name represents the MODFLOW FHB head boundaries associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFhbHeadCollection)
  TFhbHeadBoundary = class(TModflowBoundary)
  private
    FPestFhbBoundaryMethod: TPestParamMethod;
    FPestFhbBoundaryFormula: IFormulaObject;
    FUsedObserver: TObserver;
    FPestBoundaryObserver: TObserver;
    function GetPestBoundaryObserver: TObserver;
    procedure SetFhbBoundaryMethod(const Value: TPestParamMethod);
    function GetPestFhbBoundaryFormula: string;
    procedure SetPestFhbBoundaryFormula(const Value: string);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TDrn_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    class function InterpolatedValue(Value1, Value2, Time1, Time2, Time: Double): double;

    procedure InvalidateBoundaryData(Sender: TObject); virtual;
    procedure HandleChangedValue(Observer: TObserver); //override;
    function GetUsedObserver: TObserver; //override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure CreateFormulaObjects; //override;
    function BoundaryObserverPrefix: string; override;
    procedure CreateObservers; //override;
    property PestBoundaryObserver: TObserver read GetPestBoundaryObserver;
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
    property PestFhbBoundaryFormula: string read GetPestFhbBoundaryFormula
      write SetPestFhbBoundaryFormula;
    property PestFhbBoundaryMethod: TPestParamMethod read FPestFhbBoundaryMethod
      write SetFhbBoundaryMethod;
  end;

  // @name represents the MODFLOW FHB flow boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TFhbItem.BoundaryValue
  // TFhbItem.BoundaryValue) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - BoundaryValue / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - BoundaryValue.)
  // )
  // @seealso(TFhbFlowCollection)
  TFhbFlowBoundary = class(TFhbHeadBoundary)
  private
    FFormulaInterpretation: TFormulaInterpretation;
    procedure SetFormulaInterpretation(const Value: TFormulaInterpretation);
  protected
    procedure InvalidateBoundaryData(Sender: TObject); override;
    function BoundaryObserverPrefix: string; override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
  public
    // @name copies @link(FormulaInterpretation) from the Source
    // @classname to this @classname and then calls inherited Assign.
    procedure Assign(Source: TPersistent);override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    // @name determines whether the a formula represents
    // @unorderedlist(
    //   @item(fiSpecific - formula / the length or area of
    //     intersection between the @link(TScreenObject) and grid cell.)
    //   @item(fiTotal - formula.)
    // )
    property FormulaInterpretation: TFormulaInterpretation
      read FFormulaInterpretation write SetFormulaInterpretation;
  end;

const
  FhbBoundaryValuePosition = 0;

implementation

uses
  PhastModelUnit, frmGoPhastUnit, ScreenObjectUnit,
  ModflowTimeUnit, GIS_Functions,
  frmFormulaErrorsUnit;

resourcestring
  StrFHBHeads = 'FHB Heads';
  StrFHBFlows = 'FHB Flows';
  StrInterpolatedValueA = 'Interpolated value assigned by %s.';

{ TFhbItem }

procedure TFhbItem.Assign(Source: TPersistent);
begin
  if Source is TFhbItem then
  begin
    BoundaryValue := TFhbItem(Source).BoundaryValue;
  end;
  inherited;

end;

procedure TFhbItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFhbHeadCollection;
  BoundaryValueObserver: TObserver;
begin
  inherited;

  ParentCollection := Collection as TFhbHeadCollection;
  BoundaryValueObserver := FObserverList[FhbBoundaryValuePosition];
  BoundaryValueObserver.OnUpToDateSet := ParentCollection.InvalidateBoundaryValueData;

end;

function TFhbItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TFhbItem.CreateFormulaObjects;
begin
  FBoundaryValue := CreateFormulaObject(dso3D);
end;

destructor TFhbItem.Destroy;
begin
  BoundaryValue := '0';
  inherited;
end;

function TFhbItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    FhbBoundaryValuePosition:
      result := BoundaryValue;
    else Assert(False);
  end;
end;

function TFhbItem.GetBoundaryValue: string;
begin
  Result := FBoundaryValue.Formula;
  ResetItemObserver(FhbBoundaryValuePosition);
end;

procedure TFhbItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FBoundaryValue as TObject then
  begin
    List.Add(FObserverList[FhbBoundaryValuePosition]);
  end;
end;

procedure TFhbItem.InvalidateModel;
var
  PhastModel: TPhastModel;
  ParentCollection: TFhbHeadCollection;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    ParentCollection := Collection as TFhbHeadCollection;

    if ParentCollection is TFhbFlowCollection then
    begin
      PhastModel.InvalidateMfFhbFlows(self);
    end
    else
    begin
      PhastModel.InvalidateMfFhbHeads(self);
    end;
  end;
end;

function TFhbItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFhbItem;
begin
  result := (AnotherItem is TFhbItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFhbItem(AnotherItem);
    result := (Item.BoundaryValue = BoundaryValue);
  end;
end;

procedure TFhbItem.RemoveFormulaObjects;
begin
  inherited;
  frmGoPhast.PhastModel.FormulaManager.Remove(FBoundaryValue,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFhbItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    FhbBoundaryValuePosition:
      BoundaryValue := Value;
    else Assert(False);
  end;
end;

procedure TFhbItem.SetBoundaryValue(const Value: string);
begin
  UpdateFormulaBlocks(Value, FhbBoundaryValuePosition, FBoundaryValue);
end;

{ TFhbTimeListLink }

procedure TFhbHeadTimeListLink.CreateTimeLists;
begin
  inherited;

  FBoundaryValueData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBoundaryValueData.NonParamDescription := StrFHBHeads;
  FBoundaryValueData.ParamDescription := StrFHBHeads;
  if Model <> nil then
  begin
    FBoundaryValueData.OnInvalidate := (Model as TCustomModel).InvalidateMfFhbHeads;
  end;
  AddTimeList(FBoundaryValueData);
end;

destructor TFhbHeadTimeListLink.Destroy;
begin
  FBoundaryValueData.Free;
  inherited;
end;

{ TFhbHeadCollection }

procedure TFhbHeadCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFhbStorage.Create(AModel));
end;

function TFhbHeadCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  ScreenObject: TScreenObject;
  Item: TFhbItem;
  Boundary: TFhbHeadBoundary;
begin
  Item := Items[ItemIndex] as TFhbItem;
  if FormulaIndex = FhbBoundaryValuePosition then
  begin
    Boundary := BoundaryGroup as TFhbHeadBoundary;
    ScreenObject := Boundary.ScreenObject as TScreenObject;
    if Boundary is TFhbFlowBoundary then
    begin
      case TFhbFlowBoundary(Boundary).FormulaInterpretation of
        fiSpecific:
          begin
            if ScreenObject.ScreenObjectLength = 0 then
            begin
              result := Item.BoundaryValue;
            end
            else if ScreenObject.Closed then
            begin
              result := '(' + Item.BoundaryValue
                + ') * ' + StrObjectIntersectArea;
            end
            else
            begin
              result := '(' + Item.BoundaryValue
                + ') * ' + StrObjectSectionIntersectLength;
            end;
          end;
        fiDirect:
          begin
            result := Item.BoundaryValue;
          end;
        fiTotal:
          begin
            if ScreenObject.ScreenObjectLength = 0 then
            begin
              result := Item.BoundaryValue;
            end
            else if ScreenObject.Closed then
            begin
              result := '((' + Item.BoundaryValue
                + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
            end
            else
            begin
              result := '((' + Item.BoundaryValue
                + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
            end;
          end;
        else Assert(False);
      end;
    end
    else
    begin
      result := Item.BoundaryValue;
    end;
  end
  else
  begin
    result := Item.BoundaryFormula[FormulaIndex];
  end;
end;

procedure TFhbHeadCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject;
  PestName: string; PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  FhbStorage: TFhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  Compiler: TRbwParser;
  NewFormula: string;
begin
  Assert(BoundaryFunctionIndex in [FhbBoundaryValuePosition]);
  Assert(Expression <> nil);

  FhbStorage := BoundaryStorage as TFhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    try
      Expression.Evaluate;
    except on E: EMathError do
      begin
        frmFormulaErrors.AddFormulaError((AScreenObject as TScreenObject).Name,
          '', Expression.Decompile, E.Message);
        Compiler := (AModel as TCustomModel).rpThreeDFormulaCompiler;
        NewFormula := '0';
        Compiler.Compile(NewFormula);
        Expression := Compiler.CurrentExpression;
        Expression.Evaluate;
      end;
    end;
    with FhbStorage.FhbArray[Index] do
    begin
      case BoundaryFunctionIndex of
        FhbBoundaryValuePosition:
          begin
            BoundaryValue := Expression.DoubleResult;
            BoundaryValueAnnotation := ACell.Annotation;
            BoundaryValuePest := PestName;
            BoundaryValuePestSeriesName := PestSeriesName;
            BoundaryValuePestSeriesMethod := PestSeriesMethod;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TFhbHeadCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  FhbStorage: TFhbStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  FhbStorage := BoundaryStorage as TFhbStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with FhbStorage.FhbArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TFhbHeadCollection.DeleteItemsWithZeroDuration;
var
  Item1: TCustomBoundaryItem;
  Item2: TCustomBoundaryItem;
  Index: Integer;
begin
  for Index := Count - 1 downto 1 do
  begin
    Item1 := Items[Index - 1] as TCustomBoundaryItem;
    if not (Item1 is TCustomModflowBoundaryItem) then
    begin
      break;
    end;
    Item2 := Items[Index] as TCustomModflowBoundaryItem;
    if (TCustomModflowBoundaryItem(Item2).StartTime
      < TCustomModflowBoundaryItem(Item1).EndTime) then
    begin
      TCustomModflowBoundaryItem(Item2).StartTime :=
        TCustomModflowBoundaryItem(Item1).EndTime;
    end;
    // Don't delete items with zero duration for FHB package.
//    if (TCustomModflowBoundaryItem(Item2).StartTime
//      >= TCustomModflowBoundaryItem(Item2).EndTime) then
//    begin
//      Delete(Index);
//    end;
  end;
end;

function TFhbHeadCollection.GetItem(Index: Integer): TFhbItem;
begin
  result := inherited Items[Index] as TFhbItem;
end;

class function TFhbHeadCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFhbHeadTimeListLink;
end;

procedure TFhbHeadCollection.InvalidateBoundaryValueData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TFhbHeadTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFhbHeadTimeListLink;
    Link.FBoundaryValueData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TFhbHeadTimeListLink;
        Link.FBoundaryValueData.Invalidate;
      end;
    end;
  end;
end;

class function TFhbHeadCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFhbItem;
end;

procedure TFhbHeadCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFhbStorage).FFhbArray, BoundaryCount);
  inherited;
end;

procedure TFhbHeadCollection.SetItem(Index: Integer; const Value: TFhbItem);
begin
  inherited Items[Index] := Value;
end;

{ TFhbRecord }

procedure TFhbRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, BoundaryValue);
  WriteCompInt(Comp, Strings.IndexOf(BoundaryValueAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryValuePest));
  WriteCompInt(Comp, Strings.IndexOf(BoundaryValuePestSeriesName));
  WriteCompInt(Comp, Ord(BoundaryValuePestSeriesMethod));
end;

procedure TFhbRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(BoundaryValueAnnotation);
  Strings.Add(BoundaryValuePest);
  Strings.Add(BoundaryValuePestSeriesName);
end;

procedure TFhbRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  BoundaryValue := ReadCompReal(Decomp);
  BoundaryValueAnnotation := Annotations[ReadCompInt(Decomp)];
  BoundaryValuePest := Annotations[ReadCompInt(Decomp)];
  BoundaryValuePestSeriesName := Annotations[ReadCompInt(Decomp)];
  BoundaryValuePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TFhbStorage }

procedure TFhbStorage.Clear;
begin
  SetLength(FFhbArray, 0);
  FCleared := True;
end;

function TFhbStorage.GetFhbArray: TFhbArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFhbArray;
end;

procedure TFhbStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFhbArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FFhbArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TFhbStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FFhbArray);
    for Index := 0 to Count - 1 do
    begin
      FFhbArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFhbArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TFhb_Cell }

procedure TFhb_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
end;

function TFhb_Cell.GetBoundaryValue: double;
begin
  result := Values.BoundaryValue;
end;

function TFhb_Cell.GetBoundaryValueAnnotation: string;
begin
  result := Values.BoundaryValueAnnotation;
end;

function TFhb_Cell.GetBoundaryValuePest: string;
begin
  result := Values.BoundaryValuePest;
end;

function TFhb_Cell.GetBoundaryValuePestSeriesMethod: TPestParamMethod;
begin
  result := Values.BoundaryValuePestSeriesMethod;
end;

function TFhb_Cell.GetBoundaryValuePestSeriesName: string;
begin
  result := Values.BoundaryValuePestSeriesName;
end;

function TFhb_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFhb_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TFhb_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TFhb_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TFhb_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    FhbBoundaryValuePosition: result := BoundaryValuePest;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TFhb_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    FhbBoundaryValuePosition: result := BoundaryValuePestSeriesMethod;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TFhb_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    FhbBoundaryValuePosition: result := BoundaryValuePestSeriesName;
    else
    begin
      result := inherited;
      Assert(False);
    end;
  end;
end;

function TFhb_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    FhbBoundaryValuePosition: result := BoundaryValueAnnotation;
    else Assert(False);
  end;
end;

function TFhb_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    FhbBoundaryValuePosition: result := BoundaryValue;
    else Assert(False);
  end;
end;

function TFhb_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TFhb_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TFhb_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Fhb_Cell: TFhb_Cell;
begin
  result := AnotherCell is TFhb_Cell;
  if result then
  begin
    Fhb_Cell := TFhb_Cell(AnotherCell);
    result :=
      (BoundaryValue = Fhb_Cell.BoundaryValue)
      and (IFace = Fhb_Cell.IFace);
  end;
end;

procedure TFhb_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TFhb_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
end;

procedure TFhb_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;

end;

procedure TFhb_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TFhb_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TCustomFhbBoundary }

procedure TFhbFlowBoundary.Assign(Source: TPersistent);
begin
  if Source is TFhbFlowBoundary then
  begin
    FormulaInterpretation :=
      TFhbFlowBoundary(Source).FormulaInterpretation;
  end;
  inherited;
end;

procedure TFhbHeadBoundary.Assign(Source: TPersistent);
var
  FhbSource: TFhbHeadBoundary;
begin
  if Source is TFhbHeadBoundary then
  begin
    FhbSource := TFhbHeadBoundary(Source);
    PestFhbBoundaryFormula := FhbSource.PestFhbBoundaryFormula;
    PestFhbBoundaryMethod := FhbSource.PestFhbBoundaryMethod;
  end;
  inherited;

end;

procedure TFhbHeadBoundary.AssignCells(
  BoundaryStorage: TCustomBoundaryStorage; ValueTimeList: TList;
  AModel: TBaseModel);
var
  Cell: TFhb_Cell;
  PriorCell: TFhb_Cell;
  BoundaryValues: TFhbRecord;
  BoundaryIndex: Integer;
  Cells: TFhbCellList;
  PriorCells: TFhbCellList;
  TestCells: TFhbCellList;
  LocalBoundaryStorage: TFhbStorage;
  LocalModel: TCustomModel;
  TIndex: Integer;
  PriorIndex: Integer;
  InterpolatedAnnotation: string;
  LocalScreenObject: TScreenObject;
  TimeIndex: Integer;
  PriorTimeIndex: Integer;
  InterpCells: TFhbCellList;
  InterpCell: TFhb_Cell;
  MatchList: Boolean;
  StartEpsilon: double;
  EndEpsilon: double;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFhbStorage;
  LocalScreenObject := ScreenObject as TScreenObject;
  InterpolatedAnnotation := Format(StrInterpolatedValueA,
    [LocalScreenObject.Name]);
  begin
    Cells := nil;
    PriorCells := nil;
    PriorTimeIndex := -1;
    TimeIndex := -1;
    // Find the list of cells that has the same starting time as
    // LocalBoundaryStorage.
    for TIndex := 0 to ValueTimeList.Count - 1 do
    begin
      TestCells := ValueTimeList[TIndex];
      if TestCells.FStartTime = TestCells.FEndTime then
      begin
        MatchList := (TestCells.FStartTime = LocalBoundaryStorage.StartingTime)
          and (TestCells.FEndTime = LocalBoundaryStorage.EndingTime)
      end
      else
      begin
        StartEpsilon := Abs(LocalBoundaryStorage.StartingTime)/ 1e8;
        EndEpsilon := Abs(LocalBoundaryStorage.EndingTime)/ 1e8;
        MatchList := (TestCells.FStartTime+ StartEpsilon >= LocalBoundaryStorage.StartingTime)
          and (TestCells.FEndTime - EndEpsilon <= LocalBoundaryStorage.EndingTime)
      end;

      if MatchList then
      begin
        Cells := TestCells;
        TimeIndex := TIndex;
        // Find the prior list of cells that contains cells assigned by
        // the current object. In the end all the lists will have the
        // same number of cells and all the cells for one TScreenObject
        // are assigned before handling the next TScreenObject. Therefore,
        // just finding the previous list that has more cells will
        // identify the last list that was used with this TScreenObject.
        for PriorIndex := TIndex -1 downto 0 do
        begin
          TestCells := ValueTimeList[PriorIndex];
          if TestCells.Count > Cells.Count then
          begin
            PriorTimeIndex := PriorIndex;
            PriorCells := TestCells;
            break;
          end;
        end;
        break;
      end;
    end;
    if Cells = nil then
    begin
      Exit;
    end;

    Assert(Cells <> nil);
    if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.FhbArray) then
    begin
      Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.FhbArray)
    end;
    for BoundaryIndex := 0 to Length(LocalBoundaryStorage.FhbArray) - 1 do
    begin
      // Create a FHB cell and assign its properties.
      BoundaryValues := LocalBoundaryStorage.FhbArray[BoundaryIndex];
      Cell := TFhb_Cell.Create;
      Assert(ScreenObject <> nil);
      Cell.IFace := (ScreenObject as TScreenObject).IFace;
      Cells.Add(Cell);
      Cell.Values := BoundaryValues;
      LocalModel.AdjustCellPosition(Cell);
      if (PriorCells <> nil) and ((TimeIndex - PriorTimeIndex) > 1) then
      begin
        // If there are intermediate times between this and the last for
        // which this TScreenObject did not assign values, assign
        // values by interpolation.

        PriorCell := PriorCells[Cells.Count-1] as TFhb_Cell;
        Assert(Cell.Layer = PriorCell.Layer);
        Assert(Cell.Row = PriorCell.Row);
        Assert(Cell.Column = PriorCell.Column);
        for PriorIndex := PriorTimeIndex+1 to TimeIndex - 1 do
        begin
          InterpCells := ValueTimeList[PriorIndex];
          InterpCells.Capacity := PriorCells.Count;
          InterpCell := TFhb_Cell.Create;
          Assert(ScreenObject <> nil);
          InterpCell.IFace := (ScreenObject as TScreenObject).IFace;
          InterpCells.Add(InterpCell);
          Assert(InterpCells.Count = Cells.Count);
  //        InterpCell.StressPeriod := TimeIndex;
          InterpCell.Values := BoundaryValues;
          InterpCell.Values.BoundaryValue := InterpolatedValue(
            PriorCell.Values.BoundaryValue, Cell.Values.BoundaryValue,
            PriorCells.StartTime, Cells.StartTime, InterpCells.StartTime);
          InterpCell.Values.BoundaryValueAnnotation := InterpolatedAnnotation;
          InterpCell.ScreenObject := ScreenObject;
          LocalModel.AdjustCellPosition(InterpCell);
        end;
      end;
    end;
    Cells.Cache;
    if PriorCells <> nil then
    begin
      PriorCells.Cache;
      if ((TimeIndex - PriorTimeIndex) > 1) then
      begin
        for PriorIndex := PriorTimeIndex+1 to TimeIndex - 1 do
        begin
          InterpCells := ValueTimeList[PriorIndex];
          InterpCells.Cache;
        end;
      end;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TFhbHeadBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFhbHeadCollection;
end;

function TFhbHeadBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestFhbHead_';
end;

constructor TFhbHeadBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestFhbBoundaryFormula := '';
  PestFhbBoundaryMethod := DefaultBoundaryMethod(FhbBoundaryValuePosition);
end;

procedure TFhbHeadBoundary.CreateFormulaObjects;
begin
  FPestFhbBoundaryFormula := CreateFormulaObjectBlocks(dso3D);
end;

procedure TFhbHeadBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestBoundaryObserver);
  end;
end;

class function TFhbHeadBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmAdd;
end;

destructor TFhbHeadBoundary.Destroy;
begin
  PestFhbBoundaryFormula := '';

  inherited;
end;

procedure TFhbHeadBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFhbStorage;
  ItemIndex: Integer;
  FhbItem: TFhbItem;
  PriorItem: TFhbItem;
  NextItem: TFhbItem;
  LastTime: double;
begin
  inherited;

  for ItemIndex := 0 to Values.Count - 1 do
  begin
    FhbItem := Values[ItemIndex] as TFhbItem;
    if ItemIndex < Values.Count - 1 then
    begin
      NextItem := Values[ItemIndex+1] as TFhbItem;
      if NextItem.StartTime > FhbItem.StartTime then
      begin
        FhbItem.EndTime := NextItem.StartTime;
      end;
      if FhbItem.StartTime > NextItem.StartTime then
      begin
        FhbItem.StartTime := NextItem.StartTime;
      end;
    end
    else
    begin
      LastTime := (AModel as TCustomModel).ModflowStressPeriods.Last.EndTime;
      if LastTime >= FhbItem.StartTime then
      begin
        FhbItem.EndTime := LastTime;
      end
      else
      begin
        FhbItem.EndTime := FhbItem.StartTime;
      end;
    end;
  end;
  for ItemIndex := Values.Count - 1 downto 1 do
  begin
    FhbItem := Values[ItemIndex] as TFhbItem;
    PriorItem := Values[ItemIndex-1] as TFhbItem;
    if FhbItem.StartTime = PriorItem.StartTime then
    begin
      Values.Delete(ItemIndex);
    end;
  end;

  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFhbStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;

end;

function TFhbHeadBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  case FormulaIndex of
    FhbBoundaryValuePosition:
      begin
        result := PestFhbBoundaryFormula;
      end;
    else
      Assert(False);
  end;
end;

function TFhbHeadBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    FhbBoundaryValuePosition:
      begin
        result := PestFhbBoundaryMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TFhbHeadBoundary.GetPestBoundaryObserver: TObserver;
begin
  if FPestBoundaryObserver = nil then
  begin
    CreateObserver('PestFhbBoundary_', FPestBoundaryObserver, nil);
    FPestBoundaryObserver.OnUpToDateSet := InvalidateBoundaryData;
  end;
  result := FPestBoundaryObserver;
end;

function TFhbHeadBoundary.GetPestFhbBoundaryFormula: string;
begin
  Result := FPestFhbBoundaryFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(FhbBoundaryValuePosition);
  end;
end;

procedure TFhbHeadBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestFhbBoundaryFormula as TObject then
  begin
    if FhbBoundaryValuePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[FhbBoundaryValuePosition]);
    end;
  end;
end;

function TFhbHeadBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestFhb_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFhbHeadBoundary.HandleChangedValue(Observer: TObserver);
begin
//  inherited;
  InvalidateDisplay;
end;

class function TFhbHeadBoundary.InterpolatedValue(Value1, Value2, Time1, Time2,
  Time: Double): double;
begin
  if (Time2 = Time1) or (Time = Time1) or (Value1 = Value2) then
  begin
    result := Value1;
  end
  else if (Time = Time2) then
  begin
    result := Value2;
  end
  else
  begin
    result := (Time-Time1)/(Time2-Time1)*(Value2-Value1) + Value1;
  end;
end;

procedure TFhbHeadBoundary.InvalidateBoundaryData(Sender: TObject);
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
    PhastModel.InvalidateMfFhbHeads(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfFhbHeads(self);
      end;
    end;
  end;
end;

procedure TFhbHeadBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfFhbHeads(self);
    Model.InvalidateMfFhbFlows(self);
  end;
end;

procedure TFhbHeadBoundary.SetFhbBoundaryMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestFhbBoundaryMethod, Value);
end;

procedure TFhbHeadBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    FhbBoundaryValuePosition:
      begin
        PestFhbBoundaryFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TFhbHeadBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    FhbBoundaryValuePosition:
      begin
        PestFhbBoundaryMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TFhbHeadBoundary.SetPestFhbBoundaryFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, FhbBoundaryValuePosition, FPestFhbBoundaryFormula);
end;

class function TFhbFlowBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TFhbFlowCollection;
end;

function TFhbFlowBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestFhbFlow_';
end;

class function TFhbFlowBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := ppmMultiply;
end;

procedure TFhbFlowBoundary.InvalidateBoundaryData(Sender: TObject);
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
    PhastModel.InvalidateMfFhbFlows(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.InvalidateMfFhbFlows(self);
      end;
    end;
  end;
end;

procedure TFhbFlowBoundary.SetFormulaInterpretation(
  const Value: TFormulaInterpretation);
begin
  if FFormulaInterpretation <> Value then
  begin
    FFormulaInterpretation := Value;
    InvalidateModel;
    InvalidateDisplay;
  end;
end;

{ TFhbFlowTimeListLink }

procedure TFhbFlowTimeListLink.CreateTimeLists;
begin
  inherited;
  FBoundaryValueData.NonParamDescription := StrFHBFlows;
  FBoundaryValueData.ParamDescription := StrFHBFlows;
  if Model <> nil then
  begin
    FBoundaryValueData.OnInvalidate := (Model as TCustomModel).InvalidateMfFhbFlows;
  end;
end;

{ TFhbFlowCollection }

class function TFhbFlowCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TFhbFlowTimeListLink;
end;

end.
