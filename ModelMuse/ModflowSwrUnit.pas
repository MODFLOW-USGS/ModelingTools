unit ModflowSwrUnit;

interface

uses
  SysUtils, Classes, ModflowBoundaryUnit, FormulaManagerUnit,
  OrderedCollectionUnit, GoPhastTypes, RbwParser, ModflowCellUnit, ZLib,
  SubscriptionUnit;

type
  TSwrRecord = record
    Cell: TCellLocation;
    SwrValue: double;
    StartingTime: double;
    EndingTime: double;
    SwrValueAnnotation: string;
    SwrValuePestName: string;
    SwrValuePestSeriesName: string;
    SwrValuePestSeriesMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TSwrArray = array of TSwrRecord;

  TSwrStorage = class(TCustomBoundaryStorage)
  private
    FSwrArray: TSwrArray;
    function GetSwrArray: TSwrArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SwrArray: TSwrArray read GetSwrArray;
  end;

  TCustomSwrBoundaryItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(SwrValue).
    FSwrValue: TFormulaObject;
    // See @link(SwrValue).
    procedure SetSwrValue(const Value: string);
    function GetSwrValue: string;
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
//    procedure InvalidateModel; override;
    function BoundaryFormulaCount: integer; override;
  public
    Destructor Destroy; override;
  published
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    // @name is the formula used to set the pumping rate
    // or the pumping rate multiplier of this boundary.
    property SwrValue: string read GetSwrValue write SetSwrValue;
  end;

  TCustomSwrTimeListLink = class(TTimeListsModelLink)
  private
    FSwrData: TModflowTimeList;
  public
    Destructor Destroy; override;
  end;

  TCustomSwrListCollection = class(TCustomListArrayBoundColl)
  protected
    procedure InvalidateCollectionData(Sender: TObject);
  strict protected
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure HandleError(E: EMathError; ValueStorage: TSwrStorage;
     ACell: TObject; Index: Integer; AModel: TBaseModel); virtual; abstract;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string; PestSeriesName: string;
      PestSeriesMethod: TPestParamMethod; TimeSeriesName: string); override;
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList; PestItemNames: TStringListObjectList); override;
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames: TStringListObjectList; Writer: TObject); override;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual; abstract;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
//    class function ItemClass: TBoundaryItemClass; override;
  end;

  TSwrValueCell = class(TValueCell)
  private
    Values: TSwrRecord;
    StressPeriod: integer;
    function GetSwrValue: double;
    function GetSwrValueAnnotation: string;
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
    property SwrValue: double read GetSwrValue;
    property SwrValueAnnotation: string read GetSwrValueAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

  TCustomSwrBoundary = class(TModflowBoundary)
  private
    FPestValueMethod: TPestParamMethod;
    FPestValueFormula: TFormulaObject;
    FUsedObserver: TObserver;
    FPestValueObserver: TObserver;
    function GetPestValueFormula: string;
    procedure SetPestValueFormula(const Value: string);
    procedure SetPestValueMethod(const Value: TPestParamMethod);
    function GetPestValueObserver: TObserver;
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TSwrValueCell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    function SpecificationMethod: TSwrSpecificationMethod; virtual; abstract;
    // PEST
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
    procedure InvalidateValueData(Sender: TObject); virtual; abstract;
    property PestValueObserver: TObserver
      read GetPestValueObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure Assign(Source: TPersistent);override;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
  published
    property PestValueFormula: string
      read GetPestValueFormula
      write SetPestValueFormula
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
    property PestValueMethod: TPestParamMethod
      read FPestValueMethod
      write SetPestValueMethod
      {$IFNDEF PEST}
      Stored False
      {$ENDIF}
      ;
  end;

  TCustomFormulaInterpSwrBoundary = class(TCustomSwrBoundary)
  private
    FFormulaInterpretation: TFormulaInterpretation;
    procedure SetFormulaInterpretation(const Value: TFormulaInterpretation);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property FormulaInterpretation: TFormulaInterpretation
      read FFormulaInterpretation write SetFormulaInterpretation;
  end;

  TSwrRainItem = class(TCustomSwrBoundaryItem)
  protected
    procedure InvalidateModel; override;
  end;

  TMfSwrRainTimeListLink = class(TCustomSwrTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSwrRainListCollection = class(TCustomSwrListCollection)
  protected
    procedure HandleError(E: EMathError; ValueStorage: TSwrStorage;
     ACell: TObject; Index: Integer; AModel: TBaseModel); override;
  strict protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure InvalidateModel; override;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TSwrRainBoundary = class(TCustomSwrBoundary)
  protected
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    function SpecificationMethod: TSwrSpecificationMethod; override;
    procedure InvalidateValueData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
  end;

  TSwrEvapItem = class(TCustomSwrBoundaryItem)
  protected
    procedure InvalidateModel; override;
  end;

  TMfSwrEvapTimeListLink = class(TCustomSwrTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSwrEvapListCollection = class(TCustomSwrListCollection)
  protected
    procedure HandleError(E: EMathError; ValueStorage: TSwrStorage;
     ACell: TObject; Index: Integer; AModel: TBaseModel); override;
  strict protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure InvalidateModel; override;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TSwrEvapBoundary = class(TCustomSwrBoundary)
  protected
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    function SpecificationMethod: TSwrSpecificationMethod; override;
    procedure InvalidateValueData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
  end;

  TSwrLatInflowItem = class(TCustomSwrBoundaryItem)
  protected
    procedure InvalidateModel; override;
  end;

  TMfSwrLatInflowTimeListLink = class(TCustomSwrTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSwrLatInflowListCollection = class(TCustomSwrListCollection)
  protected
    procedure HandleError(E: EMathError; ValueStorage: TSwrStorage;
     ACell: TObject; Index: Integer; AModel: TBaseModel); override;
  strict protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure InvalidateModel; override;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TSwrLatInflowBoundary = class(TCustomFormulaInterpSwrBoundary)
  protected
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    function SpecificationMethod: TSwrSpecificationMethod; override;
    procedure InvalidateValueData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
  end;

  TSwrStageItem = class(TCustomSwrBoundaryItem)
  protected
    procedure InvalidateModel; override;
  end;

  TMfSwrStageTimeListLink = class(TCustomSwrTimeListLink)
  protected
    procedure CreateTimeLists; override;
  end;

  TSwrStageListCollection = class(TCustomSwrListCollection)
  protected
    procedure HandleError(E: EMathError; ValueStorage: TSwrStorage;
     ACell: TObject; Index: Integer; AModel: TBaseModel); override;
  strict protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure InvalidateModel; override;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
  end;

  TSwrStageBoundary = class(TCustomSwrBoundary)
  protected
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    function SpecificationMethod: TSwrSpecificationMethod; override;
    procedure InvalidateValueData(Sender: TObject); override;
  public
    procedure InvalidateDisplay; override;
  end;

const
  SwrValuePosition = 0;

implementation

uses
  frmGoPhastUnit, PhastModelUnit,
  frmErrorsAndWarningsUnit, DataSetUnit, AbstractGridUnit, ModflowTimeUnit,
  ScreenObjectUnit, GIS_Functions;

resourcestring
  StrRainFormulaError = 'Rain rate set to zero because of a math error';
  StrEvapFormulaError = 'Evaporation rate set to zero because of a math error';
  StrLatInflowFormulaError = 'Lateral inflow rate set to zero because of a math error';
  StrStageFormulaError = 'Stage set to zero because of a math error';
  StrSWRRainRAINRAIN2 = 'SWR Rain (RAIN/RAIN2D)';
  StrSwrRain = ' swr rain';
  StrSWREvaporationEVA = 'SWR Evaporation (EVAP/EVAP2D)';
  StrSwrEvaporation = ' swr evaporation';
  StrSWRLateralInflowQLATFLOW = 'SWR Lateral inflow (QLATFLOW/QLATFLOW2D)';
  StrSwrLateralInflow = ' swr lateral inflow';
  StrSWRStageSTAGE2D = 'SWR Stage (STAGE2D)';
  StrSwrStage = ' swr stage';

  { TSwrBoundaryItem }

procedure TCustomSwrBoundaryItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomSwrBoundaryItem then
  begin
    SwrValue := TCustomSwrBoundaryItem(Source).SwrValue;
  end;
  inherited;
end;

procedure TCustomSwrBoundaryItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TCustomSwrListCollection;
  SwrObserver: TObserver;
begin
  ParentCollection := Collection as TCustomSwrListCollection;
  SwrObserver := FObserverList[SwrValuePosition];
  SwrObserver.OnUpToDateSet := ParentCollection.InvalidateCollectionData;
end;

function TCustomSwrBoundaryItem.BoundaryFormulaCount: integer;
begin
  result := 1;
end;

procedure TCustomSwrBoundaryItem.CreateFormulaObjects;
begin
  FSwrValue := CreateFormulaObject(dso3D);
end;

destructor TCustomSwrBoundaryItem.Destroy;
begin
  SwrValue := '0';
  inherited;
end;

function TCustomSwrBoundaryItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    SwrValuePosition: result := SwrValue;
    else Assert(False);
  end;
end;

procedure TCustomSwrBoundaryItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  Assert(Sender = FSwrValue);
  List.Add(FObserverList[SwrValuePosition]);
end;

function TCustomSwrBoundaryItem.GetSwrValue: string;
begin
  Result := FSwrValue.Formula;
  ResetItemObserver(SwrValuePosition);
end;

procedure TSwrRainItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrRainfall(self);
  end;
end;

function TCustomSwrBoundaryItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TCustomSwrBoundaryItem;
begin
  result := (AnotherItem is TCustomSwrBoundaryItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TCustomSwrBoundaryItem(AnotherItem);
    result := (Item.SwrValue = SwrValue)
  end;
end;

procedure TCustomSwrBoundaryItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FSwrValue,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TCustomSwrBoundaryItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  inherited;
  case Index of
    SwrValuePosition: SwrValue := Value;
    else Assert(False);
  end;
end;

procedure TCustomSwrBoundaryItem.SetSwrValue(const Value: string);
begin
  UpdateFormulaBlocks(Value, SwrValuePosition, FSwrValue);
end;

{ TCustomSwrListCollection }

procedure TCustomSwrListCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TSwrStorage.Create(AModel));
end;

function TCustomSwrListCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TCustomFormulaInterpSwrBoundary;
  Item: TCustomSwrBoundaryItem;
  ScreenObject: TScreenObject;
begin
  result := '';
  if FormulaIndex = SwrValuePosition then
  begin
    Item := Items[ItemIndex] as TCustomSwrBoundaryItem;
    if BoundaryGroup is TCustomFormulaInterpSwrBoundary then
    begin
      Boundary := TCustomFormulaInterpSwrBoundary(BoundaryGroup);
      ScreenObject := Boundary.ScreenObject as TScreenObject;
      case Boundary.FormulaInterpretation of
        fiSpecific:
          begin
            if ScreenObject.ScreenObjectLength = 0 then
            begin
              result := Item.SwrValue;
            end
            else if ScreenObject.Closed then
            begin
              result := '(' + Item.SwrValue
                + ') * ' + StrObjectIntersectArea;
            end
            else
            begin
              result := '(' + Item.SwrValue
                + ') * ' + StrObjectSectionIntersectLength;
            end;
          end;
        fiDirect:
          begin
            result := Item.SwrValue;
          end;
        fiTotal:
          begin
            if ScreenObject.ScreenObjectLength = 0 then
            begin
              result := Item.SwrValue;
            end
            else if ScreenObject.Closed then
            begin
              result := '((' + Item.SwrValue
                + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
            end
            else
            begin
              result := '((' + Item.SwrValue
                + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
            end;
          end;
        else Assert(False);
      end;
    end
    else
    begin
      result := Item.SwrValue;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TCustomSwrListCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames: TStringListObjectList);
var
  SwrArray: TDataArray;
  Boundary: TSwrStorage;
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
  LocalValuePestSeries: string;
  LocalValuePestMethod: TPestParamMethod;
  ValuePestItems: TStringList;
  LocalValuePest: string;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  SwrArray := DataSets[SwrValuePosition];
  Boundary := Boundaries[ItemIndex, AModel] as TSwrStorage;
  SwrArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);

  LocalValuePestSeries := PestSeries[SwrValuePosition];
  LocalValuePestMethod := PestMethods[SwrValuePosition];
  ValuePestItems := PestItemNames[SwrValuePosition];
  LocalValuePest := ValuePestItems[ItemIndex];


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
            if SwrArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              with Boundary.SwrArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                SwrValue := SwrArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                SwrValueAnnotation := SwrArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                SwrValuePestName := LocalValuePest;
                SwrValuePestSeriesName := LocalValuePestSeries;
                SwrValuePestSeriesMethod := LocalValuePestMethod;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  SwrArray.CacheData;
  Boundary.CacheData;
end;

procedure TCustomSwrListCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string;
  PestSeriesName: string; PestSeriesMethod: TPestParamMethod;
  TimeSeriesName: string);
var
  ValueStorage: TSwrStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Assert(BoundaryFunctionIndex = 0);
  Assert(Expression <> nil);

  ValueStorage := BoundaryStorage as TSwrStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    try
      Expression.Evaluate;
      with ValueStorage.SwrArray[Index] do
      begin
        SwrValue := Expression.DoubleResult;
        SwrValueAnnotation := ACell.Annotation;
        SwrValuePestName := PestName;
        SwrValuePestSeriesName := PestSeriesName;
        SwrValuePestSeriesMethod := PestSeriesMethod;
      end;
    except on E: EMathError do
      begin
        HandleError(E, ValueStorage, ACell, Index, AModel);
      end;
    end;
  end;
end;

procedure TCustomSwrListCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  SwrStorage: TSwrStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  SwrStorage := BoundaryStorage as TSwrStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with SwrStorage.SwrArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

procedure TSwrRainListCollection.HandleError(E: EMathError;
  ValueStorage: TSwrStorage; ACell: TObject; Index: Integer; AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalCell: TCellAssignment;
begin
  with ValueStorage.SwrArray[Index] do
  begin
    SwrValue := 0;
    SwrValueAnnotation := StrRainFormulaError;
  end;
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalCell := ACell as TCellAssignment;
  frmErrorsAndWarnings.AddError(AModel, StrRainFormulaError,
    Format(StrObject0sLayerError, [LocalScreenObject.Name,
    LocalCell.Layer + 1, LocalCell.Row + 1, LocalCell.Column + 1, E.Message]),
    LocalScreenObject);
end;

function TSwrRainListCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMfSwrRainTimeListLink;
end;

procedure TCustomSwrListCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TCustomSwrBoundaryItem;
  ScreenObject: TScreenObject;
  ALink: TCustomSwrTimeListLink;
  SwrData: TModflowTimeList;
  DataArrayIndex: Integer;
  DataArray: TTransientRealSparseDataSet;
  Grid: TCustomModelGrid;
  RowIndex: Integer;
  ColIndex: Integer;
  LayerIndex: Integer;
  ShouldRemove: Boolean;
  PestValueSeriesName: string;
  ValueMethod: TPestParamMethod;
  ValueItems: TStringList;
  ItemFormula: string;
begin
  ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
  SetLength(BoundaryValues, Count);

  PestValueSeriesName := BoundaryGroup.PestBoundaryFormula[SwrValuePosition];
  PestSeries.Add(PestValueSeriesName);
  ValueMethod := BoundaryGroup.PestBoundaryMethod[SwrValuePosition];
  PestMethods.Add(ValueMethod);
  ValueItems := TStringList.Create;
  PestItemNames.Add(ValueItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TCustomSwrBoundaryItem;
    BoundaryValues[Index].Time := Item.StartTime;
//    BoundaryValues[Index].Formula := Item.SwrValue;
    ItemFormula := Item.SwrValue;
    AssignBoundaryFormula(AModel, PestValueSeriesName, ValueMethod,
      ValueItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  ALink := TimeListLink.GetLink(AModel) as TCustomSwrTimeListLink;
  SwrData := ALink.FSwrData;
  SwrData.Initialize(BoundaryValues, ScreenObject, lctUse);
  Assert(SwrData.Count = Count);

  if PackageAssignmentMethod(AModel) = umAdd then
  begin
    Grid := (AModel as TCustomModel).Grid;
    for DataArrayIndex := 0 to SwrData.Count - 1 do
    begin
      DataArray := SwrData[DataArrayIndex] as TTransientRealSparseDataSet;
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        for ColIndex := 0 to Grid.ColumnCount - 1 do
        begin
          ShouldRemove := False;
          for LayerIndex := Grid.LayerCount -1 downto 0 do
          begin
            if ShouldRemove then
            begin
              DataArray.RemoveValue(LayerIndex, RowIndex, ColIndex);
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
  SetBoundaryCapacity(SwrData.Count, AModel);
  for TimeIndex := 0 to SwrData.Count - 1 do
  begin
    AddBoundary(TSwrStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(SwrData);
end;

procedure TCustomSwrListCollection.InvalidateCollectionData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TCustomSwrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TCustomSwrTimeListLink;
    Link.FSwrData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TCustomSwrTimeListLink;
      Link.FSwrData.Invalidate;
    end;
  end;
end;

procedure TSwrRainListCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrRainfall(self);
  end;
end;

class function TSwrRainListCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSwrRainItem;
end;

//class function TCustomSwrListCollection.ItemClass: TBoundaryItemClass;
//begin
//  result := TCustomSwrBoundaryItem;
//end;

function TSwrRainListCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.SwrPackage.RainAssignmentMethod
end;

procedure TCustomSwrListCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSwrStorage).FSwrArray, BoundaryCount);
  inherited;
end;

{ TRainRecord }

procedure TSwrRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, SwrValue);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, Strings.IndexOf(SwrValueAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SwrValuePestName));
  WriteCompInt(Comp, Strings.IndexOf(SwrValuePestSeriesName));
  WriteCompInt(Comp, Ord(SwrValuePestSeriesMethod));
end;

procedure TSwrRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(SwrValueAnnotation);
  Strings.Add(SwrValuePestName);
  Strings.Add(SwrValuePestSeriesName);
end;

procedure TSwrRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  SwrValue := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  SwrValueAnnotation := Annotations[ReadCompInt(Decomp)];
  SwrValuePestName := Annotations[ReadCompInt(Decomp)];
  SwrValuePestSeriesName := Annotations[ReadCompInt(Decomp)];
  SwrValuePestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TRainStorage }

procedure TSwrStorage.Clear;
begin
  SetLength(FSwrArray, 0);
  FCleared := True;
end;

function TSwrStorage.GetSwrArray: TSwrArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSwrArray;
end;

procedure TSwrStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSwrArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSwrArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TSwrStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FSwrArray);
    for Index := 0 to Count - 1 do
    begin
      FSwrArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FSwrArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TMfWelTimeListLink }

procedure TMfSwrRainTimeListLink.CreateTimeLists;
begin
  inherited;
  FSwrData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSwrData.NonParamDescription := StrSWRRainRAINRAIN2;
  FSwrData.ParamDescription := StrSwrRain;
  FSwrData.Orientation := dsoTop;
  if Model <> nil then
  begin
    FSwrData.OnInvalidate := (Model as TCustomModel).InvalidateMfSwrRainfall;
  end;
  AddTimeList(FSwrData);
end;

destructor TCustomSwrTimeListLink.Destroy;
begin
  FSwrData.Free;
  inherited;
end;

{ TSwrRain_Cell }

procedure TSwrValueCell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TSwrValueCell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSwrValueCell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSwrValueCell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSwrValueCell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSwrValueCell.GetPestName(Index: Integer): string;
begin
  case Index of
    SwrValuePosition:
      begin
        result := Values.SwrValuePestName;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSwrValueCell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    SwrValuePosition:
      begin
        result := Values.SwrValuePestSeriesMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSwrValueCell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    SwrValuePosition:
      begin
        result := Values.SwrValuePestSeriesName;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TSwrValueCell.GetSwrValue: double;
begin
  result := Values.SwrValue;
end;

function TSwrValueCell.GetSwrValueAnnotation: string;
begin
  result := Values.SwrValueAnnotation;
end;

function TSwrValueCell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := SwrValueAnnotation;
    else Assert(False);
  end;
end;

function TSwrValueCell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := SwrValue;
    else Assert(False);
  end;
end;

function TSwrValueCell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSwrValueCell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TSwrValueCell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  RainCell: TSwrValueCell;
begin
  result := AnotherCell is TSwrValueCell;
  if result then
  begin
    RainCell := TSwrValueCell(AnotherCell);
    result :=
      (SwrValue = RainCell.SwrValue)
      and (IFace = RainCell.IFace);
  end;
end;

procedure TSwrValueCell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TSwrValueCell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TSwrValueCell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TSwrValueCell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TSwrValueCell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TSwrRainBoundary }

procedure TCustomFormulaInterpSwrBoundary.Assign(Source: TPersistent);
begin
  if Source is TCustomFormulaInterpSwrBoundary then
  begin
    FormulaInterpretation := TCustomFormulaInterpSwrBoundary(Source).FormulaInterpretation
  end;
  inherited;
end;

procedure TCustomSwrBoundary.Assign(Source: TPersistent);
var
  SwrSource: TCustomSwrBoundary;
begin
  if Source is TCustomSwrBoundary then
  begin
    SwrSource := TCustomSwrBoundary(Source);
    PestValueFormula := SwrSource.PestValueFormula;
    PestValueMethod := SwrSource.PestValueMethod;
  end;
  inherited;
end;

procedure TCustomSwrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TSwrValueCell;
  BoundaryValues: TSwrRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSwrStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TSwrStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TSwrValueCell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.SwrArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.SwrArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.SwrArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.SwrArray[BoundaryIndex];
        Cell := TSwrValueCell.Create;
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

class function TSwrRainBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSwrRainListCollection;
end;

function TCustomSwrBoundary.BoundaryObserverPrefix: string;
begin
  result := 'PestSwr_';
end;

constructor TCustomSwrBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  PestValueFormula := '';
  PestValueMethod := DefaultBoundaryMethod(SwrValuePosition);

end;

procedure TCustomSwrBoundary.CreateFormulaObjects;
begin
  FPestValueFormula := CreateFormulaObjectBlocks(dsoTop);
end;

procedure TCustomSwrBoundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(PestValueObserver);
  end;
end;

class function TCustomSwrBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    SwrValuePosition:
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

destructor TCustomSwrBoundary.Destroy;
begin
  PestValueFormula := '';

  inherited;
end;

procedure TCustomSwrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TSwrStorage;
begin
  case SpecificationMethod of
    smObject:
      begin
        EvaluateListBoundaries(AModel);
      end;
    smArray:
      begin
        EvaluateArrayBoundaries(AModel, Writer);
      end;
    else
      Assert(False);
  end;
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TSwrStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
end;

function TCustomSwrBoundary.GetPestBoundaryFormula(
  FormulaIndex: integer): string;
begin
  case FormulaIndex of
    SwrValuePosition:
      begin
        result := PestValueFormula;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TCustomSwrBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  case FormulaIndex of
    SwrValuePosition:
      begin
        result := PestValueMethod;
      end;
    else
      begin
        result := inherited;
        Assert(False);
      end;
  end;
end;

function TCustomSwrBoundary.GetPestValueFormula: string;
begin
  Result := FPestValueFormula.Formula;
  if ScreenObject <> nil then
  begin
    ResetBoundaryObserver(SwrValuePosition);
  end;
end;

function TCustomSwrBoundary.GetPestValueObserver: TObserver;
begin
  if FPestValueObserver = nil then
  begin
    CreateObserver('PestValue_', FPestValueObserver, nil);
    FPestValueObserver.OnUpToDateSet := InvalidateValueData;
  end;
  result := FPestValueObserver;
end;

procedure TCustomSwrBoundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FPestValueFormula then
  begin
    if SwrValuePosition < FObserverList.Count then
    begin
      List.Add(FObserverList[SwrValuePosition]);
    end;
  end;
end;

function TCustomSwrBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver('PestSwr_Used_', FUsedObserver, nil);
//    FUsedObserver.OnUpToDateSet := HandleChangedValue;
  end;
  result := FUsedObserver;
end;

procedure TCustomSwrBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

procedure TCustomSwrBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  case FormulaIndex of
    SwrValuePosition:
      begin
        PestValueFormula := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TCustomSwrBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  case FormulaIndex of
    SwrValuePosition:
      begin
        PestValueMethod := Value;
      end;
    else
      begin
        inherited;
        Assert(False);
      end;
  end;
end;

procedure TCustomSwrBoundary.SetPestValueFormula(const Value: string);
begin
  UpdateFormulaBlocks(Value, SwrValuePosition, FPestValueFormula);
end;

procedure TCustomSwrBoundary.SetPestValueMethod(const Value: TPestParamMethod);
begin
  SetPestParamMethod(FPestValueMethod, Value);
end;

procedure TSwrRainBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    (ParentModel as TPhastModel).InvalidateMfSwrRainfall(self);
  end;
end;

procedure TSwrRainBoundary.InvalidateValueData(Sender: TObject);
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
    PhastModel.InvalidateMfSwrRainfall(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfSwrRainfall(self);
    end;
  end;
end;

procedure TCustomFormulaInterpSwrBoundary.SetFormulaInterpretation(
  const Value: TFormulaInterpretation);
begin
  if FFormulaInterpretation <> Value then
  begin
    FFormulaInterpretation := Value;
    InvalidateModel;
    InvalidateDisplay;
  end;
end;

function TSwrRainBoundary.SpecificationMethod: TSwrSpecificationMethod;
begin
  result := frmGoPhast.PhastModel.ModflowPackages.SwrPackage.RainSpecification;
end;

{ TMfSwrEvapTimeListLink }

procedure TMfSwrEvapTimeListLink.CreateTimeLists;
begin
  inherited;
  FSwrData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSwrData.NonParamDescription := StrSWREvaporationEVA;
  FSwrData.ParamDescription := StrSwrEvaporation;
  FSwrData.Orientation := dsoTop;
  if Model <> nil then
  begin
    FSwrData.OnInvalidate := (Model as TCustomModel).InvalidateMfSwrEvaporation;
  end;
  AddTimeList(FSwrData);
end;

{ TSwrEvapListCollection }

function TSwrEvapListCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMfSwrEvapTimeListLink;
end;

procedure TSwrEvapListCollection.HandleError(E: EMathError;
  ValueStorage: TSwrStorage; ACell: TObject; Index: Integer;
  AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalCell: TCellAssignment;
begin
  with ValueStorage.SwrArray[Index] do
  begin
    SwrValue := 0;
    SwrValueAnnotation := StrEvapFormulaError;
  end;
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalCell := ACell as TCellAssignment;
  frmErrorsAndWarnings.AddError(AModel, StrEvapFormulaError,
    Format(StrObject0sLayerError, [LocalScreenObject.Name,
    LocalCell.Layer + 1, LocalCell.Row + 1, LocalCell.Column + 1, E.Message]),
    LocalScreenObject);
end;

procedure TSwrEvapListCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrEvaporation(self);
  end;
end;

class function TSwrEvapListCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSwrEvapItem;
end;

function TSwrEvapListCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.SwrPackage.EvapAssignmentMethod
end;

{ TSwrEvapBoundary }

class function TSwrEvapBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSwrEvapListCollection;
end;

procedure TSwrEvapBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    if (ParentModel as TPhastModel).Clearing then
    begin
      Exit;
    end;
    (ParentModel as TPhastModel).InvalidateMfSwrEvaporation(self);
  end;
end;

procedure TSwrEvapBoundary.InvalidateValueData(Sender: TObject);
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
    PhastModel.InvalidateMfSwrEvaporation(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfSwrEvaporation(self);
    end;
  end;
end;

function TSwrEvapBoundary.SpecificationMethod: TSwrSpecificationMethod;
begin
  result := frmGoPhast.PhastModel.ModflowPackages.SwrPackage.EvapSpecification;
end;

{ TMfSwrLatInflowTimeListLink }

procedure TMfSwrLatInflowTimeListLink.CreateTimeLists;
begin
  inherited;
  FSwrData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSwrData.NonParamDescription := StrSWRLateralInflowQLATFLOW;
  FSwrData.ParamDescription := StrSwrLateralInflow;
  FSwrData.Orientation := dsoTop;
  if Model <> nil then
  begin
    FSwrData.OnInvalidate := (Model as TCustomModel).InvalidateMfSwrLateralInflow;
  end;
  AddTimeList(FSwrData);
end;

{ TSwrLatInflowListCollection }

function TSwrLatInflowListCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMfSwrLatInflowTimeListLink
end;

procedure TSwrLatInflowListCollection.HandleError(E: EMathError;
  ValueStorage: TSwrStorage; ACell: TObject; Index: Integer;
  AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalCell: TCellAssignment;
begin
  with ValueStorage.SwrArray[Index] do
  begin
    SwrValue := 0;
    SwrValueAnnotation := StrLatInflowFormulaError;
  end;
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalCell := ACell as TCellAssignment;
  frmErrorsAndWarnings.AddError(AModel, StrLatInflowFormulaError,
    Format(StrObject0sLayerError, [LocalScreenObject.Name,
    LocalCell.Layer + 1, LocalCell.Row + 1, LocalCell.Column + 1, E.Message]),
    LocalScreenObject);
end;

procedure TSwrLatInflowListCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrLateralInflow(self);
  end;
end;

class function TSwrLatInflowListCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TSwrLatInflowItem;
end;

function TSwrLatInflowListCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.SwrPackage.LatInflowAssignmentMethod
end;

{ TSwrLatInflowBoundary }

class function TSwrLatInflowBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSwrLatInflowListCollection;
end;

procedure TSwrLatInflowBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    if (ParentModel as TPhastModel).Clearing then
    begin
      Exit;
    end;
    (ParentModel as TPhastModel).InvalidateMfSwrLateralInflow(self);
  end;
end;

procedure TSwrLatInflowBoundary.InvalidateValueData(Sender: TObject);
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
    PhastModel.InvalidateMfSwrLateralInflow(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfSwrLateralInflow(self);
    end;
  end;
end;

function TSwrLatInflowBoundary.SpecificationMethod: TSwrSpecificationMethod;
begin
  result := frmGoPhast.PhastModel.ModflowPackages.SwrPackage.LateralInflowSpecification;
end;

{ TMfSwrStageTimeListLink }

procedure TMfSwrStageTimeListLink.CreateTimeLists;
begin
  inherited;
  FSwrData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSwrData.NonParamDescription := StrSWRStageSTAGE2D;
  FSwrData.ParamDescription := StrSwrStage;
  FSwrData.Orientation := dsoTop;
  if Model <> nil then
  begin
    FSwrData.OnInvalidate := (Model as TCustomModel).InvalidateMfSwrStage;
  end;
  AddTimeList(FSwrData);
end;

{ TSwrStageListCollection }

function TSwrStageListCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TMfSwrStageTimeListLink;
end;

procedure TSwrStageListCollection.HandleError(E: EMathError;
  ValueStorage: TSwrStorage; ACell: TObject; Index: Integer;
  AModel: TBaseModel);
var
  LocalScreenObject: TScreenObject;
  LocalCell: TCellAssignment;
begin
  with ValueStorage.SwrArray[Index] do
  begin
    SwrValue := 0;
    SwrValueAnnotation := StrStageFormulaError;
  end;
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalCell := ACell as TCellAssignment;
  frmErrorsAndWarnings.AddError(AModel, StrStageFormulaError,
    Format(StrObject0sLayerError, [LocalScreenObject.Name,
    LocalCell.Layer + 1, LocalCell.Row + 1, LocalCell.Column + 1, E.Message]),
    LocalScreenObject);
end;

procedure TSwrStageListCollection.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrStage(self);
  end;
end;

class function TSwrStageListCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TSwrStageItem;
end;

function TSwrStageListCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
var
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  result := LocalModel.ModflowPackages.SwrPackage.StageAssignmentMethod
end;

{ TSwrStageBoundary }

class function TSwrStageBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSwrStageListCollection;
end;

procedure TSwrStageBoundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    if (ParentModel as TPhastModel).Clearing then
    begin
      Exit;
    end;
    (ParentModel as TPhastModel).InvalidateMfSwrStage(self);
  end;
end;

procedure TSwrStageBoundary.InvalidateValueData(Sender: TObject);
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
    PhastModel.InvalidateMfSwrStage(self);

    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      ChildModel.InvalidateMfSwrStage(self);
    end;
  end;
end;

function TSwrStageBoundary.SpecificationMethod: TSwrSpecificationMethod;
begin
  result := frmGoPhast.PhastModel.ModflowPackages.SwrPackage.StageSpecification;
end;

{ TSwrEvapItem }

procedure TSwrEvapItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrEvaporation(self);
  end;
end;

{ TSwrLatInflowItem }

procedure TSwrLatInflowItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrLateralInflow(self);
  end;
end;

{ TSwrStageItem }

procedure TSwrStageItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfSwrStage(self);
  end;
end;

end.
