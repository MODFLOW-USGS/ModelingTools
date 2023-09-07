unit ModflowFmp4LandUseBoundaryUnit;

interface

uses Windows, ZLib, SysUtils, Classes, OrderedCollectionUnit,
  ModflowBoundaryUnit, DataSetUnit, ModflowCellUnit,
  SubscriptionUnit, GoPhastTypes, System.Generics.Collections,
  RbwParser, ScreenObjectInterfaceUnit;

type
  {
    @name stores the location, time and reference evapotranspiration rate for a cell.
  }
  TFmp4LandUseRecord = record
    Cell: TCellLocation;
    StartingTime: double;
    EndingTime: double;
    LandUseData: TLandUseData;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  // @name is an array of @link(TFmp4LandUseRecord)s.
  TFmp4LandUseArray = array of TFmp4LandUseRecord;

  // @name extends @link(TCustomBoundaryStorage) by adding the locations
  // and values of series of reference evapotranspiration cells.
  TFmp4LandUseStorage = class(TCustomBoundaryStorage)
  private
    FFmp4LandUseArray: TFmp4LandUseArray;
    function GetFmp4LandUseArray: TFmp4LandUseArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream;
      Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property Fmp4LandUseArray: TFmp4LandUseArray read GetFmp4LandUseArray;
  end;


  // @name is used in FMP4 Land Use
  TLandUseStringValueItem = class(TCustomStringValueItem)
    constructor Create(Collection: TCollection); override;
  end;

  // @name is used in FMP4 Land Use
  TLandUseStringCollection = class(TCustomStringCollection)
  private
    FUsedForPestSeries: Boolean;
    function GetLandUseItem(Index: Integer): TLandUseStringValueItem;
    procedure SetLandUseItem(Index: Integer; const Value: TLandUseStringValueItem);
    procedure SetUsedForPestSeries(const Value: Boolean);
  public
    property UsedForPestSeries: Boolean read FUsedForPestSeries
      write SetUsedForPestSeries;
    constructor Create(Model: TBaseModel; AScreenObject: TObject;
      ParentCollection: TCustomMF_BoundColl);
    property Items[Index: Integer]: TLandUseStringValueItem read GetLandUseItem
      write SetLandUseItem; default;
    function Add: TLandUseStringValueItem;
  end;

  // @name represents a MODFLOW land use values for the Farm Process
  // for one time interval.
  // @name is stored by @link(TFmp4LandUseCollection).
  TFmp4LandUseItem = class(TCustomModflowBoundaryItem)
  private
    // See @link(Fmp4LandUseValue).
    FFmp4LandUseValues: TLandUseStringCollection;
    procedure SetFmp4LandUseValues(const Value: TLandUseStringCollection);
    // See @link(Fmp4LandUseValue).
  protected
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure CreateFormulaObjects; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    // Is RemoveFormulaObjects needed?
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
    // @name checks whether AnotherItem is the same as the current @classname.
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function BoundaryFormulaCount: integer; override;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    // @name is the formula used to set the values of this boundary.
    property Fmp4LandUseValues: TLandUseStringCollection read FFmp4LandUseValues
      write SetFmp4LandUseValues;
  end;

  TFmp4LandUseTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the values for a series of
    // cells over a series of time intervals.
    // @name descends from TObjectList.
    FLandUseList: TModflowTimeLists;
    procedure AddLandUseTimeLists(CropIndex: Integer);
    procedure RemoveLandUseTimeLists(CropIndex: Integer);
    procedure UpdateLandUseTimeLists; virtual;
  protected
    // @name must be assigned in Create by descendents.
    FOnInvalidateLanduse: TNotifyEvent;
    procedure CreateTimeLists; override;
    // override CreateTimeLists in descendents
//    procedure CreateTimeLists; override;
    { TODO -cFMP4 : override GetDescription  in each descendent}
    class function GetDescription: string; virtual; abstract;
    // override in descendents
    function MultipleCropsPerCellUsed: Boolean; virtual; abstract;
    function GetDefaultDataType: TRbwDataType; virtual;
  public
    Destructor Destroy; override;
    procedure CreateLandUseTimeLists;
    property DefaultDataType: TRbwDataType read GetDefaultDataType;
  end;

  TFmp4TimeListLinkClass = class of TFmp4LandUseTimeListLink;

  // @name represents MODFLOW Farm Process land use values
  // for a series of time intervals.
  TFmp4LandUseCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateLandUseValues(Sender: TObject);
  protected
    // override in descendents
    function MultipleCropsPerCellUsed: Boolean; virtual; abstract;
    function PackageAssignmentMethod(AModel: TBaseModel): TUpdateMethod; virtual;
    { TODO -cFMP4 : override GetTimeListLinkClass  in each descendent}
    // override this
//    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList; AModel: TBaseModel;
      PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TFmp4LandUseStorage.Fmp4LandUseArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
    procedure CreateLandUseTimeLists(AModel: TBaseModel);
  end;

  TFmp4CollectionClass = class of TFmp4LandUseCollection;

  // Each @name stores a @link(TFmp4LandUseCollection).
  // @classname is stored by @link(TModflowParameters).

  TFmp4LandUse_Cell = class(TValueCell)
  private
    FValues: TFmp4LandUseRecord;
    FStressPeriod: integer;
    function GetLandUseAnnotation(const Index: Integer): string;
    function GetLandUsePestName(const Index: Integer): string;
    function GetLandUsePestSeriesMethod(const Index: Integer): TPestParamMethod;
    function GetLandUsePestSeriesName(const Index: Integer): string;
    function GetLandUseTimeSeriesName(const Index: Integer): string;
    function GetLandUseValue(const Index: Integer): double;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    procedure SetColumn(const Value: integer); override;
    procedure SetLayer(const Value: integer); override;
    procedure SetRow(const Value: integer); override;
    function GetBooleanValue(Index: integer; AModel: TBaseModel): boolean; override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetBooleanAnnotation(Index: integer; AModel: TBaseModel): string; override;
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
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property Values: TFmp4LandUseRecord read FValues write FValues;
    // LandUses
    property LandUseValues[const Index: Integer]: double
      read GetLandUseValue;
    property LandUseAnnotations[const Index: Integer]: string
      read GetLandUseAnnotation;
    property LandUsePestNames[const Index: Integer]: string
      read GetLandUsePestName;
    property LandUsePestSeriesNames[const Index: Integer]: string
      read GetLandUsePestSeriesName;
    property LandUsePestSeriesMethods[const Index: Integer]: TPestParamMethod
      read GetLandUsePestSeriesMethod;
    property LandUseTimeSeriesNames[const Index: Integer]: string
      read GetLandUseTimeSeriesName;
  end;


  // @name represents the MODFLOW Farm Process land use values associated with
  // a single @link(TScreenObject).
  //
  // @seealso(TFmp4LandUseCollection)
  TFmp4LandUseBoundary = class(TModflowBoundary)
  private
    FUsedObserver: TObserver;
    FPestLandUseFormulas: TLandUseStringCollection;
    FPestLandUseMethods: TLandUsePestMethodCollection;
    FLandUseObservers: TObserverList;
    procedure SetPestLandUseFormulas(const Value: TLandUseStringCollection);
    procedure SetPestLandUseMethods(const Value: TLandUsePestMethodCollection);
    function GetLandUseObserver(const Index: Integer): TObserver;
  protected
    function MultipleCropsPerCellUsed: Boolean; virtual; abstract;
    procedure InvalidateLandUseData(Sender: TObject); virtual; abstract;
    { TODO -cFMP4 : override GetPestValueObserver in each descendent}
//    function GetPestValueObserver: TObserver;
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TFmp4LandUse_Cell)s for that stress period.
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    // See @link(TModflowBoundary.BoundaryCollectionClass
    // TModflowBoundary.BoundaryCollectionClass).
    { TODO -cFMP4 : override BoundaryCollectionClass in each descendent}
    // override this
//    class function BoundaryCollectionClass: TMF_BoundCollClass; override;

    procedure HandleChangedValue(Observer: TObserver); //override;
    { TODO -cFMP4 : override GetUsedObserver in each descendent}
    // Is GetUsedObserver needed?
    function GetUsedObserver: TObserver;
    procedure CreateFormulaObjects; //override;
    procedure CreateObservers; //override;
    function GetPestBoundaryFormula(FormulaIndex: integer): string; override;
    procedure SetPestBoundaryFormula(FormulaIndex: integer;
      const Value: string); override;
    function GetPestBoundaryMethod(FormulaIndex: integer): TPestParamMethod; override;
    procedure SetPestBoundaryMethod(FormulaIndex: integer;
      const Value: TPestParamMethod); override;
    function BoundaryObserverPrefix: string; override;
//    procedure InvalidateData(Sender: TObject); virtual; abstract;
    property LandUseObserver[const Index: Integer]: TObserver
      read GetLandUseObserver;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    Procedure Assign(Source: TPersistent); override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TFmp4LandUseStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // ParamList  is not used.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel; Writer: TObject); override;
    function Used: boolean; override;
    function NonParameterColumns: integer; override;
    { TODO -cFMP4 : override ValueDescription in each descendent}
    class function ValueDescription: string; virtual; abstract;
    class function DefaultBoundaryMethod(
      FormulaIndex: integer): TPestParamMethod; override;
    { TODO -cFMP4 : override InvalidateDisplay in each descendent}
    // be sure to overide this
//    procedure InvalidateDisplay; override;
    procedure CreateLandUseTimeLists(AModel: TBaseModel);
    function IsSame(FmpBoundary: TFmp4LandUseBoundary): Boolean;
  published
    property PestLandUseFormulas: TLandUseStringCollection
      read FPestLandUseFormulas write SetPestLandUseFormulas;
    property PestLandUseMethods: TLandUsePestMethodCollection
      read FPestLandUseMethods write SetPestLandUseMethods;
  end;

implementation

uses ScreenObjectUnit, PhastModelUnit, ModflowTimeUnit,
  frmGoPhastUnit,
  ModflowPackageSelectionUnit;

function LandUseCount(Model: TCustomModel): Integer;
begin
  Assert(Model <> nil);
  if Model.ModflowPackages.FarmLandUse.LandUseOption = luoMultiple then
  begin
    result := Model.FmpCrops.Count;
  end
  else
  begin
    result := 1;
  end;
end;

function LandUseName(Model: TCustomModel; LanduseIndex: Integer): String;
begin
  Assert(Model <> nil);
  if Model.ModflowPackages.FarmLandUse.LandUseOption = luoMultiple then
  begin
    result := Model.FmpCrops[LanduseIndex].CropName;
  end
  else
  begin
    result := 'Land_Use';
  end;
end;

{ TFmp4LandUseItem }

procedure TFmp4LandUseItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TFmp4LandUseItem then
  begin
    Fmp4LandUseValues := TFmp4LandUseItem(Source).Fmp4LandUseValues;
  end;
  inherited;
end;

procedure TFmp4LandUseItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TFmp4LandUseCollection;
  LandUseIndex: Integer;
begin
  ParentCollection := Collection as TFmp4LandUseCollection;
  for LandUseIndex := 0 to Fmp4LandUseValues.Count - 1 do
  begin
    Fmp4LandUseValues[LandUseIndex].Observer.OnUpToDateSet
      := ParentCollection.InvalidateLandUseValues;
  end;
end;

function TFmp4LandUseItem.BoundaryFormulaCount: integer;
begin
  result := 1;
  if Fmp4LandUseValues <> nil then
  begin
    if (Model <> nil) then
    begin
      Fmp4LandUseValues.Count := LandUseCount(Model as TCustomModel);
      result := Fmp4LandUseValues.Count;
    end;
  end;
end;

constructor TFmp4LandUseItem.Create(Collection: TCollection);
var
  LandUseCol: TFmp4LandUseCollection;
begin
  LandUseCol := Collection as TFmp4LandUseCollection;
  FFmp4LandUseValues := TLandUseStringCollection.Create(Model as TCustomModel, ScreenObject,
    LandUseCol);
  inherited;
end;

procedure TFmp4LandUseItem.CreateFormulaObjects;
begin
  inherited;
//  FFmp4LandUseValue := CreateFormulaObject(dsoTop);
end;

destructor TFmp4LandUseItem.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FFmp4LandUseValues.Count - 1 do
  begin
    FFmp4LandUseValues[Index].Value := '0';
  end;
  FFmp4LandUseValues.Free;
  inherited;
end;

function TFmp4LandUseItem.GetBoundaryFormula(Index: integer): string;
var
  Item: TLandUseStringValueItem;
begin
  inherited;
  while Index >= Fmp4LandUseValues.Count do
  begin
    Fmp4LandUseValues.Add;
  end;
  Item := Fmp4LandUseValues[Index];
  result := Item.Value;
end;

procedure TFmp4LandUseItem.GetPropertyObserver(Sender: TObject; List: TList);
var
  LandUseIndex: Integer;
  Item: TLandUseStringValueItem;
begin
  for LandUseIndex := 0 to Fmp4LandUseValues.Count - 1 do
  begin
    Item := Fmp4LandUseValues.Items[LandUseIndex];
    if (Item.ValueObject as TObject) = Sender then
    begin
      List.Add(Item.Observer);
    end;
  end;

end;

function TFmp4LandUseItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TFmp4LandUseItem;
begin
  result := (AnotherItem is TFmp4LandUseItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TFmp4LandUseItem(AnotherItem);
    result := Fmp4LandUseValues.IsSame(Item.Fmp4LandUseValues)
//    result := (Item.Fmp4LandUseValue = Fmp4LandUseValue)
  end;
end;

procedure TFmp4LandUseItem.RemoveFormulaObjects;
begin
  // Is RemoveFormulaObjects needed?
//  frmGoPhast.PhastModel.FormulaManager.Remove(FFmp4LandUseValue,
//    GlobalRemoveModflowBoundaryItemSubscription,
//    GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TFmp4LandUseItem.SetBoundaryFormula(Index: integer; const Value: string);
var
  Item: TLandUseStringValueItem;
begin
  while Index >= Fmp4LandUseValues.Count do
  begin
    Fmp4LandUseValues.Add;
  end;
  Item := Fmp4LandUseValues[Index];
  Item.Value := Value;
end;

procedure TFmp4LandUseItem.SetFmp4LandUseValues(
  const Value: TLandUseStringCollection);
begin
  FFmp4LandUseValues.Assign(Value);
end;

{ TFmp4LandUseCollection }

procedure TFmp4LandUseCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TFmp4LandUseStorage.Create(AModel));
end;

procedure TFmp4LandUseCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  Boundary: TFmp4LandUseStorage;
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
  CropIndex: Integer;
  LandUseArray: TDataArray;
  LocalLandUsePestSeries: string;
  LocalLandUsePestMethod: TPestParamMethod;
  LandUsePestItems: TStringList;
  LocalLandUsePest: string;
  LandUseTimeItems: TStringList;
  LocalLandUseTimeSeries: string;
begin
  LocalModel := AModel as TCustomModel;
  Boundary := Boundaries[ItemIndex, AModel] as TFmp4LandUseStorage;
  if MultipleCropsPerCellUsed then
  begin
    for CropIndex := 0 to LocalModel.FmpCrops.Count - 1 do
    begin
      LandUseArray := DataSets[CropIndex];
      LandUseArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
        LayerMax, RowMax, ColMax);

      LocalLandUsePestSeries := PestSeries[CropIndex];
      LocalLandUsePestMethod := PestMethods[CropIndex];
      LandUsePestItems := PestItemNames[CropIndex];
      LocalLandUsePest := LandUsePestItems[ItemIndex];
      LandUseTimeItems := TimeSeriesNames[CropIndex];
      LocalLandUseTimeSeries := LandUseTimeItems[ItemIndex];

      BoundaryIndex := 0;
      case LandUseArray.DataType of
        rdtDouble:
          begin
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
                      if LandUseArray.IsValue[LayerIndex, RowIndex, ColIndex] then
                      begin
                        with Boundary.Fmp4LandUseArray[BoundaryIndex] do
                        begin
                          Cell.Layer := LayerIndex;
                          Cell.Row := RowIndex;
                          Cell.Column := ColIndex;
                          LandUseData.Values[CropIndex] := LandUseArray.
                            RealData[LayerIndex, RowIndex, ColIndex];
                          LandUseData.ValueAnnotations[CropIndex] := LandUseArray.
                            Annotation[LayerIndex, RowIndex, ColIndex];
                          LandUseData.ValuePestNames[CropIndex] := LocalLandUsePest;
                          LandUseData.ValuePestSeriesNames[CropIndex] := LocalLandUsePestSeries;
                          LandUseData.ValuePestSeriesMethods[CropIndex] := LocalLandUsePestMethod;
                          LandUseData.ValueTimeSeriesNames[CropIndex] := LocalLandUseTimeSeries;
                        end;
                        Inc(BoundaryIndex);
                      end;
                    end;
                  end;
                end;
              end;
            end
          end;
        rdtInteger:
          begin
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
                      if LandUseArray.IsValue[LayerIndex, RowIndex, ColIndex] then
                      begin
                        with Boundary.Fmp4LandUseArray[BoundaryIndex] do
                        begin
                          Cell.Layer := LayerIndex;
                          Cell.Row := RowIndex;
                          Cell.Column := ColIndex;
                          LandUseData.IntValues[CropIndex] := LandUseArray.
                            IntegerData[LayerIndex, RowIndex, ColIndex];
                          LandUseData.ValueAnnotations[CropIndex] := LandUseArray.
                            Annotation[LayerIndex, RowIndex, ColIndex];
                          LandUseData.ValuePestNames[CropIndex] := LocalLandUsePest;
                          LandUseData.ValuePestSeriesNames[CropIndex] := LocalLandUsePestSeries;
                          LandUseData.ValuePestSeriesMethods[CropIndex] := LocalLandUsePestMethod;
                          LandUseData.ValueTimeSeriesNames[CropIndex] := LocalLandUseTimeSeries;
                        end;
                        Inc(BoundaryIndex);
                      end;
                    end;
                  end;
                end;
              end;
            end
          end;
        rdtBoolean:
          begin
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
                      if LandUseArray.IsValue[LayerIndex, RowIndex, ColIndex] then
                      begin
                        with Boundary.Fmp4LandUseArray[BoundaryIndex] do
                        begin
                          Cell.Layer := LayerIndex;
                          Cell.Row := RowIndex;
                          Cell.Column := ColIndex;
                          LandUseData.IntValues[CropIndex] := Ord(LandUseArray.
                            BooleanData[LayerIndex, RowIndex, ColIndex]);
                          LandUseData.ValueAnnotations[CropIndex] := LandUseArray.
                            Annotation[LayerIndex, RowIndex, ColIndex];
                          LandUseData.ValuePestNames[CropIndex] := LocalLandUsePest;
                          LandUseData.ValuePestSeriesNames[CropIndex] := LocalLandUsePestSeries;
                          LandUseData.ValuePestSeriesMethods[CropIndex] := LocalLandUsePestMethod;
                          LandUseData.ValueTimeSeriesNames[CropIndex] := LocalLandUseTimeSeries;
                        end;
                        Inc(BoundaryIndex);
                      end;
                    end;
                  end;
                end;
              end;
            end
          end;
        rdtString:
          begin
            Assert(False);
          end;
        else
          Assert(False);
      end;
      LandUseArray.CacheData;
    end;
  end;
  Boundary.CacheData;
end;

procedure TFmp4LandUseCollection.CreateLandUseTimeLists(AModel: TBaseModel);
var
  Link: TFmp4LandUseTimeListLink;
begin
  inherited;
  Link := TimeListLink.GetLink(AModel) as TFmp4LandUseTimeListLink;
  Link.CreateLandUseTimeLists;
end;

procedure TFmp4LandUseCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TFmp4LandUseItem;
  ScreenObject: TScreenObject;
  ALink: TFmp4LandUseTimeListLink;
//  RowIndex: Integer;
//  ColIndex: Integer;
//  LayerIndex: Integer;
  LocalModel: TCustomModel;
  ItemFormula: string;
  CropCount: Integer;
  CropIndex: Integer;
  LandUseSeriesName: string;
  LandUseMethod: TPestParamMethod;
  LandUseItems: TStringList;
  LandUseTimeSeriesItems: TStringList;
  LandUsePestItemList: TList<TStringList>;
  LandUseTimeSeriesItemList: TList<TStringList>;
  LandUsenData: TModflowTimeList;
  TimeIndex: Integer;
begin
  LandUsePestItemList := TList<TStringList>.Create;
  LandUseTimeSeriesItemList := TList<TStringList>.Create;
  try
    LocalModel := AModel as TCustomModel;
    ScreenObject := BoundaryGroup.ScreenObject as TScreenObject;
    SetLength(BoundaryValues, Count);
    CropCount := LocalModel.FmpCrops.Count;

    if MultipleCropsPerCellUsed then
    begin
      for CropIndex := 0 to CropCount - 1 do
      begin
        LandUseSeriesName := BoundaryGroup.PestBoundaryFormula[0 + CropIndex];
        PestSeries.Add(LandUseSeriesName);
        LandUseMethod := BoundaryGroup.PestBoundaryMethod[0 + CropIndex];
        PestMethods.Add(LandUseMethod);

        LandUseItems := TStringList.Create;
        PestItemNames.Add(LandUseItems);
        LandUsePestItemList.Add(LandUseItems);

        LandUseTimeSeriesItems := TStringList.Create;
        TimeSeriesNames.Add(LandUseTimeSeriesItems);
        LandUseTimeSeriesItemList.Add(LandUseTimeSeriesItems);
      end;
    end;

    ALink := TimeListLink.GetLink(AModel) as TFmp4LandUseTimeListLink;

    if MultipleCropsPerCellUsed then
    begin
      for CropIndex := 0 to CropCount - 1 do
      begin
        for Index := 0 to Count - 1 do
        begin
          Item := Items[Index] as TFmp4LandUseItem;
          BoundaryValues[Index].Time := Item.StartTime;

          LandUseSeriesName := BoundaryGroup.PestBoundaryFormula[0 + CropIndex];
          LandUseMethod := BoundaryGroup.PestBoundaryMethod[0 + CropIndex];
          LandUseItems := LandUsePestItemList[CropIndex];
          LandUseTimeSeriesItems := LandUseTimeSeriesItemList[CropIndex];
          ItemFormula := Item.Fmp4LandUseValues[CropIndex].Value;
          AssignBoundaryFormula(AModel, LandUseSeriesName, LandUseMethod,
            LandUseItems, LandUseTimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
        end;

        LandUsenData := ALink.FLandUseList[CropIndex];
        LandUsenData.Initialize(BoundaryValues, ScreenObject, lctUse);
        Assert(LandUsenData.Count = Count);
      end;
    end;

    ClearBoundaries(AModel);
    for TimeIndex := 0 to Count - 1 do
    begin
      AddSpecificBoundary(AModel);
    end;

    if MultipleCropsPerCellUsed then
    begin
      for CropIndex := 0 to CropCount - 1 do
      begin
        LandUsenData := ALink.FLandUseList[CropIndex];
        ListOfTimeLists.Add(LandUsenData);
      end;
    end;
  finally
    LandUseTimeSeriesItemList.Free;
    LandUsePestItemList.Free;
  end;
end;

procedure TFmp4LandUseCollection.InvalidateLandUseValues(Sender: TObject);
var
  Index: Integer;
  TimeList: TModflowTimeList;
  PhastModel: TPhastModel;
  Link: TFmp4LandUseTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TFmp4LandUseTimeListLink;
    for Index := 0 to Link.FLandUseList.Count - 1 do
    begin
      TimeList := Link.FLandUseList[Index];
      TimeList.Invalidate;
    end;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TFmp4LandUseTimeListLink;
        for Index := 0 to Link.FLandUseList.Count - 1 do
        begin
          TimeList := Link.FLandUseList[Index];
          TimeList.Invalidate;
        end;
      end;
    end;
  end;
end;

//procedure TFmp4LandUseCollection.InvalidateFmp4LandUseData(Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  Link: TFmp4LandUseTimeListLink;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
//begin
//  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    if PhastModel.Clearing then
//    begin
//      Exit;
//    end;
//    Link := TimeListLink.GetLink(PhastModel) as TFmp4LandUseTimeListLink;
//    Link.FFmp4LandUseValueData.Invalidate;
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      if ChildModel <> nil then
//      begin
//        Link := TimeListLink.GetLink(ChildModel) as TFmp4LandUseTimeListLink;
//        Link.FFmp4LandUseValueData.Invalidate;
//      end;
//    end;
//  end;
//end;

class function TFmp4LandUseCollection.ItemClass: TBoundaryItemClass;
begin
  result := TFmp4LandUseItem;
end;

function TFmp4LandUseCollection.PackageAssignmentMethod(
  AModel: TBaseModel): TUpdateMethod;
begin
  // This could be made a property of the package.
  result := umAssign;
end;

procedure TFmp4LandUseCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
var
  Index: Integer;
  CropCount: Integer;
begin
  SetLength((Boundaries[ItemIndex, AModel] as TFmp4LandUseStorage).FFmp4LandUseArray,
    BoundaryCount);
  CropCount := (AModel as TCustomModel).FmpCrops.Count;
  for Index := 0 to BoundaryCount - 1 do
  begin
    TFmp4LandUseStorage(Boundaries[ItemIndex, AModel]).
      FFmp4LandUseArray[Index].LandUseData.LandUseCount := CropCount;
  end;
  inherited;
end;


{ TFmp4LandUse_Cell }

procedure TFmp4LandUse_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TFmp4LandUse_Cell.GetBooleanAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := FValues.LandUseData.ValueAnnotations[Index];
end;

function TFmp4LandUse_Cell.GetBooleanValue(Index: integer;
  AModel: TBaseModel): boolean;
begin
  result := FValues.LandUseData.IntValues[Index] <> 0;
end;

function TFmp4LandUse_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TFmp4LandUse_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  Result := FValues.LandUseData.ValueAnnotations[Index];
end;

function TFmp4LandUse_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  Result := FValues.LandUseData.IntValues[Index];
end;

function TFmp4LandUse_Cell.GetLandUseAnnotation(const Index: Integer): string;
begin
  result := FValues.LandUseData.ValueAnnotations[Index];
end;

function TFmp4LandUse_Cell.GetLandUsePestName(const Index: Integer): string;
begin
  result := FValues.LandUseData.ValuePestNames[Index];
end;

function TFmp4LandUse_Cell.GetLandUsePestSeriesMethod(
  const Index: Integer): TPestParamMethod;
begin
  result := FValues.LandUseData.ValuePestSeriesMethods[Index];
end;

function TFmp4LandUse_Cell.GetLandUsePestSeriesName(
  const Index: Integer): string;
begin
  result := FValues.LandUseData.ValueTimeSeriesNames[Index];
end;

function TFmp4LandUse_Cell.GetLandUseTimeSeriesName(
  const Index: Integer): string;
begin
  result := FValues.LandUseData.ValueTimeSeriesNames[Index];
end;

function TFmp4LandUse_Cell.GetLandUseValue(const Index: Integer): double;
begin
  result := FValues.LandUseData.Values[Index];
end;

function TFmp4LandUse_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TFmp4LandUse_Cell.GetMf6TimeSeriesName(Index: Integer): string;
begin
  result := FValues.LandUseData.ValueTimeSeriesNames[Index];
end;

function TFmp4LandUse_Cell.GetPestName(Index: Integer): string;
begin
  result := FValues.LandUseData.ValuePestNames[Index];
end;

function TFmp4LandUse_Cell.GetPestSeriesMethod(
  Index: Integer): TPestParamMethod;
begin
  result := FValues.LandUseData.ValuePestSeriesMethods[Index];
end;

function TFmp4LandUse_Cell.GetPestSeriesName(Index: Integer): string;
begin
  result := FValues.LandUseData.ValuePestSeriesNames[Index];
end;

function TFmp4LandUse_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  Result := FValues.LandUseData.ValueAnnotations[Index];
end;

function TFmp4LandUse_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  Result := FValues.LandUseData.Values[Index];
end;

function TFmp4LandUse_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TFmp4LandUse_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

procedure TFmp4LandUse_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TFmp4LandUse_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TFmp4LandUse_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TFmp4LandUse_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TFmp4LandUse_Cell.SetMf6TimeSeriesName(Index: Integer;
  const Value: string);
begin
  FValues.LandUseData.ValueTimeSeriesNames[Index] := Value;
end;

procedure TFmp4LandUse_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TFmp4LandUseBoundary }

procedure TFmp4LandUseBoundary.Assign(Source: TPersistent);
var
  FmpSource: TFmp4LandUseBoundary;
begin
  if Source is TFmp4LandUseBoundary then
  begin
    FmpSource := TFmp4LandUseBoundary(Source);
    PestLandUseFormulas := FmpSource.PestLandUseFormulas;
    PestLandUseMethods := FmpSource.PestLandUseMethods;
  end;
  inherited;

end;

procedure TFmp4LandUseBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TFmp4LandUse_Cell;
  BoundaryValues: TFmp4LandUseRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TFmp4LandUseStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TFmp4LandUseStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TFmp4LandUse_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.Fmp4LandUseArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.Fmp4LandUseArray)
      end;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.Fmp4LandUseArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.Fmp4LandUseArray[BoundaryIndex];
        Cell := TFmp4LandUse_Cell.Create;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.Values := BoundaryValues;
        Cell.ScreenObject := ScreenObjectI;
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

function TFmp4LandUseBoundary.BoundaryObserverPrefix: string;
begin
  result := ValueDescription + '_';
end;

constructor TFmp4LandUseBoundary.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FPestLandUseFormulas:= TLandUseStringCollection.Create(Model, ScreenObject, nil);
  FPestLandUseFormulas.UsedForPestSeries := True;
  FPestLandUseMethods := TLandUsePestMethodCollection.Create(Model as TCustomModel);
  FLandUseObservers := TObserverList.Create;

  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;
end;

procedure TFmp4LandUseBoundary.CreateFormulaObjects;
var
  CropIndex: Integer;
  LocalModel: TPhastModel;
begin
  LocalModel := ParentModel as TPhastModel;
  if (LocalModel <> nil) and MultipleCropsPerCellUsed then
  begin
    for CropIndex := 0 to LocalModel.FmpCrops.Count - 1 do
    begin
      FPestLandUseFormulas.Add;
    end;
  end;
end;

procedure TFmp4LandUseBoundary.CreateLandUseTimeLists(AModel: TBaseModel);
begin
  (Values as TFmp4LanduseCollection).CreateLandUseTimeLists(AModel);
end;

procedure TFmp4LandUseBoundary.CreateObservers;
var
  Index: Integer;
begin
  if ScreenObject <> nil then
  begin
    for Index := 0 to FPestLandUseFormulas.Count - 1 do
    begin
      FObserverList.Add(LandUseObserver[Index]);
    end;
  end;
end;

class function TFmp4LandUseBoundary.DefaultBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  result := inherited;
end;

destructor TFmp4LandUseBoundary.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to FPestLandUseFormulas.Count - 1 do
  begin
    FPestLandUseFormulas[Index].Value := '';
  end;
  inherited;
  FPestLandUseMethods.Free;
  FPestLandUseFormulas.Free;
  FLandUseObservers.Free;

//  inherited;
end;

procedure TFmp4LandUseBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel; Writer: TObject);
var
  ValueIndex: Integer;
  BoundaryStorage: TFmp4LandUseStorage;
begin
  EvaluateArrayBoundaries(AModel, Writer);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
    if ValueIndex < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TFmp4LandUseStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
    end;
  end;
  ClearBoundaries(AModel);
end;

function TFmp4LandUseBoundary.GetLandUseObserver(
  const Index: Integer): TObserver;
var
  AObserver: TObserver;
begin
  while Index >= FLandUseObservers.Count do
  begin
    CreateObserver(Format('LandUse_%d', [Index+1]), AObserver, nil);
    FLandUseObservers.Add(AObserver);
    AObserver.OnUpToDateSet := InvalidateLandUseData;
  end;
  result := FLandUseObservers[Index];
end;

function TFmp4LandUseBoundary.GetPestBoundaryFormula(FormulaIndex: integer): string;
begin
  result := '';
  while FormulaIndex >= PestLandUseFormulas.Count do
  begin
    PestLandUseFormulas.Add;
  end;
  result := PestLandUseFormulas[FormulaIndex].Value;
end;

function TFmp4LandUseBoundary.GetPestBoundaryMethod(
  FormulaIndex: integer): TPestParamMethod;
begin
  while FormulaIndex >= FPestLandUseMethods.Count do
  begin
    FPestLandUseMethods.Add;
  end;
  result := FPestLandUseMethods[FormulaIndex].PestParamMethod;
end;

function TFmp4LandUseBoundary.GetUsedObserver: TObserver;
begin
  if FUsedObserver = nil then
  begin
    CreateObserver(ValueDescription + '_Used_', FUsedObserver, nil);
  end;
  result := FUsedObserver;
end;

procedure TFmp4LandUseBoundary.HandleChangedValue(Observer: TObserver);
begin
  InvalidateDisplay;
end;

function TFmp4LandUseBoundary.IsSame(FmpBoundary: TFmp4LandUseBoundary): Boolean;
begin
  result := PestLandUseFormulas.IsSame(FmpBoundary.PestLandUseFormulas)
    and PestLandUseMethods.IsSame(FmpBoundary.PestLandUseMethods)
end;

function TFmp4LandUseBoundary.NonParameterColumns: integer;
begin
  result := inherited NonParameterColumns;
end;

procedure TFmp4LandUseBoundary.SetPestBoundaryFormula(FormulaIndex: integer;
  const Value: string);
begin
  while FormulaIndex >= PestLandUseFormulas.Count do
  begin
    PestLandUseFormulas.Add;
  end;
  PestLandUseFormulas[FormulaIndex].Value := Value;
end;

procedure TFmp4LandUseBoundary.SetPestBoundaryMethod(FormulaIndex: integer;
  const Value: TPestParamMethod);
begin
  while FormulaIndex >= FPestLandUseMethods.Count do
  begin
    FPestLandUseMethods.Add;
  end;
  FPestLandUseMethods[FormulaIndex].PestParamMethod := Value;
end;

procedure TFmp4LandUseBoundary.SetPestLandUseFormulas(
  const Value: TLandUseStringCollection);
begin
  FPestLandUseFormulas.Assign(Value);
end;

procedure TFmp4LandUseBoundary.SetPestLandUseMethods(
  const Value: TLandUsePestMethodCollection);
begin
  FPestLandUseMethods.Assign(Value);
end;

function TFmp4LandUseBoundary.Used: boolean;
begin
  result := inherited Used;
  if result and (ParentModel <> nil) then
  begin
    result := Values.Used;
  end;
end;

{ TFmp4LandUseStorage }

procedure TFmp4LandUseStorage.Clear;
begin
  SetLength(FFmp4LandUseArray, 0);
  FCleared := True;
end;

procedure TFmp4LandUseStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    InitializeStrings(Strings);
    Count := Length(FFmp4LandUseArray);
    for Index := 0 to Count - 1 do
    begin
      FFmp4LandUseArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FFmp4LandUseArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TFmp4LandUseStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FFmp4LandUseArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FFmp4LandUseArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TFmp4LandUseStorage.GetFmp4LandUseArray: TFmp4LandUseArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FFmp4LandUseArray;
end;

{ TFmp4LandUseRecord }

procedure TFmp4LandUseRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  LandUseData.Cache(Comp, Strings);
end;

procedure TFmp4LandUseRecord.RecordStrings(Strings: TStringList);
begin
  LandUseData.RecordStrings(Strings);
end;

procedure TFmp4LandUseRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  LandUseData.Restore(Decomp, Annotations)
end;

{ TFmp4LandUseTimeListLink }

procedure TFmp4LandUseTimeListLink.AddLandUseTimeLists(CropIndex: Integer);
var
  LandTimeList: TModflowTimeList;
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  LandTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  LandTimeList.NonParamDescription := LandUseName(PhastModel, CropIndex);
  LandTimeList.ParamDescription := LandTimeList.NonParamDescription;
  if Model <> nil then
  begin
    LandTimeList.OnInvalidate := FOnInvalidateLanduse
  end;
  AddTimeList(LandTimeList);
  FLandUseList.Add(LandTimeList);
end;

procedure TFmp4LandUseTimeListLink.CreateLandUseTimeLists;
var
  PhastModel: TPhastModel;
  CropIndex: Integer;
  LandUseTimeList: TModflowTimeList;
begin
  PhastModel := frmGoPhast.PhastModel;
  if MultipleCropsPerCellUsed then
  begin
    CropIndex := 0;
    while CropIndex < PhastModel.FmpCrops.Count do
    begin
      if CropIndex < FLandUseList.Count then
      begin
        LandUseTimeList := FLandUseList[CropIndex];
        LandUseTimeList.NonParamDescription := PhastModel.FmpCrops[CropIndex].CropName;
        LandUseTimeList.ParamDescription :=  LandUseTimeList.NonParamDescription;
      end
      else
      begin
        LandUseTimeList := TModflowTimeList.Create(Model, Boundary.ScreenObject);
        LandUseTimeList.NonParamDescription := PhastModel.FmpCrops[CropIndex].CropName;
        LandUseTimeList.ParamDescription :=  LandUseTimeList.NonParamDescription;
        LandUseTimeList.DataType := DefaultDataType;
        if Model <> nil then
        begin
          LandUseTimeList.OnInvalidate := FOnInvalidateLanduse;
        end;
        AddTimeList(LandUseTimeList);
        FLandUseList.Add(LandUseTimeList);
      end;
      Inc(CropIndex);
    end;
    while FLandUseList.Count > PhastModel.FmpCrops.Count do
    begin
      LandUseTimeList := FLandUseList.Last;
      RemoveTimeList(LandUseTimeList);
      FLandUseList.Create.Delete(FLandUseList.Count--1);
    end;
  end
  else
  begin
    for CropIndex := 0 to FLandUseList.Count - 1 do
    begin
      LandUseTimeList := FLandUseList[CropIndex];
      RemoveTimeList(LandUseTimeList);
    end;
    FLandUseList.Clear;
  end;
end;

procedure TFmp4LandUseTimeListLink.CreateTimeLists;
begin
  inherited;
  FLandUseList := TModflowTimeLists.Create;

  CreateLandUseTimeLists;
end;

destructor TFmp4LandUseTimeListLink.Destroy;
begin
  FLandUseList.Free;
  inherited;
end;

function TFmp4LandUseTimeListLink.GetDefaultDataType: TRbwDataType;
begin
  result := rdtDouble;
end;

procedure TFmp4LandUseTimeListLink.RemoveLandUseTimeLists(CropIndex: Integer);
var
  LandUseTimeList: TModflowTimeList;
begin
  LandUseTimeList := FLandUseList[CropIndex];
  RemoveTimeList(LandUseTimeList);
  FLandUseList.Delete(CropIndex);
end;

procedure TFmp4LandUseTimeListLink.UpdateLandUseTimeLists;
begin
  CreateLandUseTimeLists;
end;

{ TLandUseStringValueItem }

constructor TLandUseStringValueItem.Create(Collection: TCollection);
begin
  inherited;
  if Collection is TLandUseStringCollection then
  begin
    if not TLandUseStringCollection(Collection).UsedForPestSeries then
    begin
      Value := '0';
    end;
  end
  else
  begin
    Value := '0';
  end;
end;

{ TLandUseStringCollection }

function TLandUseStringCollection.Add: TLandUseStringValueItem;
begin
  result := inherited Add as TLandUseStringValueItem;
end;

constructor TLandUseStringCollection.Create(Model: TBaseModel;
  AScreenObject: TObject; ParentCollection: TCustomMF_BoundColl);
begin
  inherited Create(TLandUseStringValueItem, Model, AScreenObject,
    ParentCollection);
end;

function TLandUseStringCollection.GetLandUseItem(
  Index: Integer): TLandUseStringValueItem;
begin
  while Index >= Count do
  begin
    Add;
  end;
  result := inherited Items[Index] as  TLandUseStringValueItem
end;

procedure TLandUseStringCollection.SetLandUseItem(Index: Integer;
  const Value: TLandUseStringValueItem);
begin
  Assert(Index >= 0);
  while Index >= Count do
  begin
    Add;
  end;
  inherited Items[Index] := Value;
end;

procedure TLandUseStringCollection.SetUsedForPestSeries(const Value: Boolean);
begin
  FUsedForPestSeries := Value;
end;

end.
