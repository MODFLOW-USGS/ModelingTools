unit ModflowSfrUnsatSegment;

interface

uses Windows, Classes, SysUtils, ZLib, GoPhastTypes,
  OrderedCollectionUnit, ModflowCellUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, SubscriptionUnit;

type
  TSfrUnsatSegmentRecord = record
    Cell: TCellLocation;
    SaturatedWaterContent: double;
    InitialWaterContent: double;
    BrooksCoreyExponent: double;
    VerticalSaturatedK: double;
    StartingTime: double;
    EndingTime: double;
    BrooksCoreyExponentAnnotation: string;
    SaturatedWaterContentAnnotation: string;
    InitialWaterContentAnnotation: string;
    VerticalSaturatedKAnnotation: string;

    BrooksCoreyExponentPestItem: string;
    SaturatedWaterContentPestItem: string;
    InitialWaterContentPestItem: string;
    VerticalSaturatedKPestItem: string;
//    BrooksCoreyExponentPestSeriesItem: string;
//    SaturatedWaterContentPestSeriesItem: string;
//    InitialWaterContentPestSeriesItem: string;
//    VerticalSaturatedKPestSeriesItem: string;
//    BrooksCoreyExponentPestSeriesMethod: TPestParamMethod;
//    SaturatedWaterContentPestSeriesMethod: TPestParamMethod;
//    InitialWaterContentPestSeriesMethod: TPestParamMethod;
//    VerticalSaturatedKPestSeriesMethod: TPestParamMethod;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
  PSfrUnsatSegmentRecord = ^TSfrUnsatSegmentRecord;

  TSrfUnsatSegmentArray = array of TSfrUnsatSegmentRecord;

  TSfrUnsatSegmentStorage = class(TCustomBoundaryStorage)
  private
    FSrfUnsatSegmentArray: TSrfUnsatSegmentArray;
    function GetSrfUnsatSegmentArray: TSrfUnsatSegmentArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SrfUnsatSegmentArray: TSrfUnsatSegmentArray
      read GetSrfUnsatSegmentArray;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrUnsatSegmentCollection).
  TSfrUnsatSegmentItem = class(TCustomModflowBoundaryItem)
  private
    FInitialWaterContent: TFormulaObject;
    FSaturatedWaterContent: TFormulaObject;
    FBrooksCoreyExponent: TFormulaObject;
    FVerticalSaturatedK: TFormulaObject;
    procedure SetBrooksCoreyExponent(const Value: string);
    procedure SetSaturatedWaterContent(const Value: string);
    procedure SetInitialWaterContent(const Value: string);
    procedure SetVerticalSaturatedK(const Value: string);
    function GetBrooksCoreyExponent: string;
    function GetInitialWaterContent: string;
    function GetSaturatedWaterContent: string;
    function GetVerticalSaturatedK: string;
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
    function BoundaryFormulaCount: integer; override;
  public
    constructor Create(Collection: TCollection); override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    property BrooksCoreyExponent: string read GetBrooksCoreyExponent write SetBrooksCoreyExponent;
    property InitialWaterContent: string read GetInitialWaterContent write SetInitialWaterContent;
    property SaturatedWaterContent: string read GetSaturatedWaterContent write SetSaturatedWaterContent;
    property VerticalSaturatedK: string read GetVerticalSaturatedK write SetVerticalSaturatedK;
  end;

  TSfrUnsatSegmentTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the Brooks Corey Exponent for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FBrooksCoreyExponentData: TModflowTimeList;
    // @name is used to compute the initial water content for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FInitialWaterContentData: TModflowTimeList;
    // @name is used to compute the saturated water content for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FSaturatedWaterContentData: TModflowTimeList;
    // @name is used to compute the vertical saturated hydraulic conductivity
    // for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FVerticalSaturatedKData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrUnsatSegmentCollection = class(TCustomMF_ArrayBoundColl)
  private
    FAssignmentLocation: TAssignmentLocation;
    procedure InvalidateSaturatedWaterContentData(Sender: TObject);
    procedure InvalidateInitialWaterContentData(Sender: TObject);
    procedure InvalidateBrooksCoreyExponentData(Sender: TObject);
    procedure InvalidateVerticalSaturatedKData(Sender: TObject);
  protected
    class function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList;ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject); override;
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
    function ShouldDeleteItemsWithZeroDuration: Boolean; override;
  public
    property AssignmentLocation: TAssignmentLocation read FAssignmentLocation
      write FAssignmentLocation;
    function GetItemByStartTime(StartTime: double): TSfrUnsatSegmentItem;
  end;


  TSfrUnsatSegment_Cell = class(TValueCell)
  private
    FValues: TSfrUnsatSegmentRecord;
    FStressPeriod: integer;
    function GetInitialWaterContent: double;
    function GetBrooksCoreyExponent: double;
    function GetSaturatedWaterContent: double;
    function GetBrooksCoreyExponentAnnotation: string;
    function GetSaturatedWaterContentAnnotation: string;
    function GetInitialWaterContentAnnotation: string;
    function GetVerticalSaturatedK: double;
    function GetVerticalSaturatedKAnnotation: string;
  protected
    function GetColumn: integer; override;
    function GetLayer: integer; override;
    function GetRow: integer; override;
    function GetIntegerValue(Index: integer; AModel: TBaseModel): integer; override;
    function GetRealValue(Index: integer; AModel: TBaseModel): double; override;
    function GetRealAnnotation(Index: integer; AModel: TBaseModel): string; override;
    function GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList); override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    procedure RecordStrings(Strings: TStringList); override;
    function GetPestName(Index: Integer): string; override;
//    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
//    function GetPestSeriesName(Index: Integer): string; override;
  public
    property Values: TSfrUnsatSegmentRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property BrooksCoreyExponent: double read GetBrooksCoreyExponent;
    property SaturatedWaterContent: double read GetSaturatedWaterContent;
    property InitialWaterContent: double read GetInitialWaterContent;
    property VerticalSaturatedK: double read GetVerticalSaturatedK;
    property BrooksCoreyExponentAnnotation: string read GetBrooksCoreyExponentAnnotation;
    property SaturatedWaterContentAnnotation: string read GetSaturatedWaterContentAnnotation;
    property InitialWaterContentAnnotation: string read GetInitialWaterContentAnnotation;
    property VerticalSaturatedKAnnotation: string read GetVerticalSaturatedKAnnotation;
  end;

const
  UnsatSaturatedWaterContentPosition = 0;
  UnSatInitialWaterContentPosition = 1;
  UnSatBrooksCoreyExponentPosition = 2;
  UnSatVerticalSaturatedKPosition = 3;

implementation

uses DataSetUnit, ScreenObjectUnit, PhastModelUnit,
  ModflowSfrUnit, frmGoPhastUnit;

resourcestring
  StrInitialWaterConten = 'Initial water content';
  StrSaturatedWaterCont = 'Saturated water content';
  StrBrooksCoreyExponen = 'Brooks-Corey exponent';
  StrMaximumVerticalK = 'Maximum vertical K';
  StrMaximumVerticalK_LC = ' maximum vertical K';

{ TSfrUnsatSegmentItem }

procedure TSfrUnsatSegmentItem.Assign(Source: TPersistent);
var
  Sfr: TSfrUnsatSegmentItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrUnsatSegmentItem then
  begin
    Sfr := TSfrUnsatSegmentItem(Source);
    BrooksCoreyExponent := Sfr.BrooksCoreyExponent;
    InitialWaterContent := Sfr.InitialWaterContent;
    SaturatedWaterContent := Sfr.SaturatedWaterContent;
    VerticalSaturatedK := Sfr.VerticalSaturatedK;
  end;
  inherited;
end;

procedure TSfrUnsatSegmentItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrUnsatSegmentCollection;
  SaturatedWaterContentObserver: TObserver;
  InitialWaterContentObserver: TObserver;
  BrooksCoreyExponentObserver: TObserver;
  VerticalSaturatedKObserver: TObserver;
begin
  ParentCollection := Collection as TSfrUnsatSegmentCollection;
  SaturatedWaterContentObserver := FObserverList[UnsatSaturatedWaterContentPosition];
  SaturatedWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateSaturatedWaterContentData;
  InitialWaterContentObserver := FObserverList[UnSatInitialWaterContentPosition];
  InitialWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateInitialWaterContentData;
  BrooksCoreyExponentObserver := FObserverList[UnSatBrooksCoreyExponentPosition];
  BrooksCoreyExponentObserver.OnUpToDateSet := ParentCollection.InvalidateBrooksCoreyExponentData;
  VerticalSaturatedKObserver := FObserverList[UnSatVerticalSaturatedKPosition];
  VerticalSaturatedKObserver.OnUpToDateSet := ParentCollection.InvalidateVerticalSaturatedKData;
end;

function TSfrUnsatSegmentItem.BoundaryFormulaCount: integer;
begin
  result := 4;
end;

constructor TSfrUnsatSegmentItem.Create(Collection: TCollection);
begin
  inherited;
  SaturatedWaterContent := '0.3';
  InitialWaterContent := '0.2';
  BrooksCoreyExponent := '3.5';
  VerticalSaturatedK := 'Kx';
end;

procedure TSfrUnsatSegmentItem.CreateFormulaObjects;
begin
  FSaturatedWaterContent := CreateFormulaObject(dso3D);
  FInitialWaterContent := CreateFormulaObject(dso3D);
  FBrooksCoreyExponent := CreateFormulaObject(dso3D);
  FVerticalSaturatedK := CreateFormulaObject(dso3D);
end;

destructor TSfrUnsatSegmentItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrUnsatSegmentItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    UnsatSaturatedWaterContentPosition:
      result := SaturatedWaterContent;
    UnSatInitialWaterContentPosition:
      result := InitialWaterContent;
    UnSatBrooksCoreyExponentPosition:
      result := BrooksCoreyExponent;
    UnSatVerticalSaturatedKPosition:
      result := VerticalSaturatedK;
    else Assert(False);
  end;
end;

function TSfrUnsatSegmentItem.GetBrooksCoreyExponent: string;
begin
  Result := FBrooksCoreyExponent.Formula;
  ResetItemObserver(UnSatBrooksCoreyExponentPosition);
end;

function TSfrUnsatSegmentItem.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  ResetItemObserver(UnSatInitialWaterContentPosition);
end;

procedure TSfrUnsatSegmentItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FSaturatedWaterContent then
  begin
    List.Add(FObserverList[UnsatSaturatedWaterContentPosition]);
  end;
  if Sender = FInitialWaterContent then
  begin
    List.Add(FObserverList[UnSatInitialWaterContentPosition]);
  end;
  if Sender = FBrooksCoreyExponent then
  begin
    List.Add(FObserverList[UnSatBrooksCoreyExponentPosition]);
  end;
  if Sender = FVerticalSaturatedK then
  begin
    List.Add(FObserverList[UnSatVerticalSaturatedKPosition]);
  end;
end;

function TSfrUnsatSegmentItem.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  ResetItemObserver(UnsatSaturatedWaterContentPosition);
end;

function TSfrUnsatSegmentItem.GetVerticalSaturatedK: string;
begin
  Result := FVerticalSaturatedK.Formula;
  ResetItemObserver(UnSatVerticalSaturatedKPosition);
end;

function TSfrUnsatSegmentItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrUnsatSegmentItem;
begin
  result := (AnotherItem is TSfrUnsatSegmentItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrUnsatSegmentItem(AnotherItem);
    result := (Item.BrooksCoreyExponent = BrooksCoreyExponent)
      and (Item.InitialWaterContent = InitialWaterContent)
      and (Item.SaturatedWaterContent = SaturatedWaterContent)
      and (Item.VerticalSaturatedK = VerticalSaturatedK);
  end;
end;

procedure TSfrUnsatSegmentItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FVerticalSaturatedK,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBrooksCoreyExponent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInitialWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSaturatedWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSfrUnsatSegmentItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    UnsatSaturatedWaterContentPosition:
      SaturatedWaterContent := Value;
    UnSatInitialWaterContentPosition:
      InitialWaterContent := Value;
    UnSatBrooksCoreyExponentPosition:
      BrooksCoreyExponent := Value;
    UnSatVerticalSaturatedKPosition:
      VerticalSaturatedK := Value;
    else Assert(False);
  end;
end;


procedure TSfrUnsatSegmentItem.SetBrooksCoreyExponent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TScreenObject;
begin
  if FBrooksCoreyExponent.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, UnSatBrooksCoreyExponentPosition, FBrooksCoreyExponent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject as TScreenObject;
      if (ScreenObj <> nil)
        and ScreenObj.CanInvalidateModel
        and (ScreenObj.ModflowSfrBoundary <> nil) then
      begin
        if Collection = ScreenObj.
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamBrooksCorey(self);
        end
        else
        begin
          Assert(Collection = ScreenObj.
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamBrooksCorey(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentItem.SetSaturatedWaterContent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TScreenObject;
begin
  if FSaturatedWaterContent.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, UnsatSaturatedWaterContentPosition, FSaturatedWaterContent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil) and not PhastModel.Clearing
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject as TScreenObject;
      if (ScreenObj <> nil)
        and ScreenObj.CanInvalidateModel
        and (ScreenObj.ModflowSfrBoundary <> nil) then
      begin
        if Collection = ScreenObj.ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamUnsaturatedWaterContent(self);
        end
        else
        begin
          Assert(Collection = ScreenObj.
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamUnsaturatedWaterContent(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentItem.SetInitialWaterContent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TScreenObject;
begin
  if FInitialWaterContent.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, UnSatInitialWaterContentPosition, FInitialWaterContent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject as TScreenObject;
      if (ScreenObj <> nil)
        and ScreenObj.CanInvalidateModel
        and (ScreenObj.ModflowSfrBoundary <> nil) then
      begin
        if Collection = ScreenObj.
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamUnsatInitialWaterContent(self);
        end
        else
        begin
          Assert(Collection = ScreenObj.
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamUnsatInitialWaterContent(self);
        end;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentItem.SetVerticalSaturatedK(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TScreenObject;
begin
  if FVerticalSaturatedK.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, UnSatVerticalSaturatedKPosition, FVerticalSaturatedK);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject as TScreenObject;
      if (ScreenObj <> nil)
        and ScreenObj.CanInvalidateModel
        and (ScreenObj.ModflowSfrBoundary <> nil) then
      begin
        if Collection = (ScreenObj as TScreenObject).
          ModflowSfrBoundary.UpstreamUnsatSegmentValues then
        begin
          PhastModel.InvalidateMfSfrUpstreamUnsatKz(self);
        end
        else
        begin
          Assert(Collection = ScreenObj.
            ModflowSfrBoundary.DownstreamUnsatSegmentValues);
          PhastModel.InvalidateMfSfrDownstreamUnsatKz(self);
        end;
      end;
    end;
  end;
end;

{ TSfrUnsatSegmentCollection }

procedure TSfrUnsatSegmentCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TSfrUnsatSegmentStorage.Create(AModel));
end;

procedure TSfrUnsatSegmentCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  BrooksCoreyExponentArray: TDataArray;
  InitialWaterContentArray: TDataArray;
  SaturatedWaterContentArray: TDataArray;
  VerticalSaturatedKArray: TDataArray;
  Boundary: TSfrUnsatSegmentStorage;
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
//  LocalSaturatedWaterContentPestSeries: string;
//  LocalSaturatedWaterContentPestMethod: TPestParamMethod;
  SaturatedWaterContentPestItems: TStringList;
  LocalSaturatedWaterContentPest: string;
//  LocalInitialWaterContentPestSeries: string;
//  LocalInitialWaterContentPestMethod: TPestParamMethod;
  InitialWaterContentPestItems: TStringList;
  LocalInitialWaterContentPest: string;
//  LocalBrooksCoreyExponentPestSeries: string;
//  LocalBrooksCoreyExponentPestMethod: TPestParamMethod;
  BrooksCoreyExponentPestItems: TStringList;
  LocalBrooksCoreyExponentPest: string;
//  LocalVerticalSaturatedKPestSeries: string;
//  LocalVerticalSaturatedKPestMethod: TPestParamMethod;
  VerticalSaturatedKPestItems: TStringList;
  LocalVerticalSaturatedKPest: string;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  SaturatedWaterContentArray := DataSets[UnsatSaturatedWaterContentPosition];
  InitialWaterContentArray := DataSets[UnSatInitialWaterContentPosition];
  BrooksCoreyExponentArray := DataSets[UnSatBrooksCoreyExponentPosition];
  VerticalSaturatedKArray := DataSets[UnSatVerticalSaturatedKPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TSfrUnsatSegmentStorage;

//  LocalSaturatedWaterContentPestSeries := PestSeries[UnsatSaturatedWaterContentPosition];
//  LocalSaturatedWaterContentPestMethod := PestMethods[UnsatSaturatedWaterContentPosition];
  SaturatedWaterContentPestItems := PestItemNames[UnsatSaturatedWaterContentPosition];
  LocalSaturatedWaterContentPest := SaturatedWaterContentPestItems[ItemIndex];

//  LocalInitialWaterContentPestSeries := PestSeries[UnsatInitialWaterContentPosition];
//  LocalInitialWaterContentPestMethod := PestMethods[UnsatInitialWaterContentPosition];
  InitialWaterContentPestItems := PestItemNames[UnsatInitialWaterContentPosition];
  LocalInitialWaterContentPest := InitialWaterContentPestItems[ItemIndex];

//  LocalBrooksCoreyExponentPestSeries := PestSeries[UnsatBrooksCoreyExponentPosition];
//  LocalBrooksCoreyExponentPestMethod := PestMethods[UnsatBrooksCoreyExponentPosition];
  BrooksCoreyExponentPestItems := PestItemNames[UnsatBrooksCoreyExponentPosition];
  LocalBrooksCoreyExponentPest := BrooksCoreyExponentPestItems[ItemIndex];

//  LocalVerticalSaturatedKPestSeries := PestSeries[UnsatVerticalSaturatedKPosition];
//  LocalVerticalSaturatedKPestMethod := PestMethods[UnsatVerticalSaturatedKPosition];
  VerticalSaturatedKPestItems := PestItemNames[UnsatVerticalSaturatedKPosition];
  LocalVerticalSaturatedKPest := VerticalSaturatedKPestItems[ItemIndex];

  SaturatedWaterContentArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
    LayerMax, RowMax, ColMax);
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
            if SaturatedWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(InitialWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(BrooksCoreyExponentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(VerticalSaturatedKArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(BoundaryIndex < Length(Boundary.SrfUnsatSegmentArray));
              with Boundary.SrfUnsatSegmentArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;
//                Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
                BrooksCoreyExponent := BrooksCoreyExponentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                BrooksCoreyExponentAnnotation := BrooksCoreyExponentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                BrooksCoreyExponentPestItem := LocalBrooksCoreyExponentPest;
//                BrooksCoreyExponentPestSeriesItem := LocalBrooksCoreyExponentPestSeries;
//                BrooksCoreyExponentPestSeriesMethod := LocalBrooksCoreyExponentPestMethod;

                InitialWaterContent := InitialWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                InitialWaterContentAnnotation := InitialWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                InitialWaterContentPestItem := LocalInitialWaterContentPest;
//                InitialWaterContentPestSeriesItem := LocalInitialWaterContentPestSeries;
//                InitialWaterContentPestSeriesMethod := LocalInitialWaterContentPestMethod;

                SaturatedWaterContent := SaturatedWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                SaturatedWaterContentAnnotation := SaturatedWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                SaturatedWaterContentPestItem := LocalSaturatedWaterContentPest;
//                SaturatedWaterContentPestSeriesItem := LocalSaturatedWaterContentPestSeries;
//                SaturatedWaterContentPestSeriesMethod := LocalSaturatedWaterContentPestMethod;

                VerticalSaturatedK := VerticalSaturatedKArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                VerticalSaturatedKAnnotation := VerticalSaturatedKArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                VerticalSaturatedKPestItem := LocalVerticalSaturatedKPest;
//                VerticalSaturatedKPestSeriesItem := LocalVerticalSaturatedKPestSeries;
//                VerticalSaturatedKPestSeriesMethod := LocalVerticalSaturatedKPestMethod;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  SaturatedWaterContentArray.CacheData;
  InitialWaterContentArray.CacheData;
  BrooksCoreyExponentArray.CacheData;
  VerticalSaturatedKArray.CacheData;
  Boundary.CacheData;
end;

function TSfrUnsatSegmentCollection.GetItemByStartTime(
  StartTime: double): TSfrUnsatSegmentItem;
var
  Index: Integer;
  Item: TSfrUnsatSegmentItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    if Item.StartTime = StartTime then
    begin
      result := Item;
      Exit;
    end;
    if Item.StartTime < StartTime then
    begin
      result := Item;
    end;
    if Item.EndTime > StartTime then
    begin
      Exit;
    end;
  end;
end;

class function TSfrUnsatSegmentCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSfrUnsatSegmentTimeListLink;
end;

procedure TSfrUnsatSegmentCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSfrUnsatSegmentItem;
  Boundary: TSfrBoundary;
  ScreenObject: TScreenObject;
  ISFROPT: integer;
  ICALC: integer;
  ItemUsed: boolean;
  ALink: TSfrUnsatSegmentTimeListLink;
  BrooksCoreyExponentData: TModflowTimeList;
  InitialWaterContentData: TModflowTimeList;
  SaturatedWaterContentData: TModflowTimeList;
  VerticalSaturatedKData: TModflowTimeList;
//  OffSet: Integer;
//  PestSaturatedWaterContentSeriesName: string;
//  PestSaturatedWaterContentMethod: TPestParamMethod;
  PestSaturatedWaterContentItems: TStringList;
//  PestInitialWaterContentSeriesName: string;
//  PestInitialWaterContentMethod: TPestParamMethod;
  PestInitialWaterContentItems: TStringList;
//  PestBrooksCoreyExponentSeriesName: string;
//  PestBrooksCoreyExponentMethod: TPestParamMethod;
  PestBrooksCoreyExponentItems: TStringList;
//  PestVerticalSaturatedKSeriesName: string;
//  PestVerticalSaturatedKMethod: TPestParamMethod;
  PestVerticalSaturatedKItems: TStringList;
  ItemFormula: string;
  BrooksCoreyExponentTimeSeriesItems: TStringList;
  InitialWaterContentTimeSeriesItems: TStringList;
  SaturatedWaterContentTimeSeriesItems: TStringList;
  VerticalSaturatedKTimeSeriesItems: TStringList;
begin
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;

  SetLength(BoundaryValues, Count);
  Boundary := BoundaryGroup as TSfrBoundary;
//  if Boundary.UpstreamUnsatSegmentValues = self then
//  begin
//    OffSet := 5;
//  end
//  else
//  begin
//    OffSet := 14;
//    Assert(Boundary.DownstreamUnsatSegmentValues = self);
//  end;

//  PestSaturatedWaterContentSeriesName :=
//    BoundaryGroup.PestBoundaryFormula[UnsatSaturatedWaterContentPosition+Offset];
//  PestSeries.Add(PestSaturatedWaterContentSeriesName);
//  PestSaturatedWaterContentMethod :=
//    BoundaryGroup.PestBoundaryMethod[UnsatSaturatedWaterContentPosition+Offset];
//  PestMethods.Add(PestSaturatedWaterContentMethod);
  PestSaturatedWaterContentItems := TStringList.Create;
  PestItemNames.Add(PestSaturatedWaterContentItems);
  SaturatedWaterContentTimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(SaturatedWaterContentTimeSeriesItems);

//  PestInitialWaterContentSeriesName :=
//    BoundaryGroup.PestBoundaryFormula[UnsatInitialWaterContentPosition+Offset];
//  PestSeries.Add(PestInitialWaterContentSeriesName);
//  PestInitialWaterContentMethod :=
//    BoundaryGroup.PestBoundaryMethod[UnsatInitialWaterContentPosition+Offset];
//  PestMethods.Add(PestInitialWaterContentMethod);
  PestInitialWaterContentItems := TStringList.Create;
  PestItemNames.Add(PestInitialWaterContentItems);
  InitialWaterContentTimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(InitialWaterContentTimeSeriesItems);

//  PestBrooksCoreyExponentSeriesName :=
//    BoundaryGroup.PestBoundaryFormula[UnsatBrooksCoreyExponentPosition+Offset];
//  PestSeries.Add(PestBrooksCoreyExponentSeriesName);
//  PestBrooksCoreyExponentMethod :=
//    BoundaryGroup.PestBoundaryMethod[UnsatBrooksCoreyExponentPosition+Offset];
//  PestMethods.Add(PestBrooksCoreyExponentMethod);
  PestBrooksCoreyExponentItems := TStringList.Create;
  PestItemNames.Add(PestBrooksCoreyExponentItems);
  BrooksCoreyExponentTimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(BrooksCoreyExponentTimeSeriesItems);

//  PestVerticalSaturatedKSeriesName :=
//    BoundaryGroup.PestBoundaryFormula[UnsatVerticalSaturatedKPosition+Offset];
//  PestSeries.Add(PestVerticalSaturatedKSeriesName);
//  PestVerticalSaturatedKMethod :=
//    BoundaryGroup.PestBoundaryMethod[UnsatVerticalSaturatedKPosition+Offset];
//  PestMethods.Add(PestVerticalSaturatedKMethod);
  PestVerticalSaturatedKItems := TStringList.Create;
  PestItemNames.Add(PestVerticalSaturatedKItems);
  VerticalSaturatedKTimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(VerticalSaturatedKTimeSeriesItems);

  ScreenObject := Boundary.ScreenObject as TScreenObject;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  in [4,5]) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      ItemFormula := Item.BrooksCoreyExponent;
//      BoundaryValues[Index].Formula := Item.BrooksCoreyExponent;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, '',
      ppmMultiply, PestBrooksCoreyExponentItems, BrooksCoreyExponentTimeSeriesItems,
      ItemFormula, Writer, BoundaryValues[Index]);
  end;
  ALink := TimeListLink.GetLink(AModel) as TSfrUnsatSegmentTimeListLink;
  BrooksCoreyExponentData := ALink.FBrooksCoreyExponentData;
  BrooksCoreyExponentData.Initialize(BoundaryValues, ScreenObject, lctZero,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  in [4,5]) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      ItemFormula := Item.InitialWaterContent;
//      BoundaryValues[Index].Formula := Item.InitialWaterContent;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, '',
      ppmMultiply, PestInitialWaterContentItems, InitialWaterContentTimeSeriesItems,
      ItemFormula, Writer, BoundaryValues[Index]);
  end;
  InitialWaterContentData := ALink.FInitialWaterContentData;
  InitialWaterContentData.Initialize(BoundaryValues, ScreenObject, lctZero,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  in [4,5]) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      ItemFormula := Item.SaturatedWaterContent;
//      BoundaryValues[Index].Formula := Item.SaturatedWaterContent;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, '',
      ppmMultiply, PestSaturatedWaterContentItems, SaturatedWaterContentTimeSeriesItems,
      ItemFormula, Writer, BoundaryValues[Index]);
  end;
  SaturatedWaterContentData := ALink.FSaturatedWaterContentData;
  SaturatedWaterContentData.Initialize(BoundaryValues, ScreenObject, lctZero,
    AssignmentLocation);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrUnsatSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := (ISFROPT  = 5) and (ICALC in [1,2]) and (Index = 0);
    if ItemUsed then
    begin
      ItemFormula := Item.VerticalSaturatedK;
//      BoundaryValues[Index].Formula := Item.VerticalSaturatedK;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, '',
      ppmMultiply, PestVerticalSaturatedKItems, VerticalSaturatedKTimeSeriesItems,
      ItemFormula, Writer, BoundaryValues[Index]);
  end;
  VerticalSaturatedKData := ALink.FVerticalSaturatedKData;
  VerticalSaturatedKData.Initialize(BoundaryValues, ScreenObject, lctZero,
    AssignmentLocation);

  Assert(BrooksCoreyExponentData.Count = Count);
  Assert(InitialWaterContentData.Count = Count);
  Assert(SaturatedWaterContentData.Count = Count);
  Assert(VerticalSaturatedKData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(BrooksCoreyExponentData.Count, AModel);
  for TimeIndex := 0 to BrooksCoreyExponentData.Count - 1 do
  begin
    AddBoundary(TSfrUnsatSegmentStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(SaturatedWaterContentData);
  ListOfTimeLists.Add(InitialWaterContentData);
  ListOfTimeLists.Add(BrooksCoreyExponentData);
  ListOfTimeLists.Add(VerticalSaturatedKData);
end;

procedure TSfrUnsatSegmentCollection.InvalidateBrooksCoreyExponentData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrUnsatSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrUnsatSegmentTimeListLink;
    Link.FBrooksCoreyExponentData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
        Link.FBrooksCoreyExponentData.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentCollection.InvalidateInitialWaterContentData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrUnsatSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrUnsatSegmentTimeListLink;
    Link.FInitialWaterContentData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
        Link.FInitialWaterContentData.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentCollection.InvalidateSaturatedWaterContentData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrUnsatSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrUnsatSegmentTimeListLink;
    Link.FSaturatedWaterContentData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
        Link.FSaturatedWaterContentData.Invalidate;
      end;
    end;
  end;
end;

procedure TSfrUnsatSegmentCollection.InvalidateVerticalSaturatedKData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrUnsatSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrUnsatSegmentTimeListLink;
    Link.FVerticalSaturatedKData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
        Link.FVerticalSaturatedKData.Invalidate;
      end;
    end;
  end;
end;

class function TSfrUnsatSegmentCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrUnsatSegmentItem;
end;

procedure TSfrUnsatSegmentCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSfrUnsatSegmentStorage).
    FSrfUnsatSegmentArray, BoundaryCount);
  inherited;
end;

function TSfrUnsatSegmentCollection.ShouldDeleteItemsWithZeroDuration: Boolean;
begin
  result := False;
end;

{ TSfrUnsatSegment_Cell }

function TSfrUnsatSegment_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

procedure TSfrUnsatSegment_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TSfrUnsatSegment_Cell.GetBrooksCoreyExponent: double;
begin
  result := Values.BrooksCoreyExponent;
end;

function TSfrUnsatSegment_Cell.GetBrooksCoreyExponentAnnotation: string;
begin
  result := Values.BrooksCoreyExponentAnnotation;
end;

function TSfrUnsatSegment_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSfrUnsatSegment_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfrUnsatSegment_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSfrUnsatSegment_Cell.GetPestName(Index: Integer): string;
begin
  result := '';
  case Index of
    UnsatSaturatedWaterContentPosition: result := FValues.SaturatedWaterContentPestItem;
    UnSatInitialWaterContentPosition: result := FValues.InitialWaterContentPestItem;
    UnSatBrooksCoreyExponentPosition: result := FValues.BrooksCoreyExponentPestItem;
    UnSatVerticalSaturatedKPosition: result := FValues.VerticalSaturatedKPestItem;
    else Assert(False);
  end;
end;

//function TSfrUnsatSegment_Cell.GetPestSeriesMethod(
//  Index: Integer): TPestParamMethod;
//begin
//  case Index of
//    UnsatSaturatedWaterContentPosition: result := FValues.SaturatedWaterContentPestSeriesMethod;
//    UnSatInitialWaterContentPosition: result := FValues.InitialWaterContentPestSeriesMethod;
//    UnSatBrooksCoreyExponentPosition: result := FValues.BrooksCoreyExponentPestSeriesMethod;
//    UnSatVerticalSaturatedKPosition: result := FValues.VerticalSaturatedKPestSeriesMethod;
//    else
//      begin
//        result := inherited;
//        Assert(False);
//      end;
//  end;
//end;

//function TSfrUnsatSegment_Cell.GetPestSeriesName(Index: Integer): string;
//begin
//  result := '';
//  case Index of
//    UnsatSaturatedWaterContentPosition: result := FValues.SaturatedWaterContentPestSeriesItem;
//    UnSatInitialWaterContentPosition: result := FValues.InitialWaterContentPestSeriesItem;
//    UnSatBrooksCoreyExponentPosition: result := FValues.BrooksCoreyExponentPestSeriesItem;
//    UnSatVerticalSaturatedKPosition: result := FValues.VerticalSaturatedKPestSeriesItem;
//    else Assert(False);
//  end;
//end;

function TSfrUnsatSegment_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    UnsatSaturatedWaterContentPosition: result := SaturatedWaterContentAnnotation;
    UnSatInitialWaterContentPosition: result := InitialWaterContentAnnotation;
    UnSatBrooksCoreyExponentPosition: result := BrooksCoreyExponentAnnotation;
    UnSatVerticalSaturatedKPosition: result := VerticalSaturatedKAnnotation;
    else Assert(False);
  end;
end;

function TSfrUnsatSegment_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    UnsatSaturatedWaterContentPosition: result := SaturatedWaterContent;
    UnSatInitialWaterContentPosition: result := InitialWaterContent;
    UnSatBrooksCoreyExponentPosition: result := BrooksCoreyExponent;
    UnSatVerticalSaturatedKPosition: result := VerticalSaturatedK;
    else Assert(False);
  end;
end;

function TSfrUnsatSegment_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSfrUnsatSegment_Cell.GetSaturatedWaterContent: double;
begin
  result := Values.SaturatedWaterContent;
end;

function TSfrUnsatSegment_Cell.GetSaturatedWaterContentAnnotation: string;
begin
  result := Values.SaturatedWaterContentAnnotation;
end;

function TSfrUnsatSegment_Cell.GetInitialWaterContent: double;
begin
  result := Values.InitialWaterContent;
end;

function TSfrUnsatSegment_Cell.GetInitialWaterContentAnnotation: string;
begin
  result := Values.InitialWaterContentAnnotation;
end;

function TSfrUnsatSegment_Cell.GetVerticalSaturatedK: double;
begin
  result := Values.VerticalSaturatedK;
end;

function TSfrUnsatSegment_Cell.GetVerticalSaturatedKAnnotation: string;
begin
  result := Values.VerticalSaturatedKAnnotation;
end;
procedure TSfrUnsatSegment_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TSfrUnsatSegment_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TSfrUnsatSegmentRecord }

procedure TSfrUnsatSegmentRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, SaturatedWaterContent);
  WriteCompReal(Comp, InitialWaterContent);
  WriteCompReal(Comp, BrooksCoreyExponent);
  WriteCompReal(Comp, VerticalSaturatedK);

  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompInt(Comp, Strings.IndexOf(BrooksCoreyExponentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SaturatedWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(InitialWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(VerticalSaturatedKAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(BrooksCoreyExponentPestItem));
  WriteCompInt(Comp, Strings.IndexOf(SaturatedWaterContentPestItem));
  WriteCompInt(Comp, Strings.IndexOf(InitialWaterContentPestItem));
  WriteCompInt(Comp, Strings.IndexOf(VerticalSaturatedKPestItem));
//  WriteCompInt(Comp, Strings.IndexOf(BrooksCoreyExponentPestSeriesItem));
//  WriteCompInt(Comp, Strings.IndexOf(SaturatedWaterContentPestSeriesItem));
//  WriteCompInt(Comp, Strings.IndexOf(InitialWaterContentPestSeriesItem));
//  WriteCompInt(Comp, Strings.IndexOf(VerticalSaturatedKPestSeriesItem));
//  WriteCompInt(Comp, Ord(BrooksCoreyExponentPestSeriesMethod));
//  WriteCompInt(Comp, Ord(SaturatedWaterContentPestSeriesMethod));
//  WriteCompInt(Comp, Ord(InitialWaterContentPestSeriesMethod));
//  WriteCompInt(Comp, Ord(VerticalSaturatedKPestSeriesMethod));
end;

procedure TSfrUnsatSegmentRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(BrooksCoreyExponentAnnotation);
  Strings.Add(SaturatedWaterContentAnnotation);
  Strings.Add(InitialWaterContentAnnotation);
  Strings.Add(VerticalSaturatedKAnnotation);

  Strings.Add(BrooksCoreyExponentPestItem);
  Strings.Add(SaturatedWaterContentPestItem);
  Strings.Add(InitialWaterContentPestItem);
  Strings.Add(VerticalSaturatedKPestItem);
//  Strings.Add(BrooksCoreyExponentPestSeriesItem);
//  Strings.Add(SaturatedWaterContentPestSeriesItem);
//  Strings.Add(InitialWaterContentPestSeriesItem);
//  Strings.Add(VerticalSaturatedKPestSeriesItem);
end;

procedure TSfrUnsatSegmentRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  SaturatedWaterContent := ReadCompReal(Decomp);
  InitialWaterContent := ReadCompReal(Decomp);
  BrooksCoreyExponent := ReadCompReal(Decomp);
  VerticalSaturatedK := ReadCompReal(Decomp);

  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  BrooksCoreyExponentAnnotation := Annotations[ReadCompInt(Decomp)];
  SaturatedWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  InitialWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  VerticalSaturatedKAnnotation := Annotations[ReadCompInt(Decomp)];

  BrooksCoreyExponentPestItem := Annotations[ReadCompInt(Decomp)];
  SaturatedWaterContentPestItem := Annotations[ReadCompInt(Decomp)];
  InitialWaterContentPestItem := Annotations[ReadCompInt(Decomp)];
  VerticalSaturatedKPestItem := Annotations[ReadCompInt(Decomp)];
//  BrooksCoreyExponentPestSeriesItem := Annotations[ReadCompInt(Decomp)];
//  SaturatedWaterContentPestSeriesItem := Annotations[ReadCompInt(Decomp)];
//  InitialWaterContentPestSeriesItem := Annotations[ReadCompInt(Decomp)];
//  VerticalSaturatedKPestSeriesItem := Annotations[ReadCompInt(Decomp)];
//  BrooksCoreyExponentPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
//  SaturatedWaterContentPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
//  InitialWaterContentPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
//  VerticalSaturatedKPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TSfrUnsatSegmentStorage }

procedure TSfrUnsatSegmentStorage.Clear;
begin
  SetLength(FSrfUnsatSegmentArray, 0);
  FCleared := True;
end;

procedure TSfrUnsatSegmentStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FSrfUnsatSegmentArray);
    for Index := 0 to Count - 1 do
    begin
      FSrfUnsatSegmentArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FSrfUnsatSegmentArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TSfrUnsatSegmentStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSrfUnsatSegmentArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSrfUnsatSegmentArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TSfrUnsatSegmentStorage.GetSrfUnsatSegmentArray: TSrfUnsatSegmentArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSrfUnsatSegmentArray;
end;

{ TSfrUnsatSegmentTimeListLink }

procedure TSfrUnsatSegmentTimeListLink.CreateTimeLists;
begin
  inherited;
  FBrooksCoreyExponentData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInitialWaterContentData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSaturatedWaterContentData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FVerticalSaturatedKData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInitialWaterContentData.NonParamDescription := StrInitialWaterConten;
  FInitialWaterContentData.ParamDescription := ' ' + LowerCase(StrInitialWaterConten);
  FSaturatedWaterContentData.NonParamDescription := StrSaturatedWaterCont;
  FSaturatedWaterContentData.ParamDescription := ' ' + LowerCase(StrSaturatedWaterCont);
  FBrooksCoreyExponentData.NonParamDescription := StrBrooksCoreyExponen;
  FBrooksCoreyExponentData.ParamDescription := ' ' + StrBrooksCoreyExponen;
  FVerticalSaturatedKData.NonParamDescription := StrMaximumVerticalK;
  FVerticalSaturatedKData.ParamDescription := StrMaximumVerticalK_LC;
  if Model <> nil then
  begin
  end;
  AddTimeList(FSaturatedWaterContentData);
  AddTimeList(FInitialWaterContentData);
  AddTimeList(FBrooksCoreyExponentData);
  AddTimeList(FVerticalSaturatedKData);

end;

destructor TSfrUnsatSegmentTimeListLink.Destroy;
begin
  FInitialWaterContentData.Free;
  FSaturatedWaterContentData.Free;
  FBrooksCoreyExponentData.Free;
  FVerticalSaturatedKData.Free;
  inherited;
end;

end.

