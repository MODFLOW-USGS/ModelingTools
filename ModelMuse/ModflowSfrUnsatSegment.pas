unit ModflowSfrUnsatSegment;

interface

uses Windows, Classes, SysUtils, ZLib, RbwParser, GoPhastTypes,
  OrderedCollectionUnit, ModflowCellUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, SubscriptionUnit, SparseDataSets;

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
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList;ItemIndex: Integer;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames: TStringListObjectList); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
      PestItemNames: TStringListObjectList; Writer: TObject); override;
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

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, TempFiles, frmGoPhastUnit;

resourcestring
  StrInitialWaterConten = 'Initial water content';
  StrSaturatedWaterCont = 'Saturated water content';
  StrBrooksCoreyExponen = 'Brooks-Corey exponent';
  StrMaximumVerticalK = 'Maximum vertical K';
  StrMaximumVerticalK_LC = ' maximum vertical K';

const
  SaturatedWaterContentPosition = 0;
  InitialWaterContentPosition = 1;
  BrooksCoreyExponentPosition = 2;
  VerticalSaturatedKPosition = 3;

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
  SaturatedWaterContentObserver := FObserverList[SaturatedWaterContentPosition];
  SaturatedWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateSaturatedWaterContentData;
  InitialWaterContentObserver := FObserverList[InitialWaterContentPosition];
  InitialWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateInitialWaterContentData;
  BrooksCoreyExponentObserver := FObserverList[BrooksCoreyExponentPosition];
  BrooksCoreyExponentObserver.OnUpToDateSet := ParentCollection.InvalidateBrooksCoreyExponentData;
  VerticalSaturatedKObserver := FObserverList[VerticalSaturatedKPosition];
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
    SaturatedWaterContentPosition:
      result := SaturatedWaterContent;
    InitialWaterContentPosition:
      result := InitialWaterContent;
    BrooksCoreyExponentPosition:
      result := BrooksCoreyExponent;
    VerticalSaturatedKPosition:
      result := VerticalSaturatedK;
    else Assert(False);
  end;
end;

function TSfrUnsatSegmentItem.GetBrooksCoreyExponent: string;
begin
  Result := FBrooksCoreyExponent.Formula;
  ResetItemObserver(BrooksCoreyExponentPosition);
end;

function TSfrUnsatSegmentItem.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  ResetItemObserver(InitialWaterContentPosition);
end;

procedure TSfrUnsatSegmentItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FSaturatedWaterContent then
  begin
    List.Add(FObserverList[SaturatedWaterContentPosition]);
  end;
  if Sender = FInitialWaterContent then
  begin
    List.Add(FObserverList[InitialWaterContentPosition]);
  end;
  if Sender = FBrooksCoreyExponent then
  begin
    List.Add(FObserverList[BrooksCoreyExponentPosition]);
  end;
  if Sender = FVerticalSaturatedK then
  begin
    List.Add(FObserverList[VerticalSaturatedKPosition]);
  end;
end;

function TSfrUnsatSegmentItem.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  ResetItemObserver(SaturatedWaterContentPosition);
end;

function TSfrUnsatSegmentItem.GetVerticalSaturatedK: string;
begin
  Result := FVerticalSaturatedK.Formula;
  ResetItemObserver(VerticalSaturatedKPosition);
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
    SaturatedWaterContentPosition:
      SaturatedWaterContent := Value;
    InitialWaterContentPosition:
      InitialWaterContent := Value;
    BrooksCoreyExponentPosition:
      BrooksCoreyExponent := Value;
    VerticalSaturatedKPosition:
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
    UpdateFormulaBlocks(Value, BrooksCoreyExponentPosition, FBrooksCoreyExponent);
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
    UpdateFormulaBlocks(Value, SaturatedWaterContentPosition, FSaturatedWaterContent);
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
    UpdateFormulaBlocks(Value, InitialWaterContentPosition, FInitialWaterContent);
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
    UpdateFormulaBlocks(Value, VerticalSaturatedKPosition, FVerticalSaturatedK);
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
  PestMethods: TPestMethodList; PestItemNames: TStringListObjectList);
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
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  SaturatedWaterContentArray := DataSets[0];
  InitialWaterContentArray := DataSets[1];
  BrooksCoreyExponentArray := DataSets[2];
  VerticalSaturatedKArray := DataSets[3];
  Boundary := Boundaries[ItemIndex, AModel] as TSfrUnsatSegmentStorage;

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
                InitialWaterContent := InitialWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                InitialWaterContentAnnotation := InitialWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                SaturatedWaterContent := SaturatedWaterContentArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                SaturatedWaterContentAnnotation := SaturatedWaterContentArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                VerticalSaturatedK := VerticalSaturatedKArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                VerticalSaturatedKAnnotation := VerticalSaturatedKArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
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

function TSfrUnsatSegmentCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSfrUnsatSegmentTimeListLink;
end;

procedure TSfrUnsatSegmentCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames: TStringListObjectList; Writer: TObject);
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
begin
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;

  SetLength(BoundaryValues, Count);
  Boundary := BoundaryGroup as TSfrBoundary;
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
      BoundaryValues[Index].Formula := Item.BrooksCoreyExponent;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
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
      BoundaryValues[Index].Formula := Item.InitialWaterContent;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
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
      BoundaryValues[Index].Formula := Item.SaturatedWaterContent;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
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
      BoundaryValues[Index].Formula := Item.VerticalSaturatedK;
    end
    else
    begin
      BoundaryValues[Index].Formula := '0';
    end;
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
      Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
      Link.FBrooksCoreyExponentData.Invalidate;
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
      Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
      Link.FInitialWaterContentData.Invalidate;
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
      Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
      Link.FSaturatedWaterContentData.Invalidate;
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
      Link := TimeListLink.GetLink(ChildModel) as TSfrUnsatSegmentTimeListLink;
      Link.FVerticalSaturatedKData.Invalidate;
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

function TSfrUnsatSegment_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := SaturatedWaterContentAnnotation;
    1: result := InitialWaterContentAnnotation;
    2: result := BrooksCoreyExponentAnnotation;
    3: result := VerticalSaturatedKAnnotation;
    else Assert(False);
  end;
end;

function TSfrUnsatSegment_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := SaturatedWaterContent;
    1: result := InitialWaterContent;
    2: result := BrooksCoreyExponent;
    3: result := VerticalSaturatedK;
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
//  WriteCompString(Comp, BrooksCoreyExponentAnnotation);
//  WriteCompString(Comp, SaturatedWaterContentAnnotation);
//  WriteCompString(Comp, InitialWaterContentAnnotation);
//  WriteCompString(Comp, VerticalSaturatedKAnnotation);

end;

procedure TSfrUnsatSegmentRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(BrooksCoreyExponentAnnotation);
  Strings.Add(SaturatedWaterContentAnnotation);
  Strings.Add(InitialWaterContentAnnotation);
  Strings.Add(VerticalSaturatedKAnnotation);
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
//  BrooksCoreyExponentAnnotation := ReadCompString(Decomp, Annotations);
//  SaturatedWaterContentAnnotation := ReadCompString(Decomp, Annotations);
//  InitialWaterContentAnnotation := ReadCompString(Decomp, Annotations);
//  VerticalSaturatedKAnnotation := ReadCompString(Decomp, Annotations);
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
