unit ModflowSfrSegment;

interface

uses Windows, Classes, SysUtils, ZLib, RbwParser, GoPhastTypes,
  OrderedCollectionUnit, ModflowCellUnit, ModflowBoundaryUnit,
  FormulaManagerUnit, SubscriptionUnit, SparseDataSets;

type
  TSfrSegmentRecord = record
    Cell: TCellLocation;
    StreambedElevation: double;
    StreamBedThickness: double;
    HydraulicConductivity: double;
    StreamWidth: double;
    StreamDepth: double;
    StartingTime: double;
    EndingTime: double;
    HydraulicConductivityAnnotation: string;
    StreambedElevationAnnotation: string;
    StreamBedThicknessAnnotation: string;
    StreamWidthAnnotation: string;
    StreamDepthAnnotation: string;

    HydraulicConductivityPestItem: string;
    StreambedElevationPestItem: string;
    StreamBedThicknessPestItem: string;
    StreamWidthPestItem: string;
    StreamDepthPestItem: string;

    HydraulicConductivityPestSeriesItem: string;
    StreambedElevationPestSeriesItem: string;
    StreamBedThicknessPestSeriesItem: string;
    StreamWidthPestSeriesItem: string;
    StreamDepthPestSeriesItem: string;

    HydraulicConductivityPestSeriesMethod: TPestParamMethod;
    StreambedElevationPestSeriesMethod: TPestParamMethod;
    StreamBedThicknessPestSeriesMethod: TPestParamMethod;
    StreamWidthPestSeriesMethod: TPestParamMethod;
    StreamDepthPestSeriesMethod: TPestParamMethod;

    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;
  PSfrSegmentRecord = ^TSfrSegmentRecord;

  TSrfSegmentArray = array of TSfrSegmentRecord;

  TSfrSegmentStorage = class(TCustomBoundaryStorage)
  private
    FSrfSegmentArray: TSrfSegmentArray;
    function GetSrfSegmentArray: TSrfSegmentArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SrfSegmentArray: TSrfSegmentArray read GetSrfSegmentArray;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrSegmentCollection).
  TSfrSegmentItem = class(TCustomModflowBoundaryItem)
  private
    FStreamBedThickness: TFormulaObject;
    FStreambedElevation: TFormulaObject;
    FHydraulicConductivity: TFormulaObject;
    FStreamWidth: TFormulaObject;
    FStreamDepth: TFormulaObject;
    procedure SetHydraulicConductivity(const Value: string);
    procedure SetStreambedElevation(const Value: string);
    procedure SetStreamBedThickness(const Value: string);
    procedure SetStreamDepth(const Value: string);
    procedure SetStreamWidth(const Value: string);
    function GetHydraulicConductivity: string;
    function GetStreambedElevation: string;
    function GetStreamBedThickness: string;
    function GetStreamDepth: string;
    function GetStreamWidth: string;
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
    property HydraulicConductivity: string read GetHydraulicConductivity write SetHydraulicConductivity;
    property StreamBedThickness: string read GetStreamBedThickness write SetStreamBedThickness;
    property StreambedElevation: string read GetStreambedElevation write SetStreambedElevation;
    property StreamWidth: string read GetStreamWidth write SetStreamWidth;
    property StreamDepth: string read GetStreamDepth write SetStreamDepth;
  end;

  TSfrSegmentTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to compute the hydraulic conductivity for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FHydraulicConductivityData: TModflowTimeList;
    // @name is used to compute the streambed thickness for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamBedThicknessData: TModflowTimeList;
    // @name is used to compute the streambed elevation for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamBedElevationData: TModflowTimeList;
    // @name is used to compute the stream width for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamWidthData: TModflowTimeList;
    // @name is used to compute the stream depth for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamDepthData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrSegmentCollection = class(TCustomMF_ArrayBoundColl)
  private
    FAssignmentLocation: TAssignmentLocation;
    procedure InvalidateHydraulicConductivityData(Sender: TObject);
    procedure InvalidateStreamBedThicknessData(Sender: TObject);
    procedure InvalidateStreambedElevationData(Sender: TObject);
    procedure InvalidateStreamWidthData(Sender: TObject);
    procedure InvalidateStreamDepthData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
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
    // the @link(TRivStorage.RivArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel); override;
  public
    property AssignmentLocation: TAssignmentLocation read FAssignmentLocation
      write FAssignmentLocation;
    function GetItemByStartTime(StartTime: double): TSfrSegmentItem;
  end;

  TSfrSegmentParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TSfrSegment_Cell = class(TValueCell)
  private
    FValues: TSfrSegmentRecord;
    FStressPeriod: integer;
    function GetStreamBedThickness: double;
    function GetHydraulicConductivity: double;
    function GetStreambedElevation: double;
    function GetHydraulicConductivityAnnotation: string;
    function GetStreambedElevationAnnotation: string;
    function GetStreamBedThicknessAnnotation: string;
    function GetStreamDepth: double;
    function GetStreamDepthAnnotation: string;
    function GetStreamWidth: double;
    function GetStreamWidthAnnotation: string;
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
    function GetPestSeriesMethod(Index: Integer): TPestParamMethod; override;
    function GetPestSeriesName(Index: Integer): string; override;
  public
    property Values: TSfrSegmentRecord read FValues write FValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property HydraulicConductivity: double read GetHydraulicConductivity;
    property StreambedElevation: double read GetStreambedElevation;
    property StreamBedThickness: double read GetStreamBedThickness;
    property StreamWidth: double read GetStreamWidth;
    property StreamDepth: double read GetStreamDepth;
    property HydraulicConductivityAnnotation: string read GetHydraulicConductivityAnnotation;
    property StreambedElevationAnnotation: string read GetStreambedElevationAnnotation;
    property StreamBedThicknessAnnotation: string read GetStreamBedThicknessAnnotation;
    property StreamWidthAnnotation: string read GetStreamWidthAnnotation;
    property StreamDepthAnnotation: string read GetStreamDepthAnnotation;
  end;

resourcestring
  StrSfrInvalid = 'The following objects do not define SFR streams c' +
  'orrectly because they don''t set the values of intersected cells.';

implementation

uses Contnrs, DataSetUnit, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, TempFiles, frmGoPhastUnit, frmErrorsAndWarningsUnit;

resourcestring
  StrStreambedThickness = 'Streambed thickness';
  StrStreambedElevation = 'Streambed elevation';
  StrHydraulicConductivi = 'Hydraulic conductivity';
  StrHydraulicConductivMult = ' Hydraulic conductivity multiplier';
  StrStreamWidth = 'Stream width';
  StrStreamDepth = 'Stream depth';

const
  HydraulicConductivityPosition = 0;
  StreamBedThicknessPosition = 1;
  StreambedElevationPosition = 2;
  StreamWidthPosition = 3;
  StreamDepthPosition = 4;

{ TSfrSegmentItem }

procedure TSfrSegmentItem.Assign(Source: TPersistent);
var
  Sfr: TSfrSegmentItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrSegmentItem then
  begin
    Sfr := TSfrSegmentItem(Source);
    HydraulicConductivity := Sfr.HydraulicConductivity;
    StreamBedThickness := Sfr.StreamBedThickness;
    StreambedElevation := Sfr.StreambedElevation;
    StreamWidth := Sfr.StreamWidth;
    StreamDepth := Sfr.StreamDepth;
  end;
  inherited;
end;

procedure TSfrSegmentItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrSegmentCollection;
  HydraulicConductivityObserver: TObserver;
  StreamBedThicknessObserver: TObserver;
  StreambedElevationObserver: TObserver;
  StreamWidthObserver: TObserver;
  StreamDepthObserver: TObserver;
begin
  ParentCollection := Collection as TSfrSegmentCollection;
  HydraulicConductivityObserver := FObserverList[HydraulicConductivityPosition];
  HydraulicConductivityObserver.OnUpToDateSet := ParentCollection.InvalidateHydraulicConductivityData;
  StreamBedThicknessObserver := FObserverList[StreamBedThicknessPosition];
  StreamBedThicknessObserver.OnUpToDateSet := ParentCollection.InvalidateStreamBedThicknessData;
  StreambedElevationObserver := FObserverList[StreambedElevationPosition];
  StreambedElevationObserver.OnUpToDateSet := ParentCollection.InvalidateStreambedElevationData;
  StreamWidthObserver := FObserverList[StreamWidthPosition];
  StreamWidthObserver.OnUpToDateSet := ParentCollection.InvalidateStreamWidthData;
  StreamDepthObserver := FObserverList[StreamDepthPosition];
  StreamDepthObserver.OnUpToDateSet := ParentCollection.InvalidateStreamDepthData;
end;

function TSfrSegmentItem.BoundaryFormulaCount: integer;
begin
  result := 5;
end;

constructor TSfrSegmentItem.Create(Collection: TCollection);
begin
  inherited;
  StreamBedThickness := '1';
  StreambedElevation := '0';
  HydraulicConductivity := 'Kx';
  StreamWidth := '10';
  StreamDepth := '1';
end;

procedure TSfrSegmentItem.CreateFormulaObjects;
begin
  FHydraulicConductivity := CreateFormulaObject(dso3D);
  FStreamBedThickness := CreateFormulaObject(dso3D);
  FStreambedElevation := CreateFormulaObject(dso3D);
  FStreamWidth := CreateFormulaObject(dso3D);
  FStreamDepth := CreateFormulaObject(dso3D);
end;

destructor TSfrSegmentItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrSegmentItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    HydraulicConductivityPosition:
      result := HydraulicConductivity;
    StreamBedThicknessPosition:
      result := StreamBedThickness;
    StreambedElevationPosition:
      result := StreambedElevation;
    StreamWidthPosition:
      result := StreamWidth;
    StreamDepthPosition:
      result := StreamDepth;
    else Assert(False);
  end;
end;

function TSfrSegmentItem.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivity.Formula;
  ResetItemObserver(HydraulicConductivityPosition);
end;

procedure TSfrSegmentItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FHydraulicConductivity then
  begin
    List.Add(FObserverList[HydraulicConductivityPosition]);
  end;
  if Sender = FStreamBedThickness then
  begin
    List.Add(FObserverList[StreamBedThicknessPosition]);
  end;
  if Sender = FStreambedElevation then
  begin
    List.Add(FObserverList[StreambedElevationPosition]);
  end;
  if Sender = FStreamWidth then
  begin
    List.Add(FObserverList[StreamWidthPosition]);
  end;
  if Sender = FStreamDepth then
  begin
    List.Add(FObserverList[StreamDepthPosition]);
  end;
end;

function TSfrSegmentItem.GetStreambedElevation: string;
begin
  Result := FStreambedElevation.Formula;
  ResetItemObserver(StreambedElevationPosition);
end;

function TSfrSegmentItem.GetStreamBedThickness: string;
begin
  Result := FStreamBedThickness.Formula;
  ResetItemObserver(StreamBedThicknessPosition);
end;

function TSfrSegmentItem.GetStreamDepth: string;
begin
  Result := FStreamDepth.Formula;
  ResetItemObserver(StreamDepthPosition);
end;

function TSfrSegmentItem.GetStreamWidth: string;
begin
  Result := FStreamWidth.Formula;
  ResetItemObserver(StreamWidthPosition);
end;

function TSfrSegmentItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrSegmentItem;
begin
  result := (AnotherItem is TSfrSegmentItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrSegmentItem(AnotherItem);
    result := (Item.HydraulicConductivity = HydraulicConductivity)
      and (Item.StreamBedThickness = StreamBedThickness)
      and (Item.StreambedElevation = StreambedElevation)
      and (Item.StreamWidth = StreamWidth)
      and (Item.StreamDepth = StreamDepth);
  end;
end;

procedure TSfrSegmentItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamDepth,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamWidth,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedElevation,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamBedThickness,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHydraulicConductivity,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

procedure TSfrSegmentItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    HydraulicConductivityPosition:
      HydraulicConductivity := Value;
    StreamBedThicknessPosition:
      StreamBedThickness := Value;
    StreambedElevationPosition:
      StreambedElevation := Value;
    StreamWidthPosition:
      StreamWidth := Value;
    StreamDepthPosition:
      StreamDepth := Value;
    else Assert(False);
  end;
end;


procedure TSfrSegmentItem.SetHydraulicConductivity(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
  ModflowSfrBoundary: TSfrBoundary;
begin
  if FHydraulicConductivity.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, HydraulicConductivityPosition, FHydraulicConductivity);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        ModflowSfrBoundary := (ScreenObj as TScreenObject).
          ModflowSfrBoundary ;
        if ModflowSfrBoundary <> nil then
        begin
          if Collection = ModflowSfrBoundary.UpstreamSegmentValues then
          begin
            PhastModel.InvalidateMfSfrUpstreamHydraulicConductivity(self);
          end
          else
          begin
            Assert(Collection = ModflowSfrBoundary.DownstreamSegmentValues);
            PhastModel.InvalidateMfSfrDownstreamHydraulicConductivity(self);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreambedElevation(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
  ModflowSfrBoundary: TSfrBoundary;
begin
  if FStreambedElevation.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, StreambedElevationPosition, FStreambedElevation);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        ModflowSfrBoundary := (ScreenObj as TScreenObject).
          ModflowSfrBoundary ;
        if ModflowSfrBoundary <> nil then
        begin
          if Collection = ModflowSfrBoundary.UpstreamSegmentValues then
          begin
            PhastModel.InvalidateMfSfrUpstreamElevation(self);
          end
          else
          begin
            Assert(Collection = ModflowSfrBoundary.DownstreamSegmentValues);
            PhastModel.InvalidateMfSfrDownstreamElevation(self);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreamBedThickness(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
  ModflowSfrBoundary: TSfrBoundary;
begin
  if FStreamBedThickness.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, StreamBedThicknessPosition, FStreamBedThickness);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        ModflowSfrBoundary := (ScreenObj as TScreenObject).
          ModflowSfrBoundary ;
        if ModflowSfrBoundary <> nil then
        begin
          if Collection = ModflowSfrBoundary.UpstreamSegmentValues then
          begin
            PhastModel.InvalidateMfSfrUpstreamThickness(self);
          end
          else
          begin
            Assert(Collection = ModflowSfrBoundary.DownstreamSegmentValues);
            PhastModel.InvalidateMfSfrDownstreamThickness(self);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreamDepth(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
  ModflowSfrBoundary: TSfrBoundary;
begin
  if FStreamDepth.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, StreamDepthPosition, FStreamDepth);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        ModflowSfrBoundary := (ScreenObj as TScreenObject).
          ModflowSfrBoundary ;
        if ModflowSfrBoundary <> nil then
        begin
          if Collection = ModflowSfrBoundary.UpstreamSegmentValues then
          begin
            PhastModel.InvalidateMfSfrUpstreamDepth(self);
          end
          else
          begin
            Assert(Collection = ModflowSfrBoundary.DownstreamSegmentValues);
            PhastModel.InvalidateMfSfrDownstreamDepth(self);
          end;
        end;
      end;
    end;
  end;
end;

procedure TSfrSegmentItem.SetStreamWidth(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
  ModflowSfrBoundary: TSfrBoundary;
begin
  if FStreamWidth.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, StreamWidthPosition, FStreamWidth);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        ModflowSfrBoundary := (ScreenObj as TScreenObject).
          ModflowSfrBoundary ;
        if ModflowSfrBoundary <> nil then
        begin
          if Collection = ModflowSfrBoundary.UpstreamSegmentValues then
          begin
            PhastModel.InvalidateMfSfrUpstreamWidth(self);
          end
          else
          begin
            Assert(Collection = ModflowSfrBoundary.DownstreamSegmentValues);
            PhastModel.InvalidateMfSfrDownstreamWidth(self);
          end;
        end;
      end;
    end;
  end;
end;

{ TSfrSegmentCollection }

procedure TSfrSegmentCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TSfrSegmentStorage.Create(AModel));
end;

procedure TSfrSegmentCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel; PestSeries: TStringList;
  PestMethods: TPestMethodList; PestItemNames, TimeSeriesNames: TStringListObjectList);
var
  HydraulicConductivityArray: TDataArray;
  StreamBedThicknessArray: TDataArray;
  StreambedElevationArray: TDataArray;
  StreamWidthArray: TDataArray;
  Boundary: TSfrSegmentStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  StreamDepthArray: TDataArray;
  LocalModel: TCustomModel;
  LayerMin: Integer;
  RowMin: Integer;
  ColMin: Integer;
  LayerMax: Integer;
  RowMax: Integer;
  ColMax: Integer;
//  SfrBoundary: TSfrBoundary;
//  OffSet: Integer;
  LocalHydraulicConductivityPestSeries: string;
  LocalHydraulicConductivityPestMethod: TPestParamMethod;
  HydraulicConductivityPestItems: TStringList;
  LocalHydraulicConductivityPest: string;
  LocalStreamBedThicknessPestSeries: string;
  LocalStreamBedThicknessPestMethod: TPestParamMethod;
  StreamBedThicknessPestItems: TStringList;
  LocalStreamBedThicknessPest: string;
  LocalStreambedElevationPestSeries: string;
  LocalStreambedElevationPestMethod: TPestParamMethod;
  StreambedElevationPestItems: TStringList;
  LocalStreambedElevationPest: string;
  LocalStreamWidthPestSeries: string;
  LocalStreamWidthPestMethod: TPestParamMethod;
  StreamWidthPestItems: TStringList;
  LocalStreamWidthPest: string;
  LocalStreamDepthPestSeries: string;
  LocalStreamDepthPestMethod: TPestParamMethod;
  StreamDepthPestItems: TStringList;
  LocalStreamDepthPest: string;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryIndex := 0;
  HydraulicConductivityArray := DataSets[HydraulicConductivityPosition];
  StreamBedThicknessArray := DataSets[StreamBedThicknessPosition];
  StreambedElevationArray := DataSets[StreambedElevationPosition];
  StreamWidthArray := DataSets[StreamWidthPosition];
  StreamDepthArray := DataSets[StreamDepthPosition];
  Boundary := Boundaries[ItemIndex, AModel] as TSfrSegmentStorage;
//  SfrBoundary := BoundaryGroup as TSfrBoundary;
//  if SfrBoundary.UpstreamSegmentValues = self then
//  begin
//    OffSet := 0;
//  end
//  else
//  begin
//    OffSet := 5;
//    Assert(SfrBoundary.DownstreamSegmentValues = self);
//  end;

  LocalHydraulicConductivityPestSeries := PestSeries[HydraulicConductivityPosition];
  LocalHydraulicConductivityPestMethod := PestMethods[HydraulicConductivityPosition];
  HydraulicConductivityPestItems := PestItemNames[HydraulicConductivityPosition];
  LocalHydraulicConductivityPest := HydraulicConductivityPestItems[ItemIndex];

  LocalStreamBedThicknessPestSeries := PestSeries[StreamBedThicknessPosition];
  LocalStreamBedThicknessPestMethod := PestMethods[StreamBedThicknessPosition];
  StreamBedThicknessPestItems := PestItemNames[StreamBedThicknessPosition];
  LocalStreamBedThicknessPest := StreamBedThicknessPestItems[ItemIndex];

  LocalStreambedElevationPestSeries := PestSeries[StreambedElevationPosition];
  LocalStreambedElevationPestMethod := PestMethods[StreambedElevationPosition];
  StreambedElevationPestItems := PestItemNames[StreambedElevationPosition];
  LocalStreambedElevationPest := StreambedElevationPestItems[ItemIndex];

  LocalStreamWidthPestSeries := PestSeries[StreamWidthPosition];
  LocalStreamWidthPestMethod := PestMethods[StreamWidthPosition];
  StreamWidthPestItems := PestItemNames[StreamWidthPosition];
  LocalStreamWidthPest := StreamWidthPestItems[ItemIndex];

  LocalStreamDepthPestSeries := PestSeries[StreamDepthPosition];
  LocalStreamDepthPestMethod := PestMethods[StreamDepthPosition];
  StreamDepthPestItems := PestItemNames[StreamDepthPosition];
  LocalStreamDepthPest := StreamDepthPestItems[ItemIndex];

{
  HydraulicConductivityPosition = 0;
  StreamBedThicknessPosition = 1;
  StreambedElevationPosition = 2;
  StreamWidthPosition = 3;
  StreamDepthPosition = 4;
}

  HydraulicConductivityArray.GetMinMaxStoredLimits(LayerMin, RowMin, ColMin,
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
            if HydraulicConductivityArray.IsValue[LayerIndex, RowIndex, ColIndex] then
            begin
              Assert(StreamBedThicknessArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(StreambedElevationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(StreamWidthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(StreamDepthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
              Assert(BoundaryIndex < Length(Boundary.SrfSegmentArray));
              with Boundary.SrfSegmentArray[BoundaryIndex] do
              begin
                Cell.Layer := LayerIndex;
                Cell.Row := RowIndex;
                Cell.Column := ColIndex;

                HydraulicConductivity := HydraulicConductivityArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                HydraulicConductivityAnnotation := HydraulicConductivityArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreamBedThickness := StreamBedThicknessArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreamBedThicknessAnnotation := StreamBedThicknessArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreambedElevation := StreambedElevationArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreambedElevationAnnotation := StreambedElevationArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreamWidth := StreamWidthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreamWidthAnnotation := StreamWidthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];
                StreamDepth := StreamDepthArray.
                  RealData[LayerIndex, RowIndex, ColIndex];
                StreamDepthAnnotation := StreamDepthArray.
                  Annotation[LayerIndex, RowIndex, ColIndex];

                HydraulicConductivityPestItem := LocalHydraulicConductivityPest;
                StreambedElevationPestItem := LocalStreambedElevationPest;
                StreamBedThicknessPestItem := LocalStreamBedThicknessPest;
                StreamWidthPestItem := LocalStreamWidthPest;
                StreamDepthPestItem := LocalStreamDepthPest;

                HydraulicConductivityPestSeriesItem := LocalHydraulicConductivityPestSeries;
                StreambedElevationPestSeriesItem := LocalStreambedElevationPestSeries;
                StreamBedThicknessPestSeriesItem := LocalStreamBedThicknessPestSeries;
                StreamWidthPestSeriesItem := LocalStreamWidthPestSeries;
                StreamDepthPestSeriesItem := LocalStreamDepthPestSeries;

                HydraulicConductivityPestSeriesMethod := LocalHydraulicConductivityPestMethod;
                StreambedElevationPestSeriesMethod := LocalStreambedElevationPestMethod;
                StreamBedThicknessPestSeriesMethod := LocalStreamBedThicknessPestMethod;
                StreamWidthPestSeriesMethod := LocalStreamWidthPestMethod;
                StreamDepthPestSeriesMethod := LocalStreamDepthPestMethod;
              end;
              Inc(BoundaryIndex);
            end;
          end;
        end;
      end;
    end;
  end;
  HydraulicConductivityArray.CacheData;
  StreamBedThicknessArray.CacheData;
  StreambedElevationArray.CacheData;
  StreamWidthArray.CacheData;
  StreamDepthArray.CacheData;
  Boundary.CacheData;
end;

function TSfrSegmentCollection.GetItemByStartTime(
  StartTime: double): TSfrSegmentItem;
var
  Index: Integer;
  Item: TSfrSegmentItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
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

function TSfrSegmentCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSfrSegmentTimeListLink;
end;

procedure TSfrSegmentCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel; PestSeries: TStringList; PestMethods: TPestMethodList;
  PestItemNames, TimeSeriesNames: TStringListObjectList; Writer: TObject);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSfrSegmentItem;
  Boundary: TSfrBoundary;
  ScreenObject: TScreenObject;
  ISFROPT: integer;
  ICALC: integer;
  ItemUsed: boolean;
  HydraulicConductivityData: TModflowTimeList;
  StreamBedThicknessData: TModflowTimeList;
  StreamBedElevationData: TModflowTimeList;
  StreamWidthData: TModflowTimeList;
  StreamDepthData: TModflowTimeList;
  ALink: TSfrSegmentTimeListLink;
  OffSet: Integer;
  PestHydraulicConductivitySeriesName: string;
  PestHydraulicConductivityMethod: TPestParamMethod;
  PestHydraulicConductivityItems: TStringList;
  ItemFormula: string;
  PestStreamBedThicknessSeriesName: string;
  PestStreamBedThicknessMethod: TPestParamMethod;
  PestStreamBedThicknessItems: TStringList;
  PestStreamBedElevationSeriesName: string;
  PestStreamBedElevationMethod: TPestParamMethod;
  PestStreamBedElevationItems: TStringList;
  PestStreamWidthSeriesName: string;
  PestStreamWidthMethod: TPestParamMethod;
  PestStreamWidthItems: TStringList;
  PestStreamDepthSeriesName: string;
  PestStreamDepthMethod: TPestParamMethod;
  PestStreamDepthItems: TStringList;
  TimeSeriesItems: TStringList;
begin
  ISFROPT := (AModel as TCustomModel).ModflowPackages.SfrPackage.Isfropt;
  SetLength(BoundaryValues, Count);

  Boundary := BoundaryGroup as TSfrBoundary;
  if Boundary.UpstreamSegmentValues = self then
  begin
    OffSet := 0;
  end
  else
  begin
    OffSet := 5;
    Assert(Boundary.DownstreamSegmentValues = self);
  end;

  ScreenObject := Boundary.ScreenObject as TScreenObject;
  if (AssignmentLocation <> alAll)
    and (not ScreenObject.SetValuesOfIntersectedCells) then
  begin
    frmErrorsAndWarnings.AddError(AModel, StrSfrInvalid, ScreenObject.Name,
      ScreenObject);
    Exit;
  end;

  PestHydraulicConductivitySeriesName :=
    BoundaryGroup.PestBoundaryFormula[HydraulicConductivityPosition+Offset];
  PestSeries.Add(PestHydraulicConductivitySeriesName);
  PestHydraulicConductivityMethod :=
    BoundaryGroup.PestBoundaryMethod[HydraulicConductivityPosition+Offset];
  PestMethods.Add(PestHydraulicConductivityMethod);
  PestHydraulicConductivityItems := TStringList.Create;
  PestItemNames.Add(PestHydraulicConductivityItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    ItemUsed := ISFROPT  in [0,4,5];
    if ItemUsed then
    begin
      ItemFormula := Item.HydraulicConductivity;
//      BoundaryValues[Index].Formula := Item.HydraulicConductivity;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, PestHydraulicConductivitySeriesName,
      PestHydraulicConductivityMethod, PestHydraulicConductivityItems,
      TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  ALink := TimeListLink.GetLink(AModel) as TSfrSegmentTimeListLink;
  HydraulicConductivityData := ALink.FHydraulicConductivityData;
  HydraulicConductivityData.Initialize(BoundaryValues, ScreenObject, lctUse,
    AssignmentLocation);

  PestStreamBedThicknessSeriesName :=
    BoundaryGroup.PestBoundaryFormula[StreamBedThicknessPosition+Offset];
  PestSeries.Add(PestStreamBedThicknessSeriesName);
  PestStreamBedThicknessMethod :=
    BoundaryGroup.PestBoundaryMethod[StreamBedThicknessPosition+Offset];
  PestMethods.Add(PestStreamBedThicknessMethod);
  PestStreamBedThicknessItems := TStringList.Create;
  PestItemNames.Add(PestStreamBedThicknessItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    ItemUsed := ISFROPT in [0,4,5];
    if ItemUsed then
    begin
      if ISFROPT in [4,5] then
      begin
        Assert(ScreenObject.ModflowSfrBoundary <> nil);
        ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
        if ICALC in [1,2] then
        begin
//          ItemUsed := Index = 0;
        end
        else
        begin
          ItemUsed := True;
        end;
      end;
    end;
    if ItemUsed then
    begin
      ItemFormula := Item.StreamBedThickness;
//      BoundaryValues[Index].Formula := Item.StreamBedThickness;
    end
    else
    begin
      ItemFormula := '0';
      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, PestStreamBedThicknessSeriesName,
      PestStreamBedThicknessMethod, PestStreamBedThicknessItems,
      TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  StreamBedThicknessData := ALink.FStreamBedThicknessData;
  StreamBedThicknessData.Initialize(BoundaryValues, ScreenObject,
    lctUse, AssignmentLocation);

  PestStreamBedElevationSeriesName :=
    BoundaryGroup.PestBoundaryFormula[StreamBedElevationPosition+Offset];
  PestSeries.Add(PestStreamBedElevationSeriesName);
  PestStreamBedElevationMethod :=
    BoundaryGroup.PestBoundaryMethod[StreamBedElevationPosition+Offset];
  PestMethods.Add(PestStreamBedElevationMethod);
  PestStreamBedElevationItems := TStringList.Create;
  PestItemNames.Add(PestStreamBedElevationItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    ItemUsed := ISFROPT in [0,4,5];
    if ItemUsed then
    begin
      if ISFROPT in [4,5] then
      begin
        Assert(ScreenObject.ModflowSfrBoundary <> nil);
        ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
        if ICALC in [1,2] then
        begin
          ItemUsed := Index = 0;
        end
        else
        begin
          ItemUsed := True;
        end;
      end;
    end;
    if ItemUsed then
    begin
      ItemFormula := Item.StreamBedElevation;
//      BoundaryValues[Index].Formula := Item.StreamBedElevation;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, PestStreamBedElevationSeriesName,
      PestStreamBedElevationMethod, PestStreamBedElevationItems,
      TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  StreamBedElevationData := ALink.FStreamBedElevationData;
  StreamBedElevationData.Initialize(BoundaryValues, ScreenObject,
    lctUse, AssignmentLocation);

  PestStreamWidthSeriesName :=
    BoundaryGroup.PestBoundaryFormula[StreamWidthPosition+Offset];
  PestSeries.Add(PestStreamWidthSeriesName);
  PestStreamWidthMethod :=
    BoundaryGroup.PestBoundaryMethod[StreamWidthPosition+Offset];
  PestMethods.Add(PestStreamWidthMethod);
  PestStreamWidthItems := TStringList.Create;
  PestItemNames.Add(PestStreamWidthItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := ICALC < 2;
    if ItemUsed then
    begin
      if (ISFROPT > 1) and (ICALC = 1) then
      begin
        ItemUsed := Index = 0;
      end;
    end;
    if ItemUsed then
    begin
      ItemFormula := Item.StreamWidth;
//      BoundaryValues[Index].Formula := Item.StreamWidth;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, PestStreamWidthSeriesName,
      PestStreamWidthMethod, PestStreamWidthItems,
      TimeSeriesItems, ItemFormula, Writer, BoundaryValues[Index]);
  end;
  StreamWidthData := ALink.FStreamWidthData;
  StreamWidthData.Initialize(BoundaryValues, ScreenObject,
    lctUse, AssignmentLocation);

  PestStreamDepthSeriesName :=
    BoundaryGroup.PestBoundaryFormula[StreamDepthPosition+Offset];
  PestSeries.Add(PestStreamDepthSeriesName);
  PestStreamDepthMethod :=
    BoundaryGroup.PestBoundaryMethod[StreamDepthPosition+Offset];
  PestMethods.Add(PestStreamDepthMethod);
  PestStreamDepthItems := TStringList.Create;
  PestItemNames.Add(PestStreamDepthItems);
  TimeSeriesItems := TStringList.Create;
  TimeSeriesNames.Add(TimeSeriesItems);

  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as TSfrSegmentItem;
    BoundaryValues[Index].Time := Item.StartTime;
    Assert(ScreenObject.ModflowSfrBoundary <> nil);
    ICALC := ScreenObject.ModflowSfrBoundary.ParamIcalc.ICalc(Item.StartTime);
    ItemUsed := ICALC = 0;
    if ItemUsed then
    begin
      ItemFormula := Item.StreamDepth;
//      BoundaryValues[Index].Formula := Item.StreamDepth;
    end
    else
    begin
      ItemFormula := '0';
//      BoundaryValues[Index].Formula := '0';
    end;
    AssignBoundaryFormula(AModel, PestStreamDepthSeriesName,
      PestStreamDepthMethod, PestStreamDepthItems, TimeSeriesItems,
      ItemFormula, Writer, BoundaryValues[Index]);
  end;
  StreamDepthData := ALink.FStreamDepthData;
  StreamDepthData.Initialize(BoundaryValues, ScreenObject,
    lctUse, AssignmentLocation);

  Assert(HydraulicConductivityData.Count = Count);
  Assert(StreamBedThicknessData.Count = Count);
  Assert(StreamBedElevationData.Count = Count);
  Assert(StreamWidthData.Count = Count);
  Assert(StreamDepthData.Count = Count);
  ClearBoundaries(AModel);
  SetBoundaryCapacity(HydraulicConductivityData.Count, AModel);
  for TimeIndex := 0 to HydraulicConductivityData.Count - 1 do
  begin
    AddBoundary(TSfrSegmentStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(HydraulicConductivityData);
  ListOfTimeLists.Add(StreamBedThicknessData);
  ListOfTimeLists.Add(StreamBedElevationData);
  ListOfTimeLists.Add(StreamWidthData);
  ListOfTimeLists.Add(StreamDepthData);
end;

procedure TSfrSegmentCollection.InvalidateHydraulicConductivityData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrSegmentTimeListLink;
    Link.FHydraulicConductivityData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrSegmentTimeListLink;
      Link.FHydraulicConductivityData.Invalidate;
    end;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreambedElevationData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrSegmentTimeListLink;
    Link.FStreamBedElevationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrSegmentTimeListLink;
      Link.FStreamBedElevationData.Invalidate;
    end;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreamBedThicknessData(
  Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrSegmentTimeListLink;
    Link.FStreamBedThicknessData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrSegmentTimeListLink;
      Link.FStreamBedThicknessData.Invalidate;
    end;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreamDepthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrSegmentTimeListLink;
    Link.FStreamDepthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrSegmentTimeListLink;
      Link.FStreamDepthData.Invalidate;
    end;
  end;
end;

procedure TSfrSegmentCollection.InvalidateStreamWidthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrSegmentTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrSegmentTimeListLink;
    Link.FStreamWidthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrSegmentTimeListLink;
      Link.FStreamWidthData.Invalidate;
    end;
  end;
end;

class function TSfrSegmentCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrSegmentItem;
end;

procedure TSfrSegmentCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSfrSegmentStorage).FSrfSegmentArray,
    BoundaryCount);
  inherited;
end;

{ TSfrSegmentParamItem }

class function TSfrSegmentParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TSfrSegmentCollection;
end;

{ TSfrSegment_Cell }

procedure TSfrSegment_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TSfrSegment_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSfrSegment_Cell.GetHydraulicConductivity: double;
begin
  result := Values.HydraulicConductivity;
end;

function TSfrSegment_Cell.GetHydraulicConductivityAnnotation: string;
begin
  result := Values.HydraulicConductivityAnnotation;
end;

function TSfrSegment_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSfrSegment_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfrSegment_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSfrSegment_Cell.GetPestName(Index: Integer): string;
begin
  case Index of
    HydraulicConductivityPosition:
      begin
        result := Values.HydraulicConductivityPestItem;
      end;
    StreamBedThicknessPosition:
      begin
        result := Values.StreamBedThicknessPestItem;
      end;
    StreambedElevationPosition:
      begin
        result := Values.StreambedElevationPestItem;
      end;
    StreamWidthPosition:
      begin
        result := Values.StreamWidthPestItem;
      end;
    StreamDepthPosition:
      begin
        result := Values.StreamDepthPestItem;
      end;
    else
      begin
        result := '';
        Assert(False);
      end;

  end;
end;

function TSfrSegment_Cell.GetPestSeriesMethod(Index: Integer): TPestParamMethod;
begin
  case Index of
    HydraulicConductivityPosition:
      begin
        result := Values.HydraulicConductivityPestSeriesMethod;
      end;
    StreamBedThicknessPosition:
      begin
        result := Values.StreamBedThicknessPestSeriesMethod;
      end;
    StreambedElevationPosition:
      begin
        result := Values.StreambedElevationPestSeriesMethod;
      end;
    StreamWidthPosition:
      begin
        result := Values.StreamWidthPestSeriesMethod;
      end;
    StreamDepthPosition:
      begin
        result := Values.StreamDepthPestSeriesMethod;
      end;
    else
      begin
        result := ppmMultiply;
        Assert(False);
      end;
  end;
end;

function TSfrSegment_Cell.GetPestSeriesName(Index: Integer): string;
begin
  case Index of
    HydraulicConductivityPosition:
      begin
        result := Values.HydraulicConductivityPestSeriesItem;
      end;
    StreamBedThicknessPosition:
      begin
        result := Values.StreamBedThicknessPestSeriesItem;
      end;
    StreambedElevationPosition:
      begin
        result := Values.StreambedElevationPestSeriesItem;
      end;
    StreamWidthPosition:
      begin
        result := Values.StreamWidthPestSeriesItem;
      end;
    StreamDepthPosition:
      begin
        result := Values.StreamDepthPestSeriesItem;
      end;
    else
      begin
        result := '';
        Assert(False);
      end;

  end;
end;

function TSfrSegment_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    HydraulicConductivityPosition: result := HydraulicConductivityAnnotation;
    StreamBedThicknessPosition: result := StreamBedThicknessAnnotation;
    StreambedElevationPosition: result := StreambedElevationAnnotation;
    StreamWidthPosition: result := StreamWidthAnnotation;
    StreamDepthPosition: result := StreamDepthAnnotation;
    else Assert(False);
  end;
end;

function TSfrSegment_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    HydraulicConductivityPosition: result := HydraulicConductivity;
    StreamBedThicknessPosition: result := StreamBedThickness;
    StreambedElevationPosition: result := StreambedElevation;
    StreamWidthPosition: result := StreamWidth;
    StreamDepthPosition: result := StreamDepth;
    else Assert(False);
  end;
end;

function TSfrSegment_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSfrSegment_Cell.GetStreambedElevation: double;
begin
  result := Values.StreambedElevation;
end;

function TSfrSegment_Cell.GetStreambedElevationAnnotation: string;
begin
  result := Values.StreambedElevationAnnotation;
end;

function TSfrSegment_Cell.GetStreamBedThickness: double;
begin
  result := Values.StreamBedThickness;
end;

function TSfrSegment_Cell.GetStreamBedThicknessAnnotation: string;
begin
  result := Values.StreamBedThicknessAnnotation;
end;

function TSfrSegment_Cell.GetStreamDepth: double;
begin
  result := Values.StreamDepth;
end;

function TSfrSegment_Cell.GetStreamDepthAnnotation: string;
begin
  result := Values.StreamDepthAnnotation;
end;

function TSfrSegment_Cell.GetStreamWidth: double;
begin
  result := Values.StreamWidth;
end;

function TSfrSegment_Cell.GetStreamWidthAnnotation: string;
begin
  result := Values.StreamWidthAnnotation;
end;

procedure TSfrSegment_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TSfrSegment_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

{ TSfrSegmentRecord }

procedure TSfrSegmentRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, StreambedElevation);
  WriteCompReal(Comp, StreamBedThickness);
  WriteCompReal(Comp, HydraulicConductivity);
  WriteCompReal(Comp, StreamWidth);
  WriteCompReal(Comp, StreamDepth);

  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompInt(Comp, Strings.IndexOf(HydraulicConductivityAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreambedElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreamBedThicknessAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreamWidthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreamDepthAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(HydraulicConductivityPestItem));
  WriteCompInt(Comp, Strings.IndexOf(StreambedElevationPestItem));
  WriteCompInt(Comp, Strings.IndexOf(StreamBedThicknessPestItem));
  WriteCompInt(Comp, Strings.IndexOf(StreamWidthPestItem));
  WriteCompInt(Comp, Strings.IndexOf(StreamDepthPestItem));

  WriteCompInt(Comp, Strings.IndexOf(HydraulicConductivityPestSeriesItem));
  WriteCompInt(Comp, Strings.IndexOf(StreambedElevationPestSeriesItem));
  WriteCompInt(Comp, Strings.IndexOf(StreamBedThicknessPestSeriesItem));
  WriteCompInt(Comp, Strings.IndexOf(StreamWidthPestSeriesItem));
  WriteCompInt(Comp, Strings.IndexOf(StreamDepthPestSeriesItem));

  WriteCompInt(Comp, Ord(HydraulicConductivityPestSeriesMethod));
  WriteCompInt(Comp, Ord(StreambedElevationPestSeriesMethod));
  WriteCompInt(Comp, Ord(StreamBedThicknessPestSeriesMethod));
  WriteCompInt(Comp, Ord(StreamWidthPestSeriesMethod));
  WriteCompInt(Comp, Ord(StreamDepthPestSeriesMethod));
{
    HydraulicConductivityPestItem: string;
    StreambedElevationPestItem: string;
    StreamBedThicknessPestItem: string;
    StreamWidthPestItem: string;
    StreamDepthPestItem: string;

    HydraulicConductivityPestSeriesItem: string;
    StreambedElevationPestSeriesItem: string;
    StreamBedThicknessPestSeriesItem: string;
    StreamWidthPestSeriesItem: string;
    StreamDepthPestSeriesItem: string;

    HydraulicConductivityPestSeriesMethod: TPestParamMethod;
    StreambedElevationPestSeriesMethod: TPestParamMethod;
    StreamBedThicknessPestSeriesMethod: TPestParamMethod;
    StreamWidthPestSeriesMethod: TPestParamMethod;
    StreamDepthPestSeriesMethod: TPestParamMethod;
}
end;

procedure TSfrSegmentRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(HydraulicConductivityAnnotation);
  Strings.Add(StreambedElevationAnnotation);
  Strings.Add(StreamBedThicknessAnnotation);
  Strings.Add(StreamWidthAnnotation);
  Strings.Add(StreamDepthAnnotation);

  Strings.Add(HydraulicConductivityPestItem);
  Strings.Add(StreambedElevationPestItem);
  Strings.Add(StreamBedThicknessPestItem);
  Strings.Add(StreamWidthPestItem);
  Strings.Add(StreamDepthPestItem);

  Strings.Add(HydraulicConductivityPestSeriesItem);
  Strings.Add(StreambedElevationPestSeriesItem);
  Strings.Add(StreamBedThicknessPestSeriesItem);
  Strings.Add(StreamWidthPestSeriesItem);
  Strings.Add(StreamDepthPestSeriesItem);
end;

procedure TSfrSegmentRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  StreambedElevation := ReadCompReal(Decomp);
  StreamBedThickness := ReadCompReal(Decomp);
  HydraulicConductivity := ReadCompReal(Decomp);
  StreamWidth := ReadCompReal(Decomp);
  StreamDepth := ReadCompReal(Decomp);

  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  HydraulicConductivityAnnotation := Annotations[ReadCompInt(Decomp)];
  StreambedElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  StreamBedThicknessAnnotation := Annotations[ReadCompInt(Decomp)];
  StreamWidthAnnotation := Annotations[ReadCompInt(Decomp)];
  StreamDepthAnnotation := Annotations[ReadCompInt(Decomp)];

  HydraulicConductivityPestItem := Annotations[ReadCompInt(Decomp)];
  StreambedElevationPestItem := Annotations[ReadCompInt(Decomp)];
  StreamBedThicknessPestItem := Annotations[ReadCompInt(Decomp)];
  StreamWidthPestItem := Annotations[ReadCompInt(Decomp)];
  StreamDepthPestItem := Annotations[ReadCompInt(Decomp)];

  HydraulicConductivityPestSeriesItem := Annotations[ReadCompInt(Decomp)];
  StreambedElevationPestSeriesItem := Annotations[ReadCompInt(Decomp)];
  StreamBedThicknessPestSeriesItem := Annotations[ReadCompInt(Decomp)];
  StreamWidthPestSeriesItem := Annotations[ReadCompInt(Decomp)];
  StreamDepthPestSeriesItem := Annotations[ReadCompInt(Decomp)];

  HydraulicConductivityPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StreambedElevationPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StreamBedThicknessPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StreamWidthPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
  StreamDepthPestSeriesMethod := TPestParamMethod(ReadCompInt(Decomp));
end;

{ TSfrSegmentStorage }

procedure TSfrSegmentStorage.Clear;
begin
  SetLength(FSrfSegmentArray, 0);
  FCleared := True;
end;

procedure TSfrSegmentStorage.Store(Compressor: TCompressionStream);
var
  Count: Integer;
  Index: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FSrfSegmentArray);
    for Index := 0 to Count - 1 do
    begin
      FSrfSegmentArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FSrfSegmentArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TSfrSegmentStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Count: Integer;
  Index: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSrfSegmentArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSrfSegmentArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TSfrSegmentStorage.GetSrfSegmentArray: TSrfSegmentArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSrfSegmentArray;
end;

{ TSfrSegmentTimeListLink }

procedure TSfrSegmentTimeListLink.CreateTimeLists;
begin
  inherited;
  FHydraulicConductivityData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamBedThicknessData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamBedElevationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamWidthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamDepthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamBedThicknessData.NonParamDescription := StrStreambedThickness;
  FStreamBedThicknessData.ParamDescription := ' ' + LowerCase(StrStreambedThickness);
  FStreamBedElevationData.NonParamDescription := StrStreambedElevation;
  FStreamBedElevationData.ParamDescription := ' ' + LowerCase(StrStreambedElevation);
  FHydraulicConductivityData.NonParamDescription := StrHydraulicConductivi;
  FHydraulicConductivityData.ParamDescription := StrHydraulicConductivMult;
  FStreamWidthData.NonParamDescription := StrStreamWidth;
  FStreamWidthData.ParamDescription := ' ' + LowerCase(StrStreamWidth);
  FStreamDepthData.NonParamDescription := StrStreamDepth;
  FStreamDepthData.ParamDescription := ' ' + LowerCase(StrStreamDepth);
  if Model <> nil then
  begin
  end;
  AddTimeList(FHydraulicConductivityData);
  AddTimeList(FStreamBedThicknessData);
  AddTimeList(FStreamBedElevationData);
  AddTimeList(FStreamWidthData);
  AddTimeList(FStreamDepthData);

end;

destructor TSfrSegmentTimeListLink.Destroy;
begin
  FHydraulicConductivityData.Free;
  FStreamBedThicknessData.Free;
  FStreamBedElevationData.Free;
  FStreamWidthData.Free;
  FStreamDepthData.Free;
  inherited;
end;

end.
