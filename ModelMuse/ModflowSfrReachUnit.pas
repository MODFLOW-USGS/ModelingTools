unit ModflowSfrReachUnit;

interface

uses Windows, ZLib, SysUtils, Classes, RbwParser, OrderedCollectionUnit,
  ModflowCellUnit, ModflowBoundaryUnit, DataSetUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, GoPhastTypes;

type
  TSfrRecord = record
    Cell: TCellLocation;
    ReachLength: double;
    LgrReachLength: double;
    StreamSlope: double;
    StreambedElevation: double;
    StreamBedThickness: double;
    HydraulicConductivity: double;
    SaturatedWaterContent: double;
    InitialWaterContent: double;
    BrooksCoreyExponent: double;
    VerticalK: double;
    StartingTime: double;
    EndingTime: double;
    ReachLengthAnnotation: string;
    StreamSlopeAnnotation: string;
    HydraulicConductivityAnnotation: string;
    StreambedElevationAnnotation: string;
    StreamBedThicknessAnnotation: string;
    SaturatedWaterContentAnnotation: string;
    InitialWaterContentAnnotation: string;
    BrooksCoreyExponentAnnotation: string;
    VerticalKAnnotation: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    procedure AdjustReachLength(Factor: double);
  end;

  TSrfArray = array of TSfrRecord;

  TSfrStorage = class(TCustomBoundaryStorage)
  private
    FSfrArray: TSrfArray;
    function GetSfrArray: TSrfArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SfrArray: TSrfArray read GetSfrArray;
  end;

  // @name represents a MODFLOW Streamflow Routing boundary for one time interval.
  // @name is stored by @link(TSfrCollection).
  TSfrItem = class(TCustomModflowBoundaryItem)
  private
    FStreamBedThickness: TFormulaObject;
    FStreambedElevation: TFormulaObject;
    FHydraulicConductivity: TFormulaObject;
    FStreamSlope: TFormulaObject;
    FBrooksCoreyExponent: TFormulaObject;
    FVerticalK: TFormulaObject;
    FInitialWaterContent: TFormulaObject;
    FSaturatedWaterContent: TFormulaObject;
    FReachLength: TFormulaObject;
    procedure SetHydraulicConductivity(const Value: string);
    procedure SetStreambedElevation(const Value: string);
    procedure SetStreamBedThickness(const Value: string);
    procedure SetStreamSlope(const Value: string);
    procedure SetBrooksCoreyExponent(const Value: string);
    procedure SetInitialWaterContent(const Value: string);
    procedure SetSaturatedWaterContent(const Value: string);
    procedure SetVerticalK(const Value: string);
    procedure SetReachLength(const Value: string);
    function ScreenObject: TObject;
    function GetBrooksCoreyExponent: string;
    function GetHydraulicConductivity: string;
    function GetInitialWaterContent: string;
    function GetReachLength: string;
    function GetSaturatedWaterContent: string;
    function GetStreambedElevation: string;
    function GetStreamBedThickness: string;
    function GetStreamSlope: string;
    function GetVerticalK: string;
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
    constructor Create(Collection: TCollection); override;
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent);override;
    Destructor Destroy; override;
  published
    property ReachLength: string read GetReachLength write SetReachLength;
    property HydraulicConductivity: string read GetHydraulicConductivity
      write SetHydraulicConductivity;
    property StreamBedThickness: string read GetStreamBedThickness
      write SetStreamBedThickness;
    property StreambedElevation: string read GetStreambedElevation
      write SetStreambedElevation;
    property StreamSlope: string read GetStreamSlope write SetStreamSlope;
    property SaturatedWaterContent: string read GetSaturatedWaterContent
      write SetSaturatedWaterContent;
    property InitialWaterContent: string read GetInitialWaterContent
      write SetInitialWaterContent;
    property BrooksCoreyExponent: string read GetBrooksCoreyExponent
      write SetBrooksCoreyExponent;
    property VerticalK: string read GetVerticalK write SetVerticalK;
  end;

  TSfrTimeListLink = class(TTimeListsModelLink)
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
    // @name is used to compute the stream slope for a series of
    // Streamflow Routing Boundaries over a series of time intervals.
    FStreamSlopeData: TModflowTimeList;
    FSaturatedWaterContent: TModflowTimeList;
    FInitialWaterContent: TModflowTimeList;
    FBrooksCoreyExponent: TModflowTimeList;
    FVerticalK: TModflowTimeList;
    FReachLength: TModflowTimeList;
    FLgrReachLength: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;


  // @name represents MODFLOW Streamflow Routing boundaries
  // for a series of time intervals.
  TSfrCollection = class(TCustomMF_ArrayBoundColl)
  private
    procedure InvalidateReachLengthData(Sender: TObject);
    procedure InvalidateHydraulicConductivityData(Sender: TObject);
    procedure InvalidateBedThicknessData(Sender: TObject);
    procedure InvalidateStreambedElevationData(Sender: TObject);
    procedure InvalidateStreamSlopeData(Sender: TObject);
    procedure InvalidateSaturatedWaterContentData(Sender: TObject);
    procedure InvalidateInitialWaterContentData(Sender: TObject);
    procedure InvalidateBrooksCoreyExponentData(Sender: TObject);
    procedure InvalidateVerticalKData(Sender: TObject);
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure CountArrayBoundaryCells(var BoundaryCount: Integer;
      DataArray1: TDataArray; DataSets: TList; AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.AssignArrayCellValues
    // TCustomListArrayBoundColl.AssignArrayCellValues)
    procedure AssignArrayCellValues(DataSets: TList; ItemIndex: Integer;
      AModel: TBaseModel); override;
    // See @link(TCustomListArrayBoundColl.InitializeTimeLists
    // TCustomListArrayBoundColl.InitializeTimeLists)
    procedure InitializeTimeLists(ListOfTimeLists: TList;
      AModel: TBaseModel); override;
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
  end;

  TSfr_Cell = class(TValueCell)
  private
    FValues: TSfrRecord;
    FStressPeriod: integer;
    function GetStreamBedThickness: double;
    function GetHydraulicConductivity: double;
    function GetStreambedElevation: double;
    function GetHydraulicConductivityAnnotation: string;
    function GetStreambedElevationAnnotation: string;
    function GetStreamBedThicknessAnnotation: string;
    function GetStreamSlope: double;
    function GetStreamSlopeAnnotation: string;
    function GetBrooksCoreyExponent: double;
    function GetBrooksCoreyExponentAnnotation: string;
    function GetInitialWaterContent: double;
    function GetInitialWaterContentAnnotation: string;
    function GetSaturatedWaterContent: double;
    function GetSaturatedWaterContentAnnotation: string;
    function GetVerticalK: double;
    function GetVerticalKAnnotation: string;
    function GetReachLength: double;
    function GetReachLengthAnnotation: string;
    function GetLgrReachLength: double;
    procedure SetValues(const Value: TSfrRecord);
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
    property Values: TSfrRecord read FValues write SetValues;
    property StressPeriod: integer read FStressPeriod write FStressPeriod;
    property HydraulicConductivity: double read GetHydraulicConductivity;
    property StreambedElevation: double read GetStreambedElevation;
    property StreamBedThickness: double read GetStreamBedThickness;
    property StreamSlope: double read GetStreamSlope;
    property HydraulicConductivityAnnotation: string read GetHydraulicConductivityAnnotation;
    property StreambedElevationAnnotation: string read GetStreambedElevationAnnotation;
    property StreamBedThicknessAnnotation: string read GetStreamBedThicknessAnnotation;
    property StreamSlopeAnnotation: string read GetStreamSlopeAnnotation;

    property SaturatedWaterContent: double read GetSaturatedWaterContent;
    property InitialWaterContent: double read GetInitialWaterContent;
    property BrooksCoreyExponent: double read GetBrooksCoreyExponent;
    property VerticalK: double read GetVerticalK;
    property ReachLength: double read GetReachLength;
    property LgrReachLength: double read GetLgrReachLength;

    property ReachLengthAnnotation: string read GetReachLengthAnnotation;
    property SaturatedWaterContentAnnotation: string read GetSaturatedWaterContentAnnotation;
    property InitialWaterContentAnnotation: string read GetInitialWaterContentAnnotation;
    property BrooksCoreyExponentAnnotation: string read GetBrooksCoreyExponentAnnotation;
    property VerticalKAnnotation: string read GetVerticalKAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;
  end;

procedure CountArrayBoundaryCellsSfr(var BoundaryCount: Integer;
  DataArray1: TDataArray; DataSets: TList; AModel: TBaseModel;
  ScreenObject: TObject);


implementation

uses Contnrs, ScreenObjectUnit, ModflowTimeUnit, PhastModelUnit,
  ModflowSfrUnit, TempFiles, frmGoPhastUnit;

resourcestring
  StrReachLength = 'Reach length';
  StrLGRReachLength = 'LGR Reach length';
  StrLGRReachLengthLc = ' LGR reach length';
  StrStreambedThickness = 'Streambed thickness';
  StrStreambedElevation = 'Streambed elevation';
  StrHydraulicConductivi = 'Hydraulic conductivity';
  StrHydraulicConductivMult = ' Hydraulic conductivity multiplier';
  StrSlope = 'Slope';
  StrSaturatedWaterCont = 'Saturated water content';
  StrInitialWaterConten = 'Initial water content';
  StrBrooksCoreyExponen = 'Brooks-Corey exponent';
  StrMaximumVerticalUns = 'Maximum vertical unsaturated hydraulic conductivi' +
  'ty';

const
  ReachLengthPosition = 0;
  HydraulicConductivityPosition = 1;
  StreamBedThicknessPosition = 2;
  StreambedElevationPosition = 3;
  StreamSlopePosition = 4;
  SaturatedWaterContentPosition = 5;
  InitialWaterContentPosition = 6;
  BrooksCoreyExponentPosition = 7;
  VerticalKPosition = 8;

{ TSfrItem }

procedure TSfrItem.Assign(Source: TPersistent);
var
  Sfr: TSfrItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TSfrItem then
  begin
    Sfr := TSfrItem(Source);
    ReachLength := Sfr.ReachLength;
    HydraulicConductivity := Sfr.HydraulicConductivity;
    StreamBedThickness := Sfr.StreamBedThickness;
    StreambedElevation := Sfr.StreambedElevation;
    StreamSlope := Sfr.StreamSlope;
    SaturatedWaterContent := Sfr.SaturatedWaterContent;
    InitialWaterContent := Sfr.InitialWaterContent;
    BrooksCoreyExponent := Sfr.BrooksCoreyExponent;
    VerticalK := Sfr.VerticalK;
  end;
  inherited;
end;

procedure TSfrItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrCollection;
  ReachLengthObserver: TObserver;
  HydraulicConductivityObserver: TObserver;
  BedThicknessObserver: TObserver;
  StreambedElevationObserver: TObserver;
  StreamSlopeObserver: TObserver;
  SaturatedWaterContentObserver: TObserver;
  InitialWaterContentObserver: TObserver;
  BrooksCoreyExponentObserver: TObserver;
  VerticalKObserver: TObserver;
begin
  ParentCollection := Collection as TSfrCollection;
  ReachLengthObserver := FObserverList[ReachLengthPosition];
  ReachLengthObserver.OnUpToDateSet := ParentCollection.InvalidateReachLengthData;
  HydraulicConductivityObserver := FObserverList[HydraulicConductivityPosition];
  HydraulicConductivityObserver.OnUpToDateSet := ParentCollection.InvalidateHydraulicConductivityData;
  BedThicknessObserver := FObserverList[StreamBedThicknessPosition];
  BedThicknessObserver.OnUpToDateSet := ParentCollection.InvalidateBedThicknessData;
  StreambedElevationObserver  := FObserverList[StreambedElevationPosition];
  StreambedElevationObserver.OnUpToDateSet := ParentCollection.InvalidateStreambedElevationData;
  StreamSlopeObserver := FObserverList[StreamSlopePosition];
  StreamSlopeObserver.OnUpToDateSet := ParentCollection.InvalidateStreamSlopeData;
  SaturatedWaterContentObserver := FObserverList[SaturatedWaterContentPosition];
  SaturatedWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateSaturatedWaterContentData;
  InitialWaterContentObserver := FObserverList[InitialWaterContentPosition];
  InitialWaterContentObserver.OnUpToDateSet := ParentCollection.InvalidateInitialWaterContentData;
  BrooksCoreyExponentObserver := FObserverList[BrooksCoreyExponentPosition];
  BrooksCoreyExponentObserver.OnUpToDateSet := ParentCollection.InvalidateBrooksCoreyExponentData;
  VerticalKObserver := FObserverList[VerticalKPosition];
  VerticalKObserver.OnUpToDateSet := ParentCollection.InvalidateVerticalKData;
end;

function TSfrItem.BoundaryFormulaCount: integer;
begin
  result := 9;
end;

constructor TSfrItem.Create(Collection: TCollection);
begin
  inherited;
  ReachLength := StrObjectIntersectLength;
  StreambedElevation := '0';
  HydraulicConductivity := '0';
  StreamSlope := '0';
  BrooksCoreyExponent := '0';
  VerticalK := '0';
  InitialWaterContent := '0';
  SaturatedWaterContent := '0';
end;

procedure TSfrItem.CreateFormulaObjects;
begin
  FReachLength := CreateFormulaObject(dso3D);
  FHydraulicConductivity := CreateFormulaObject(dso3D);
  FStreamBedThickness := CreateFormulaObject(dso3D);
  FStreambedElevation := CreateFormulaObject(dso3D);
  FStreamSlope := CreateFormulaObject(dso3D);
  FSaturatedWaterContent := CreateFormulaObject(dso3D);
  FInitialWaterContent := CreateFormulaObject(dso3D);
  FBrooksCoreyExponent := CreateFormulaObject(dso3D);
  FVerticalK := CreateFormulaObject(dso3D);
end;

destructor TSfrItem.Destroy;
var
  Index: integer;
begin
  for Index := 0 to BoundaryFormulaCount - 1 do
  begin
    BoundaryFormula[Index] := '0';
  end;
  inherited;
end;

function TSfrItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    0:
      result := ReachLength;
    1:
      result := HydraulicConductivity;
    2:
      result := StreamBedThickness;
    3:
      result := StreambedElevation;
    4:
      result := StreamSlope;
    5:
      result := SaturatedWaterContent;
    6:
      result := InitialWaterContent;
    7:
      result := BrooksCoreyExponent;
    8:
      result := VerticalK;
    else Assert(False);
  end;
end;

function TSfrItem.GetBrooksCoreyExponent: string;
begin
  Result := FBrooksCoreyExponent.Formula;
  ResetItemObserver(BrooksCoreyExponentPosition);
end;

function TSfrItem.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivity.Formula;
  ResetItemObserver(HydraulicConductivityPosition);
end;

function TSfrItem.GetInitialWaterContent: string;
begin
  Result := FInitialWaterContent.Formula;
  ResetItemObserver(InitialWaterContentPosition);
end;

procedure TSfrItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FReachLength then
  begin
    List.Add(FObserverList[ReachLengthPosition]);
  end;
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
  if Sender = FStreamSlope then
  begin
    List.Add(FObserverList[StreamSlopePosition]);
  end;
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
  if Sender = FVerticalK then
  begin
    List.Add(FObserverList[VerticalKPosition]);
  end;
end;

function TSfrItem.GetReachLength: string;
begin
  Result := FReachLength.Formula;
  ResetItemObserver(ReachLengthPosition);
end;

function TSfrItem.GetSaturatedWaterContent: string;
begin
  Result := FSaturatedWaterContent.Formula;
  ResetItemObserver(SaturatedWaterContentPosition);
end;

function TSfrItem.GetStreambedElevation: string;
begin
  Result := FStreambedElevation.Formula;
  ResetItemObserver(StreambedElevationPosition);
end;

function TSfrItem.GetStreamBedThickness: string;
begin
  Result := FStreamBedThickness.Formula;
  ResetItemObserver(StreamBedThicknessPosition);
end;

function TSfrItem.GetStreamSlope: string;
begin
  Result := FStreamSlope.Formula;
  ResetItemObserver(StreamSlopePosition);
end;

function TSfrItem.GetVerticalK: string;
begin
  Result := FVerticalK.Formula;
  ResetItemObserver(VerticalKPosition);
end;

procedure TSfrItem.InvalidateModel;
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    ScreenObj := ScreenObject;
    if (ScreenObj <> nil)
      and (ScreenObj as TScreenObject).CanInvalidateModel then
    begin
      PhastModel.InvalidateMfSfrSegmentReachAndIcalc(self);
    end;
  end;
end;

function TSfrItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrItem;
begin
  result := (AnotherItem is TSfrItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrItem(AnotherItem);
    result :=
      (Item.ReachLength = ReachLength)
      and (Item.HydraulicConductivity = HydraulicConductivity)
      and (Item.StreamBedThickness = StreamBedThickness)
      and (Item.StreambedElevation = StreambedElevation)
      and (Item.StreamSlope = StreamSlope)
      and (Item.SaturatedWaterContent = SaturatedWaterContent)
      and (Item.InitialWaterContent = InitialWaterContent)
      and (Item.BrooksCoreyExponent = BrooksCoreyExponent)
      and (Item.VerticalK = VerticalK);
  end;
end;

procedure TSfrItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FVerticalK,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBrooksCoreyExponent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FInitialWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSaturatedWaterContent,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamSlope,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedElevation,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreamBedThickness,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHydraulicConductivity,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReachLength,
    GlobalRemoveModflowBoundaryItemSubscription, GlobalRestoreModflowBoundaryItemSubscription, self);
end;

function TSfrItem.ScreenObject: TObject;
var
  SfrCollection: TSfrCollection;
begin
  SfrCollection := Collection as TSfrCollection;
  Assert(SfrCollection <> nil);
  result := SfrCollection.ScreenObject;
end;

procedure TSfrItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    0:
      ReachLength := Value;
    1:
      HydraulicConductivity := Value;
    2:
      StreamBedThickness := Value;
    3:
      StreambedElevation := Value;
    4:
      StreamSlope := Value;
    5:
      SaturatedWaterContent := Value;
    6:
      InitialWaterContent := Value;
    7:
      BrooksCoreyExponent := Value;
    8:
      VerticalK := Value;
    else Assert(False);
  end;
end;

procedure TSfrItem.SetBrooksCoreyExponent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FBrooksCoreyExponent.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, BrooksCoreyExponentPosition, FBrooksCoreyExponent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrBrooksCorey(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetHydraulicConductivity(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
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
        PhastModel.InvalidateMfSfrStreamK(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetInitialWaterContent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FInitialWaterContent.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, InitialWaterContentPosition, FInitialWaterContent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrInitialWaterContent(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetReachLength(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FReachLength.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, ReachLengthPosition, FReachLength);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrReachLength(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetSaturatedWaterContent(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FSaturatedWaterContent.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, SaturatedWaterContentPosition, FSaturatedWaterContent);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrSaturatedWaterContent(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetStreambedElevation(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
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
        PhastModel.InvalidateMfSfrStreamTop(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetStreamBedThickness(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
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
        PhastModel.InvalidateMfSfrStreamThickness(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetStreamSlope(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamSlope.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, StreamSlopePosition, FStreamSlope);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrStreamSlope(self);
      end;
    end;
  end;
end;

procedure TSfrItem.SetVerticalK(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FVerticalK.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, VerticalKPosition, FVerticalK);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateMfSfrVerticalUnsatK(self);
      end;
    end;
  end;
end;

{ TSfrCollection }

procedure TSfrCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TSfrStorage.Create(AModel));
end;

procedure TSfrCollection.AssignArrayCellValues(DataSets: TList;
  ItemIndex: Integer; AModel: TBaseModel);
var
  ReachLengthArray: TDataArray;
  LgrReachLengthArray: TDataArray;
  HydraulicConductivityArray: TDataArray;
  StreamBedThicknessArray: TDataArray;
  StreambedElevationArray: TDataArray;
  StreamSlopeArray: TDataArray;
  Boundary: TSfrStorage;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundaryIndex: Integer;
  SaturatedWaterContentArray: TDataArray;
  InitialWaterContentArray: TDataArray;
  BrooksCoreyExponentArray: TDataArray;
  VerticalKArray: TDataArray;
  ISFROPT: integer;
  LocalScreenObject: TScreenObject;
  SegmentIndex: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;
  ReachLengthArray := DataSets[0];
  LgrReachLengthArray := DataSets[1];
  if ISFROPT in [1,2,3] then
  begin
    HydraulicConductivityArray := DataSets[2];
    StreamBedThicknessArray := DataSets[3];
    StreambedElevationArray := DataSets[4];
    StreamSlopeArray := DataSets[5];
    if ISFROPT in [2,3] then
    begin
      SaturatedWaterContentArray := DataSets[6];
      InitialWaterContentArray := DataSets[7];
      BrooksCoreyExponentArray := DataSets[8];
      if ISFROPT = 3 then
      begin
        VerticalKArray := DataSets[9];
      end
      else
      begin
        VerticalKArray := nil;
      end;
    end
    else
    begin
      SaturatedWaterContentArray := nil;
      InitialWaterContentArray := nil;
      BrooksCoreyExponentArray := nil;
      VerticalKArray := nil;
    end;
  end
  else
  begin
    HydraulicConductivityArray := nil;
    StreamBedThicknessArray := nil;
    StreambedElevationArray := nil;
    StreamSlopeArray := nil;
    SaturatedWaterContentArray := nil;
    InitialWaterContentArray := nil;
    BrooksCoreyExponentArray := nil;
    VerticalKArray := nil;
  end;

//  LastBoundaryIndex := -1;
  BoundaryIndex := -1;
  Boundary := Boundaries[ItemIndex, AModel] as TSfrStorage;

  LocalScreenObject := ScreenObject as TScreenObject;
  for SegmentIndex := 0 to LocalScreenObject.Segments[LocalModel].Count - 1 do
  begin
    Segment := LocalScreenObject.Segments[LocalModel][SegmentIndex];
    ColIndex := Segment.Col;
    RowIndex := Segment.Row;
    LayerIndex := Segment.Layer;
    if not LocalModel.IsLayerSimulated(LayerIndex) then
    begin
      Continue;
    end;
    if not ReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex] then
    begin
      Continue;
    end;
    if (ColIndex = PriorCol)
      and (RowIndex = PriorRow)
      and (LayerIndex = PriorLayer) then
    begin
      Continue
    end;
    Inc(BoundaryIndex);
    PriorCol := Segment.Col;
    PriorRow := Segment.Row;
    PriorLayer := Segment.Layer;

    Assert(BoundaryIndex < Length(Boundary.SfrArray));
    if ISFROPT in [1,2,3] then
    begin
      if ReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex] then
      begin
        Assert(LgrReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(HydraulicConductivityArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(StreamBedThicknessArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(StreambedElevationArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        Assert(StreamSlopeArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        if ISFROPT in [2,3] then
        begin
          Assert(SaturatedWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
          Assert(InitialWaterContentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
          Assert(BrooksCoreyExponentArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        end;
        if ISFROPT = 3 then
        begin
          Assert(VerticalKArray.IsValue[LayerIndex, RowIndex, ColIndex]);
        end;
      end;
    end;
    if ReachLengthArray.IsValue[LayerIndex, RowIndex, ColIndex] then
    begin
//      Boundary := Boundaries[ItemIndex] as TSfrStorage;
      Assert(BoundaryIndex < Length(Boundary.SfrArray));
      with Boundary.SfrArray[BoundaryIndex] do
      begin
        Cell.Layer := LayerIndex;
        Cell.Row := RowIndex;
        Cell.Column := ColIndex;
//        Cell.Section := Sections[LayerIndex, RowIndex, ColIndex];
        ReachLength := ReachLengthArray.
          RealData[LayerIndex, RowIndex, ColIndex];
        ReachLengthAnnotation := ReachLengthArray.
          Annotation[LayerIndex, RowIndex, ColIndex];
        LgrReachLength := LgrReachLengthArray.
          RealData[LayerIndex, RowIndex, ColIndex];
        if ISFROPT in [1,2,3] then
        begin
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
          StreamSlope := StreamSlopeArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          StreamSlopeAnnotation := StreamSlopeArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;

        if ISFROPT in [2,3] then
        begin
          SaturatedWaterContent := SaturatedWaterContentArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          SaturatedWaterContentAnnotation := SaturatedWaterContentArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
          InitialWaterContent := InitialWaterContentArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          InitialWaterContentAnnotation := InitialWaterContentArray.
            Annotation[LayerIndex, RowIndex, ColIndex];

          BrooksCoreyExponent := BrooksCoreyExponentArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          BrooksCoreyExponentAnnotation := BrooksCoreyExponentArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
        if ISFROPT = 3 then
        begin
          VerticalK := VerticalKArray.
            RealData[LayerIndex, RowIndex, ColIndex];
          VerticalKAnnotation := VerticalKArray.
            Annotation[LayerIndex, RowIndex, ColIndex];
        end;
      end;
    end;
  end;
  ReachLengthArray.CacheData;
  LgrReachLengthArray.CacheData;
  if HydraulicConductivityArray <> nil then
  begin
    HydraulicConductivityArray.CacheData;
  end;
  if StreamBedThicknessArray <> nil then
  begin
    StreamBedThicknessArray.CacheData;
  end;
  if StreambedElevationArray <> nil then
  begin
    StreambedElevationArray.CacheData;
  end;
  if StreamSlopeArray <> nil then
  begin
    StreamSlopeArray.CacheData;
  end;
  if SaturatedWaterContentArray <> nil then
  begin
    SaturatedWaterContentArray.CacheData;
  end;
  if InitialWaterContentArray <> nil then
  begin
    InitialWaterContentArray.CacheData;
  end;
  if BrooksCoreyExponentArray <> nil then
  begin
    BrooksCoreyExponentArray.CacheData;
  end;
  if VerticalKArray <> nil then
  begin
    VerticalKArray.CacheData;
  end;
  Boundary.CacheData;
end;

procedure CountArrayBoundaryCellsSfr(var BoundaryCount: Integer;
  DataArray1: TDataArray; DataSets: TList; AModel: TBaseModel;
  ScreenObject: TObject);
var
  DSIndex: Integer;
  ColIndex: Integer;
  RowIndex: Integer;
  LayerIndex: Integer;
  DataArray2: TDataArray;
  SO: TScreenObject;
  Index: Integer;
  Segment: TCellElementSegment;
  PriorCol, PriorRow, PriorLayer: integer;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  BoundaryCount := 0;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;

  SO := ScreenObject as TScreenObject;
  for Index := 0 to SO.Segments[LocalModel].Count - 1 do
  begin
    Segment := SO.Segments[LocalModel][Index];
    LayerIndex := Segment.Layer;
    if not LocalModel.IsLayerSimulated(LayerIndex) then
    begin
      Continue;
    end;
    RowIndex := Segment.Row;
    ColIndex := Segment.Col;
    if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex]
      and ((LayerIndex <> PriorLayer)
      or (RowIndex <> PriorRow)
      or (ColIndex <> PriorCol)) then
    begin
      for DSIndex := 1 to DataSets.Count - 1 do
      begin
        DataArray2 := DataSets[DSIndex];
        Assert(DataArray2.IsValue[LayerIndex, RowIndex, ColIndex]);
      end;
      Inc(BoundaryCount);
      PriorLayer := Segment.Layer;
      PriorRow := Segment.Row;
      PriorCol := Segment.Col;
    end;
  end;
end;

procedure TSfrCollection.CountArrayBoundaryCells(var BoundaryCount: Integer;
  DataArray1: TDataArray; DataSets: TList; AModel: TBaseModel);
//var
//  DSIndex: Integer;
//  ColIndex: Integer;
//  RowIndex: Integer;
//  LayerIndex: Integer;
//  DataArray2: TDataArray;
//  SO: TScreenObject;
//  Index: Integer;
//  Segment: TCellElementSegment;
//  PriorCol, PriorRow, PriorLayer: integer;
//  LocalModel: TCustomModel;
begin
  CountArrayBoundaryCellsSfr(BoundaryCount, DataArray1, DataSets, AModel, ScreenObject);
{  LocalModel := AModel as TCustomModel;
  BoundaryCount := 0;
  PriorCol := -1;
  PriorRow := -1;
  PriorLayer := -1;

  SO := ScreenObject as TScreenObject;
  for Index := 0 to SO.Segments[LocalModel].Count - 1 do
  begin
    Segment := SO.Segments[LocalModel][Index];
    LayerIndex := Segment.Layer;
    if not LocalModel.IsLayerSimulated(LayerIndex) then
    begin
      Continue;
    end;
    RowIndex := Segment.Row;
    ColIndex := Segment.Col;
    if DataArray1.IsValue[LayerIndex, RowIndex, ColIndex]
      and ((LayerIndex <> PriorLayer)
      or (RowIndex <> PriorRow)
      or (ColIndex <> PriorCol)) then
    begin
      for DSIndex := 1 to DataSets.Count - 1 do
      begin
        DataArray2 := DataSets[DSIndex];
        Assert(DataArray2.IsValue[LayerIndex, RowIndex, ColIndex]);
      end;
      Inc(BoundaryCount);
      PriorLayer := Segment.Layer;
      PriorRow := Segment.Row;
      PriorCol := Segment.Col;
    end;
  end;  }
end;

function TSfrCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSfrTimeListLink;
end;

procedure TSfrCollection.InitializeTimeLists(ListOfTimeLists: TList;
  AModel: TBaseModel);
var
  TimeIndex: Integer;
  BoundaryValues: TBoundaryValueArray;
  Index: Integer;
  Item: TSfrItem;
  Boundary: TSfrBoundary;
  ScreenObject: TScreenObject;
  ISFROPT: integer;
  ALink: TSfrTimeListLink;
  HydraulicConductivityData: TModflowTimeList;
  ReachLength: TModflowTimeList;
  StreamBedThicknessData: TModflowTimeList;
  StreamBedElevationData: TModflowTimeList;
  StreamSlopeData: TModflowTimeList;
  SaturatedWaterContent: TModflowTimeList;
  InitialWaterContent: TModflowTimeList;
  BrooksCoreyExponent: TModflowTimeList;
  VerticalK: TModflowTimeList;
  LgrReachLength: TModflowTimeList;
begin
  ISFROPT := (Model as TPhastModel).ModflowPackages.SfrPackage.Isfropt;

  Assert(Count = 1);
  SetLength(BoundaryValues, Count);

  Boundary := BoundaryGroup as TSfrBoundary;
  ScreenObject := Boundary.ScreenObject as TScreenObject;
  ScreenObject.FullObjectIntersectLength := True;
  try
    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.ReachLength;
    end;
    ALink := TimeListLink.GetLink(AModel) as TSfrTimeListLink;
    ReachLength := ALink.FReachLength;
    ReachLength.Initialize(BoundaryValues, ScreenObject, lctZero);
    Assert(ReachLength.Count = Count);

    LgrReachLength := ALink.FLgrReachLength;
    LgrReachLength.Initialize(BoundaryValues, ScreenObject, lctUse);
    Assert(LgrReachLength.Count = Count);
  finally
    ScreenObject.FullObjectIntersectLength := False;
  end;

  HydraulicConductivityData := ALink.FHydraulicConductivityData;
  StreamBedThicknessData := ALink.FStreamBedThicknessData;
  StreamBedElevationData := ALink.FStreamBedElevationData;
  StreamSlopeData := ALink.FStreamSlopeData;
  SaturatedWaterContent := ALink.FSaturatedWaterContent;
  InitialWaterContent := ALink.FInitialWaterContent;
  BrooksCoreyExponent := ALink.FBrooksCoreyExponent;
  VerticalK := ALink.FVerticalK;

  if ISFROPT in [1,2,3] then
  begin
    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.HydraulicConductivity;
    end;
    HydraulicConductivityData.Initialize(BoundaryValues, ScreenObject, lctZero);
    Assert(HydraulicConductivityData.Count = Count);

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.StreamBedThickness;
    end;
    StreamBedThicknessData.Initialize(BoundaryValues, ScreenObject, lctZero);
    Assert(StreamBedThicknessData.Count = Count);

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.StreamBedElevation;
    end;
    StreamBedElevationData.Initialize(BoundaryValues, ScreenObject, lctZero);
    Assert(StreamBedElevationData.Count = Count);

    for Index := 0 to Count - 1 do
    begin
      Item := Items[Index] as TSfrItem;
      BoundaryValues[Index].Time := Item.StartTime;
      BoundaryValues[Index].Formula := Item.StreamSlope;
    end;
    StreamSlopeData.Initialize(BoundaryValues, ScreenObject, lctZero);
    Assert(StreamSlopeData.Count = Count);

    if ISFROPT in [2,3] then
    begin
      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TSfrItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := Item.SaturatedWaterContent;
      end;
      SaturatedWaterContent.Initialize(BoundaryValues, ScreenObject, lctZero);
      Assert(SaturatedWaterContent.Count = Count);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TSfrItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := Item.InitialWaterContent;
      end;
      InitialWaterContent.Initialize(BoundaryValues, ScreenObject, lctZero);
      Assert(InitialWaterContent.Count = Count);

      for Index := 0 to Count - 1 do
      begin
        Item := Items[Index] as TSfrItem;
        BoundaryValues[Index].Time := Item.StartTime;
        BoundaryValues[Index].Formula := Item.BrooksCoreyExponent;
      end;
      BrooksCoreyExponent.Initialize(BoundaryValues, ScreenObject, lctZero);
      Assert(BrooksCoreyExponent.Count = Count);

      if ISFROPT = 3 then
      begin
        for Index := 0 to Count - 1 do
        begin
          Item := Items[Index] as TSfrItem;
          BoundaryValues[Index].Time := Item.StartTime;
          BoundaryValues[Index].Formula := Item.VerticalK;
        end;
        VerticalK.Initialize(BoundaryValues, ScreenObject, lctZero);
        Assert(VerticalK.Count = Count);
      end;
    end;
  end;


  ClearBoundaries(AModel);
  SetBoundaryCapacity(ReachLength.Count, AModel);
  for TimeIndex := 0 to ReachLength.Count - 1 do
  begin
    AddBoundary(TSfrStorage.Create(AModel));
  end;
  ListOfTimeLists.Add(ReachLength);
  ListOfTimeLists.Add(LgrReachLength);
  if ISFROPT in [1,2,3] then
  begin
    ListOfTimeLists.Add(HydraulicConductivityData);
    ListOfTimeLists.Add(StreamBedThicknessData);
    ListOfTimeLists.Add(StreamBedElevationData);
    ListOfTimeLists.Add(StreamSlopeData);
    if ISFROPT in [2,3] then
    begin
      ListOfTimeLists.Add(SaturatedWaterContent);
      ListOfTimeLists.Add(InitialWaterContent);
      ListOfTimeLists.Add(BrooksCoreyExponent);
      if ISFROPT = 3 then
      begin
        ListOfTimeLists.Add(VerticalK);
      end;
    end;
  end;
end;

procedure TSfrCollection.InvalidateBedThicknessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FStreamBedThicknessData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FStreamBedThicknessData.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateBrooksCoreyExponentData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FBrooksCoreyExponent.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FBrooksCoreyExponent.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateHydraulicConductivityData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FHydraulicConductivityData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FHydraulicConductivityData.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateInitialWaterContentData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FInitialWaterContent.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FInitialWaterContent.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateReachLengthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FReachLength.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FReachLength.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateSaturatedWaterContentData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FSaturatedWaterContent.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FSaturatedWaterContent.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateStreambedElevationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FStreamBedElevationData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FStreamBedElevationData.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateStreamSlopeData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FStreamSlopeData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FStreamSlopeData.Invalidate;
    end;
  end;
end;

procedure TSfrCollection.InvalidateVerticalKData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrTimeListLink;
    Link.FVerticalK.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrTimeListLink;
      Link.FVerticalK.Invalidate;
    end;
  end;
end;

class function TSfrCollection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrItem;
end;

procedure TSfrCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSfrStorage).FSfrArray, BoundaryCount);
  inherited;
end;

{ TSfr_Cell }

procedure TSfr_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TSfr_Cell.GetBrooksCoreyExponent: double;
begin
  result := Values.BrooksCoreyExponent;
end;

function TSfr_Cell.GetBrooksCoreyExponentAnnotation: string;
begin
  result := Values.BrooksCoreyExponentAnnotation;
end;

function TSfr_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TSfr_Cell.GetHydraulicConductivity: double;
begin
  result := Values.HydraulicConductivity;
end;

function TSfr_Cell.GetHydraulicConductivityAnnotation: string;
begin
  result := Values.HydraulicConductivityAnnotation;
end;

function TSfr_Cell.GetInitialWaterContent: double;
begin
  result := Values.InitialWaterContent;
end;

function TSfr_Cell.GetInitialWaterContentAnnotation: string;
begin
  result := Values.InitialWaterContentAnnotation;
end;

function TSfr_Cell.GetIntegerAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSfr_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfr_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;

function TSfr_Cell.GetLgrReachLength: double;
begin
  result := Values.LgrReachLength;
end;

function TSfr_Cell.GetReachLength: double;
begin
  result := Values.ReachLength;
end;

function TSfr_Cell.GetReachLengthAnnotation: string;
begin
  result := Values.ReachLengthAnnotation;
end;

function TSfr_Cell.GetRealAnnotation(Index: integer; AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    0: result := ReachLengthAnnotation;
    1: result := HydraulicConductivityAnnotation;
    2: result := StreambedElevationAnnotation;
    3: result := StreamBedThicknessAnnotation;
    4: result := StreamSlopeAnnotation;
    5: result := SaturatedWaterContentAnnotation;
    6: result := InitialWaterContentAnnotation;
    7: result := BrooksCoreyExponentAnnotation;
    8: result := VerticalKAnnotation;
    else Assert(False);
  end;
end;

function TSfr_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    0: result := ReachLength;
    1: result := HydraulicConductivity;
    2: result := StreambedElevation;
    3: result := StreamBedThickness;
    4: result := StreamSlope;
    5: result := SaturatedWaterContent;
    6: result := InitialWaterContent;
    7: result := BrooksCoreyExponent;
    8: result := VerticalK;
    else Assert(False);
  end;
end;

function TSfr_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TSfr_Cell.GetSaturatedWaterContent: double;
begin
  result := Values.SaturatedWaterContent;
end;

function TSfr_Cell.GetSaturatedWaterContentAnnotation: string;
begin
  result := Values.SaturatedWaterContentAnnotation;
end;

function TSfr_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TSfr_Cell.GetStreambedElevation: double;
begin
  result := Values.StreambedElevation;
end;

function TSfr_Cell.GetStreambedElevationAnnotation: string;
begin
  result := Values.StreambedElevationAnnotation;
end;

function TSfr_Cell.GetStreamBedThickness: double;
begin
  result := Values.StreamBedThickness;
end;

function TSfr_Cell.GetStreamBedThicknessAnnotation: string;
begin
  result := Values.StreamBedThicknessAnnotation;
end;


function TSfr_Cell.GetStreamSlope: double;
begin
  result := Values.StreamSlope;
end;

function TSfr_Cell.GetStreamSlopeAnnotation: string;
begin
  result := Values.StreamSlopeAnnotation;
end;

function TSfr_Cell.GetVerticalK: double;
begin
  result := Values.VerticalK;
end;

function TSfr_Cell.GetVerticalKAnnotation: string;
begin
  result := Values.VerticalKAnnotation;
end;

function TSfr_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  SfrCell: TSfr_Cell;
begin
  result := AnotherCell is TSfr_Cell;
  if result then
  begin
    SfrCell := TSfr_Cell(AnotherCell);
    result := (HydraulicConductivity = SfrCell.HydraulicConductivity)
      and (StreambedElevation = SfrCell.StreambedElevation)
      and (StreamBedThickness = SfrCell.StreamBedThickness)
      and (StreamSlope = SfrCell.StreamSlope)
      and (SaturatedWaterContent = SfrCell.SaturatedWaterContent)
      and (InitialWaterContent = SfrCell.InitialWaterContent)
      and (BrooksCoreyExponent = SfrCell.BrooksCoreyExponent)
      and (VerticalK = SfrCell.VerticalK)
      and (ReachLength = SfrCell.ReachLength)
      and (LgrReachLength = SfrCell.LgrReachLength)
      and (HydraulicConductivity = SfrCell.HydraulicConductivity)
      and (HydraulicConductivity = SfrCell.HydraulicConductivity)
  end;
end;

procedure TSfr_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TSfr_Cell.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TSfr_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TSfr_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TSfr_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

procedure TSfr_Cell.SetValues(const Value: TSfrRecord);
begin
  FValues := Value;
end;

{ TSfrRecord }

procedure TSfrRecord.AdjustReachLength(Factor: double);
begin
  ReachLength := ReachLength * Factor;
  LgrReachLength := LgrReachLength * Factor;
end;

procedure TSfrRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, ReachLength);
  WriteCompReal(Comp, LgrReachLength);
  WriteCompReal(Comp, StreamSlope);
  WriteCompReal(Comp, StreambedElevation);
  WriteCompReal(Comp, StreamBedThickness);

  WriteCompReal(Comp, HydraulicConductivity);
  WriteCompReal(Comp, SaturatedWaterContent);
  WriteCompReal(Comp, InitialWaterContent);
  WriteCompReal(Comp, BrooksCoreyExponent);
  WriteCompReal(Comp, VerticalK);

  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);

  WriteCompInt(Comp, Strings.IndexOf(ReachLengthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreamSlopeAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreambedElevationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreamBedThicknessAnnotation));
//  WriteCompString(Comp, ReachLengthAnnotation);
//  WriteCompString(Comp, StreamSlopeAnnotation);
//  WriteCompString(Comp, StreambedElevationAnnotation);
//  WriteCompString(Comp, StreamBedThicknessAnnotation);

  WriteCompInt(Comp, Strings.IndexOf(HydraulicConductivityAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SaturatedWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(InitialWaterContentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BrooksCoreyExponentAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(VerticalKAnnotation));
//  WriteCompString(Comp, HydraulicConductivityAnnotation);
//  WriteCompString(Comp, SaturatedWaterContentAnnotation);
//  WriteCompString(Comp, InitialWaterContentAnnotation);
//  WriteCompString(Comp, BrooksCoreyExponentAnnotation);
//  WriteCompString(Comp, VerticalKAnnotation);
end;

procedure TSfrRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ReachLengthAnnotation);
  Strings.Add(StreamSlopeAnnotation);
  Strings.Add(StreambedElevationAnnotation);
  Strings.Add(StreamBedThicknessAnnotation);

  Strings.Add(HydraulicConductivityAnnotation);
  Strings.Add(SaturatedWaterContentAnnotation);
  Strings.Add(InitialWaterContentAnnotation);
  Strings.Add(BrooksCoreyExponentAnnotation);
  Strings.Add(VerticalKAnnotation);
end;

procedure TSfrRecord.Restore(Decomp: TDecompressionStream; Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  ReachLength := ReadCompReal(Decomp);
  LgrReachLength := ReadCompReal(Decomp);
  StreamSlope := ReadCompReal(Decomp);
  StreambedElevation := ReadCompReal(Decomp);
  StreamBedThickness := ReadCompReal(Decomp);

  HydraulicConductivity := ReadCompReal(Decomp);
  SaturatedWaterContent := ReadCompReal(Decomp);
  InitialWaterContent := ReadCompReal(Decomp);
  BrooksCoreyExponent := ReadCompReal(Decomp);
  VerticalK := ReadCompReal(Decomp);

  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);

  ReachLengthAnnotation := Annotations[ReadCompInt(Decomp)];
  StreamSlopeAnnotation := Annotations[ReadCompInt(Decomp)];
  StreambedElevationAnnotation := Annotations[ReadCompInt(Decomp)];
  StreamBedThicknessAnnotation := Annotations[ReadCompInt(Decomp)];
//  ReachLengthAnnotation := ReadCompString(Decomp, Annotations);
//  StreamSlopeAnnotation := ReadCompString(Decomp, Annotations);
//  StreambedElevationAnnotation := ReadCompString(Decomp, Annotations);
//  StreamBedThicknessAnnotation := ReadCompString(Decomp, Annotations);

  HydraulicConductivityAnnotation := Annotations[ReadCompInt(Decomp)];
  SaturatedWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  InitialWaterContentAnnotation := Annotations[ReadCompInt(Decomp)];
  BrooksCoreyExponentAnnotation := Annotations[ReadCompInt(Decomp)];
  VerticalKAnnotation := Annotations[ReadCompInt(Decomp)];
//  HydraulicConductivityAnnotation := ReadCompString(Decomp, Annotations);
//  SaturatedWaterContentAnnotation := ReadCompString(Decomp, Annotations);
//  InitialWaterContentAnnotation := ReadCompString(Decomp, Annotations);
//  BrooksCoreyExponentAnnotation := ReadCompString(Decomp, Annotations);
//  VerticalKAnnotation := ReadCompString(Decomp, Annotations);


end;

{ TSfrStorage }

procedure TSfrStorage.Clear;
begin
  SetLength(FSfrArray, 0);
  FCleared := True;
end;

procedure TSfrStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FSfrArray);
    for Index := 0 to Count - 1 do
    begin
      FSfrArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FSfrArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

procedure TSfrStorage.Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSfrArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FSfrArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

function TSfrStorage.GetSfrArray: TSrfArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSfrArray;
end;

{ TSfrTimeListLink }

procedure TSfrTimeListLink.CreateTimeLists;
begin
  inherited;
  FReachLength := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FLgrReachLength := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FHydraulicConductivityData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamBedThicknessData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamBedElevationData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamSlopeData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSaturatedWaterContent := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInitialWaterContent := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBrooksCoreyExponent := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FVerticalK := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReachLength.NonParamDescription := StrReachLength;
  FReachLength.ParamDescription := ' ' + LowerCase(StrReachLength);
  FLgrReachLength.NonParamDescription := StrLGRReachLength;
  FLgrReachLength.ParamDescription := StrLGRReachLengthLc;
  FStreamBedThicknessData.NonParamDescription := StrStreambedThickness;
  FStreamBedThicknessData.ParamDescription := ' ' + LowerCase(StrStreambedThickness);
  FStreamBedElevationData.NonParamDescription := StrStreambedElevation;
  FStreamBedElevationData.ParamDescription := ' ' + LowerCase(StrStreambedElevation);
  FHydraulicConductivityData.NonParamDescription := StrHydraulicConductivi;
  FHydraulicConductivityData.ParamDescription := StrHydraulicConductivMult;
  FStreamSlopeData.NonParamDescription := StrSlope;
  FStreamSlopeData.ParamDescription := ' ' + LowerCase(StrSlope);
  FSaturatedWaterContent.NonParamDescription := StrSaturatedWaterCont;
  FSaturatedWaterContent.ParamDescription := ' ' + LowerCase(StrSaturatedWaterCont);
  FInitialWaterContent.NonParamDescription := StrInitialWaterConten;
  FInitialWaterContent.ParamDescription := ' ' + LowerCase(StrInitialWaterConten);
  FBrooksCoreyExponent.NonParamDescription := StrBrooksCoreyExponen;
  FBrooksCoreyExponent.ParamDescription := ' ' + StrBrooksCoreyExponen;
  FVerticalK.NonParamDescription := StrMaximumVerticalUns;
  FVerticalK.ParamDescription := ' ' + LowerCase(StrMaximumVerticalUns);
  AddTimeList(FReachLength);
  AddTimeList(FHydraulicConductivityData);
  AddTimeList(FStreamBedThicknessData);
  AddTimeList(FStreamBedElevationData);
  AddTimeList(FStreamSlopeData);
  AddTimeList(FSaturatedWaterContent);
  AddTimeList(FInitialWaterContent);
  AddTimeList(FBrooksCoreyExponent);
  AddTimeList(FVerticalK);
  AddTimeList(FLgrReachLength);
end;

destructor TSfrTimeListLink.Destroy;
begin
  FReachLength.Free;
  FLgrReachLength.Free;
  FHydraulicConductivityData.Free;
  FStreamBedThicknessData.Free;
  FStreamBedElevationData.Free;
  FStreamSlopeData.Free;
  FSaturatedWaterContent.Free;
  FInitialWaterContent.Free;
  FBrooksCoreyExponent.Free;
  FVerticalK.Free;
  inherited;
end;

end.
