unit ModflowStrUnit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowBoundaryUnit,
  OrderedCollectionUnit, DataSetUnit, ModflowCellUnit, FormulaManagerUnit,
  SubscriptionUnit, SparseDataSets, RbwParser, GoPhastTypes;

type
  TStrRecord = record
    Cell: TCellLocation;
    Conductance: double;
    Stage: double;
    BedTop: double;
    BedBottom: double;
    Flow: double;
    Width: double;
    Slope: Double;
    Roughness: double;
    StartingTime: double;
    EndingTime: double;
    ConductanceAnnotation: string;
    StageAnnotation: string;
    BedTopAnnotation: string;
    BedBottomAnnotation: string;
    FlowAnnotation: string;
    WidthAnnotation: string;
    SlopeAnnotation: string;
    RoughnessAnnotation: string;
    SegmentNumber: Integer;
    ReachNumber: Integer;
    ConductancePest: string;
    StagePest: string;
    BedTopPest: string;
    BedBottomPest: string;
    FlowPest: string;
    WidthPest: string;
    SlopePest: string;
    RoughnessPest: string;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TStrArray = array of TStrRecord;

  TStrStorage = class(TCustomBoundaryStorage)
  private
    FStrArray: TStrArray;
    function GetStrArray: TStrArray;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property StrArray: TStrArray read GetStrArray;
  end;

  // @name represents a MODFLOW STR boundary for one time interval.
  // @name is stored by @link(TStrCollection).
  TStrItem = class(TCustomModflowBoundaryItem)
  private
    FOutflowSegment: integer;
    FDiversionSegment: integer;
    // See @link(Stage).
    procedure SetStage(const Value: string);
    // See @link(Conductance).
    procedure SetConductance(const Value: string);
    function GetConductance: string;
    function GetStage: string;
    function GetBedBottom: string;
    function GetBedTop: string;
    function GetFlow: string;
    procedure SetBedBottom(const Value: string);
    procedure SetBedTop(const Value: string);
    procedure SetFlow(const Value: string);
    function GetRoughness: string;
    function GetSlope: string;
    function GetWidth: string;
    procedure SetRoughness(const Value: string);
    procedure SetSlope(const Value: string);
    procedure SetWidth(const Value: string);
    procedure SetDiversionSegment(const Value: integer);
    procedure SetOutflowSegment(const Value: integer);
    function GetSegmentNumber: Integer;
  protected
    // See @link(Stage).
    FStage: TFormulaObject;
    // See @link(Conductance).
    FConductance: TFormulaObject;
    FBedTop: TFormulaObject;
    FBedBottom: TFormulaObject;
    FFlow: TFormulaObject;
    FWidth: TFormulaObject;
    Fslope: TFormulaObject;
    FRoughness: TFormulaObject;
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
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    Destructor Destroy; override;
    property SegmentNumber: Integer read GetSegmentNumber;
  published
    // @name is the formula used to set the stage
    // of this boundary.
    property Stage: string read GetStage write SetStage;
    // @name is the formula used to set the conductance
    // or the conductance multiplier of this boundary.
    property Conductance: string read GetConductance write SetConductance;
    property BedTop: string read GetBedTop write SetBedTop;
    property BedBottom: string read GetBedBottom write SetBedBottom;
    property Flow: string read GetFlow write SetFlow;
    property Width: string read GetWidth write SetWidth;
    property Slope: string read GetSlope write SetSlope;
    property Roughness: string read GetRoughness write SetRoughness;
    property OutflowSegment: integer read FOutflowSegment
      write SetOutflowSegment;
    property DiversionSegment: integer read FDiversionSegment
      write SetDiversionSegment;
  end;

  TStrTimeListLink = class(TTimeListsModelLink)
  private
    // @name is used to perform notifications of the Stages for a series of
    // Stream Boundaries over a series of time intervals.
    FStageData: TModflowTimeList;
    // @name is used to perform notifications of the Conductances
    // for a series of
    // STR Boundaries over a series of time intervals.
    FConductanceData: TModflowTimeList;
    FBedTopData: TModflowTimeList;
    FBedBottomData: TModflowTimeList;
    FFlowData: TModflowTimeList;
    FWidthData: TModflowTimeList;
    FSlopeData: TModflowTimeList;
    FroughnessData: TModflowTimeList;
    FStrSegmentNumberData: TModflowTimeList;
    FStrOutflowSegmentNumberData: TModflowTimeList;
    FStrReachNumberData: TModflowTimeList;
    FStrDiversionSegmentNumberData: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  // @name represents MODFLOW STR boundaries
  // for a series of time intervals.
  TStrCollection = class(TCustomMF_ListBoundColl)
  private
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateConductanceData(Sender: TObject);
    procedure InvalidateBedTopData(Sender: TObject);
    procedure InvalidateBedBottomData(Sender: TObject);
    procedure InvalidateFlowData(Sender: TObject);
    procedure InvalidateWidthData(Sender: TObject);
    procedure InvalidateSlopeData(Sender: TObject);
    procedure InvalidateRoughnessData(Sender: TObject);
    procedure FixItems;
  protected
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean); override;

    // See @link(TCustomNonSpatialBoundColl.ItemClass
    // TCustomNonSpatialBoundColl.ItemClass)
    class function ItemClass: TBoundaryItemClass; override;
    // @name calls inherited @name and then sets the length of
    // the @link(TStrStorage.StrArray) at ItemIndex in
    // @link(TCustomMF_BoundColl.Boundaries) to BoundaryCount.
    // @SeeAlso(TCustomMF_BoundColl.SetBoundaryStartAndEndTime
    // TCustomMF_BoundColl.SetBoundaryStartAndEndTime)
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
      override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  // Each @name stores a @link(TStrCollection).
  // @classname is stored by @link(TModflowParameters).
  TStrParamItem = class(TModflowParamItem)
  protected
    class function BoundaryClass: TMF_BoundCollClass; override;
  end;

  TStr_Cell = class(TValueCell)
  private
    Values: TStrRecord;
    StressPeriod: integer;
    function GetStage: double;
    function GetConductance: double;
    function GetBedTop: double;
    function GetBedBottom: double;
    function GetFlow: double;
    function GetConductanceAnnotation: string;
    function GetStageAnnotation: string;
    function GetBedTopAnnotation: string;
    function GetBedBottomAnnotation: string;
    function GetFlowAnnotation: string;
    function GetRoughness: double;
    function GetRoughnessAnnotation: string;
    function GetSlope: double;
    function GetSlopeAnnotation: string;
    function GetWidth: double;
    function GetWidthAnnotation: string;
    procedure SetFlow(const Value: double);
    procedure SetFlowAnnotation(const Value: string);
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
    property Stage: double read GetStage;
    property BedTop: double read GetBedTop;
    property BedBottom: double read GetBedBottom;
    property Flow: double read GetFlow write SetFlow;
    property Width: double read GetWidth;
    property Slope: double read GetSlope;
    property Roughness: double read GetRoughness;
    property SegmentNumber: Integer read Values.SegmentNumber
      write Values.SegmentNumber;
    property ReachNumber: Integer read Values.ReachNumber
      write Values.ReachNumber;
    property ConductanceAnnotation: string read GetConductanceAnnotation;
    property StageAnnotation: string read GetStageAnnotation;
    property BedTopAnnotation: string read GetBedTopAnnotation;
    property BedBottomAnnotation: string read GetBedBottomAnnotation;
    property FlowAnnotation: string read GetFlowAnnotation write SetFlowAnnotation;
    property WidthAnnotation: string read GetWidthAnnotation;
    property SlopeAnnotation: string read GetSlopeAnnotation;
    property RoughnessAnnotation: string read GetRoughnessAnnotation;
    function IsIdentical(AnotherCell: TValueCell): boolean; override;

  end;

  // @name represents the MODFLOW STR boundaries associated with
  // a single @link(TScreenObject).
  //
  // FormulaInterpretation determines whether the @Link(TStrItem.Conductance
  // TStrItem.Conductance) formulas represent
  // @unorderedlist(
  //   @item(fiSpecific - Conductance / the length or area of
  //     intersection between the @link(TScreenObject) and grid cell.)
  //   @item(fiTotal - Conductance.)
  // )
  // @seealso(TStrCollection)
  TStrBoundary = class(TSpecificModflowBoundary)
  private
    FSegmentNumber: Integer;
    procedure TestIfObservationsPresent(var EndOfLastStressPeriod: Double;
      var StartOfFirstStressPeriod: Double;
      var ObservationsPresent: Boolean);
    procedure SetSegmentNumber(const Value: Integer);
  protected
    // @name fills ValueTimeList with a series of TObjectLists - one for
    // each stress period.  Each such TObjectList is filled with
    // @link(TStr_Cell)s for that stress period.
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
    // @name insures that there are no time gaps in @link(TStrCollection)
    // by filling in any time gaps with inactive TStrItems.
    procedure FixItems;
    // @name copies @link(SegmentNumber) from the Source
    // @classname to this @classname and then calls inherited Assign.
    procedure Assign(Source: TPersistent);override;
    // @name fills ValueTimeList via a call to AssignCells for each
    // link  @link(TStrStorage) in
    // @link(TCustomMF_BoundColl.Boundaries Values.Boundaries);
    // Those represent non-parameter boundary conditions.
    // @name fills ParamList with the names of the
    // MODFLOW STR parameters that are in use.
    // The Objects property of ParamList has TObjectLists
    // Each such TObjectList is filled via a call to AssignCells
    // with each @link(TStrStorage) in @link(TCustomMF_BoundColl.Boundaries
    // Param.Param.Boundaries)
    // Those represent parameter boundary conditions.
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  published
    property SegmentNumber: Integer read FSegmentNumber write SetSegmentNumber;
  end;

const
  StreamConductancePosition = 0;
  StreamBedTopPosition = 1;
  StreamBedBottomPosition = 2;
  StreamFlowPosition = 3;
  StreamStagePosition = 4;
  StreamWidthPosition = 5;
  StreamSlopePosition = 6;
  StreamRoughnessPosition = 7;

implementation

uses PhastModelUnit, ScreenObjectUnit, ModflowTimeUnit, TempFiles,
  frmGoPhastUnit, GIS_Functions, ModflowPackageSelectionUnit;

resourcestring
  StrHead = 'Head';
  StrStreambedTop = 'Streambed Top';
  StrStreambedBottom = 'Streambed Bottom';
  StrFlow = 'Flow';
  StrWidth = 'Width';
  StrSlope = 'Slope';
  StrRoughness = 'Roughness';
  StrSegmentNumber = 'Segment Number';
  StrOutflowSegmentNumber = 'Outflow Segment Number';
  StrDiversionSegmentNu = 'Diversion Segment Number';
  StrReachNumber = 'Reach Number';

{ TStrRecord }

procedure TStrRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompCell(Comp, Cell);
  WriteCompReal(Comp, Conductance);
  WriteCompReal(Comp, Stage);
  WriteCompReal(Comp, BedTop);
  WriteCompReal(Comp, BedBottom);
  WriteCompReal(Comp, Flow);
  WriteCompReal(Comp, Width);
  WriteCompReal(Comp, Slope);
  WriteCompReal(Comp, Roughness);
  WriteCompReal(Comp, StartingTime);
  WriteCompReal(Comp, EndingTime);
  WriteCompInt(Comp, SegmentNumber);
  WriteCompInt(Comp, ReachNumber);
  WriteCompInt(Comp, Strings.IndexOf(ConductanceAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BedTopAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BedBottomAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(FlowAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(WidthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(SlopeAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessAnnotation));

  WriteCompInt(Comp, Strings.IndexOf(ConductancePest));
  WriteCompInt(Comp, Strings.IndexOf(StagePest));
  WriteCompInt(Comp, Strings.IndexOf(BedTopPest));
  WriteCompInt(Comp, Strings.IndexOf(BedBottomPest));
  WriteCompInt(Comp, Strings.IndexOf(FlowPest));
  WriteCompInt(Comp, Strings.IndexOf(WidthPest));
  WriteCompInt(Comp, Strings.IndexOf(SlopePest));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessPest));

end;

procedure TStrRecord.RecordStrings(Strings: TStringList);
begin
  Strings.Add(ConductanceAnnotation);
  Strings.Add(StageAnnotation);
  Strings.Add(BedTopAnnotation);
  Strings.Add(BedBottomAnnotation);
  Strings.Add(FlowAnnotation);
  Strings.Add(WidthAnnotation);
  Strings.Add(SlopeAnnotation);
  Strings.Add(RoughnessAnnotation);

  Strings.Add(ConductancePest);
  Strings.Add(StagePest);
  Strings.Add(BedTopPest);
  Strings.Add(BedBottomPest);
  Strings.Add(FlowPest);
  Strings.Add(WidthPest);
  Strings.Add(SlopePest);
  Strings.Add(RoughnessPest);

end;

procedure TStrRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  Cell := ReadCompCell(Decomp);
  Conductance := ReadCompReal(Decomp);
  Stage := ReadCompReal(Decomp);
  BedTop := ReadCompReal(Decomp);
  BedBottom := ReadCompReal(Decomp);
  Flow := ReadCompReal(Decomp);
  Width := ReadCompReal(Decomp);
  Slope := ReadCompReal(Decomp);
  Roughness := ReadCompReal(Decomp);
  StartingTime := ReadCompReal(Decomp);
  EndingTime := ReadCompReal(Decomp);
  SegmentNumber := ReadCompInt(Decomp);
  ReachNumber := ReadCompInt(Decomp);
  ConductanceAnnotation := Annotations[ReadCompInt(Decomp)];
  StageAnnotation := Annotations[ReadCompInt(Decomp)];
  BedTopAnnotation := Annotations[ReadCompInt(Decomp)];
  BedBottomAnnotation := Annotations[ReadCompInt(Decomp)];
  FlowAnnotation := Annotations[ReadCompInt(Decomp)];
  WidthAnnotation := Annotations[ReadCompInt(Decomp)];
  SlopeAnnotation := Annotations[ReadCompInt(Decomp)];
  RoughnessAnnotation := Annotations[ReadCompInt(Decomp)];

  ConductancePest := Annotations[ReadCompInt(Decomp)];
  StagePest := Annotations[ReadCompInt(Decomp)];
  BedTopPest := Annotations[ReadCompInt(Decomp)];
  BedBottomPest := Annotations[ReadCompInt(Decomp)];
  FlowPest := Annotations[ReadCompInt(Decomp)];
  WidthPest := Annotations[ReadCompInt(Decomp)];
  SlopePest := Annotations[ReadCompInt(Decomp)];
  RoughnessPest := Annotations[ReadCompInt(Decomp)];

end;

{ TStrStorage }

procedure TStrStorage.Clear;
begin
  SetLength(FStrArray, 0);
  FCleared := True;
end;

function TStrStorage.GetStrArray: TStrArray;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FStrArray;
end;

procedure TStrStorage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FStrArray, Count);
  for Index := 0 to Count - 1 do
  begin
    FStrArray[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TStrStorage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FStrArray);
    for Index := 0 to Count - 1 do
    begin
      FStrArray[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FStrArray[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TStrItem }

procedure TStrItem.Assign(Source: TPersistent);
var
  Str: TStrItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TStrItem then
  begin
    Str := TStrItem(Source);
    Stage := Str.Stage;
    Conductance := Str.Conductance;
    BedTop := Str.BedTop;
    BedBottom := Str.BedBottom;
    Flow := Str.Flow;
    Width := Str.Width;
    Slope := Str.Slope;
    Roughness := Str.Roughness;
    OutflowSegment := Str.OutflowSegment;
    DiversionSegment := Str.DiversionSegment;
  end;
  inherited;
end;

procedure TStrItem.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TStrCollection;
  StageObserver: TObserver;
  ConductanceObserver: TObserver;
  BedTopObserver: TObserver;
  BedBottomObserver: TObserver;
  FlowObserver: TObserver;
  WidthObserver: TObserver;
  SlopeObserver: TObserver;
  RoughnessObserver: TObserver;
begin
  ParentCollection := Collection as TStrCollection;
  StageObserver := FObserverList[StreamStagePosition];
  StageObserver.OnUpToDateSet := ParentCollection.InvalidateStageData;
  ConductanceObserver := FObserverList[StreamConductancePosition];
  ConductanceObserver.OnUpToDateSet := ParentCollection.InvalidateConductanceData;
  BedTopObserver := FObserverList[StreamBedTopPosition];
  BedTopObserver.OnUpToDateSet := ParentCollection.InvalidateBedTopData;
  BedBottomObserver := FObserverList[StreamBedBottomPosition];
  BedBottomObserver.OnUpToDateSet := ParentCollection.InvalidateBedBottomData;
  FlowObserver := FObserverList[StreamFlowPosition];
  FlowObserver.OnUpToDateSet := ParentCollection.InvalidateFlowData;
  WidthObserver := FObserverList[StreamWidthPosition];
  WidthObserver.OnUpToDateSet := ParentCollection.InvalidateWidthData;
  SlopeObserver := FObserverList[StreamSlopePosition];
  SlopeObserver.OnUpToDateSet := ParentCollection.InvalidateSlopeData;
  RoughnessObserver := FObserverList[StreamRoughnessPosition];
  RoughnessObserver.OnUpToDateSet := ParentCollection.InvalidateRoughnessData;
end;

function TStrItem.BoundaryFormulaCount: integer;
//var
//  StrPackage: TStrPackageSelection;
begin
//  StrPackage := (CurrentModel as TCustomModel).ModflowPackages.StrPackage;
//  if StrPackage.CalculateStage then
//  begin
    result := 8;
//  end
//  else
//  begin
//    result := 5;
//  end;
end;

procedure TStrItem.CreateFormulaObjects;
begin
  FStage := CreateFormulaObject(dso3D);
  FConductance := CreateFormulaObject(dso3D);
  FBedTop := CreateFormulaObject(dso3D);
  FBedBottom := CreateFormulaObject(dso3D);
  FFlow := CreateFormulaObject(dso3D);
  FWidth := CreateFormulaObject(dso3D);
  FSlope := CreateFormulaObject(dso3D);
  FRoughness := CreateFormulaObject(dso3D);
end;

destructor TStrItem.Destroy;
begin
  Stage := '0';
  Conductance := '0';
  BedTop := '0';
  BedBottom := '0';
  Flow := '0';
  Width := '0';
  Slope := '0';
  Roughness := '0';
  inherited;
end;

function TStrItem.GetBedBottom: string;
begin
  Result := FBedBottom.Formula;
  ResetItemObserver(StreamBedBottomPosition);
end;

function TStrItem.GetBedTop: string;
begin
  Result := FBedTop.Formula;
  ResetItemObserver(StreamBedTopPosition);
end;

function TStrItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    StreamConductancePosition: result := Conductance;
    StreamBedTopPosition: result := BedTop;
    StreamBedBottomPosition: result := BedBottom;
    StreamFlowPosition: result := Flow;
    StreamStagePosition: result := Stage;
    StreamWidthPosition: result := Width;
    StreamSlopePosition: result := Slope;
    StreamRoughnessPosition: result := Roughness;
    else Assert(False);
  end;
end;

function TStrItem.GetConductance: string;
begin
  Result := FConductance.Formula;
  ResetItemObserver(StreamConductancePosition);
end;

function TStrItem.GetFlow: string;
begin
  Result := FFlow.Formula;
  ResetItemObserver(StreamFlowPosition);
end;

procedure TStrItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FConductance then
  begin
    List.Add(FObserverList[StreamConductancePosition]);
  end;
  if Sender = FStage then
  begin
    List.Add(FObserverList[StreamStagePosition]);
  end;
  if Sender = FBedTop then
  begin
    List.Add(FObserverList[StreamBedTopPosition]);
  end;
  if Sender = FBedBottom then
  begin
    List.Add(FObserverList[StreamBedBottomPosition]);
  end;
  if Sender = FFlow then
  begin
    List.Add(FObserverList[StreamFlowPosition]);
  end;
  if Sender = FWidth then
  begin
    List.Add(FObserverList[StreamWidthPosition]);
  end;
  if Sender = FSlope then
  begin
    List.Add(FObserverList[StreamSlopePosition]);
  end;
  if Sender = FRoughness then
  begin
    List.Add(FObserverList[StreamRoughnessPosition]);
  end;

end;

function TStrItem.GetRoughness: string;
begin
  Result := FRoughness.Formula;
  ResetItemObserver(StreamRoughnessPosition);
end;

function TStrItem.GetSegmentNumber: Integer;
var
  StrCollection: TStrCollection;
  StrBoundary: TStrBoundary;
begin
  StrCollection := Collection as TStrCollection;
  StrBoundary := StrCollection.BoundaryGroup as TStrBoundary;
  result := StrBoundary.SegmentNumber;
end;

function TStrItem.GetSlope: string;
begin
  Result := FSlope.Formula;
  ResetItemObserver(StreamSlopePosition);
end;

function TStrItem.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(StreamStagePosition);
end;

function TStrItem.GetWidth: string;
begin
  Result := FWidth.Formula;
  ResetItemObserver(StreamWidthPosition);
end;

procedure TStrItem.InvalidateModel;
var
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil)
    and not (csDestroying in PhastModel.ComponentState)
    and not PhastModel.Clearing then
  begin
    PhastModel.InvalidateMfStrConductance(self);
    PhastModel.InvalidateMfStrStage(self);
    PhastModel.InvalidateMfStrBedTop(self);
    PhastModel.InvalidateMfStrBedBottom(self);
    PhastModel.InvalidateMfStrFlow(self);
    PhastModel.InvalidateMfStrWidth(self);
    PhastModel.InvalidateMfStrSlope(self);
    PhastModel.InvalidateMfStrRoughness(self);
    PhastModel.InvalidateMfStrSegmentNumber(self);
    PhastModel.InvalidateMfStrOutflowSegmentNumber(self);
    PhastModel.InvalidateMfStrDiversionSegmentNumber(self);
    PhastModel.InvalidateMfStrReachNumber(self);
  end;
end;

function TStrItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TStrItem;
begin
  result := (AnotherItem is TStrItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TStrItem(AnotherItem);
    result := (Item.Stage = Stage)
      and (Item.Conductance = Conductance)
      and (Item.BedTop = BedTop)
      and (Item.BedBottom = BedBottom)
      and (Item.Flow = Flow)
      and (Item.Width = Width)
      and (Item.Slope = Slope)
      and (Item.Roughness = Roughness)
      and (Item.OutflowSegment = OutflowSegment)
      and (Item.DiversionSegment = DiversionSegment);
  end;
end;

procedure TStrItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FConductance,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBedTop,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FBedBottom,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FFlow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FWidth,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FSlope,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

end;

procedure TStrItem.SetBedBottom(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamBedBottomPosition, FBedBottom);
end;

procedure TStrItem.SetBedTop(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamBedTopPosition, FBedTop);
end;

procedure TStrItem.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    StreamConductancePosition:
      Conductance := Value;
    StreamBedTopPosition:
      BedTop := Value;
    StreamBedBottomPosition:
      BedBottom := Value;
    StreamFlowPosition:
      Flow := Value;
    StreamStagePosition:
      Stage := Value;
    StreamWidthPosition:
      Width := Value;
    StreamSlopePosition:
      Slope := Value;
    StreamRoughnessPosition:
      Roughness := Value;
    else Assert(False);
  end;
end;

procedure TStrItem.SetConductance(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamConductancePosition, FConductance);
end;

procedure TStrItem.SetDiversionSegment(const Value: integer);
var
  Model: TBaseModel;
begin
  if FDiversionSegment <> Value then
  begin
    InvalidateModel;
    FDiversionSegment := Value;
    Model := (Collection as TStrCollection).Model;
    if Model <> nil then
    begin
      (Model as TCustomModel).ModflowPackages.StrPackage.
        MfStrSegmentNumber.Invalidate;
    end;
  end;
end;

procedure TStrItem.SetFlow(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamFlowPosition, FFlow);
end;

procedure TStrItem.SetOutflowSegment(const Value: integer);
var
  Model: TBaseModel;
begin
  if FOutflowSegment <> Value then
  begin
    InvalidateModel;
    FOutflowSegment := Value;
    Model := (Collection as TStrCollection).Model;
    if Model <> nil then
    begin
      (Model as TCustomModel).ModflowPackages.StrPackage.
        MfStrSegmentNumber.Invalidate;
    end;
  end;
end;

procedure TStrItem.SetRoughness(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamRoughnessPosition, FRoughness);
end;

procedure TStrItem.SetSlope(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamSlopePosition, FSlope);
end;

procedure TStrItem.SetStage(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamStagePosition, FStage);
end;

procedure TStrItem.SetWidth(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreamWidthPosition, FWidth);
end;

{ TStrTimeListLink }

procedure TStrTimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  inherited;
  FStageData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStageData.NonParamDescription := StrHead;
  FStageData.ParamDescription := ' ' + LowerCase(StrHead);

  FConductanceData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FConductanceData.NonParamDescription := StrConductance;
  FConductanceData.ParamDescription := StrConductanceMultipl;

  FBedTopData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBedTopData.NonParamDescription := StrStreambedTop;
  FBedTopData.ParamDescription := ' ' + LowerCase(StrStreambedTop);

  FBedBottomData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FBedBottomData.NonParamDescription := StrStreambedBottom;
  FBedBottomData.ParamDescription := ' ' + LowerCase(StrStreambedBottom);

  FFlowData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FFlowData.NonParamDescription := StrFlow;
  FFlowData.ParamDescription := ' ' + LowerCase(StrFlow);

  FWidthData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FWidthData.NonParamDescription := StrWidth;
  FWidthData.ParamDescription := ' ' + LowerCase(StrWidth);

  FSlopeData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FSlopeData.NonParamDescription := StrSlope;
  FSlopeData.ParamDescription := ' ' + LowerCase(StrSlope);

  FRoughnessData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRoughnessData.NonParamDescription := StrRoughness;
  FRoughnessData.ParamDescription := ' ' + LowerCase(StrRoughness);

  FStrSegmentNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrSegmentNumberData.NonParamDescription := StrSegmentNumber;
  FStrSegmentNumberData.ParamDescription := ' ' + LowerCase(StrSegmentNumber);

  FStrOutflowSegmentNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrOutflowSegmentNumberData.NonParamDescription := StrOutflowSegmentNumber;
  FStrOutflowSegmentNumberData.ParamDescription := ' ' + LowerCase(StrOutflowSegmentNumber);

  FStrDiversionSegmentNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrDiversionSegmentNumberData.NonParamDescription := StrDiversionSegmentNu;
  FStrDiversionSegmentNumberData.ParamDescription := ' ' + LowerCase(StrDiversionSegmentNu);

  FStrReachNumberData := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStrReachNumberData.NonParamDescription := StrReachNumber;
  FStrReachNumberData.ParamDescription := ' ' + LowerCase(StrReachNumber);

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FStageData.OnInvalidate := LocalModel.InvalidateMfStrStage;
    FConductanceData.OnInvalidate := LocalModel.InvalidateMfStrConductance;
    FBedTopData.OnInvalidate := LocalModel.InvalidateMfStrBedTop;
    FBedBottomData.OnInvalidate := LocalModel.InvalidateMfStrBedBottom;
    FFlowData.OnInvalidate := LocalModel.InvalidateMfStrFlow;
    FWidthData.OnInvalidate := LocalModel.InvalidateMfStrWidth;
    FSlopeData.OnInvalidate := LocalModel.InvalidateMfStrSlope;
    FRoughnessData.OnInvalidate := LocalModel.InvalidateMfStrRoughness;

    FStrSegmentNumberData.OnInvalidate := LocalModel.InvalidateMfStrSegmentNumber;
    FStrOutflowSegmentNumberData.OnInvalidate := LocalModel.InvalidateMfStrOutflowSegmentNumber;
    FStrDiversionSegmentNumberData.OnInvalidate := LocalModel.InvalidateMfStrDiversionSegmentNumber;
    FStrReachNumberData.OnInvalidate := LocalModel.InvalidateMfStrReachNumber;
  end;

  AddTimeList(FConductanceData);
  AddTimeList(FBedTopData);
  AddTimeList(FBedBottomData);
  AddTimeList(FFlowData);
  AddTimeList(FStageData);
  AddTimeList(FWidthData);
  AddTimeList(FSlopeData);
  AddTimeList(FRoughnessData);
  AddTimeList(FStrSegmentNumberData);
  AddTimeList(FStrOutflowSegmentNumberData);
  AddTimeList(FStrDiversionSegmentNumberData);
  AddTimeList(FStrReachNumberData);
end;

destructor TStrTimeListLink.Destroy;
begin
  FStrReachNumberData.Free;
  FStrDiversionSegmentNumberData.Free;
  FStrOutflowSegmentNumberData.Free;
  FStrSegmentNumberData.Free;
  FRoughnessData.Free;
  FSlopeData.Free;
  FWidthData.Free;
  FStageData.Free;
  FConductanceData.Free;
  FBedTopData.Free;
  FBedBottomData.Free;
  FFlowData.Free;
  inherited;
end;

{ TStrCollection }

procedure TStrCollection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TStrStorage.Create(AModel));
end;

function TStrCollection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
  Boundary: TStrBoundary;
  ScreenObject: TScreenObject;
  Item: TStrItem;
begin
  Item := Items[ItemIndex] as TStrItem;
  if FormulaIndex = StreamConductancePosition then
  begin
    Boundary := BoundaryGroup as TStrBoundary;
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

procedure TStrCollection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string);
var
  StrStorage: TStrStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
        { TODO -cPEST : Add PEST support for PEST here }
        // record PEST parameter name if present.
        // record PEST DataArray name if present.
        // cache and restore PEST data.
  Assert(BoundaryFunctionIndex in [StreamStagePosition, StreamConductancePosition,
    StreamBedTopPosition, StreamBedBottomPosition, StreamFlowPosition,
    StreamWidthPosition, StreamSlopePosition, StreamRoughnessPosition]);
  Assert(Expression <> nil);

  StrStorage := BoundaryStorage as TStrStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    // 2. update locations
    Expression.Evaluate;
    with StrStorage.StrArray[Index] do
    begin
      case BoundaryFunctionIndex of
        StreamConductancePosition:
          begin
            Conductance := Expression.DoubleResult;
            ConductanceAnnotation := ACell.Annotation;
            ConductancePest := PestName;
          end;
        StreamBedTopPosition:
          begin
            BedTop := Expression.DoubleResult;
            BedTopAnnotation := ACell.Annotation;
            BedTopPest := PestName;
          end;
        StreamBedBottomPosition:
          begin
            BedBottom := Expression.DoubleResult;
            BedBottomAnnotation := ACell.Annotation;
            BedBottomPest := PestName;
          end;
        StreamFlowPosition:
          begin
            Flow := Expression.DoubleResult;
            FlowAnnotation := ACell.Annotation;
            FlowPest := PestName;
          end;
        StreamStagePosition:
          begin
            Stage := Expression.DoubleResult;
            StageAnnotation := ACell.Annotation;
            StagePest := PestName;
          end;
        StreamWidthPosition:
          begin
            Width := Expression.DoubleResult;
            WidthAnnotation := ACell.Annotation;
            WidthPest := PestName;
          end;
        StreamSlopePosition:
          begin
            Slope := Expression.DoubleResult;
            SlopeAnnotation := ACell.Annotation;
            SlopePest := PestName;
          end;
        StreamRoughnessPosition:
          begin
            Roughness := Expression.DoubleResult;
            RoughnessAnnotation := ACell.Annotation;
            RoughnessPest := PestName;
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TStrCollection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  StrStorage: TStrStorage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  StrStorage := BoundaryStorage as TStrStorage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with StrStorage.StrArray[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

constructor TStrCollection.Create(Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;
  SectionDuplicatesAllowed := True;
end;

procedure TStrCollection.FixItems;
var
  ItemIndex: Integer;
  Item1: TCustomModflowBoundaryItem;
  Item2: TCustomModflowBoundaryItem;
  StressPeriods: TModflowStressPeriods;
  StartTime: Double;
  EndTime: Double;
  Item: TStrItem;
  NewItem: TStrItem;
  StrItem2: TStrItem;
  StrItem1: TStrItem;
begin
  if Count > 0 then
  begin
    for ItemIndex := 0 to Count - 2 do
    begin
      Item1 := Items[ItemIndex] as TCustomModflowBoundaryItem;
      Item2 := Items[ItemIndex+1] as TCustomModflowBoundaryItem;
      if Item2.StartTime < Item1.EndTime then
      begin
        Item2.StartTime := Item1.EndTime;
      end;
    end;
    StressPeriods := (Model as TCustomModel).ModflowStressPeriods;
    StartTime := StressPeriods.First.StartTime;
    EndTime := StressPeriods.Last.EndTime;
    Item := Items[0] as TStrItem;
    if Item.StartTime > StartTime then
    begin
      NewItem := Insert(0) as TStrItem;
      NewItem.Assign(Item);
      NewItem.OutflowSegment := 0;
      NewItem.DiversionSegment := 0;
      NewItem.Flow := '0';
      NewItem.Conductance := '0';
      NewItem.StartTime := StartTime;
      NewItem.EndTime := Item.StartTime;
    end;
    Item := Items[Count-1] as TStrItem;
    if Item.EndTime < EndTime then
    begin
      NewItem := Add as TStrItem;
      NewItem.Assign(Item);
      NewItem.OutflowSegment := 0;
      NewItem.DiversionSegment := 0;
      NewItem.Flow := '0';
      NewItem.Conductance := '0';
      NewItem.StartTime := Item.EndTime;
      NewItem.EndTime := EndTime;
    end;
    for ItemIndex := Count - 2 downto 0 do
    begin
      StrItem2 := Items[ItemIndex+1] as TStrItem;
      StrItem1 := Items[ItemIndex] as TStrItem;
      if StrItem1.EndTime < StrItem2.StartTime then
      begin
      NewItem := Insert(ItemIndex+1) as TStrItem;
      NewItem.Assign(StrItem1);
      NewItem.OutflowSegment := 0;
      NewItem.DiversionSegment := 0;
      NewItem.Flow := '0';
      NewItem.Conductance := '0';
      NewItem.StartTime := StartTime;
      NewItem.EndTime := Item.StartTime;
      end;
    end;
  end;
end;

function TStrCollection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TStrTimeListLink;
end;

procedure TStrCollection.InvalidateBedBottomData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FBedBottomData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FBedBottomData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateBedTopData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FBedTopData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FBedTopData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateConductanceData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FConductanceData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FConductanceData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateFlowData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FFlowData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FFlowData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateRoughnessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FRoughnessData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FRoughnessData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateSlopeData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FSlopeData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FSlopeData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FStageData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FStageData.Invalidate;
    end;
  end;
end;

procedure TStrCollection.InvalidateWidthData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TStrTimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TStrTimeListLink;
    Link.FWidthData.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TStrTimeListLink;
      Link.FWidthData.Invalidate;
    end;
  end;
end;

class function TStrCollection.ItemClass: TBoundaryItemClass;
begin
  result := TStrItem;
end;

procedure TStrCollection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TStrStorage).FStrArray, BoundaryCount);
  inherited;
end;

procedure TStrCollection.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  Boundary: TStrBoundary;
begin
  // If observations exist, the list of cells must
  // be identical in every stress period.
  // To do that, introduce dummy values in BoundaryValues
  // for times that are not defined explicitly.
  // Set their conductances to zero so they have no effect
  // on the model.
  Boundary := BoundaryGroup as TStrBoundary;
  Boundary.TestIfObservationsPresent(EndOfLastStressPeriod,
    StartOfFirstStressPeriod, ObservationsPresent);
end;

{ TStrParamItem }

class function TStrParamItem.BoundaryClass: TMF_BoundCollClass;
begin
  result := TStrCollection;
end;

{ TStr_Cell }

procedure TStr_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  inherited;
  Values.Cache(Comp, Strings);
  WriteCompInt(Comp, StressPeriod);
end;

function TStr_Cell.GetBedBottom: double;
begin
  result := Values.BedBottom;
end;

function TStr_Cell.GetBedBottomAnnotation: string;
begin
  result := Values.BedBottomAnnotation;
end;

function TStr_Cell.GetBedTop: double;
begin
  result := Values.BedTop;
end;

function TStr_Cell.GetBedTopAnnotation: string;
begin
  result := Values.BedTopAnnotation;
end;

function TStr_Cell.GetColumn: integer;
begin
  result := Values.Cell.Column;
end;

function TStr_Cell.GetConductance: double;
begin
  result := Values.Conductance;
end;

function TStr_Cell.GetConductanceAnnotation: string;
begin
  result := Values.ConductanceAnnotation;
end;

function TStr_Cell.GetFlow: double;
begin
  result := Values.Flow;
end;

function TStr_Cell.GetFlowAnnotation: string;
begin
  result := Values.FlowAnnotation;
end;

function TStr_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TStr_Cell.GetIntegerValue(Index: integer; AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TStr_Cell.GetLayer: integer;
begin
  result := Values.Cell.Layer;
end;


function TStr_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    StreamConductancePosition: result := ConductanceAnnotation;
    StreamBedTopPosition: result := BedTopAnnotation;
    StreamBedBottomPosition: result := BedBottomAnnotation;
    StreamFlowPosition: result := FlowAnnotation;
    StreamStagePosition: result := StageAnnotation;
    StreamWidthPosition: result := WidthAnnotation;
    StreamSlopePosition: result := SlopeAnnotation;
    StreamRoughnessPosition: result := RoughnessAnnotation;
    else Assert(False);
  end;
end;

function TStr_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
  result := 0;
  case Index of
    StreamConductancePosition: result := Conductance;
    StreamBedTopPosition: result := BedTop;
    StreamBedBottomPosition: result := BedBottom;
    StreamFlowPosition: result := Flow;
    StreamStagePosition: result := Stage;
    StreamWidthPosition: result := Width;
    StreamSlopePosition: result := Slope;
    StreamRoughnessPosition: result := Roughness;
    else Assert(False);
  end;
end;

function TStr_Cell.GetRoughness: double;
begin
  result := Values.Roughness;
end;

function TStr_Cell.GetRoughnessAnnotation: string;
begin
  result := Values.RoughnessAnnotation;
end;

function TStr_Cell.GetRow: integer;
begin
  result := Values.Cell.Row;
end;

function TStr_Cell.GetSection: integer;
begin
  result := Values.Cell.Section;
end;

function TStr_Cell.GetSlope: double;
begin
  result := Values.Slope;
end;

function TStr_Cell.GetSlopeAnnotation: string;
begin
  result := Values.SlopeAnnotation;
end;

function TStr_Cell.GetStage: double;
begin
  result := Values.Stage;
end;

function TStr_Cell.GetStageAnnotation: string;
begin
  result := Values.StageAnnotation;
end;

function TStr_Cell.GetWidth: double;
begin
  result := Values.Width;
end;

function TStr_Cell.GetWidthAnnotation: string;
begin
  result := Values.WidthAnnotation;
end;

function TStr_Cell.IsIdentical(AnotherCell: TValueCell): boolean;
var
  Str_Cell: TStr_Cell;
begin
  result := AnotherCell is TStr_Cell;
  if result then
  begin
    Str_Cell := TStr_Cell(AnotherCell);
    result :=
      (Conductance = Str_Cell.Conductance)
      and (BedTop = Str_Cell.BedTop)
      and (BedBottom = Str_Cell.BedBottom)
      and (Flow = Str_Cell.Flow)
      and (Stage = Str_Cell.Stage)
      and (Width = Str_Cell.Width)
      and (Slope = Str_Cell.Slope)
      and (Roughness = Str_Cell.Roughness)
      and (SegmentNumber = Str_Cell.SegmentNumber)
      and (ReachNumber = Str_Cell.ReachNumber)
      and (IFace = Str_Cell.IFace)
      ;
  end;
end;

procedure TStr_Cell.RecordStrings(Strings: TStringList);
begin
  inherited;
  Values.RecordStrings(Strings);
end;

procedure TStr_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
  inherited;
  Values.Restore(Decomp, Annotations);
  StressPeriod := ReadCompInt(Decomp);
end;

procedure TStr_Cell.SetColumn(const Value: integer);
begin
  Values.Cell.Column := Value;
end;

procedure TStr_Cell.SetFlow(const Value: double);
begin
  Values.Flow := Value;
end;

procedure TStr_Cell.SetFlowAnnotation(const Value: string);
begin
  Values.FlowAnnotation := Value;
end;

procedure TStr_Cell.SetLayer(const Value: integer);
begin
  Values.Cell.Layer := Value;
end;

procedure TStr_Cell.SetRow(const Value: integer);
begin
  Values.Cell.Row := Value;
end;

{ TStrBoundary }

procedure TStrBoundary.Assign(Source: TPersistent);
begin
  if Source is TStrBoundary then
  begin
    SegmentNumber := TStrBoundary(Source).SegmentNumber;
  end;
  inherited;
end;

procedure TStrBoundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TStr_Cell;
  BoundaryValues: TStrRecord;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TStrStorage;
  LocalModel: TCustomModel;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TStrStorage;
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TStr_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.StrArray) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.StrArray)
      end;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.StrArray) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.StrArray[BoundaryIndex];
        Cell := TStr_Cell.Create;
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

class function TStrBoundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TStrCollection;
end;

procedure TStrBoundary.FixItems;
var
  ParamIndex: Integer;
  Param: TModflowParamItem;
begin
  (Values as TStrCollection).FixItems;
  for ParamIndex := 0 to Parameters.Count - 1 do
  begin
    Param := Parameters[ParamIndex];
    (Param.Param as TStrCollection).FixItems;
  end;
end;

procedure TStrBoundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TStrStorage;
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
begin
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
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TStrStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
      PriorTime := Item.EndTime;
    end;
    if ValueCount < Values.BoundaryCount[AModel] then
    begin
      BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TStrStorage;
      AssignCells(BoundaryStorage, ValueTimeList, AModel);
      Inc(ValueCount);
    end;
    if {(ValueIndex = Values.Count - 1) and} ObservationsPresent then
    begin
      if Item.EndTime < EndOfLastStressPeriod then
      begin
        if ValueCount < Values.BoundaryCount[AModel] then
        begin
          BoundaryStorage := Values.Boundaries[ValueCount, AModel] as TStrStorage;
          AssignCells(BoundaryStorage, ValueTimeList, AModel);
          Inc(ValueCount);
        end;
      end;
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
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TStrStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
        PriorTime := Item.EndTime;
      end;
      if ValueCount < Param.Param.BoundaryCount[AModel] then
      begin
        BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TStrStorage;
        AssignCells(BoundaryStorage, Times, AModel);
        Inc(ValueCount);
      end;
      if {(ValueIndex = Param.Param.Count - 1) and} ObservationsPresent then
      begin
        if Item.EndTime < EndOfLastStressPeriod then
        begin
          if ValueCount < Param.Param.BoundaryCount[AModel] then
          begin
            BoundaryStorage := Param.Param.Boundaries[ValueCount, AModel] as TStrStorage;
            AssignCells(BoundaryStorage, Times, AModel);
            Inc(ValueCount);
          end;
        end;
      end;
    end;
  end;
end;

procedure TStrBoundary.InvalidateDisplay;
var
  Model: TPhastModel;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    Model := ParentModel as TPhastModel;
    Model.InvalidateMfStrConductance(self);
    Model.InvalidateMfStrBedTop(self);
    Model.InvalidateMfStrBedBottom(self);
    Model.InvalidateMfStrFlow(self);
    Model.InvalidateMfStrStage(self);
    Model.InvalidateMfStrWidth(self);
    Model.InvalidateMfStrSlope(self);
    Model.InvalidateMfStrRoughness(self);
    Model.InvalidateMfStrSegmentNumber(self);
    Model.InvalidateMfStrOutflowSegmentNumber(self);
    Model.InvalidateMfStrDiversionSegmentNumber(self);
    Model.InvalidateMfStrReachNumber(self);
  end;
end;

class function TStrBoundary.ModflowParamItemClass: TModflowParamItemClass;
begin
  result := TStrParamItem;
end;

function TStrBoundary.ParameterType: TParameterType;
begin
  result := ptSTR;
end;

procedure TStrBoundary.SetSegmentNumber(const Value: Integer);
var
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  FOldValue: Integer;
  AScreenObject: TScreenObject;
  AnotherStr: TStrBoundary;
  AnItem: TStrItem;
  AParam: TModflowParamItem;
  ItemIndex: Integer;
  ParamIndex: Integer;
begin
  if FSegmentNumber <> Value then
  begin
    InvalidateModel;
    FOldValue := FSegmentNumber;
    FSegmentNumber := Value;
    if ParentModel <> nil then
    begin
      LocalModel :=  ParentModel as TCustomModel;
      LocalModel.ModflowPackages.StrPackage.
        MfStrSegmentNumber.Invalidate;
      if FOldValue <> 0 then
      begin
        for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
          AnotherStr := AScreenObject.ModflowStrBoundary;
          if AnotherStr <> nil then
          begin
            for ItemIndex := 0 to AnotherStr.Values.Count - 1 do
            begin
              AnItem := AnotherStr.Values[ItemIndex] as TStrItem;
              if AnItem.OutflowSegment = FOldValue then
              begin
                AnItem.OutflowSegment := FSegmentNumber;
              end;
              if AnItem.DiversionSegment = FOldValue then
              begin
                AnItem.DiversionSegment := FSegmentNumber;
              end;
            end;
            for ParamIndex := 0 to AnotherStr.Parameters.Count -1 do
            begin
              AParam := AnotherStr.Parameters[ParamIndex];
              for ItemIndex := 0 to AParam.Param.Count - 1 do
              begin
                AnItem := AParam.Param[ItemIndex] as TStrItem;
                if AnItem.OutflowSegment = FOldValue then
                begin
                  AnItem.OutflowSegment := FSegmentNumber;
                end;
                if AnItem.DiversionSegment = FOldValue then
                begin
                  AnItem.DiversionSegment := FSegmentNumber;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TStrBoundary.TestIfObservationsPresent(var EndOfLastStressPeriod,
  StartOfFirstStressPeriod: Double; var ObservationsPresent: Boolean);
var
  LocalScreenObject: TScreenObject;
  LocalPhastModel: TPhastModel;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  LocalPhastModel := LocalScreenObject.Model as TPhastModel;
  Assert(LocalPhastModel <> nil);
  ObservationsPresent := LocalPhastModel.StobIsSelected
    and (LocalPhastModel.StreamObservations.Count > 0);
  StartOfFirstStressPeriod := 0;
  EndOfLastStressPeriod := 0;
  if ObservationsPresent then
  begin
    StartOfFirstStressPeriod := LocalPhastModel.ModflowStressPeriods[0].StartTime;
    EndOfLastStressPeriod := LocalPhastModel.ModflowStressPeriods[
      LocalPhastModel.ModflowStressPeriods.Count - 1].EndTime;
  end;
end;

end.
