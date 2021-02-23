unit ModflowSfr6Unit;

interface

uses Windows, ZLib, SysUtils, Classes, Contnrs, ModflowCellUnit,
  ModflowBoundaryUnit, FormulaManagerUnit, OrderedCollectionUnit, GoPhastTypes,
  System.Generics.Collections, SubscriptionUnit, RbwParser;

type
  TSfrOb = (soStage, soExtInflow, soInflow, soFromMvr, soRainfall, soRunoff, soSfr,
    soEvaporation, soOutflow, soExternalOutflow, soToMvr, soUpstreamFlow,
    soDownstreamFlow);
  TSfrObs = set of TSfrOb;

  TSfrObsLocation = (solAll, solFirst, solLast, solIndividual);

  // ssInactive = Stream is inactive.
  // ssActive = stage is calculated.
  // ssSimple = stage is specified.
  TStreamStatus = (ssInactive, ssActive, ssSimple);

  TSfrMF6Record = record
    Cell: TCellLocation;

    Inflow: double;
    Rainfall: double;
    Evaporation: double;
    Runoff: double;
    UpstreamFraction: double;
    Stage: Double;
    Roughness: Double;
    Diversions: array of double;

    InflowAnnotation: string;
    RainfallAnnotation: string;
    EvaporationAnnotation: string;
    RunoffAnnotation: string;
    UpstreamFractionAnnotation: string;
    StageAnnotation: string;
    RoughnessAnnotation: string;
    DiversionAnnotations: array of string;

    InflowPest: string;
    RainfallPest: string;
    EvaporationPest: string;
    RunoffPest: string;
    UpstreamFractionPest: string;
    StagePest: string;
    RoughnessPest: string;
    DiversionPests: array of string;

    Status: TStreamStatus;
    ReachNumber: Integer;
    MvrUsed: Boolean;
    MvrIndex: Integer;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
  end;

  TSfrMF6Array = array of TSfrMF6Record;

  TSfrMf6Storage = class(TCustomBoundaryStorage)
  private
    FSfrMF6Array: TSfrMF6Array;
    function GetStrMF6Array: TSfrMF6Array;
  protected
    procedure Restore(DecompressionStream: TDecompressionStream; Annotations: TStringList); override;
    procedure Store(Compressor: TCompressionStream); override;
    procedure Clear; override;
  public
    property SfrMF6Array: TSfrMF6Array read GetStrMF6Array;
  end;

  TSfrMf6Item = class(TCustomModflowBoundaryItem)
  private
    FDiversions: TStringList;
    FInflow: TFormulaObject;
    FEvaporation: TFormulaObject;
    FRunoff: TFormulaObject;
    FUpstreamFraction: TFormulaObject;
    FDiversionFormulas: TList<TFormulaObject>;
    FStatus: Boolean;
    FRainfall: TFormulaObject;
    FStage: TFormulaObject;
    FRoughness: TFormulaObject;
    FStreamStatus: TStreamStatus;
    function GetDiversions: TStrings;
    function GetEvaporation: string;
    function GetInflow: string;
    function GetRunoff: string;
    function GetUpstreamFraction: string;
    procedure SetDiversions(const Value: TStrings);
    procedure SetEvaporation(const Value: string);
    procedure SetInflow(const Value: string);
    procedure SetRunoff(const Value: string);
    procedure SetUpstreamFraction(const Value: string);
    function GetDiversionFormula(Index: Integer): string;
    procedure SetDiversionFormula(Index: Integer; const Value: string);
    procedure SetDiversionCount(const Value: Integer);
    function GetDiversionCount: Integer;
    procedure SetStatus(const Value: Boolean);
    function GetRainfall: string;
    procedure SetRainfall(const Value: string);
    procedure SetStreamStatus(const Value: TStreamStatus);
    function GetRoughness: string;
    function GetStage: string;
    procedure SetRoughness(const Value: string);
    procedure SetStage(const Value: string);
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
    procedure UpdateBoundaryObservers;
  public
    // @name copies Source to this @classname.
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    // @name defines formulas for diversion amounts.
    property DiversionFormulas[Index: Integer]: string read GetDiversionFormula
      write SetDiversionFormula;
    property DiversionCount: Integer read GetDiversionCount write SetDiversionCount;
    procedure Loaded;
  published
    property Inflow: string read GetInflow write SetInflow;
    property Rainfall: string read GetRainfall write SetRainfall;
    property Evaporation: string read GetEvaporation write SetEvaporation;
    property Runoff: string read GetRunoff write SetRunoff;
    property UpstreamFraction: string read GetUpstreamFraction
      write SetUpstreamFraction;
    property Stage: string read GetStage write SetStage;
    property Roughness: string read GetRoughness write SetRoughness;
    // for backwards compatibility
    property Status: Boolean read FStatus write SetStatus stored False;
    property StreamStatus: TStreamStatus read FStreamStatus write SetStreamStatus;
    // @name is used to store @link(DiversionFormulas) to file and to read
    // @link(DiversionFormulas) from a file but otherwise, use
    // @link(DiversionFormulas) instead of @name.
    property Diversions: TStrings read GetDiversions write SetDiversions;
  end;

  TSfrMf6TimeListLink = class(TTimeListsModelLink)
  private
    FInflow: TModflowTimeList;
    FRainfall: TModflowTimeList;
    FEvaporation: TModflowTimeList;
    FRunoff: TModflowTimeList;
    FUpstreamFraction: TModflowTimeList;
    FStage: TModflowTimeList;
    FRoughness: TModflowTimeList;
    FStreamStatus: TModflowTimeList;
    FReachNumber: TModflowTimeList;
  protected
    procedure CreateTimeLists; override;
  public
    Destructor Destroy; override;
  end;

  TSfrMf6Boundary = class;

  TSfrMf6Collection = class(TCustomMF_ListBoundColl)
  private
    FSfrMf6Boundary: TSfrMf6Boundary;
    procedure InvalidateInflowData(Sender: TObject);
    procedure InvalidateRainfallData(Sender: TObject);
    procedure InvalidateEvaporationData(Sender: TObject);
    procedure InvalidateRunoffData(Sender: TObject);
    procedure InvalidateUpstreamFractionData(Sender: TObject);
    procedure InvalidateDiversionsData(Sender: TObject);
    procedure InvalidateStageData(Sender: TObject);
    procedure InvalidateRoughnessData(Sender: TObject);
  protected
    class function ItemClass: TBoundaryItemClass; override;
    function GetTimeListLinkClass: TTimeListsModelLinkClass; override;
    procedure Loaded;
    procedure AddSpecificBoundary(AModel: TBaseModel); override;
    procedure SetBoundaryStartAndEndTime(BoundaryCount: Integer;
      Item: TCustomModflowBoundaryItem; ItemIndex: Integer;
      AModel: TBaseModel); override;
    procedure AssignListCellLocation(BoundaryStorage: TCustomBoundaryStorage;
      ACellList: TObject); override;
    function AdjustedFormula(FormulaIndex, ItemIndex: integer): string;
      override;
    procedure AssignCellList(Expression: TExpression; ACellList: TObject;
      BoundaryStorage: TCustomBoundaryStorage; BoundaryFunctionIndex: integer;
      Variables, DataSets: TList; AModel: TBaseModel; AScreenObject: TObject;
      PestName: string); override;
    procedure AssignDirectlySpecifiedValues( AnItem: TCustomModflowBoundaryItem;
      BoundaryStorage: TCustomBoundaryStorage); override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TSfrMf6_Cell = class(TValueCell)
  private
    FValues: TSfrMF6Record;
    StressPeriod: integer;
    function GetMvrIndex: Integer;
    function GetMvrUsed: Boolean;
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
    function GetSection: integer; override;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);  override;
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList); override;
    procedure RecordStrings(Strings: TStringList); override;
  public
    property MvrUsed: Boolean read GetMvrUsed;
    property MvrIndex: Integer read GetMvrIndex;
    property Values: TSfrMF6Record read FValues;
  end;

  TDivisionPriority = (cpFraction, cpExcess, cpThreshold, cpUpTo);

  TDivRecord = record
    ConnectedReach: Integer;
    Priority: TDivisionPriority;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
  end;

  TDiversionArray = array of TDivRecord;

  TSfrMF6ConstantRecord = record
  private
    FReachNumber: integer;
    function GetBoundaryAnnotation(Index: Integer): string;
    function GetBoundaryValue(Index: Integer): double;
    procedure SetBoundaryAnnotation(Index: Integer; const Value: string);
    procedure SetBoundaryValue(Index: Integer; const Value: double);
    procedure SetReachNumber(const Value: integer);
  public
    Cell: TCellLocation;
    ReachLength: Double;
    ReachWidth: Double;
    Gradient: Double;
    StreambedTop: Double;
    StreambedThickness: Double;
    HydraulicConductivity: Double;
//    Roughness: Double;
//    UpstreamFraction: Double;
    ConnectedReaches: array of integer;
    DownstreamDiversions: TDiversionArray;
    ReachLengthAnnotation: string;
    ReachWidthAnnotation: string;
    GradientAnnotation: string;
    StreambedTopAnnotation: string;
    StreambedThicknessAnnotation: string;
    HydraulicConductivityAnnotation: string;
    BoundName: string;
//    RoughnessAnnotation: string;
//    UpstreamFractionAnnotation: string;
    ConnectedReacheAnnotations: array of string;
    property ReachNumber: integer read FReachNumber write SetReachNumber;
    procedure Cache(Comp: TCompressionStream; Strings: TStringList);
    procedure Restore(Decomp: TDecompressionStream; Annotations: TStringList);
    procedure RecordStrings(Strings: TStringList);
    property BoundaryValue[Index: Integer]: double read GetBoundaryValue write SetBoundaryValue;
    property BoundaryAnnotation[Index: Integer]: string read GetBoundaryAnnotation write SetBoundaryAnnotation;
    function IsConnected(Value: Integer): boolean;
  end;

  TSfrMF6ConstArray = array of TSfrMF6ConstantRecord;

  TSDiversionItem = class(TOrderedItem)
  private
    FDownstreamSegment: Integer;
    FPriority: TDivisionPriority;
//    FDiversionNumber: Integer;
    procedure SetPriority(const Value: TDivisionPriority);
    procedure SetDownstreamSegment(const Value: Integer);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
//    property DiversionNumber: Integer read FDiversionNumber write FDiversionNumber;
  published
    property DownstreamSegment: Integer read FDownstreamSegment
      write SetDownstreamSegment;
    property Priority: TDivisionPriority read FPriority write SetPriority;
  end;

  TDiversionCollection = class(TOrderedCollection)
  private
    function GetItem(Index: Integer): TSDiversionItem;
    procedure SetItem(Index: Integer; const Value: TSDiversionItem);
  public
    function IsSame(AnOrderedCollection: TOrderedCollection): boolean; override;
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSDiversionItem read GetItem write SetItem; default;
    function Add: TSDiversionItem;
  end;

//  TSfrInflowLocation = (silAllCells, silFirstCell);

  TSfrMf6Boundary = class(TModflowBoundary)
  private
    FDiversions: TDiversionCollection;
    FDownstreamSegments: TIntegerCollection;
    FSegmentNumber: Integer;
    FReachLength: TFormulaObject;
    FGradient: TFormulaObject;
    FHydraulicConductivity: TFormulaObject;
    FReachWidth: TFormulaObject;
    FRoughness: string;
    FStreambedThickness: TFormulaObject;
    FStreambedTop: TFormulaObject;
    FUpstreamFraction: string;
    FReachLengthObserver: TObserver;
    FHydraulicConductivityObserver: TObserver;
    FReachWidthObserver: TObserver;
    FStreambedTopObserver: TObserver;
//    FRoughnessObserver: TObserver;
    FGradientObserver: TObserver;
    FStreambedThicknessObserver: TObserver;
//    FSfrInflowLocation: TSfrInflowLocation;
//    FUpstreamFractionObserver: TObserver;
    procedure SetDiversions(const Value: TDiversionCollection);
    procedure SetDownstreamSegments(const Value: TIntegerCollection);
    procedure SetSegmentNumber(const Value: Integer);
    function GetReachLength: string;
    procedure SetReachLength(const Value: string);
    function GetGradient: string;
    function GetHydraulicConductivity: string;
    function GetReachWidth: string;
//    function GetRoughness: string;
    function GetStreambedThickness: string;
    function GetStreambedTop: string;
//    function GetUpstreamFraction: string;
    procedure SetGradient(const Value: string);
    procedure SetHydraulicConductivity(const Value: string);
    procedure SetReachWidth(const Value: string);
//    procedure SetRoughness(const Value: string);
    procedure SetStreambedThickness(const Value: string);
    procedure SetStreambedTop(const Value: string);
//    procedure SetUpstreamFraction(const Value: string);
    procedure CreateFormulaObjects;
    procedure RemoveFormulaObjects;
    function GetGradientObserver: TObserver;
    function GetHydraulicConductivityObserver: TObserver;
    function GetReachLengthObserver: TObserver;
    function GetReachWidthObserver: TObserver;
//    function GetRoughnessObserver: TObserver;
    function GetStreambedThicknessObserver: TObserver;
    function GetStreambedTopObserver: TObserver;
//    function GetUpstreamFractionObserver: TObserver;
    procedure InvalidateDisplayTimeLists;
    procedure LinkReachLength;
    procedure LinkReachWidth;
    procedure LinkGradient;
    procedure LinkStreambedTop;
    procedure LinkStreambedThickness;
    procedure LinkHydraulicConductivity;
//    procedure SetSfrInflowLocation(const Value: TSfrInflowLocation);
  protected
    procedure AssignCells(BoundaryStorage: TCustomBoundaryStorage;
      ValueTimeList: TList; AModel: TBaseModel); override;
    class function BoundaryCollectionClass: TMF_BoundCollClass; override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    property ReachLengthObserver: TObserver read GetReachLengthObserver;
    property ReachWidthObserver: TObserver read GetReachWidthObserver;
    property GradientObserver: TObserver read GetGradientObserver;
    property StreambedTopObserver: TObserver read GetStreambedTopObserver;
    property StreambedThicknessObserver: TObserver read GetStreambedThicknessObserver;
    property HydraulicConductivityObserver: TObserver read GetHydraulicConductivityObserver;
    function BoundaryObserverPrefix: string; override;
  public
    Procedure Assign(Source: TPersistent); override;
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    Destructor Destroy; override;
    procedure CreateObservers;
    procedure Loaded;
    procedure GetCellValues(ValueTimeList: TList; ParamList: TStringList;
      AModel: TBaseModel); override;
    procedure InvalidateDisplay; override;
  published
    property SegmentNumber: Integer read FSegmentNumber write SetSegmentNumber;
    property DownstreamSegments: TIntegerCollection read FDownstreamSegments
      write SetDownstreamSegments;
    property Diversions: TDiversionCollection read FDiversions write SetDiversions;
    property ReachLength: string read GetReachLength Write SetReachLength;
    property ReachWidth: string read GetReachWidth write SetReachWidth;
    property Gradient: string read GetGradient write SetGradient;
    property StreambedTop: string read GetStreambedTop write SetStreambedTop;
    property StreambedThickness: string read GetStreambedThickness write SetStreambedThickness;
    property HydraulicConductivity: string read GetHydraulicConductivity write SetHydraulicConductivity;
    property Roughness: string read FRoughness write FRoughness stored False;
    property UpstreamFraction: string read FUpstreamFraction write FUpstreamFraction stored False;
//    property SfrInflowLocation: TSfrInflowLocation read FSfrInflowLocation
//      write SetSfrInflowLocation stored True;
  end;

const
  InflowPosition = 0;
  RainfallPosition = 1;
  EvaporationPosition = 2;
  RunoffPosition = 3;
  UpstreamFractionPosition = 4;
  StagePosition = 5;
  RoughnessPosition = 6;
  DiversionStartPosition = 7;

  ReachLengthPosition = 0;
  ReachWidthPosition = 1;
  GradientPosition = 2;
  StreambedTopPosition = 3;
  StreambedThicknessPosition = 4;
  HydraulicConductivityPosition = 5;
//  SteadyRoughnessPosition = 6;
//  SteadyUpstreamFractionPosition = 7;
function TryGetSfrOb(const SfrObName: string; var SfrOb: TSfrOb): Boolean;
function SfrObToString(const SfrOb: TSfrOb): string;
Procedure FillSfrSeriesNames(AList: TStrings);

implementation

uses
  frmGoPhastUnit, ModflowTimeUnit, PhastModelUnit,
  ScreenObjectUnit, GIS_Functions, ModflowSfrUnit, ModflowSfrReachUnit,
  ModflowSfrSegment, ModflowSfrChannelUnit, ModflowSfrParamIcalcUnit,
  ModflowSfrFlows, ModflowStrUnit, DataSetUnit, ModflowMvrUnit;

const
  SfrObName: array[TSfrOb] of string =
    ('Stage', 'ExtInflow', 'Inflow', 'FromMvr', 'Rainfall', 'Runoff', 'Sfr',
    'Evaporation', 'Outflow', 'ExternalOutflow', 'ToMvr', 'UpstreamFlow',
    'DownstreamFlow');

var
  SfrObNames: TStringList;

procedure InitializeSfrObNames;
var
  SfrOb: TSfrOb;
begin
  SfrObNames:= TStringList.Create;
  SfrObNames.CaseSensitive := False;
  for SfrOb := Low(TSfrOb) to High(TSfrOb) do
  begin
    SfrObNames.Add(SfrObName[SfrOb]);
  end;
end;

function TryGetSfrOb(const SfrObName: string; var SfrOb: TSfrOb): Boolean;
var
  Index: Integer;
begin
  Index := SfrObNames.IndexOf(SfrObName);
  result := Index >= 0;
  if result then
  begin
    SfrOb := TSfrOb(Index);
  end;
end;

Procedure FillSfrSeriesNames(AList: TStrings);
begin
  AList.Assign(SfrObNames);
end;

function SfrObToString(const SfrOb: TSfrOb): string;
begin
  result := SfrObName[SfrOb]
end;

resourcestring
  StrAllButTheFirstRe = 'All but the first reach in a segment is assigned a ' +
  'value of 1. Assigned by %s.';
  StrUpstreamFractionIs = 'Upstream fraction is automatically set to zero fo' +
  'r the first reach in an inactive segment.';
  StrInflowIsOnlySetT = 'Inflow is only set to a non-zero value in the first' +
  ' reach in the segment defined by %s';

{ TStrMF6Record }

procedure TSfrMF6Record.Cache(Comp: TCompressionStream; Strings: TStringList);
var
  index: Integer;
begin
  WriteCompCell(Comp, Cell);

  WriteCompReal(Comp, Inflow);
  WriteCompReal(Comp, Rainfall);
  WriteCompReal(Comp, Evaporation);
  WriteCompReal(Comp, Runoff);
  WriteCompReal(Comp, UpstreamFraction);
  WriteCompReal(Comp, Stage);
  WriteCompReal(Comp, Roughness);
  WriteCompInt(Comp, Ord(Status));

  WriteCompInt(Comp, Length(Diversions));
  for index := 0 to Length(Diversions) - 1 do
  begin
    WriteCompReal(Comp, Diversions[index]);
  end;

  WriteCompInt(Comp, Strings.IndexOf(InflowAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RainfallAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RunoffAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(UpstreamFractionAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StageAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessAnnotation));

  for index := 0 to Length(DiversionAnnotations) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DiversionAnnotations[index]));
  end;

  WriteCompInt(Comp, Strings.IndexOf(InflowPest));
  WriteCompInt(Comp, Strings.IndexOf(RainfallPest));
  WriteCompInt(Comp, Strings.IndexOf(EvaporationPest));
  WriteCompInt(Comp, Strings.IndexOf(RunoffPest));
  WriteCompInt(Comp, Strings.IndexOf(UpstreamFractionPest));
  WriteCompInt(Comp, Strings.IndexOf(StagePest));
  WriteCompInt(Comp, Strings.IndexOf(RoughnessPest));

  for index := 0 to Length(DiversionPests) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(DiversionPests[index]));
  end;

  WriteCompInt(Comp, ReachNumber);

  WriteCompBoolean(Comp, MvrUsed);
  WriteCompInt(Comp, MvrIndex);
  SetLength(Diversions, 0);
  SetLength(DiversionAnnotations, 0);
end;

procedure TSfrMF6Record.RecordStrings(Strings: TStringList);
var
  index: Integer;
begin
  Strings.Add(InflowAnnotation);
  Strings.Add(RainfallAnnotation);
  Strings.Add(EvaporationAnnotation);
  Strings.Add(RunoffAnnotation);
  Strings.Add(UpstreamFractionAnnotation);
  Strings.Add(StageAnnotation);
  Strings.Add(RoughnessAnnotation);
  for index := 0 to Length(DiversionAnnotations) - 1 do
  begin
    Strings.Add(DiversionAnnotations[index]);
  end;

  Strings.Add(InflowPest);
  Strings.Add(RainfallPest);
  Strings.Add(EvaporationPest);
  Strings.Add(RunoffPest);
  Strings.Add(UpstreamFractionPest);
  Strings.Add(StagePest);
  Strings.Add(RoughnessPest);
  for index := 0 to Length(DiversionPests) - 1 do
  begin
    Strings.Add(DiversionPests[index]);
  end;
end;

procedure TSfrMF6Record.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  ArraySize: Integer;
  index: Integer;
begin
  Cell := ReadCompCell(Decomp);

  Inflow := ReadCompReal(Decomp);
  Rainfall := ReadCompReal(Decomp);
  Evaporation := ReadCompReal(Decomp);
  Runoff := ReadCompReal(Decomp);
  UpstreamFraction := ReadCompReal(Decomp);
  Stage := ReadCompReal(Decomp);
  Roughness := ReadCompReal(Decomp);
  Status := TStreamStatus(ReadCompInt(Decomp));

  ArraySize := ReadCompInt(Decomp);
  SetLength(Diversions, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    Diversions[index] := ReadCompReal(Decomp);
  end;

  InflowAnnotation := Annotations[ReadCompInt(Decomp)];
  RainfallAnnotation := Annotations[ReadCompInt(Decomp)];
  EvaporationAnnotation := Annotations[ReadCompInt(Decomp)];
  RunoffAnnotation := Annotations[ReadCompInt(Decomp)];
  UpstreamFractionAnnotation := Annotations[ReadCompInt(Decomp)];
  StageAnnotation := Annotations[ReadCompInt(Decomp)];
  RoughnessAnnotation := Annotations[ReadCompInt(Decomp)];

  SetLength(DiversionAnnotations, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionAnnotations[index] := Annotations[ReadCompInt(Decomp)];
  end;

  InflowPest := Annotations[ReadCompInt(Decomp)];
  RainfallPest := Annotations[ReadCompInt(Decomp)];
  EvaporationPest := Annotations[ReadCompInt(Decomp)];
  RunoffPest := Annotations[ReadCompInt(Decomp)];
  UpstreamFractionPest := Annotations[ReadCompInt(Decomp)];
  StagePest := Annotations[ReadCompInt(Decomp)];
  RoughnessPest := Annotations[ReadCompInt(Decomp)];

  SetLength(DiversionPests, ArraySize);
  for index := 0 to ArraySize - 1 do
  begin
    DiversionPests[index] := Annotations[ReadCompInt(Decomp)];
  end;

  ReachNumber := ReadCompInt(Decomp);
  MvrUsed := ReadCompBoolean(Decomp);
  MvrIndex := ReadCompInt(Decomp);
end;

{ TStrMf6Storage }

procedure TSfrMf6Storage.Clear;
begin
  SetLength(FSfrMF6Array, 0);
  FCleared := True;
end;

function TSfrMf6Storage.GetStrMF6Array: TSfrMF6Array;
begin
  if FCached and FCleared then
  begin
    RestoreData;
  end;
  result := FSfrMF6Array;
end;

procedure TSfrMf6Storage.Restore(DecompressionStream: TDecompressionStream;
  Annotations: TStringList);
var
  Index: Integer;
  Count: Integer;
begin
  DecompressionStream.Read(Count, SizeOf(Count));
  SetLength(FSfrMF6Array, Count);
  for Index := 0 to Count - 1 do
  begin
    FSfrMF6Array[Index].Restore(DecompressionStream, Annotations);
  end;
end;

procedure TSfrMf6Storage.Store(Compressor: TCompressionStream);
var
  Index: Integer;
  Count: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Sorted := true;
    Strings.Duplicates := dupIgnore;
    Count := Length(FSfrMF6Array);
    for Index := 0 to Count - 1 do
    begin
      FSfrMF6Array[Index].RecordStrings(Strings);
    end;
    WriteCompInt(Compressor, Strings.Count);

    for Index := 0 to Strings.Count - 1 do
    begin
      WriteCompString(Compressor, Strings[Index]);
    end;

    Compressor.Write(Count, SizeOf(Count));
    for Index := 0 to Count - 1 do
    begin
      FSfrMF6Array[Index].Cache(Compressor, Strings);
    end;

  finally
    Strings.Free;
  end;
end;

{ TStrMf6Item }

procedure TSfrMf6Item.Assign(Source: TPersistent);
var
  StrSource: TSfrMf6Item;
  DivIndex: Integer;
  ParamItem: TSfrParamIcalcItem;
  StrItem: TStrItem;
  LocalModel: TCustomModel;
begin
  if Source is TSfrMf6Item then
  begin
    StrSource := TSfrMf6Item(Source);
    Inflow := StrSource.Inflow;
    Rainfall := StrSource.Rainfall;
    Evaporation := StrSource.Evaporation;
    Runoff := StrSource.Runoff;
    UpstreamFraction := StrSource.UpstreamFraction;
    Stage := StrSource.Stage;
    Roughness := StrSource.Roughness;
    StreamStatus := StrSource.StreamStatus;
    DiversionCount := StrSource.Diversions.Count;
    for DivIndex := 0 to DiversionCount - 1 do
    begin
      DiversionFormulas[DivIndex] := StrSource.Diversions[DivIndex];
    end;
  end
  else if Source is TSfrParamIcalcItem then
  begin
    ParamItem := TSfrParamIcalcItem(Source);
    if ParamItem.ICalc = 0 then
    begin
      StreamStatus := ssSimple
    end
    else
    begin
      StreamStatus := ssActive
    end;
  end
  else if Source is TStrItem then
  begin
    StrItem := TStrItem(Source);
    LocalModel := Model as TCustomModel;
    if LocalModel.ModflowPackages.StrPackage.CalculateStage then
    begin
      StreamStatus := ssActive
    end
    else
    begin
      StreamStatus := ssSimple
    end;

    Inflow := StrItem.Flow;
    Rainfall := '0';
    Evaporation := '0';
    Runoff := '0';
    UpstreamFraction := '1';
    Stage := StrItem.Stage;
    Roughness := StrItem.Roughness;
    if StrItem.DiversionSegment > 0 then
    begin
      Inflow := '0';
      Diversions.Add(StrItem.Flow)
    end
    else
    begin
      Inflow := StrItem.Flow;
    end;

  end;
  inherited;

end;

procedure TSfrMf6Item.AssignObserverEvents(Collection: TCollection);
var
  ParentCollection: TSfrMf6Collection;
  Observer: TObserver;
  DivIndex: Integer;
begin
  ParentCollection := Collection as TSfrMf6Collection;
  Observer := FObserverList[InflowPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateInflowData;

  Observer := FObserverList[RainfallPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateRainfallData;

  Observer := FObserverList[EvaporationPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateEvaporationData;

  Observer := FObserverList[RunoffPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateRunoffData;

  Observer := FObserverList[UpstreamFractionPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateUpstreamFractionData;

  Observer := FObserverList[StagePosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateStageData;

  Observer := FObserverList[RoughnessPosition];
  Observer.OnUpToDateSet := ParentCollection.InvalidateRoughnessData;

  for DivIndex := 0 to DiversionCount - 1 do
  begin
    Observer := FObserverList[DivIndex + DiversionStartPosition];
    Observer.OnUpToDateSet := ParentCollection.InvalidateDiversionsData;
  end;
end;

function TSfrMf6Item.BoundaryFormulaCount: integer;
begin
  result := DiversionCount + 7;
end;

constructor TSfrMf6Item.Create(Collection: TCollection);
begin
  FDiversions := TStringList.Create;
  FDiversionFormulas := TList<TFormulaObject>.Create;
  inherited;
  FStatus := True;

  Inflow := '0';
  Rainfall := '0';
  Runoff := '0';
  UpstreamFraction := '1';
  Stage := '0';
  Roughness := '0.03';
//  Status := True;
  StreamStatus := ssActive;

end;

procedure TSfrMf6Item.CreateFormulaObjects;
begin
  inherited;
  FInflow := CreateFormulaObject(dso3D);
  FRainfall := CreateFormulaObject(dso3D);
  FEvaporation := CreateFormulaObject(dso3D);
  FRunoff := CreateFormulaObject(dso3D);
  FUpstreamFraction := CreateFormulaObject(dso3D);
  FStage := CreateFormulaObject(dso3D);
  FRoughness := CreateFormulaObject(dso3D);
end;

destructor TSfrMf6Item.Destroy;
begin
  DiversionCount := 0;
  inherited;
  // FDiversionFormulas is accessed in RemoveFormulaObjects which is called
  // in inherited Destroy.
  FDiversionFormulas.Free;
  FDiversions.Free;
end;

function TSfrMf6Item.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    InflowPosition: result := Inflow;
    RainfallPosition: result := Rainfall;
    EvaporationPosition: result := Evaporation;
    RunoffPosition: result := Runoff;
    UpstreamFractionPosition: result := UpstreamFraction;
    StagePosition: result := Stage;
    RoughnessPosition: result := Roughness;
    else result := DiversionFormulas[Index-DiversionStartPosition];
  end;
end;

function TSfrMf6Item.GetDiversionCount: Integer;
begin
  result := FDiversionFormulas.Count;
end;

function TSfrMf6Item.GetDiversionFormula(Index: Integer): string;
begin
  Result := FDiversionFormulas[Index].Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(DiversionStartPosition+Index);
  end;
end;

function TSfrMf6Item.GetDiversions: TStrings;
var
  DivIndex: Integer;
begin
  UpdateBoundaryObservers;

  FDiversions.Clear;
  FDiversions.Capacity := DiversionCount;
  for DivIndex := 0 to DiversionCount - 1 do
  begin
    FDiversions.Add(DiversionFormulas[DivIndex])
  end;
  result := FDiversions;
end;

function TSfrMf6Item.GetEvaporation: string;
begin
  Result := FEvaporation.Formula;
  ResetItemObserver(EvaporationPosition);
end;

function TSfrMf6Item.GetInflow: string;
begin
  Result := FInflow.Formula;
  ResetItemObserver(InflowPosition);
end;

procedure TSfrMf6Item.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FInflow then
  begin
    List.Add(FObserverList[InflowPosition]);
  end;
  if Sender = FRainfall then
  begin
    List.Add(FObserverList[RainfallPosition]);
  end;
  if Sender = FEvaporation then
  begin
    List.Add(FObserverList[EvaporationPosition]);
  end;
  if Sender = FRunoff then
  begin
    List.Add(FObserverList[RunoffPosition]);
  end;
  if Sender = FUpstreamFraction then
  begin
    List.Add(FObserverList[UpstreamFractionPosition]);
  end;
  if Sender = FStage then
  begin
    List.Add(FObserverList[StagePosition]);
  end;
  if Sender = FRoughness then
  begin
    List.Add(FObserverList[RoughnessPosition]);
  end;
  if FDiversionFormulas.IndexOf(Sender as TFormulaObject) >= 0 then
  begin
    List.Add(FObserverList[DiversionStartPosition]);
  end;
end;

function TSfrMf6Item.GetRainfall: string;
begin
  Result := FRainfall.Formula;
  ResetItemObserver(RainfallPosition);
end;

function TSfrMf6Item.GetRoughness: string;
begin
  Result := FRoughness.Formula;
  ResetItemObserver(RoughnessPosition);
end;

function TSfrMf6Item.GetRunoff: string;
begin
  Result := FRunoff.Formula;
  ResetItemObserver(RunoffPosition);
end;

function TSfrMf6Item.GetStage: string;
begin
  Result := FStage.Formula;
  ResetItemObserver(StagePosition);
end;

function TSfrMf6Item.GetUpstreamFraction: string;
begin
  Result := FUpstreamFraction.Formula;
  ResetItemObserver(UpstreamFractionPosition);
end;

procedure TSfrMf6Item.InvalidateModel;
begin
  inherited;
  { TODO -cMODFLOW-6 : Invalidate displays here. }
end;

function TSfrMf6Item.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Item: TSfrMf6Item;
  Index: Integer;
begin
  result := (AnotherItem is TSfrMf6Item) and inherited IsSame(AnotherItem);
  if result then
  begin
    Item := TSfrMf6Item(AnotherItem);
    result := (Item.Inflow = Inflow)
      and (Item.Rainfall = Rainfall)
      and (Item.Evaporation = Evaporation)
      and (Item.Runoff = Runoff)
      and (Item.UpstreamFraction = UpstreamFraction)
      and (Item.Stage = Stage)
      and (Item.Roughness = Roughness)
      and (Item.StreamStatus = StreamStatus)
      and (Item.DiversionCount = DiversionCount);
    if result then
    begin
      for Index := 0 to DiversionCount - 1 do
      begin
        result := Item.Diversions[Index] = Diversions[Index];
        if not Result then
        begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TSfrMf6Item.Loaded;
var
  Index: Integer;
begin
  DiversionCount := FDiversions.Count;
  for Index := 0 to FDiversions.Count -1 do
  begin
    DiversionFormulas[Index] := FDiversions[Index];
  end;
end;

procedure TSfrMf6Item.RemoveFormulaObjects;
var
  DivIndex: Integer;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FInflow,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRainfall,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FEvaporation,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRunoff,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FUpstreamFraction,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStage,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

  for DivIndex := 0 to FDiversionFormulas.Count - 1 do
  begin
    frmGoPhast.PhastModel.FormulaManager.Remove(FDiversionFormulas[DivIndex],
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
  end;

end;

procedure TSfrMf6Item.SetBoundaryFormula(Index: integer; const Value: string);
begin
  case Index of
    InflowPosition:
      Inflow := Value;
    RainfallPosition:
      Rainfall := Value;
    EvaporationPosition:
      Evaporation := Value;
    RunoffPosition:
      Runoff := Value;
    UpstreamFractionPosition:
      UpstreamFraction := Value;
    StagePosition:
      Stage := Value;
    RoughnessPosition:
      Roughness := Value;

    else
      DiversionFormulas[Index-DiversionStartPosition] := Value;
  end;
end;

procedure TSfrMf6Item.SetDiversionCount(const Value: Integer);
var
  FormulaObj: TFormulaObject;
begin
  While Value > FDiversionFormulas.Count do
  begin
    FDiversionFormulas.Add(CreateFormulaObject(dso3D));
  end;
  UpdateBoundaryObservers;

  While Value < FDiversionFormulas.Count do
  begin
    FormulaObj := FDiversionFormulas[FDiversionFormulas.Count-1];
    UpdateFormulaBlocks('0', DiversionStartPosition +FDiversionFormulas.Count-1, FormulaObj);
    FDiversionFormulas[FDiversionFormulas.Count-1] := FormulaObj;
    frmGoPhast.PhastModel.FormulaManager.Remove(FormulaObj,
      GlobalRemoveModflowBoundaryItemSubscription,
      GlobalRestoreModflowBoundaryItemSubscription, self);
    FDiversionFormulas.Delete(FDiversionFormulas.Count-1);
  end;
end;

procedure TSfrMf6Item.SetDiversionFormula(Index: Integer; const Value: string);
var
  FormulaObject: TFormulaObject;
begin
  FormulaObject := FDiversionFormulas[Index];
  UpdateFormulaBlocks(Value, DiversionStartPosition+Index, FormulaObject);
  FDiversionFormulas[Index] := FormulaObject;
end;

procedure TSfrMf6Item.SetDiversions(const Value: TStrings);
begin
  UpdateBoundaryObservers;
  FDiversions.Assign(Value);
end;

procedure TSfrMf6Item.SetEvaporation(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FEvaporation.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, EvaporationPosition, FEvaporation);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Evaporation(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetInflow(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FInflow.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, InflowPosition, FInflow);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Inflow(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetRainfall(const Value: string);
begin
  UpdateFormulaBlocks(Value, RainfallPosition, FRainfall);
end;

procedure TSfrMf6Item.SetRoughness(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FRoughness.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, RoughnessPosition, FRoughness);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Roughness(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetRunoff(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FRunoff.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, RunoffPosition, FRunoff);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Runoff(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetStage(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStage.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, StagePosition, FStage);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6Stage(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.SetStatus(const Value: Boolean);
begin
  StreamStatus := TStreamStatus(Value);
end;

procedure TSfrMf6Item.SetStreamStatus(const Value: TStreamStatus);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FStreamStatus <> Value then
  begin
    FStreamStatus := Value;
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6StreamStatus(self);
      end;
    end;
    InvalidateModel;
  end;
end;

procedure TSfrMf6Item.SetUpstreamFraction(const Value: string);
var
  PhastModel: TPhastModel;
  ScreenObj: TObject;
begin
  if FUpstreamFraction.Formula <> Value then
  begin
    UpdateFormulaBlocks(Value, UpstreamFractionPosition, FUpstreamFraction);
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and not (csDestroying in PhastModel.ComponentState)
      and not PhastModel.Clearing then
    begin
      ScreenObj := ScreenObject;
      if (ScreenObj <> nil)
        and (ScreenObj as TScreenObject).CanInvalidateModel then
      begin
        PhastModel.InvalidateSfr6UpstreamFraction(self);
      end;
    end;
  end;
end;

procedure TSfrMf6Item.UpdateBoundaryObservers;
var
  Observer: TObserver;
  LocalScreenObject: TScreenObject;
begin
  if ScreenObject <> nil then
  begin
    while FObserverList.Count < BoundaryFormulaCount do
    begin
      Observer := TObserver.Create(nil);
      FObserverList.Add(Observer);
      LocalScreenObject := ScreenObject as TScreenObject;
      if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
      begin
        LocalScreenObject.TalksTo(Observer);
      end;
    end;
  end;
end;

{ TStrMf6Collection }

procedure TSfrMf6Collection.AddSpecificBoundary(AModel: TBaseModel);
begin
  AddBoundary(TSfrMf6Storage.Create(AModel));
end;

function TSfrMf6Collection.AdjustedFormula(FormulaIndex,
  ItemIndex: integer): string;
var
//  Boundary: TGhbBoundary;
//  ScreenObject: TScreenObject;
  Item: TSfrMf6Item;
begin
  Item := Items[ItemIndex] as TSfrMf6Item;
//  if FormulaIndex = ConductancePosition then
//  begin
//    Boundary := BoundaryGroup as TGhbBoundary;
//    ScreenObject := Boundary.ScreenObject as TScreenObject;
//    case Boundary.FormulaInterpretation of
//      fiSpecific:
//        begin
//          if ScreenObject.ScreenObjectLength = 0 then
//          begin
//            result := Item.Conductance;
//          end
//          else if ScreenObject.Closed then
//          begin
//            result := '(' + Item.Conductance
//              + ') * ' + StrObjectIntersectArea;
//          end
//          else
//          begin
//            result := '(' + Item.Conductance
//              + ') * ' + StrObjectSectionIntersectLength;
//          end;
//        end;
//      fiDirect:
//        begin
//          result := Item.Conductance;
//        end;
//      fiTotal:
//        begin
//          if ScreenObject.ScreenObjectLength = 0 then
//          begin
//            result := Item.Conductance;
//          end
//          else if ScreenObject.Closed then
//          begin
//            result := '((' + Item.Conductance
//              + ') * ' + StrObjectIntersectArea + ') / ' + StrObjectArea;
//          end
//          else
//          begin
//            result := '((' + Item.Conductance
//              + ') * ' + StrObjectSectionIntersectLength+ ') / ' + StrObjectLength;
//          end;
//        end;
//      else Assert(False);
//    end;
//  end
//  else
//  begin
    result := Item.BoundaryFormula[FormulaIndex];
//  end;
end;

procedure TSfrMf6Collection.AssignCellList(Expression: TExpression;
  ACellList: TObject; BoundaryStorage: TCustomBoundaryStorage;
  BoundaryFunctionIndex: integer; Variables, DataSets: TList;
  AModel: TBaseModel; AScreenObject: TObject; PestName: string);
var
  Sfr6Storage: TSfrMf6Storage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
  RequiredLength: Integer;
  FractionAnnotation: string;
begin
  Assert(Expression <> nil);

  Sfr6Storage := BoundaryStorage as TSfrMf6Storage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    UpdateCurrentScreenObject(AScreenObject as TScreenObject);
    UpdateRequiredListData(DataSets, Variables, ACell, AModel);
    FractionAnnotation := Format(StrAllButTheFirstRe, [(AScreenObject as TScreenObject).Name]);

    Expression.Evaluate;
    with Sfr6Storage.SfrMF6Array[Index] do
    begin
      case BoundaryFunctionIndex of
        InflowPosition:
          begin
            Inflow := Expression.DoubleResult;
            InflowAnnotation := ACell.Annotation;
            InflowPest := PestName;
          end;
        RainfallPosition:
          begin
            Rainfall := Expression.DoubleResult;
            RainfallAnnotation := ACell.Annotation;
            RainfallPest := PestName;
          end;
        EvaporationPosition:
          begin
            Evaporation := Expression.DoubleResult;
            EvaporationAnnotation := ACell.Annotation;
            EvaporationPest := PestName;
          end;
        RunoffPosition:
          begin
            Runoff := Expression.DoubleResult;
            RunoffAnnotation := ACell.Annotation;
            RunoffPest := PestName;
          end;
        UpstreamFractionPosition:
          begin
            if Index = 0 then
            begin
              UpstreamFraction := Expression.DoubleResult;
              UpstreamFractionAnnotation := ACell.Annotation;
              UpstreamFractionPest := PestName;
            end
            else
            begin
              UpstreamFraction := 1;
              UpstreamFractionAnnotation := FractionAnnotation;
              UpstreamFractionPest := PestName;
            end;
          end;
        StagePosition:
          begin
            Stage := Expression.DoubleResult;
            StageAnnotation := ACell.Annotation;
            StagePest := PestName;
          end;
        RoughnessPosition:
          begin
            Roughness := Expression.DoubleResult;
            RoughnessAnnotation := ACell.Annotation;
            RoughnessPest := PestName;
          end;
        else
          begin
            if Index = CellList.Count - 1 then
            begin
              RequiredLength := BoundaryFunctionIndex - DiversionStartPosition + 1;
              if Length(Diversions) <> RequiredLength then
              begin
                SetLength(Diversions, RequiredLength);
                SetLength(DiversionAnnotations, RequiredLength);
                SetLength(DiversionPests, RequiredLength);
              end;
              Diversions[BoundaryFunctionIndex - DiversionStartPosition]
                := Expression.DoubleResult;
              DiversionAnnotations[BoundaryFunctionIndex - DiversionStartPosition]
                := ACell.Annotation;
              DiversionPests[BoundaryFunctionIndex - DiversionStartPosition]
                := PestName;
            end;
          end;
      end;
    end;
  end;
end;

procedure TSfrMf6Collection.AssignDirectlySpecifiedValues(
  AnItem: TCustomModflowBoundaryItem; BoundaryStorage: TCustomBoundaryStorage);
var
  Sfr6Storage: TSfrMf6Storage;
  index: integer;
  SfrMf6Item: TSfrMf6Item;
begin
  inherited;
  Sfr6Storage := BoundaryStorage as TSfrMf6Storage;
  SfrMf6Item := AnItem as TSfrMf6Item;

  for index := 0 to Length(Sfr6Storage.FSfrMF6Array) - 1 do
  begin
    Sfr6Storage.FSfrMF6Array[index].Status := SfrMf6Item.StreamStatus;
    Sfr6Storage.FSfrMF6Array[index].ReachNumber := index+1;
    if (not SfrMf6Item.Status) and (index = 0) then
    begin
      Sfr6Storage.FSfrMF6Array[index].UpstreamFraction := 0;
      Sfr6Storage.FSfrMF6Array[index].UpstreamFractionAnnotation
        := StrUpstreamFractionIs
    end;
  end;
end;

procedure TSfrMf6Collection.AssignListCellLocation(
  BoundaryStorage: TCustomBoundaryStorage; ACellList: TObject);
var
  Sfr6Storage: TSfrMf6Storage;
  CellList: TCellAssignmentList;
  Index: Integer;
  ACell: TCellAssignment;
begin
  Sfr6Storage := BoundaryStorage as TSfrMf6Storage;
  CellList := ACellList as TCellAssignmentList;
  for Index := 0 to CellList.Count - 1 do
  begin
    ACell := CellList[Index];
    if ACell.LgrEdge then
    begin
      Continue;
    end;
    with Sfr6Storage.SfrMF6Array[Index] do
    begin
      Cell.Layer := ACell.Layer;
      Cell.Row := ACell.Row;
      Cell.Column := ACell.Column;
      Cell.Section := ACell.Section;
    end;
  end;
end;

constructor TSfrMf6Collection.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FSfrMf6Boundary := Boundary as TSfrMf6Boundary;
  SectionDuplicatesAllowed := True;
end;

function TSfrMf6Collection.GetTimeListLinkClass: TTimeListsModelLinkClass;
begin
  result := TSfrMf6TimeListLink;
end;

procedure TSfrMf6Collection.InvalidateDiversionsData(Sender: TObject);
//var
//  PhastModel: TPhastModel;
//  Link: TSfrMf6TimeListLink;
//  ChildIndex: Integer;
//  ChildModel: TChildModel;
begin
//  if not (Sender as TObserver).UpToDate then
//  begin
//    PhastModel := frmGoPhast.PhastModel;
//    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
//    Link.FUpstreamFraction.Invalidate;
//    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
//    begin
//      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
//      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
//      Link.FUpstreamFraction.Invalidate;
//    end;
//  end;
end;

procedure TSfrMf6Collection.InvalidateEvaporationData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FEvaporation.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FEvaporation.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateInflowData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FInflow.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FInflow.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRainfallData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FRainfall.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FRainfall.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRoughnessData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FRoughness.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FRoughness.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateRunoffData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FRunoff.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FRunoff.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateStageData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FStage.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FStage.Invalidate;
    end;
  end;
end;

procedure TSfrMf6Collection.InvalidateUpstreamFractionData(Sender: TObject);
var
  PhastModel: TPhastModel;
  Link: TSfrMf6TimeListLink;
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
    Link := TimeListLink.GetLink(PhastModel) as TSfrMf6TimeListLink;
    Link.FUpstreamFraction.Invalidate;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      Link := TimeListLink.GetLink(ChildModel) as TSfrMf6TimeListLink;
      Link.FUpstreamFraction.Invalidate;
    end;
  end;
end;

class function TSfrMf6Collection.ItemClass: TBoundaryItemClass;
begin
  result := TSfrMf6Item
end;

procedure TSfrMf6Collection.Loaded;
var
  index: Integer;
  Item: TSfrMf6Item;
begin
  for index := 0 to Count - 1 do
  begin
    Item := Items[index] as TSfrMf6Item;
    Item.Loaded;
  end;
end;

procedure TSfrMf6Collection.SetBoundaryStartAndEndTime(BoundaryCount: Integer;
  Item: TCustomModflowBoundaryItem; ItemIndex: Integer; AModel: TBaseModel);
begin
  SetLength((Boundaries[ItemIndex, AModel] as TSfrMf6Storage).FSfrMF6Array, BoundaryCount);
  inherited;
end;

{ TSDiversionItem }

procedure TSDiversionItem.Assign(Source: TPersistent);
var
  Diversion: TSDiversionItem;
  SfrParamIcalc: TSfrParamIcalcItem;
  StrItem: TStrItem;
begin
  if Source is TSDiversionItem then
  begin
    Diversion := TSDiversionItem(Source);
    Priority := Diversion.Priority;
    DownstreamSegment := Diversion.DownstreamSegment;
  end
  else if Source is TSfrParamIcalcItem then
  begin
    SfrParamIcalc := TSfrParamIcalcItem(Source);
    DownstreamSegment := SfrParamIcalc.SegmentNumber;
    case Abs(SfrParamIcalc.IPRIOR) of
      0:
        begin
          Priority := cpUpTo;
        end;
      1:
        begin
          Priority := cpThreshold;
        end;
      2:
        begin
          Priority := cpFraction;
        end;
      3:
        begin
          Priority := cpExcess;
        end;
    end;
  end
  else if Source is TStrItem then
  begin
    StrItem := TStrItem(Source);
    DownstreamSegment := StrItem.SegmentNumber;
    Priority := cpUpTo;
  end;


  inherited;
end;

function TSDiversionItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  Diversion: TSDiversionItem;
begin
  result := AnotherItem is TSDiversionItem;
  if result then
  begin
    Diversion := TSDiversionItem(AnotherItem);
    result := (Priority = Diversion.Priority)
      and (DownstreamSegment = Diversion.DownstreamSegment);
  end;
end;

procedure TSDiversionItem.SetPriority(const Value: TDivisionPriority);
begin
  if FPriority <> Value then
  begin
    FPriority := Value;
    InvalidateModel;
  end;
end;

procedure TSDiversionItem.SetDownstreamSegment(const Value: Integer);
begin
  if FDownstreamSegment <> Value then
  begin
    FDownstreamSegment := Value;
    InvalidateModel;
  end;
end;

{ TDiversionCollection }

function TDiversionCollection.Add: TSDiversionItem;
begin
  result := inherited Add as TSDiversionItem;
end;

constructor TDiversionCollection.Create(Model: TBaseModel);
begin
  inherited Create(TSDiversionItem, Model);
end;

function TDiversionCollection.GetItem(Index: Integer): TSDiversionItem;
begin
  result := inherited Items[Index] as TSDiversionItem
end;

function TDiversionCollection.IsSame(
  AnOrderedCollection: TOrderedCollection): boolean;
begin
  result := (AnOrderedCollection is TDiversionCollection)
    and inherited;
end;

procedure TDiversionCollection.SetItem(Index: Integer;
  const Value: TSDiversionItem);
begin
  inherited Items[Index] := Value;
end;

{ TStrMf6Boundary }

procedure TSfrMf6Boundary.Assign(Source: TPersistent);
var
  SourceSfr6: TSfrMf6Boundary;
  SourceSfrMf2005: TSfrBoundary;
  LocalModel: TPhastModel;
  DefineByReach: Boolean;
  SfrItem: TSfrItem;
  UpstreamSegment: TSfrSegmentItem;
  DownstreamSegment: TSfrSegmentItem;
  ChannelItem: TSfrChannelItem;
  ItemIndex: Integer;
  SfrMf6Item: TSfrMf6Item;
  FlowItem: TSfrSegmentFlowItem;
  ParamItem: TSfrParamIcalcItem;
//  DiversionItem: TSDiversionItem;
  SourceStr: TStrBoundary;
  StrItem: TStrItem;
//  DiversionSeg: TSDiversionItem;
begin
  if Source is TSfrMf6Boundary then
  begin
    SourceSfr6 := TSfrMf6Boundary(Source);
    if Used <> SourceSfr6.Used then
    begin
      if (ParentModel <> nil) and (ScreenObject <> nil)
        and (ScreenObject as TScreenObject).CanInvalidateModel  then
      begin
        InvalidateDisplayTimeLists;
      end;
    end;

    SegmentNumber := SourceSfr6.SegmentNumber;
    DownstreamSegments := SourceSfr6.DownstreamSegments;
    Diversions := SourceSfr6.Diversions;
    ReachLength := SourceSfr6.ReachLength;
    ReachWidth := SourceSfr6.ReachWidth;
    Gradient := SourceSfr6.Gradient;
    StreambedTop := SourceSfr6.StreambedTop;
    StreambedThickness := SourceSfr6.StreambedThickness;
    HydraulicConductivity := SourceSfr6.HydraulicConductivity;
//    SfrInflowLocation := SourceSfr6.SfrInflowLocation;
//    Roughness := SourceSfr6.Roughness;
//    UpstreamFraction := SourceSfr6.UpstreamFraction;
  end
  else if Source is TSfrBoundary then
  begin
    SourceSfrMf2005 := TSfrBoundary(Source);
    SegmentNumber := SourceSfrMf2005.SegmentNumber;
    LocalModel := frmGoPhast.PhastModel;
    DefineByReach := LocalModel.ModflowPackages.SfrPackage.ISFROPT in [1,2,3];

    SfrItem := SourceSfrMf2005.Values[0] as TSfrItem;
    ReachLength := SfrItem.ReachLength;

    UpstreamSegment := SourceSfrMf2005.UpstreamSegmentValues[0] as TSfrSegmentItem;
    DownstreamSegment := SourceSfrMf2005.DownstreamSegmentValues[0] as TSfrSegmentItem;
    if UpstreamSegment.StreamWidth = DownstreamSegment.StreamWidth then
    begin
      ReachWidth := UpstreamSegment.StreamWidth
    end
    else
    begin
      ReachWidth := 'Interpolate(FractionOfObjectLength, '
        + UpstreamSegment.StreamWidth
        + ', 0, '
        + DownstreamSegment.StreamWidth
        + ', 1)'
    end;


    if DefineByReach then
    begin
      Gradient := SfrItem.StreamSlope;
      StreambedTop := SfrItem.StreambedElevation;
      StreambedThickness := SfrItem.StreamBedThickness;
      HydraulicConductivity := SfrItem.HydraulicConductivity;
    end
    else
    begin
      Gradient := '((' + UpstreamSegment.StreambedElevation
        + ') - (' + DownstreamSegment.StreambedElevation
        + '))/'
        + StrObjectLength;
        
      if UpstreamSegment.StreambedElevation <> DownstreamSegment.StreambedElevation then
      begin
        StreambedTop := 'Interpolate(FractionOfObjectLength, ('
          + UpstreamSegment.StreambedElevation
          + '), 0, ('
          + DownstreamSegment.StreambedElevation
          + '), 1)';
      end
      else
      begin
        StreambedTop := UpstreamSegment.StreambedElevation;
      end; 
                                       
      if UpstreamSegment.StreambedThickness <> DownstreamSegment.StreambedThickness then      
      begin      
        StreambedThickness := 'Interpolate(FractionOfObjectLength, ('
          + UpstreamSegment.StreambedThickness
          + '), 0, ('
          + DownstreamSegment.StreambedThickness
          + '), 1)';
      end          
      else      
      begin      
        StreambedThickness := UpstreamSegment.StreambedThickness;
      end;    
              
      if UpstreamSegment.HydraulicConductivity <> DownstreamSegment.HydraulicConductivity then      
      begin      
        HydraulicConductivity := 'Interpolate(FractionOfObjectLength, ('
          + UpstreamSegment.HydraulicConductivity
          + '), 0, ('
          + DownstreamSegment.HydraulicConductivity
          + '), 1)';
      end          
      else      
      begin      
        HydraulicConductivity := UpstreamSegment.HydraulicConductivity;
      end;            
    end;

//    ChannelItem := SourceSfrMf2005.ChannelValues[0];
//    Roughness := ChannelItem.ChannelRoughness;

//    UpstreamFraction := '1';

    ParamItem := SourceSfrMf2005.ParamIcalc[0] as TSfrParamIcalcItem;
    DownstreamSegments.Clear;
    if ParamItem.OutflowSegment <> 0 then
    begin
      DownstreamSegments.Add.Value := ParamItem.OutflowSegment;
    end;

//    Diversions.Clear;
//    if ParamItem.DiversionSegment <> 0 then
//    begin
//      DiversionItem := Diversions.Add;
//      DiversionItem.DownstreamSegment := ParamItem.DiversionSegment;
//      case Abs(ParamItem.IPRIOR) of
//        0:
//          begin
//            DiversionItem.Priority := cpUpTo;
//          end;
//        1:
//          begin
//            DiversionItem.Priority := cpThreshold;
//          end;
//        2:
//          begin
//            DiversionItem.Priority := cpFraction;
//          end;
//        3:
//          begin
//            DiversionItem.Priority := cpExcess;
//          end;
//      end;
//    end;

    Values.Assign(SourceSfrMf2005.ParamIcalc);
    Assert(Values.Count = SourceSfrMf2005.UpstreamSegmentValues.Count);
    Assert(Values.Count = SourceSfrMf2005.SegmentFlows.Count);
    Assert(Values.Count = SourceSfrMf2005.ChannelValues.Count);

    for ItemIndex := 0 to Values.Count - 1 do
    begin
      SfrMf6Item := Values[ItemIndex] as TSfrMf6Item;

      UpstreamSegment := SourceSfrMf2005.UpstreamSegmentValues[ItemIndex] as TSfrSegmentItem;
      DownstreamSegment := SourceSfrMf2005.DownstreamSegmentValues[ItemIndex] as TSfrSegmentItem;
      if (UpstreamSegment.StreambedElevation = DownstreamSegment.StreambedElevation)
        and (UpstreamSegment.StreamDepth = DownstreamSegment.StreamDepth)
        then
      begin
        SfrMf6Item.Stage := '(' + UpstreamSegment.StreambedElevation + ') + ('
          + UpstreamSegment.StreamDepth + ')';
      end
      else
      begin
        SfrMf6Item.Stage := 'Interpolate(FractionOfObjectLength, ('
          + '(' + UpstreamSegment.StreambedElevation + ') + ('
            + UpstreamSegment.StreamDepth + ')'
          + '), 0, ('
          + '(' + DownstreamSegment.StreambedElevation + ') + ('
            + DownstreamSegment.StreamDepth + ')'
          + '), 1)';
      end;

      FlowItem := SourceSfrMf2005.SegmentFlows[ItemIndex] as TSfrSegmentFlowItem;
      if Diversions.Count = 0 then
      begin
        SfrMf6Item.Inflow := FlowItem.Flow;
      end
      else
      begin
        SfrMf6Item.Inflow := '0';
        SfrMf6Item.DiversionCount := 1;
        SfrMf6Item.DiversionFormulas[0] := FlowItem.Flow;
      end;
      SfrMf6Item.Rainfall := FlowItem.Precipitation;
      SfrMf6Item.Evaporation := FlowItem.Evapotranspiration;
      SfrMf6Item.Runoff := FlowItem.Runnoff;
      SfrMf6Item.Rainfall := FlowItem.Precipitation;
      SfrMf6Item.UpstreamFraction := '1';


      ChannelItem := SourceSfrMf2005.ChannelValues[ItemIndex];
      SfrMf6Item.Roughness := ChannelItem.ChannelRoughness;
    end;

    Exit;
  end
  else if Source is TStrBoundary then
  begin
    SourceStr := TStrBoundary(Source);
    SegmentNumber := SourceStr.SegmentNumber;
    ReachLength := '1';
    ReachWidth := '1';
    StreambedThickness := '1';
    if SourceStr.Values.Count > 0 then
    begin
      StrItem := SourceStr.Values[0] as TStrItem;
      Gradient := StrItem.Slope;
      StreambedTop := StrItem.BedTop;
//      Roughness := StrItem.Roughness;
      HydraulicConductivity := StrItem.Conductance;
//      UpstreamFraction := '1';
      if StrItem.OutflowSegment > 0 then
      begin
        DownstreamSegments.Add.Value := StrItem.OutflowSegment
      end;
	  
//      if StrItem.DiversionSegment > 0 then
//      begin
//        DiversionSeg := Diversions.Add;
//        DiversionSeg.DownstreamSegment := StrItem.DiversionSegment;
//        DiversionSeg.Priority := cpUpTo;
//      end;

    end;
    Values := SourceStr.Values;

    Exit;
  end;
  inherited;
end;

procedure TSfrMf6Boundary.AssignCells(BoundaryStorage: TCustomBoundaryStorage;
  ValueTimeList: TList; AModel: TBaseModel);
var
  Cell: TSfrMF6_Cell;
  BoundaryValues: TSfrMF6Record;
  BoundaryIndex: Integer;
  StressPeriod: TModflowStressPeriod;
  TimeIndex: Integer;
  Cells: TValueCellList;
  LocalBoundaryStorage: TSfrMF6Storage;
  LocalModel: TCustomModel;
  MvrUsed: Boolean;
  LastIndex: Integer;
  LocalScreenObject: TScreenObject;
  InflowAnnotation: string;
begin
  LocalModel := AModel as TCustomModel;
  LocalBoundaryStorage := BoundaryStorage as TSfrMF6Storage;
  Assert(ScreenObject <> nil);
  LocalScreenObject := ScreenObject as TScreenObject;
  InflowAnnotation := Format(StrInflowIsOnlySetT, [LocalScreenObject.Name]);
  MvrUsed := (LocalScreenObject.ModflowMvr <> nil)
    and LocalScreenObject.ModflowMvr.Used
    and (LocalScreenObject.ModflowMvr.SourcePackageChoice = spcSfr);
  for TimeIndex := 0 to
    LocalModel.ModflowFullStressPeriods.Count - 1 do
  begin
    if TimeIndex < ValueTimeList.Count then
    begin
      Cells := ValueTimeList[TimeIndex];
    end
    else
    begin
      Cells := TValueCellList.Create(TSfrMF6_Cell);
      ValueTimeList.Add(Cells);
    end;
    StressPeriod := LocalModel.ModflowFullStressPeriods[TimeIndex];
    // Check if the stress period is completely enclosed within the times
    // of the LocalBoundaryStorage;
    if (StressPeriod.StartTime + LocalModel.SP_Epsilon >= LocalBoundaryStorage.StartingTime)
      and (StressPeriod.EndTime - LocalModel.SP_Epsilon <= LocalBoundaryStorage.EndingTime) then
    begin
      if Cells.Capacity < Cells.Count + Length(LocalBoundaryStorage.SfrMF6Array) then
      begin
        Cells.Capacity := Cells.Count + Length(LocalBoundaryStorage.SfrMF6Array)
      end;
      LastIndex := Length(LocalBoundaryStorage.SfrMF6Array) - 1;
//      Cells.CheckRestore;
      for BoundaryIndex := 0 to Length(LocalBoundaryStorage.SfrMF6Array) - 1 do
      begin
        BoundaryValues := LocalBoundaryStorage.SfrMF6Array[BoundaryIndex];
        BoundaryValues.MvrUsed := MvrUsed and (BoundaryIndex = LastIndex);
        BoundaryValues.MvrIndex := BoundaryIndex;
        if (BoundaryIndex > 0) then
        begin
          BoundaryValues.Inflow := 0;
          BoundaryValues.InflowAnnotation := InflowAnnotation;
        end;
        Cell := TSfrMF6_Cell.Create;
        Assert(ScreenObject <> nil);
        Cell.IFace := (ScreenObject as TScreenObject).IFace;
        Cells.Add(Cell);
        Cell.StressPeriod := TimeIndex;
        Cell.FValues := BoundaryValues;
        SetLength(Cell.FValues.Diversions, Length(Cell.FValues.Diversions));
        SetLength(Cell.FValues.DiversionAnnotations, Length(Cell.FValues.DiversionAnnotations));
        Cell.ScreenObject := ScreenObject;
        LocalModel.AdjustCellPosition(Cell);
      end;
      Cells.Cache;
    end;
  end;
  LocalBoundaryStorage.CacheData;
end;

class function TSfrMf6Boundary.BoundaryCollectionClass: TMF_BoundCollClass;
begin
  result := TSfrMF6Collection;
end;

function TSfrMf6Boundary.BoundaryObserverPrefix: string;
begin
  result := 'Sfr6';
end;

constructor TSfrMf6Boundary.Create(Model: TBaseModel; ScreenObject: TObject);
var
  InvalidateEvent: TNotifyEvent;
begin
  inherited;
  if Model = nil then
  begin
    InvalidateEvent := nil;
  end
  else
  begin
    InvalidateEvent := Model.Invalidate;
  end;
  FDownstreamSegments := TIntegerCollection.Create(InvalidateEvent);
  FDiversions := TDiversionCollection.Create(Model);
  CreateFormulaObjects;
  CreateBoundaryObserver;
  CreateObservers;

  LinkReachLength;
  LinkReachWidth;
  LinkGradient;
  LinkStreambedTop;
  LinkStreambedThickness;
  LinkHydraulicConductivity;

  ReachLength := StrObjectIntersectLength;
  ReachWidth := '1';
  Gradient := '0.001';
  StreambedTop := '0';
  StreambedThickness := '1';
  HydraulicConductivity := '0';
//  FSfrInflowLocation := silAllCells;
//  Roughness := '3.5E-7';
//  UpstreamFraction := '1';

end;

procedure TSfrMf6Boundary.CreateFormulaObjects;
begin
  FReachLength := CreateFormulaObjectBlocks(dso3D);
  FReachWidth := CreateFormulaObjectBlocks(dso3D);
  FGradient := CreateFormulaObjectBlocks(dso3D);
  FStreambedTop := CreateFormulaObjectBlocks(dso3D);
  FStreambedThickness := CreateFormulaObjectBlocks(dso3D);
  FHydraulicConductivity := CreateFormulaObjectBlocks(dso3D);
end;

procedure TSfrMf6Boundary.CreateObservers;
begin
  if ScreenObject <> nil then
  begin
    FObserverList.Add(ReachLengthObserver);
    FObserverList.Add(ReachWidthObserver);
    FObserverList.Add(GradientObserver);
    FObserverList.Add(StreambedTopObserver);
    FObserverList.Add(StreambedThicknessObserver);
    FObserverList.Add(HydraulicConductivityObserver);
//    FObserverList.Add(RoughnessObserver);
//    FObserverList.Add(UpstreamFractionObserver);
  end;
end;

destructor TSfrMf6Boundary.Destroy;
begin
  FDiversions.Free;
  FDownstreamSegments.Free;
  RemoveFormulaObjects;
  inherited;
end;

procedure TSfrMf6Boundary.GetCellValues(ValueTimeList: TList;
  ParamList: TStringList; AModel: TBaseModel);
var
  ValueIndex: Integer;
  BoundaryStorage: TSfrMf6Storage;
begin
//  inherited;
  EvaluateListBoundaries(AModel);
  for ValueIndex := 0 to Values.Count - 1 do
  begin
//    Item := Values[ValueIndex] as TCustomModflowBoundaryItem;
    BoundaryStorage := Values.Boundaries[ValueIndex, AModel] as TSfrMf6Storage;
    AssignCells(BoundaryStorage, ValueTimeList, AModel);
  end;
end;

function TSfrMf6Boundary.GetGradient: string;
begin
  Result := FGradient.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(GradientPosition);
  end;
end;

function TSfrMf6Boundary.GetGradientObserver: TObserver;
begin
  if FGradientObserver = nil then
  begin
    CreateObserver('SFR6_Gradient_', FGradientObserver, nil);
  end;
  result := FGradientObserver;
end;

function TSfrMf6Boundary.GetHydraulicConductivity: string;
begin
  Result := FHydraulicConductivity.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(HydraulicConductivityPosition);
  end;
end;

function TSfrMf6Boundary.GetHydraulicConductivityObserver: TObserver;
begin
  if FHydraulicConductivityObserver = nil then
  begin
    CreateObserver('SFR6_HydraulicConductivity_', FHydraulicConductivityObserver, nil);
  end;
  result := FHydraulicConductivityObserver;
end;

procedure TSfrMf6Boundary.GetPropertyObserver(Sender: TObject; List: TList);
begin
  if Sender = FReachLength then
  begin
    List.Add(FObserverList[ReachLengthPosition]);
  end;
  if Sender = FReachWidth then
  begin
    List.Add(FObserverList[ReachWidthPosition]);
  end;
  if Sender = FGradient then
  begin
    List.Add(FObserverList[GradientPosition]);
  end;
  if Sender = FStreambedTop then
  begin
    List.Add(FObserverList[StreambedTopPosition]);
  end;
  if Sender = FStreambedThickness then
  begin
    List.Add(FObserverList[StreambedThicknessPosition]);
  end;
  if Sender = FHydraulicConductivity then
  begin
    List.Add(FObserverList[HydraulicConductivityPosition]);
  end;
end;

function TSfrMf6Boundary.GetReachLength: string;
begin
  Result := FReachLength.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ReachLengthPosition);
  end;
end;

function TSfrMf6Boundary.GetReachLengthObserver: TObserver;
begin
  if FReachLengthObserver = nil then
  begin
    CreateObserver('SFR6_ReachLength_', FReachLengthObserver, nil);
  end;
  result := FReachLengthObserver;
end;

function TSfrMf6Boundary.GetReachWidth: string;
begin
  Result := FReachWidth.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(ReachWidthPosition);
  end;
end;

function TSfrMf6Boundary.GetReachWidthObserver: TObserver;
begin
  if FReachWidthObserver = nil then
  begin
    CreateObserver('SFR6_ReachWidth_', FReachWidthObserver, nil);
  end;
  result := FReachWidthObserver;
end;

//function TSfrMf6Boundary.GetRoughness: string;
//begin
//  Result := FRoughness.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetItemObserver(RoughnessPosition);
//  end;
//end;

//function TSfrMf6Boundary.GetRoughnessObserver: TObserver;
//begin
//  if FRoughnessObserver = nil then
//  begin
//    CreateObserver('SFR6_Roughness_', FRoughnessObserver, nil);
//  end;
//  result := FRoughnessObserver;
//end;

function TSfrMf6Boundary.GetStreambedThickness: string;
begin
  Result := FStreambedThickness.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreambedThicknessPosition);
  end;
end;

function TSfrMf6Boundary.GetStreambedThicknessObserver: TObserver;
begin
  if FStreambedThicknessObserver = nil then
  begin
    CreateObserver('SFR6_StreambedThickness_', FStreambedThicknessObserver, nil);
  end;
  result := FStreambedThicknessObserver;
end;

function TSfrMf6Boundary.GetStreambedTop: string;
begin
  Result := FStreambedTop.Formula;
  if ScreenObject <> nil then
  begin
    ResetItemObserver(StreambedTopPosition);
  end;
end;

function TSfrMf6Boundary.GetStreambedTopObserver: TObserver;
begin
  if FStreambedTopObserver = nil then
  begin
    CreateObserver('SFR6_StreambedTop_', FStreambedTopObserver, nil);
  end;
  result := FStreambedTopObserver;
end;

//function TSfrMf6Boundary.GetUpstreamFraction: string;
//begin
//  Result := FUpstreamFraction.Formula;
//  if ScreenObject <> nil then
//  begin
//    ResetItemObserver(SteadyUpstreamFractionPosition);
//  end;
//end;

//function TSfrMf6Boundary.GetUpstreamFractionObserver: TObserver;
//begin
//  if FUpstreamFractionObserver = nil then
//  begin
//    CreateObserver('SFR6_UpstreamFraction_', FUpstreamFractionObserver, nil);
//  end;
//  result := FUpstreamFractionObserver;
//end;

procedure TSfrMf6Boundary.InvalidateDisplay;
begin
  inherited;
  if Used and (ParentModel <> nil) then
  begin
    InvalidateDisplayTimeLists;
  end;
end;

procedure TSfrMf6Boundary.InvalidateDisplayTimeLists;
var
  LocaModel: TPhastModel;
begin
  LocaModel := ParentModel as TPhastModel;
  if LocaModel.Clearing then
  begin
    Exit;
  end;
  LocaModel.InvalidateSfr6Inflow(self);
  LocaModel.InvalidateSfr6Rainfall(self);
  LocaModel.InvalidateSfr6Evaporation(self);
  LocaModel.InvalidateSfr6Runoff(self);
  LocaModel.InvalidateSfr6UpstreamFraction(self);
  LocaModel.InvalidateSfr6Stage(self);
  LocaModel.InvalidateSfr6Roughness(self);
  LocaModel.InvalidateSfr6StreamStatus(self);
  LocaModel.InvalidateSfr6ReachNumber(self);
end;

procedure TSfrMf6Boundary.LinkGradient;
var
  LocalScreenObject: TScreenObject;
  Sfr6GradientArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(GradientObserver);
    if ParentModel <> nil then
    begin
      Sfr6GradientArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KGradientSFR6);
      if Sfr6GradientArray <> nil then
      begin
        GradientObserver.TalksTo(Sfr6GradientArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkHydraulicConductivity;
var
  LocalScreenObject: TScreenObject;
  Sfr6HydraulicConductivityArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(HydraulicConductivityObserver);
    if ParentModel <> nil then
    begin
      Sfr6HydraulicConductivityArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KHydraulicConductivitySFR6);
      if Sfr6HydraulicConductivityArray <> nil then
      begin
        HydraulicConductivityObserver.TalksTo(Sfr6HydraulicConductivityArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkReachLength;
var
  LocalScreenObject: TScreenObject;
  Sfr6ReachLengthArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(ReachLengthObserver);
    if ParentModel <> nil then
    begin
      Sfr6ReachLengthArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KReachLengthSFR);
      if Sfr6ReachLengthArray <> nil then
      begin
        ReachLengthObserver.TalksTo(Sfr6ReachLengthArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkReachWidth;
var
  LocalScreenObject: TScreenObject;
  Sfr6ReachWidthArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(ReachWidthObserver);
    if ParentModel <> nil then
    begin
      Sfr6ReachWidthArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KReachWidthSFR6);
      if Sfr6ReachWidthArray <> nil then
      begin
        ReachWidthObserver.TalksTo(Sfr6ReachWidthArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkStreambedThickness;
var
  LocalScreenObject: TScreenObject;
  Sfr6StreambedThicknessArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(StreambedThicknessObserver);
    if ParentModel <> nil then
    begin
      Sfr6StreambedThicknessArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KStreambedThicknessSFR6);
      if Sfr6StreambedThicknessArray <> nil then
      begin
        StreambedThicknessObserver.TalksTo(Sfr6StreambedThicknessArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.LinkStreambedTop;
var
  LocalScreenObject: TScreenObject;
  Sfr6StreambedTopArray: TDataArray;
begin
  LocalScreenObject := ScreenObject as TScreenObject;
  if (LocalScreenObject <> nil) and LocalScreenObject.CanInvalidateModel then
  begin
    LocalScreenObject.TalksTo(StreambedTopObserver);
    if ParentModel <> nil then
    begin
      Sfr6StreambedTopArray := (ParentModel as TCustomModel).DataArrayManager.
        GetDataSetByName(KStreambedTopSFR6);
      if Sfr6StreambedTopArray <> nil then
      begin
        StreambedTopObserver.TalksTo(Sfr6StreambedTopArray);
      end;
    end;
  end;
end;

procedure TSfrMf6Boundary.Loaded;
begin
  (Values as TSfrMf6Collection).Loaded;

  LinkReachLength;
  LinkReachWidth;
  LinkGradient;
  LinkStreambedTop;
  LinkStreambedThickness;
  LinkHydraulicConductivity;
end;

procedure TSfrMf6Boundary.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FReachLength,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FGradient,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FHydraulicConductivity,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FReachWidth,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
//  frmGoPhast.PhastModel.FormulaManager.Remove(FRoughness,
//    GlobalRemoveMFBoundarySubscription,
//    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedThickness,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FStreambedTop,
    GlobalRemoveMFBoundarySubscription,
    GlobalRestoreMFBoundarySubscription, self);
//  frmGoPhast.PhastModel.FormulaManager.Remove(FUpstreamFraction,
//    GlobalRemoveMFBoundarySubscription,
//    GlobalRestoreMFBoundarySubscription, self);

end;

procedure TSfrMf6Boundary.SetDiversions(const Value: TDiversionCollection);
begin
  FDiversions.Assign(Value);
end;

procedure TSfrMf6Boundary.SetDownstreamSegments(
  const Value: TIntegerCollection);
begin
  FDownstreamSegments.Assign(Value);
end;

procedure TSfrMf6Boundary.SetGradient(const Value: string);
begin
  UpdateFormulaBlocks(Value, GradientPosition, FGradient);
end;


procedure TSfrMf6Boundary.SetHydraulicConductivity(const Value: string);
begin
  UpdateFormulaBlocks(Value, HydraulicConductivityPosition, FHydraulicConductivity);
end;


procedure TSfrMf6Boundary.SetReachLength(const Value: string);
begin
  UpdateFormulaBlocks(Value, ReachLengthPosition, FReachLength);
end;

procedure TSfrMf6Boundary.SetReachWidth(const Value: string);
begin
  UpdateFormulaBlocks(Value, ReachWidthPosition, FReachWidth);
end;

//procedure TSfrMf6Boundary.SetRoughness(const Value: string);
//begin
//  UpdateFormula(Value, SteadyRoughnessPosition, FRoughness);
//end;
//

procedure TSfrMf6Boundary.SetSegmentNumber(const Value: Integer);
begin
  if FSegmentNumber <> Value then
  begin
    FSegmentNumber := Value;
    InvalidateModel;
  end;
end;

//procedure TSfrMf6Boundary.SetSfrInflowLocation(const Value: TSfrInflowLocation);
//begin
//  if FSfrInflowLocation <> Value then
//  begin
//    FSfrInflowLocation := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSfrMf6Boundary.SetStreambedThickness(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreambedThicknessPosition, FStreambedThickness);
end;

procedure TSfrMf6Boundary.SetStreambedTop(const Value: string);
begin
  UpdateFormulaBlocks(Value, StreambedTopPosition, FStreambedTop);
end;

//procedure TSfrMf6Boundary.SetUpstreamFraction(const Value: string);
//begin
//  UpdateFormula(Value, SteadyUpstreamFractionPosition, FUpstreamFraction);
//end;

{ TDivRecord }

procedure TDivRecord.Cache(Comp: TCompressionStream; Strings: TStringList);
begin

end;

procedure TDivRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin

end;

{ TStrMf6_Cell }

procedure TSfrMf6_Cell.Cache(Comp: TCompressionStream; Strings: TStringList);
begin
  WriteCompInt(Comp, StressPeriod);
  FValues.Cache(Comp, Strings);
end;

function TSfrMf6_Cell.GetColumn: integer;
begin
  result := FValues.Cell.Column;
end;

function TSfrMf6_Cell.GetIntegerAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  Assert(False);
end;

function TSfrMf6_Cell.GetIntegerValue(Index: integer;
  AModel: TBaseModel): integer;
begin
  result := 0;
  Assert(False);
end;

function TSfrMf6_Cell.GetLayer: integer;
begin
  result := FValues.Cell.Layer;
end;

function TSfrMf6_Cell.GetMvrIndex: Integer;
begin
  result := Values.MvrIndex;
end;

function TSfrMf6_Cell.GetMvrUsed: Boolean;
begin
  result := Values.MvrUsed;
end;

function TSfrMf6_Cell.GetRealAnnotation(Index: integer;
  AModel: TBaseModel): string;
begin
  result := '';
  case Index of
    InflowPosition: result := FValues.InflowAnnotation;
    RainfallPosition: result := FValues.RainfallAnnotation;
    EvaporationPosition: result := FValues.EvaporationAnnotation;
    RunoffPosition: result := FValues.RunoffAnnotation;
    UpstreamFractionPosition: result := FValues.UpstreamFractionAnnotation;
    StagePosition: result := FValues.StageAnnotation;
    RoughnessPosition: result := FValues.RoughnessAnnotation;
    else
      begin
        result := FValues.DiversionAnnotations[Index-DiversionStartPosition];
      end;
  end;
end;

function TSfrMf6_Cell.GetRealValue(Index: integer; AModel: TBaseModel): double;
begin
//  result := 0;
  case Index of
    InflowPosition: result := FValues.Inflow;
    RainfallPosition: result := FValues.Rainfall;
    EvaporationPosition: result := FValues.Evaporation;
    RunoffPosition: result := FValues.Runoff;
    UpstreamFractionPosition: result := FValues.UpstreamFraction;
    StagePosition: result := FValues.Stage;
    RoughnessPosition: result := FValues.Roughness;
    else
      begin
        result := FValues.Diversions[Index-DiversionStartPosition];
      end;
  end;
end;

function TSfrMf6_Cell.GetRow: integer;
begin
  result := FValues.Cell.Row;
end;

function TSfrMf6_Cell.GetSection: integer;
begin
  result := FValues.Cell.Section;
end;

procedure TSfrMf6_Cell.RecordStrings(Strings: TStringList);
begin
  FValues.RecordStrings(Strings);
end;

procedure TSfrMf6_Cell.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
begin
 StressPeriod := ReadCompInt(Decomp);
 FValues.Restore(Decomp, Annotations);
end;

procedure TSfrMf6_Cell.SetColumn(const Value: integer);
begin
  FValues.Cell.Column := Value;
end;

procedure TSfrMf6_Cell.SetLayer(const Value: integer);
begin
  FValues.Cell.Layer := Value;
end;

procedure TSfrMf6_Cell.SetRow(const Value: integer);
begin
  FValues.Cell.Row := Value;
end;

{ TStrMF6ConstantRecord }

procedure TSfrMF6ConstantRecord.Cache(Comp: TCompressionStream;
  Strings: TStringList);
var
  index: Integer;
begin
  WriteCompCell(Comp, Cell);
  WriteCompInt(Comp, ReachNumber);
  WriteCompReal(Comp, ReachLength);
  WriteCompReal(Comp, ReachWidth);
  WriteCompReal(Comp, Gradient);
  WriteCompReal(Comp, StreambedTop);
  WriteCompReal(Comp, StreambedThickness);
  WriteCompReal(Comp, HydraulicConductivity);
//  WriteCompReal(Comp, Roughness);

  WriteCompInt(Comp, Length(ConnectedReaches));
  for index := 0 to Length(ConnectedReaches) - 1 do
  begin
    WriteCompInt(Comp, ConnectedReaches[index]);
  end;
  WriteCompInt(Comp, Length(DownstreamDiversions));
  for index := 0 to Length(DownstreamDiversions) - 1 do
  begin
    DownstreamDiversions[index].Cache(Comp, Strings);
  end;

  WriteCompInt(Comp, Strings.IndexOf(ReachLengthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(ReachWidthAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(GradientAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreambedTopAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(StreambedThicknessAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(HydraulicConductivityAnnotation));
  WriteCompInt(Comp, Strings.IndexOf(BoundName));
//  WriteCompInt(Comp, Strings.IndexOf(RoughnessAnnotation));
  WriteCompInt(Comp, Length(ConnectedReacheAnnotations));
  for index := 0 to Length(ConnectedReacheAnnotations) - 1 do
  begin
    WriteCompInt(Comp, Strings.IndexOf(ConnectedReacheAnnotations[index]));
  end;
end;

function TSfrMF6ConstantRecord.GetBoundaryAnnotation(Index: Integer): string;
begin
  case Index of
    ReachLengthPosition:
      begin
        result := ReachLengthAnnotation;
      end;
    ReachWidthPosition:
      begin
        result := ReachWidthAnnotation;
      end;
    GradientPosition:
      begin
        result := GradientAnnotation;
      end;
    StreambedTopPosition:
      begin
        result := StreambedTopAnnotation;
      end;
    StreambedThicknessPosition:
      begin
        result := StreambedThicknessAnnotation;
      end;
    HydraulicConductivityPosition:
      begin
        result := HydraulicConductivityAnnotation;
      end;
//    SteadyRoughnessPosition:
//      begin
//        result := RoughnessAnnotation;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        result := UpstreamFractionAnnotation;
//      end;
    else
      Assert(False);
  end
end;

function TSfrMF6ConstantRecord.GetBoundaryValue(Index: Integer): double;
begin
  result := 0;
  case Index of
    ReachLengthPosition:
      begin
        result := ReachLength;
      end;
    ReachWidthPosition:
      begin
        result := ReachWidth;
      end;
    GradientPosition:
      begin
        result := Gradient;
      end;
    StreambedTopPosition:
      begin
        result := StreambedTop;
      end;
    StreambedThicknessPosition:
      begin
        result := StreambedThickness;
      end;
    HydraulicConductivityPosition:
      begin
        result := HydraulicConductivity;
      end;
//    SteadyRoughnessPosition:
//      begin
//        result := Roughness;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        result := UpstreamFraction;
//      end;
    else
      Assert(False);
  end;
end;

function TSfrMF6ConstantRecord.IsConnected(Value: Integer): boolean;
var
  ConnectIndex: Integer;
begin
  result := False;
  for ConnectIndex := 0 to Length(ConnectedReaches) - 1 do
  begin
    if ConnectedReaches[ConnectIndex] = Value then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TSfrMF6ConstantRecord.RecordStrings(Strings: TStringList);
var
  index: Integer;
begin
  Strings.Add(ReachLengthAnnotation);
  Strings.Add(ReachWidthAnnotation);
  Strings.Add(GradientAnnotation);
  Strings.Add(StreambedTopAnnotation);
  Strings.Add(StreambedThicknessAnnotation);
  Strings.Add(HydraulicConductivityAnnotation);
  Strings.Add(BoundName);
//  Strings.Add(RoughnessAnnotation);
  for index := 0 to Length(ConnectedReacheAnnotations) - 1 do
  begin
    Strings.Add(ConnectedReacheAnnotations[index]);
  end;
end;

procedure TSfrMF6ConstantRecord.Restore(Decomp: TDecompressionStream;
  Annotations: TStringList);
var
  Count: Integer;
  index: Integer;
begin
  Cell := ReadCompCell(Decomp);
  ReachNumber := ReadCompInt(Decomp);
  ReachLength := ReadCompReal(Decomp);
  ReachWidth := ReadCompReal(Decomp);
  Gradient := ReadCompReal(Decomp);
  StreambedTop := ReadCompReal(Decomp);
  StreambedThickness := ReadCompReal(Decomp);
  HydraulicConductivity := ReadCompReal(Decomp);
//  Roughness := ReadCompReal(Decomp);

  StreambedTop := ReadCompReal(Decomp);

  Count := ReadCompInt(Decomp);
  SetLength(ConnectedReaches, Count);
  for index := 0 to Count - 1 do
  begin
    ConnectedReaches[index] := ReadCompInt(Decomp);
  end;

  Count := ReadCompInt(Decomp);
  SetLength(DownstreamDiversions, Count);
  for index := 0 to Count - 1 do
  begin
    DownstreamDiversions[index].Restore(Decomp, Annotations);
  end;

  ReachLengthAnnotation := Annotations[ReadCompInt(Decomp)];
  ReachWidthAnnotation := Annotations[ReadCompInt(Decomp)];
  GradientAnnotation := Annotations[ReadCompInt(Decomp)];
  StreambedTopAnnotation := Annotations[ReadCompInt(Decomp)];
  StreambedThicknessAnnotation := Annotations[ReadCompInt(Decomp)];
  HydraulicConductivityAnnotation := Annotations[ReadCompInt(Decomp)];
  BoundName := Annotations[ReadCompInt(Decomp)];
//  ReachLengthAnnotation := Annotations[ReadCompInt(Decomp)];
//  ReachLengthAnnotation := Annotations[ReadCompInt(Decomp)];
//  ReachLengthAnnotation := Annotations[ReadCompInt(Decomp)];

  Count := ReadCompInt(Decomp);
  SetLength(ConnectedReacheAnnotations, Count);
  for index := 0 to Count - 1 do
  begin
    ConnectedReacheAnnotations[index] := Annotations[ReadCompInt(Decomp)];
  end;
end;

procedure TSfrMF6ConstantRecord.SetBoundaryAnnotation(Index: Integer;
  const Value: string);
begin
  case Index of
    ReachLengthPosition:
      begin
        ReachLengthAnnotation := Value;
      end;
    ReachWidthPosition:
      begin
        ReachWidthAnnotation := Value;
      end;
    GradientPosition:
      begin
        GradientAnnotation := Value;
      end;
    StreambedTopPosition:
      begin
        StreambedTopAnnotation := Value;
      end;
    StreambedThicknessPosition:
      begin
        StreambedThicknessAnnotation := Value;
      end;
    HydraulicConductivityPosition:
      begin
        HydraulicConductivityAnnotation := Value;
      end;
//    SteadyRoughnessPosition:
//      begin
//        RoughnessAnnotation := Value;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        UpstreamFractionAnnotation := Value;
//      end;
    else
      Assert(False);
  end;
end;

procedure TSfrMF6ConstantRecord.SetBoundaryValue(Index: Integer;
  const Value: double);
begin
  case Index of
    ReachLengthPosition:
      begin
        ReachLength := Value;
      end;
    ReachWidthPosition:
      begin
        ReachWidth := Value;
      end;
    GradientPosition:
      begin
        Gradient := Value;
      end;
    StreambedTopPosition:
      begin
        StreambedTop := Value;
      end;
    StreambedThicknessPosition:
      begin
        StreambedThickness := Value;
      end;
    HydraulicConductivityPosition:
      begin
        HydraulicConductivity := Value;
      end;
//    SteadyRoughnessPosition:
//      begin
//        Roughness := Value;
//      end;
//    SteadyUpstreamFractionPosition:
//      begin
//        UpstreamFraction := Value;
//      end;
    else
      Assert(False);
  end;
end;

procedure TSfrMF6ConstantRecord.SetReachNumber(const Value: integer);
begin
  FReachNumber := Value;
end;

{ TSfrMf6TimeListLink }

procedure TSfrMf6TimeListLink.CreateTimeLists;
var
  LocalModel: TCustomModel;
begin
  FInflow := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FInflow.NonParamDescription := StrSFR6Inflow;
  FInflow.ParamDescription := StrSFR6Inflow;

  FRainfall := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRainfall.NonParamDescription := StrSFR6Rainfall;
  FRainfall.ParamDescription := StrSFR6Rainfall;

  FEvaporation := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FEvaporation.NonParamDescription := StrSFR6Evaporation;
  FEvaporation.ParamDescription := StrSFR6Evaporation;

  FRunoff := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRunoff.NonParamDescription := StrSFR6Runoff;
  FRunoff.ParamDescription := StrSFR6Runoff;

  FUpstreamFraction := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FUpstreamFraction.NonParamDescription := StrSFR6UpstreamFracti;
  FUpstreamFraction.ParamDescription := StrSFR6UpstreamFracti;

  FStage := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStage.NonParamDescription := StrSFR6Stage;
  FStage.ParamDescription := StrSFR6Stage;

  FRoughness := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FRoughness.NonParamDescription := StrSFR6Roughness;
  FRoughness.ParamDescription := StrSFR6Roughness;

  FStreamStatus := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FStreamStatus.NonParamDescription := StrSFR6StreamStatus;
  FStreamStatus.ParamDescription := StrSFR6StreamStatus;

  FReachNumber := TModflowTimeList.Create(Model, Boundary.ScreenObject);
  FReachNumber.NonParamDescription := StrSFR6ReachNumber;
  FReachNumber.ParamDescription := StrSFR6ReachNumber;

  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    FInflow.OnInvalidate := LocalModel.InvalidateSfr6Inflow;
    FRainfall.OnInvalidate := LocalModel.InvalidateSfr6Rainfall;
    FEvaporation.OnInvalidate := LocalModel.InvalidateSfr6Evaporation;
    FRunoff.OnInvalidate := LocalModel.InvalidateSfr6Runoff;
    FUpstreamFraction.OnInvalidate := LocalModel.InvalidateSfr6UpstreamFraction;
    FStage.OnInvalidate := LocalModel.InvalidateSfr6Stage;
    FRoughness.OnInvalidate := LocalModel.InvalidateSfr6Roughness;
    FStreamStatus.OnInvalidate := LocalModel.InvalidateSfr6StreamStatus;
    FReachNumber.OnInvalidate := LocalModel.InvalidateSfr6ReachNumber;
  end;

  AddTimeList(FInflow);
  AddTimeList(FRainfall);
  AddTimeList(FEvaporation);
  AddTimeList(FRunoff);
  AddTimeList(FUpstreamFraction);
  AddTimeList(FStage);
  AddTimeList(FRoughness);
  AddTimeList(FStreamStatus);
  AddTimeList(FReachNumber);
end;

destructor TSfrMf6TimeListLink.Destroy;
begin
  FReachNumber.Free;
  FStreamStatus.Free;
  FRoughness.Free;
  FStage.Free;
  FUpstreamFraction.Free;
  FRunoff.Free;
  FEvaporation.Free;
  FRainfall.Free;
  FInflow.Free;
  inherited;
end;

initialization
  InitializeSfrObNames

finalization
  SfrObNames.Free;

end.
