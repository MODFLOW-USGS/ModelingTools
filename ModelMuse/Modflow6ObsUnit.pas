unit Modflow6ObsUnit;

interface

uses
  System.Classes, GoPhastTypes, System.SysUtils, ModflowMawUnit,
  ModflowSfr6Unit, ModflowLakMf6Unit, ModflowUzfMf6Unit,
  ModflowCsubUnit, PestObsUnit, System.Generics.Collections;

type
  TGwFlowOb = (gfoNearestNeighbor, gfoAllNeighbors, gfoAbove, gfoBelow);
  TGwFlowObs = set of TGwFlowOb;

  TObGeneral = (ogHead, ogDrawdown, ogCHD, ogDrain, ogWell, ogGHB, ogRiv,
    ogRch, ogEVT, ogMvr, ogWellReduction, ogUndefined);
  TObGenerals = set of TObGeneral;

  TObGwt = (ogwtConcentration, ogwtCNC, ogwtSRC, ogwtUndefined);
  TObGwts = set of TObGwt;

  TObSeries = (osGeneral, osMaw, osSfr, osLak, osUzf, osCSub, osGWT, osSft,
    osLkt, osMwt, osUzt);

const
  GwtSeries = [osGWT, osSft, osLkt, osMwt, osUzt];

type
  TSpecies = Integer;
  TGenus = TIntegerSet;

  TMf6CalibrationObs = class(TCustomTimeObservationItem)
  private
    FUzfOb: TUzfOb;
    FSfrOb: TSfrOb;
    FCSubOb: TCSubOb;
    FObSeries: TObSeries;
    FMawOb: TMawOb;
    FLakOb: TLakOb;
    FObGeneral: TObGeneral;
    FInterpObsNames: TStringList;
    FMawConnectionNumber: Integer;
    FGwtOb: TObGwt;
    FSftOb: TSftOb;
    FLktOb: TLktOb;
    FMwtOb: TMwtOb;
    FSpeciesIndex: TSpecies;
    FUztOb: TUztOb;
    procedure SetCSubOb(const Value: TCSubOb);
    procedure SetLakOb(const Value: TLakOb);
    procedure SetMawOb(const Value: TMawOb);
    procedure SetObGeneral(const Value: TObGeneral);
    procedure SetObSeries(const Value: TObSeries);
    procedure SetSfrOb(const Value: TSfrOb);
    procedure SetUzfOb(const Value: TUzfOb);
    function StoreCSubOb: Boolean;
    function StoreLakOb: Boolean;
    function StoreMawOb: Boolean;
    function StoreObGeneral: Boolean;
    function StoreSfrOb: Boolean;
    function StoreUzfOb: Boolean;
    procedure SetMawConnectionNumber(const Value: Integer);
    procedure SetGwtOb(const Value: TObGwt);
    function StoreGwtOb: Boolean;
    procedure SetSftOb(const Value: TSftOb);
    function StoreSftOb: Boolean;
    procedure SetLktOb(const Value: TLktOb);
    function StoreLktOb: Boolean;
    procedure SetMwtOb(const Value: TMwtOb);
    function StoreMwtOb: Boolean;
    procedure SetSpeciesIndex(const Value: TSpecies);
    procedure SetUztOb(const Value: TUztOb);
    function StoreUztOb: Boolean;
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(Value: Integer); override;
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property InterpObsNames: TStringList read FInterpObsNames;
    function ObservationType: string; override;
  published
    property GUID;
    property ObSeries: TObSeries read FObSeries write SetObSeries;
    property ObGeneral: TObGeneral read FObGeneral write SetObGeneral
      stored StoreObGeneral;
    property MawOb: TMawOb read FMawOb write SetMawOb stored StoreMawOb;
    property SfrOb: TSfrOb read FSfrOb write SetSfrOb stored StoreSfrOb;
    property LakOb: TLakOb read FLakOb write SetLakOb stored StoreLakOb;
    property UzfOb: TUzfOb read FUzfOb write SetUzfOb stored StoreUzfOb;
    property CSubOb: TCSubOb read FCSubOb write SetCSubOb stored StoreCSubOb;
    property MawConnectionNumber: Integer read FMawConnectionNumber
      write SetMawConnectionNumber;
    // GWT
    property SpeciesIndex: TSpecies read FSpeciesIndex write SetSpeciesIndex;
    property GwtOb: TObGwt read FGwtOb write SetGwtOb stored StoreGwtOb;
    property SftOb: TSftOb read FSftOb write SetSftOb stored StoreSftOb;
    property LktOb: TLktOb read FLktOb write SetLktOb stored StoreLktOb;
    property MwtOb: TMwtOb read FMwtOb write SetMwtOb stored StoreMwtOb;
    property UztOb: TUztOb read FUztOb write SetUztOb stored StoreUztOb;
  end;

  TCalibObList = TList<TMf6CalibrationObs>;

  TMf6CalibrationObservations = class(TCustomComparisonCollection)
  private
    FMultiLayer: Boolean;
    function GetLakObs: TLakObs;
    function GetMawObs: TMawObs;
    function GetObGenerals: TObGenerals;
    function GetSfrObs: TSfrObs;
    function GetSubObsSet: TSubObsSet;
    function GetUzfObs: TUzfObs;
    function GetCalibItem(Index: Integer): TMf6CalibrationObs;
    procedure SetCalibItem(Index: Integer; const Value: TMf6CalibrationObs);
    procedure SetMultiLayer(const Value: Boolean);
    function GetGwtObs(Species: Integer): TObGwts;
    function GetSftObs(Species: Integer): TSftObs;
    function GetLktObs(Species: Integer): TLktObs;
    function GetMwtObs(Species: Integer): TMwtObs;
    function GetUztObs(Species: Integer): TUztObs;
//    function GetGenus(Series: TObSeries; Species: Integer): TGenus;
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent;
      ScreenObject: TObject);
    procedure Assign(Source: TPersistent); override;
    property ObGenerals: TObGenerals read GetObGenerals;
    property MawObs: TMawObs read GetMawObs;
    property SfrObs: TSfrObs read GetSfrObs;
    property LakObs: TLakObs read GetLakObs;
    property UzfObs: TUzfObs read GetUzfObs;
    property SubObsSet: TSubObsSet read GetSubObsSet;
    // GWT
    property GwtObs[Species: Integer]: TObGwts read GetGwtObs;
    property SftObs[Species: Integer]: TSftObs read GetSftObs;
    property LktObs[Species: Integer]: TLktObs read GetLktObs;
    property MwtObs[Species: Integer]: TMwtObs read GetMwtObs;
    property UztObs[Species: Integer]: TUztObs read GetUztObs;
    property Items[Index: Integer]: TMf6CalibrationObs read GetCalibItem
      write SetCalibItem; default;
    function UsesMawConnectionNumber(ConnectionNumber: Integer;
      AnObsType: TMawOb): Boolean; overload;
    function UsesMawConnectionNumber(ConnectionNumber: Integer;
      AnObsType: TMwtOb): Boolean; overload;
    function IndexOfTimeAndType(ATime: double; ObGeneralType: TObGeneral): integer;
    function Add: TMf6CalibrationObs;
  published
    property MultiLayer: Boolean read FMultiLayer write SetMultiLayer;
  end;

  TModflow6Obs = class(TGoPhastPersistent)
  private
    FGroundwaterFlowObs: Boolean;
    FGwFlowObsChoices: TGwFlowObs;
    FName: string;
    FMawObs: TMawObs;
    FSfrObs: TSfrObs;
    FLakObs: TLakObs;
    FSfrObsLocation: TSfrObsLocation;
    FStoredUzfObsDepthFraction: TRealStorage;
    FUzfObs: TUzfObs;
    FCSubObs: TCSubObs;
    FCSubDelayCells: TIntegerCollection;
    FCalibrationObservations: TMf6CalibrationObservations;
    FGeneral: TObGenerals;
    FScreenObject: TObject;
    FGwtObs: TObGwts;
    FGwtSpecies: Integer;
    FSftObs: TSftObs;
    FLktObs: TLktObs;
    FGenus: TGenus;
    FMwtObs: TMwtObs;
    FUztObs: TUztObs;
    procedure SetDrawdownObs(const Value: Boolean);
    procedure SetGroundwaterFlowObs(const Value: Boolean);
    procedure SetGwFlowObsChoices(const Value: TGwFlowObs);
    procedure SetHeadObs(const Value: Boolean);
    procedure SetName(Value: string);
    procedure SetChdFlowObs(const Value: Boolean);
    procedure SetDrnFlowObs(const Value: Boolean);
    procedure SetGhbFlowObs(const Value: Boolean);
    procedure SetRivFlowObs(const Value: Boolean);
    procedure SetWelFlowObs(const Value: Boolean);
    procedure SetRchFlowObs(const Value: Boolean);
    procedure SetEvtFlowObs(const Value: Boolean);
    procedure SetMawObs(const Value: TMawObs);
    procedure SetSfrObs(const Value: TSfrObs);
    procedure SetLakObs(const Value: TLakObs);
    procedure SetSfrObsLocation(const Value: TSfrObsLocation);
    procedure SetToMvrFlowObs(const Value: Boolean);
    function GetUzfObsDepthFraction: double;
    procedure SetStoredUzfObsDepthFraction(const Value: TRealStorage);
    procedure SetUzfObs(const Value: TUzfObs);
    procedure SetUzfObsDepthFraction(const Value: double);
    procedure SetCSubObs(const Value: TCSubObs);
    procedure SetCSubDelayCells(const Value: TIntegerCollection);
    procedure SetCalibrationObservations(
      const Value: TMf6CalibrationObservations);
    function StoreCalibObs: Boolean;
    function GetUsed: Boolean;
    procedure SetGeneral(const Value: TObGenerals);
    function GetHeadObs: Boolean;
    function GetDrawdownObs: Boolean;
    function GetChdFlowObs: Boolean;
    function GetDrnFlowObs: Boolean;
    function GetGhbFlowObs: Boolean;
    function GetRivFlowObs: Boolean;
    function GetWelFlowObs: Boolean;
    function GetRchFlowObs: Boolean;
    function GetEvtFlowObs: Boolean;
    function GetToMvrFlowObs: Boolean;
    procedure SetUsed(const Value: Boolean);
    function GetGeneral: TObGenerals;
    function GetMawObs: TMawObs;
    function GetCSubObs: TCSubObs;
    function GetLakObs: TLakObs;
    function GetSfrObs: TSfrObs;
    function GetUzfObs: TUzfObs;
    function GetName: string;
    function GetGwtObs: TObGwts;
    procedure SetGwtObs(const Value: TObGwts);
    procedure SetGwtSpecies(const Value: Integer);
    function GetSftObs: TSftObs;
    procedure SetSftObs(const Value: TSftObs);
    function GetLktObs: TLktObs;
    procedure SetLktObs(const Value: TLktObs);
    procedure SetGenus(const Value: TGenus);
    function GetGenusColl: Integer;
    procedure SetGenusColl(const Value: Integer);
    function GetMwtObs: TMwtObs;
    procedure SetMwtObs(const Value: TMwtObs);
    function GetUztObs: TUztObs;
    procedure SetUztObs(const Value: TUztObs);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property UzfObsDepthFraction: double read GetUzfObsDepthFraction
      write SetUzfObsDepthFraction;
    // If @name is changed, @link(GetUsed) should be changed too.
    procedure Clear;
    // If @name is changed, @link(Clear) should be changed too.
    procedure ReplaceGUID;
    // @name is the object that defines the observation.
    // If may be different from the one that defines the boundary
    // with for the observation.
    property ScreenObject: TObject read FScreenObject;
    property Genus: TGenus read FGenus write SetGenus;
  published
    property Name: string read GetName write SetName;
    property GroundwaterFlowObs: Boolean read FGroundwaterFlowObs
      write SetGroundwaterFlowObs;
    property GwFlowObsChoices: TGwFlowObs read FGwFlowObsChoices
      write SetGwFlowObsChoices;
    property General: TObGenerals read GetGeneral write SetGeneral;
    property MawObs: TMawObs read GetMawObs write SetMawObs;
    property SfrObs: TSfrObs read GetSfrObs write SetSfrObs;
    property LakObs: TLakObs read GetLakObs write SetLakObs;
    property SfrObsLocation: TSfrObsLocation read FSfrObsLocation write SetSfrObsLocation;
    property UzfObs: TUzfObs read GetUzfObs write SetUzfObs;
    property CSubObs: TCSubObs read GetCSubObs write SetCSubObs;
    property GwtObs: TObGwts read GetGwtObs write SetGwtObs;
    property CSubDelayCells: TIntegerCollection read FCSubDelayCells
      write SetCSubDelayCells;
    property StoredUzfObsDepthFraction: TRealStorage
      read FStoredUzfObsDepthFraction write SetStoredUzfObsDepthFraction;
    property CalibrationObservations: TMf6CalibrationObservations
      read FCalibrationObservations write SetCalibrationObservations
        stored StoreCalibObs;
    property GenusColl: Integer read GetGenusColl write SetGenusColl stored True;
    property GwtSpecies: Integer read FGwtSpecies write SetGwtSpecies
      stored False;
    property SftObs: TSftObs read GetSftObs write SetSftObs;
    property LktObs: TLktObs read GetLktObs write SetLktObs;
    property MwtObs: TMwtObs read GetMwtObs write SetMwtObs;
    property UztObs: TUztObs read GetUztObs write SetUztObs;
    // @name is retained for backwards compatibility.
    property Used: Boolean read GetUsed write SetUsed stored False;
    // @name is retained for backwards compatibility.
    property HeadObs: Boolean read GetHeadObs  write SetHeadObs stored False;
    // @name is retained for backwards compatibility.
    property DrawdownObs: Boolean read GetDrawdownObs write SetDrawdownObs stored False;
    // @name is retained for backwards compatibility.
    property ChdFlowObs: Boolean read GetChdFlowObs write SetChdFlowObs stored False;
    // @name is retained for backwards compatibility.
    property DrnFlowObs: Boolean read GetDrnFlowObs write SetDrnFlowObs stored False;
    // @name is retained for backwards compatibility.
    property GhbFlowObs: Boolean read GetGhbFlowObs write SetGhbFlowObs stored False;
    // @name is retained for backwards compatibility.
    property RivFlowObs: Boolean read GetRivFlowObs write SetRivFlowObs stored False;
    // @name is retained for backwards compatibility.
    property WelFlowObs: Boolean read GetWelFlowObs write SetWelFlowObs stored False;
    // @name is retained for backwards compatibility.
    property RchFlowObs: Boolean read GetRchFlowObs write SetRchFlowObs stored False;
    // @name is retained for backwards compatibility.
    property EvtFlowObs: Boolean read GetEvtFlowObs write SetEvtFlowObs stored False;
    // @name is retained for backwards compatibility.
    property ToMvrFlowObs: Boolean read GetToMvrFlowObs write SetToMvrFlowObs stored False;
  end;

function TryGetGenOb(const GenObName: string; var GenOb: TObGeneral): Boolean;
function TryGetGwtOb(const ConcObName: string; var ConcOb: TObGwt): Boolean;
function GenObToString(const GenOb: TObGeneral): string;
function GwtObToString(const GwtOb: TObGwt): string;
Procedure FillObGenSeriesNames(AList: TStrings);
Procedure FillObConcentrationSeriesNames(AList: TStrings);

function TryGetObsSeries(const SeriesName: string; var ObSeries: TObSeries): Boolean;
function ObsSeriesToString(const ObSeries: TObSeries): string;

procedure GlobalRemoveMf6CalibrationObsSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestoreMf6CalibrationObsSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses
  System.Character, RbwParser;

const
  ObGenName: array[TObGeneral] of string = ('Head', 'Drawdown', 'CHD', 'Drain', 'Well', 'GHB', 'Riv',
    'Rch', 'EVT', 'Mvr', 'WellReduction', 'undefined');
  ObConcName: array[TObGwt] of string = ('Concentration', 'CNC', 'SRC', 'undefined');

  ObSeriesName: array[TObSeries] of string = ('General', 'Maw', 'Sfr', 'Lak',
    'Uzf', 'CSub', 'GWT', 'Sft', 'Lkt', 'Mwt', 'Uzt');

var
  ObGenNames: TStringList;
  ObsSeriesNames: TStringList;
  ObConcNames: TStringList;

procedure GlobalRemoveMf6CalibrationObsSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  // At present, this doesn't do anything.
  Assert(Subject is TMf6CalibrationObs);
end;

procedure GlobalRestoreMf6CalibrationObsSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
begin
  // At present, this doesn't do anything.
  Assert(Subject is TMf6CalibrationObs);
end;

procedure InitializeObGenNames;
var
  ObGen: TObGeneral;
begin
  ObGenNames := TStringList.Create;
  ObGenNames.CaseSensitive := False;
  for ObGen := Low(TObGeneral) to Pred(High(TObGeneral)) do
  begin
    ObGenNames.Add(ObGenName[ObGen]);
  end;
end;

procedure InitializeObConcNames;
var
  ObConc: TObGwt;
begin
  ObConcNames := TStringList.Create;
  ObConcNames.CaseSensitive := False;
  for ObConc := Low(TObGwt) to Pred(High(TObGwt)) do
  begin
    ObConcNames.Add(ObConcName[ObConc]);
  end;
end;

procedure InitializeSeriesNames;
var
  ObSeries: TObSeries;
begin
  ObsSeriesNames := TStringList.Create;
  ObsSeriesNames.CaseSensitive := False;
  for ObSeries := Low(TObSeries) to High(TObSeries) do
  begin
    ObsSeriesNames.Add(ObSeriesName[ObSeries]);
  end;
end;

function TryGetGenOb(const GenObName: string; var GenOb: TObGeneral): Boolean;
var
  Index: Integer;
begin
  Index := ObGenNames.IndexOf(GenObName);
  result := Index >= 0;
  if result then
  begin
    GenOb := TObGeneral(Index);
  end;
end;

function TryGetGwtOb(const ConcObName: string; var ConcOb: TObGwt): Boolean;
var
  Index: Integer;
begin
  Index := ObConcNames.IndexOf(ConcObName);
  result := Index >= 0;
  if result then
  begin
    ConcOb := TObGwt(Index);
  end;
end;

function GenObToString(const GenOb: TObGeneral): string;
begin
  result := ObGenName[GenOb];
end;

function GwtObToString(const GwtOb: TObGwt): string;
begin
  result := ObConcName[GwtOb];
end;

Procedure FillObGenSeriesNames(AList: TStrings);
begin
  AList.Assign(ObGenNames);
end;

Procedure FillObConcentrationSeriesNames(AList: TStrings);
begin
  AList.Assign(ObConcNames);
end;

function TryGetObsSeries(const SeriesName: string; var ObSeries: TObSeries): Boolean;
var
  Index: Integer;
begin
  Index := ObsSeriesNames.IndexOf(SeriesName);
  result := Index >= 0;
  if result then
  begin
    ObSeries := TObSeries(Index);
  end;
end;

function ObsSeriesToString(const ObSeries: TObSeries): string;
begin
  result := ObSeriesName[ObSeries];
end;

{ TModflow6Obs }

procedure TModflow6Obs.Assign(Source: TPersistent);
var
  SourceObs: TModflow6Obs;
begin
  if Source is TModflow6Obs then
  begin
    SourceObs := TModflow6Obs(Source);
    Name := SourceObs.Name;
    General := SourceObs.General;

    GroundwaterFlowObs := SourceObs.GroundwaterFlowObs;
    GwFlowObsChoices := SourceObs.GwFlowObsChoices;

    MawObs := SourceObs.MawObs;
    SfrObs := SourceObs.SfrObs;
    LakObs := SourceObs.LakObs;
    UzfObs := SourceObs.UzfObs;
    GwtObs := SourceObs.GwtObs;
    SftObs := SourceObs.SftObs;
    LktObs := SourceObs.LktObs;
    MwtObs := SourceObs.MwtObs;
    UztObs := SourceObs.UztObs;
//    SourceObs.CSubObs.CSubObsSet := SourceObs.CSubObs.CSubObsSet;
    CSubObs := SourceObs.CSubObs;

    CSubDelayCells := SourceObs.CSubDelayCells;
    UzfObsDepthFraction := SourceObs.UzfObsDepthFraction;

    SfrObsLocation := SourceObs.SfrObsLocation;
    UzfObsDepthFraction := SourceObs.UzfObsDepthFraction;

    CalibrationObservations := SourceObs.CalibrationObservations;

    Genus := SourceObs.Genus;
  end
  else
  begin
    inherited;
  end;
end;

procedure TModflow6Obs.Clear;
begin
  General := [];
  GroundwaterFlowObs := False;
  MawObs := [];
  SfrObs := [];
  LakObs := [];
  UzfObs := [];
  GWTObs := [];
  SftObs := [];
  LktObs := [];
  MwtObs := [];
  UztObs := [];
  CSubObs.CSubObsSet := [];
  CalibrationObservations.Clear;
  Assert(not Used);
end;

constructor TModflow6Obs.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(InvalidateModelEvent);
  FScreenObject := ScreenObject;
  FGwFlowObsChoices := [gfoNearestNeighbor];
  FStoredUzfObsDepthFraction := TRealStorage.Create;
  FStoredUzfObsDepthFraction.OnChange := OnInvalidateModel;
  FCSubDelayCells := TIntegerCollection.Create(OnInvalidateModel);
  FCSubObs := TCSubObs.Create;
  FCalibrationObservations :=
    TMf6CalibrationObservations.Create(InvalidateModelEvent, ScreenObject);
end;

destructor TModflow6Obs.Destroy;
begin
  FCalibrationObservations.Free;
  FCSubObs.Free;
  FCSubDelayCells.Free;
  FStoredUzfObsDepthFraction.Free;
  inherited;
end;

function TModflow6Obs.GetChdFlowObs: Boolean;
begin
  result := ogCHD in General;
end;

function TModflow6Obs.GetCSubObs: TCSubObs;
begin
  FCSubObs.CSubObsSet  :=
    FCSubObs.CSubObsSet +  CalibrationObservations.SubObsSet;
  result := FCSubObs;
end;

function TModflow6Obs.GetDrawdownObs: Boolean;
begin
  result := ogDrawdown in General;
end;

function TModflow6Obs.GetDrnFlowObs: Boolean;
begin
  result := ogDrain in General;
end;

function TModflow6Obs.GetEvtFlowObs: Boolean;
begin
  result := ogEVT in General;
end;

function TModflow6Obs.GetGeneral: TObGenerals;
begin
  result := FGeneral + CalibrationObservations.ObGenerals;
end;

function TModflow6Obs.GetGenusColl: Integer;
begin
  result := Integer(Genus)
end;

function TModflow6Obs.GetGhbFlowObs: Boolean;
begin
  result := ogGHB in General;
end;

function TModflow6Obs.GetGwtObs: TObGwts;
begin
  result := FGwtObs {+ CalibrationObservations.GwtObs};
end;

function TModflow6Obs.GetHeadObs: Boolean;
begin
  result := ogHead in General;
end;

function TModflow6Obs.GetLakObs: TLakObs;
begin
  result := FLakObs + CalibrationObservations.LakObs;
end;

function TModflow6Obs.GetLktObs: TLktObs;
begin
  result := FLktObs {+ CalibrationObservations.LKtObs};
end;

function TModflow6Obs.GetMawObs: TMawObs;
begin
  result := FMawObs + CalibrationObservations.MawObs;
end;

function TModflow6Obs.GetMwtObs: TMwtObs;
begin
  result := FMwtObs;
end;

function TModflow6Obs.GetName: string;
begin
  if FName <> '' then
  begin
    if CharInSet(FName[1], ['0'..'9']) then
    begin
      FName := '_' + FName
    end;
  end;
  result := FName;
end;

function TModflow6Obs.GetRchFlowObs: Boolean;
begin
  result := ogRch in General;
end;

function TModflow6Obs.GetRivFlowObs: Boolean;
begin
  result := ogRiv in General;
end;

function TModflow6Obs.GetSfrObs: TSfrObs;
begin
  result := FSfrObs + CalibrationObservations.SfrObs;
end;

function TModflow6Obs.GetSftObs: TSftObs;
begin
  result := FSftObs {+ CalibrationObservations.SftObs};
end;

function TModflow6Obs.GetToMvrFlowObs: Boolean;
begin
  result := ogMvr in General;
end;

function TModflow6Obs.GetUsed: Boolean;
begin
  result := (General <> []) or GroundwaterFlowObs
    or (MawObs <> []) or (SfrObs <> [])
    or (LakObs <> []) or (UzfObs <> []) or (CSubObs.CSubObsSet <> [])
    or (GwtObs <> []) or (SftObs <> []) or (LktObs <> []) or (MwtObs <> [])
    or (UztObs <> [])
    or (CalibrationObservations.Count > 0);
end;

function TModflow6Obs.GetUzfObs: TUzfObs;
begin
  result := FUzfObs + CalibrationObservations.UzfObs;
end;

function TModflow6Obs.GetUzfObsDepthFraction: double;
begin
  result := FStoredUzfObsDepthFraction.Value;
end;

function TModflow6Obs.GetUztObs: TUztObs;
begin
  result := FUztObs;
end;

function TModflow6Obs.GetWelFlowObs: Boolean;
begin
  result := ogWell in General;
end;

procedure TModflow6Obs.ReplaceGUID;
begin
  CalibrationObservations.ReplaceGUID;
end;

procedure TModflow6Obs.SetCalibrationObservations(
  const Value: TMf6CalibrationObservations);
begin
  FCalibrationObservations.Assign(Value);
end;

procedure TModflow6Obs.SetChdFlowObs(const Value: Boolean);
begin
  if Value <> ChdFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogCHD);
    end
    else
    begin
      Exclude(FGeneral, ogCHD);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetCSubObs(const Value: TCSubObs);
begin
  if FCSubObs <> Value then
  begin
    FCSubObs.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetCSubDelayCells(const Value: TIntegerCollection);
begin
  FCSubDelayCells.Assign(Value);
end;

procedure TModflow6Obs.SetDrawdownObs(const Value: Boolean);
begin
  if Value <> DrawdownObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogDrawdown);
    end
    else
    begin
      Exclude(FGeneral, ogDrawdown);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetDrnFlowObs(const Value: Boolean);
begin
  if Value <> DrnFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogDrain);
    end
    else
    begin
      Exclude(FGeneral, ogDrain);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetEvtFlowObs(const Value: Boolean);
begin
  if Value <> EvtFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogEVT);
    end
    else
    begin
      Exclude(FGeneral, ogEVT);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetGeneral(const Value: TObGenerals);
begin
  if FGeneral <> Value then
  begin
    FGeneral := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetGenus(const Value: TGenus);
begin
  if FGenus <> Value then
  begin
    FGenus := Value;
  end;
end;

procedure TModflow6Obs.SetGenusColl(const Value: Integer);
begin
  FGenus := TIntegerSet(Value);
end;

procedure TModflow6Obs.SetGhbFlowObs(const Value: Boolean);
begin
  if Value <> GhbFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogGHB);
    end
    else
    begin
      Exclude(FGeneral, ogGHB);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetGroundwaterFlowObs(const Value: Boolean);
begin
  SetBooleanProperty(FGroundwaterFlowObs, Value);
end;

procedure TModflow6Obs.SetGwFlowObsChoices(const Value: TGwFlowObs);
begin
  if FGwFlowObsChoices <> Value then
  begin
    FGwFlowObsChoices := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetGwtObs(const Value: TObGwts);
begin
  if FGwtObs <> Value then
  begin
    FGwtObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetGwtSpecies(const Value: Integer);
begin
  if Value < 0 then
  begin
    Genus := [];
  end
  else
  begin
    Genus := [Value];
  end;
end;

procedure TModflow6Obs.SetHeadObs(const Value: Boolean);
begin
  if Value <> HeadObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogHead);
    end
    else
    begin
      Exclude(FGeneral, ogHead);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetLakObs(const Value: TLakObs);
begin
  if FLakObs <> Value then
  begin
    FLakObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetLktObs(const Value: TLktObs);
begin
  if FLktObs <> Value then
  begin
    FLktObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetMawObs(const Value: TMawObs);
begin
  if FMawObs <> Value then
  begin
    FMawObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetMwtObs(const Value: TMwtObs);
begin
  if FMwtObs <> Value then
  begin
    FMwtObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetName(Value: string);
var
  CharIndex: Integer;
begin
  Value := Trim(Value);
  if Length(Value) > MaxBoundNameLength then
  begin
    Value := Copy(Value, 1, MaxBoundNameLength);
  end;
  for CharIndex := 1 to Length(Value) do
  begin
    if not Value[CharIndex].IsLetterOrDigit
      and (Value[CharIndex] <> '_') then
    begin
      Value[CharIndex] := '_'
    end;
  end;
  SetStringProperty(FName, Value);
end;

procedure TModflow6Obs.SetRchFlowObs(const Value: Boolean);
begin
  if Value <> RchFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogRch);
    end
    else
    begin
      Exclude(FGeneral, ogRch);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetRivFlowObs(const Value: Boolean);
begin
  if Value <> RivFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogRiv);
    end
    else
    begin
      Exclude(FGeneral, ogRiv);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetSfrObs(const Value: TSfrObs);
begin
  if FSfrObs <> Value then
  begin
    FSfrObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetSfrObsLocation(const Value: TSfrObsLocation);
begin
  if FSfrObsLocation <> Value then
  begin
    FSfrObsLocation := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetSftObs(const Value: TSftObs);
begin
  if FSftObs <> Value then
  begin
    FSftObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetStoredUzfObsDepthFraction(const Value: TRealStorage);
begin
  FStoredUzfObsDepthFraction.Assign(Value);
end;

procedure TModflow6Obs.SetToMvrFlowObs(const Value: Boolean);
begin
  if Value <> ToMvrFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogMvr);
    end
    else
    begin
      Exclude(FGeneral, ogMvr);
    end;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetUsed(const Value: Boolean);
begin

end;

procedure TModflow6Obs.SetUzfObs(const Value: TUzfObs);
begin
  if FUzfObs <> Value then
  begin
    FUzfObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetUzfObsDepthFraction(const Value: double);
begin
  FStoredUzfObsDepthFraction.Value := Value;
end;

procedure TModflow6Obs.SetUztObs(const Value: TUztObs);
begin
  if FUztObs <> Value then
  begin
    FUztObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetWelFlowObs(const Value: Boolean);
begin
  if Value <> WelFlowObs then
  begin
    if Value then
    begin
      Include(FGeneral, ogWell);
    end
    else
    begin
      Exclude(FGeneral, ogWell);
    end;
    InvalidateModel;
  end;
end;

function TModflow6Obs.StoreCalibObs: Boolean;
begin
  result := CalibrationObservations.Count > 0;
end;

{ TMf6CalibrationObs }

procedure TMf6CalibrationObs.Assign(Source: TPersistent);
var
  ObsSource: TMf6CalibrationObs;
begin
  if Source is TMf6CalibrationObs then
  begin
    ObsSource := TMf6CalibrationObs(Source);
    ObSeries := ObsSource.ObSeries;
    ObGeneral := ObsSource.ObGeneral;
    MawOb := ObsSource.MawOb;
    SfrOb := ObsSource.SfrOb;
    LakOb := ObsSource.LakOb;
    UzfOb := ObsSource.UzfOb;
    CSubOb := ObsSource.CSubOb;

    SpeciesIndex := ObsSource.SpeciesIndex;
    GwtOb := ObsSource.GwtOb;
    SftOb := ObsSource.SftOb;
    LktOb := ObsSource.LktOb;
    MwtOb := ObsSource.MwtOb;
    UztOb := ObsSource.UztOb;

    MawConnectionNumber := ObsSource.MawConnectionNumber;
  end;
  inherited;
end;

constructor TMf6CalibrationObs.Create(Collection: TCollection);
begin
  inherited;
  FInterpObsNames := TStringList.Create;
end;

destructor TMf6CalibrationObs.Destroy;
begin
  FInterpObsNames.Free;
  inherited;
end;

function TMf6CalibrationObs.GetObsTypeIndex: Integer;
begin
  result := -1;
  case ObSeries of
    osGeneral:
      begin
        Result := Ord(ObGeneral);
      end;
    osMaw:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(MawOb) + 1;
      end;
    osSfr:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(SfrOb) + 2;
      end;
    osLak:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(LakOb) + 3;
      end;
    osUzf:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(UzfOb) + 4;
      end;
    osCSub:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(High(TUzfOb))
          + Ord(CSubOb) + 5;
      end;
    osGWT:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(High(TUzfOb))
          + Ord(High(TCSubOb))
          + Ord(GwtOb) + 6;
      end;
    osSft:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(High(TUzfOb))
          + Ord(High(TCSubOb))
          + Ord(High(TObGwt))
          + Ord(SftOb) + 7;
      end;
    osLkt:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(High(TUzfOb))
          + Ord(High(TObGwt))
          + Ord(High(TSftOb))
          + Ord(LktOb) + 8;
      end;
    osMwt:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(High(TUzfOb))
          + Ord(High(TObGwt))
          + Ord(High(TSftOb))
          + Ord(High(TLktOb))
          + Ord(MwtOb) + 9;
      end;
    osUzt:
      begin
        Result := Ord(High(TObGeneral))
          + Ord(High(TMawOb))
          + Ord(High(TSfrOb))
          + Ord(High(TLakOb))
          + Ord(High(TUzfOb))
          + Ord(High(TObGwt))
          + Ord(High(TSftOb))
          + Ord(High(TLktOb))
          + Ord(High(TMwtOb))
          + Ord(UztOb) + 10;
      end;
    else
      Assert(False);
  end;
end;

function TMf6CalibrationObs.GetObsTypeString: string;
begin
  result := '';
  case ObSeries of
    osGeneral:
      begin
        result := GenObToString(ObGeneral);
      end;
    osMaw:
      begin
        result := MawObToString(MawOb);
      end;
    osSfr:
      begin
        result := SfrObToString(SfrOb);
      end;
    osLak:
      begin
        result := LakObToString(LakOb);
      end;
    osUzf:
      begin
        result := UzfObToString(UzfOb);
      end;
    osCSub:
      begin
        result := CSubObToString(CSubOb);
      end;
    osGwt:
      begin
        result := GwtObToString(GwtOb);
      end;
    osSft:
      begin
        result := SftObToString(SftOb);
      end;
    osLkt:
      begin
        result := LktObToString(LktOb);
      end;
    osMwt:
      begin
        result := MwtObToString(MwtOb);
      end;
    osUzt:
      begin
        result := UztObToString(UztOb);
      end;
    else
      begin
        Assert(False);
      end;
  end;
end;

function TMf6CalibrationObs.ObservationType: string;
begin
  result := 'MODFLOW 6 Observation'
end;

procedure TMf6CalibrationObs.SetCSubOb(const Value: TCSubOb);
begin
  if FCSubOb <> Value then
  begin
    FCSubOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetMawConnectionNumber(const Value: Integer);
begin
  SetIntegerProperty(FMawConnectionNumber, Value);
end;

procedure TMf6CalibrationObs.SetLakOb(const Value: TLakOb);
begin
  if FLakOb <> Value then
  begin
    FLakOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetLktOb(const Value: TLktOb);
begin
  if FLktOb <> Value then
  begin
    FLktOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetMawOb(const Value: TMawOb);
begin
  if FMawOb <> Value then
  begin
    FMawOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetMwtOb(const Value: TMwtOb);
begin
  if FMwtOb <> Value then
  begin
    FMwtOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetObGeneral(const Value: TObGeneral);
begin
  if FObGeneral <> Value then
  begin
    FObGeneral := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetGwtOb(const Value: TObGwt);
begin
  if FGwtOb <> Value then
  begin
    FGwtOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetObSeries(const Value: TObSeries);
begin
  if FObSeries <> Value then
  begin
    FObSeries := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetObsTypeIndex(Value: Integer);
begin
  if Value <= Ord(High(TObGeneral)) then
  begin
    ObSeries := osGeneral;
    ObGeneral := TObGeneral(Value);
    Exit;
  end;
  Value := Value - Ord(High(TObGeneral)) - 1;

  if Value <= Ord(High(TMawOb)) then
  begin
    ObSeries := osMaw;
    MawOb := TMawOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TMawOb)) - 1;

  if Value <= Ord(High(TSfrOb)) then
  begin
    ObSeries := osSfr;
    SfrOb := TSfrOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TSfrOb)) - 1;

  if Value <= Ord(High(TLakOb)) then
  begin
    ObSeries := osLak;
    LakOb := TLakOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TLakOb)) - 1;

  if Value <= Ord(High(TUzfOb)) then
  begin
    ObSeries := osUzf;
    UzfOb := TUzfOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TUzfOb)) - 1;

  if Value <= Ord(High(TCSubOb)) then
  begin
    ObSeries := osCSub;
    CSubOb := TCSubOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TCSubOb)) - 1;

  if Value <= Ord(High(TObGwt)) then
  begin
    ObSeries := osGWT;
    GwtOb := TObGwt(Value);
    Exit;
  end;
  Value := Value - Ord(High(TObGwt)) - 1;

  if Value <= Ord(High(TSftOb)) then
  begin
    ObSeries := osSft;
    SftOb := TSftOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TSftOb)) - 1;

  if Value <= Ord(High(TLktOb)) then
  begin
    ObSeries := osLkt;
    LktOb := TLktOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TLktOb)) - 1;

  if Value <= Ord(High(TMwtOb)) then
  begin
    ObSeries := osMwt;
    MwtOb := TMwtOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TMwtOb)) - 1;

  if Value <= Ord(High(TUztOb)) then
  begin
    ObSeries := osUzt;
    UztOb := TUztOb(Value);
    Exit;
  end;
  Value := Value - Ord(High(TMwtOb)) - 1;

  Assert(False);
end;

procedure TMf6CalibrationObs.SetObsTypeString(const Value: string);
var
  ObGen: TObGeneral;
  ObMaw: TMawOb;
  ObSfr: TSfrOb;
  ObLake: TLakOb;
  ObUzf: TUzfOb;
  ObCSub: TCSubOb;
  ObGwt: TObGwt;
  ObSft: TSftOb;
  ObLkt: TLktOb;
  ObMwt: TMwtOb;
  ObUzt: TUztOb;
begin
  case ObSeries of
    osGeneral:
      begin
        if TryGetGenOb(Value, ObGen) then
        begin
          ObGeneral := ObGen;
          Exit;
        end;
      end;
    osMaw:
      begin
        if TryGetMawOb(Value, ObMaw) then
        begin
          MawOb := ObMaw;
          Exit;
        end;
      end;
    osSfr:
      begin
        if TryGetSfrOb(Value, ObSfr) then
        begin
          SfrOb := ObSfr;
          Exit;
        end;
      end;
    osLak:
      begin
        if TryGetLakOb(Value, ObLake) then
        begin
          LakOb := ObLake;
          Exit;
        end;
      end;
    osUzf:
      begin
        if TryGetUzfOb(Value, ObUzf) then
        begin
          UzfOb := ObUzf;
          Exit;
        end;
      end;
    osCSub:
      begin
        if TryGetCSubOb(Value, ObCSub) then
        begin
          CSubOb := ObCSub;
          Exit;
        end;
      end;
    osGwt:
      begin
        if TryGetGwtOb(Value, ObGwt) then
        begin
          GwtOb := ObGwt;
          Exit;
        end;
      end;
    osSft:
      begin
        if TryGetSftOb(Value, ObSft) then
        begin
          SftOb := ObSft;
          Exit;
        end;
      end;
    osLkt:
      begin
        if TryGetLktOb(Value, ObLkt) then
        begin
          LktOb := ObLkt;
          Exit;
        end;
      end;
    osMwt:
      begin
        if TryGetMwtOb(Value, ObMwt) then
        begin
          MwtOb := ObMwt;
          Exit;
        end;
      end;
    osUzt:
      begin
        if TryGetUztOb(Value, ObUzt) then
        begin
          UztOb := ObUzt;
          Exit;
        end;
      end;
    else
      begin
        Assert(False);
      end;
  end;

  if TryGetGenOb(Value, ObGen) then
  begin
    ObGeneral := ObGen;
    ObSeries := osGeneral;
  end
  else if TryGetMawOb(Value, ObMaw) then
  begin
    MawOb := ObMaw;
    ObSeries := osMaw;
  end
  else if TryGetSfrOb(Value, ObSfr) then
  begin
    SfrOb := ObSfr;
    ObSeries := osSfr;
  end
  else if TryGetLakOb(Value, ObLake) then
  begin
    LakOb := ObLake;
    ObSeries := osLak;
  end
  else if TryGetUzfOb(Value, ObUzf) then
  begin
    UzfOb := ObUzf;
    ObSeries := osUzf;
  end
  else if TryGetCSubOb(Value, ObCSub) then
  begin
    CSubOb := ObCSub;
    ObSeries := osCSub;
  end
  else if TryGetGwtOb(Value, ObGwt) then
  begin
    GwtOb := ObGwt;
    ObSeries := osGwt;
  end
  else if TryGetSftOb(Value, ObSft) then
  begin
    SftOb := ObSft;
    ObSeries := osSft;
  end
  else if TryGetLktOb(Value, ObLkt) then
  begin
    LktOb := ObLkt;
    ObSeries := osLkt;
  end
  else if TryGetMwtOb(Value, ObMwt) then
  begin
    MwtOb := ObMwt;
    ObSeries := osMwt;
  end
  else if TryGetUztOb(Value, ObUzt) then
  begin
    UztOb := ObUzt;
    ObSeries := osUzt;
  end
  else
  begin
    Assert(False);
  end;
end;

procedure TMf6CalibrationObs.SetSfrOb(const Value: TSfrOb);
begin
  if FSfrOb <> Value then
  begin
    FSfrOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetSftOb(const Value: TSftOb);
begin
  if FSftOb <> Value then
  begin
    FSftOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetSpeciesIndex(const Value: TSpecies);
begin
  if FSpeciesIndex <> Value then
  begin
    FSpeciesIndex := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetUzfOb(const Value: TUzfOb);
begin
  if FUzfOb <> Value then
  begin
    FUzfOb := Value;
    InvalidateModel;
  end;
end;

procedure TMf6CalibrationObs.SetUztOb(const Value: TUztOb);
begin
  if FUztOb <> Value then
  begin
    FUztOb := Value;
    InvalidateModel;
  end;
end;

function TMf6CalibrationObs.StoreCSubOb: Boolean;
begin
  result := ObSeries = osCSub;
end;

function TMf6CalibrationObs.StoreLakOb: Boolean;
begin
  result := ObSeries = osLak;
end;

function TMf6CalibrationObs.StoreLktOb: Boolean;
begin
  result := ObSeries = osLkt;
end;

function TMf6CalibrationObs.StoreMawOb: Boolean;
begin
  result := ObSeries = osMaw;
end;

function TMf6CalibrationObs.StoreMwtOb: Boolean;
begin
  result := ObSeries = osMwt;
end;

function TMf6CalibrationObs.StoreObGeneral: Boolean;
begin
  result := ObSeries = osGeneral;
end;

function TMf6CalibrationObs.StoreGwtOb: Boolean;
begin
  result := ObSeries = osGWT;
end;

function TMf6CalibrationObs.StoreSfrOb: Boolean;
begin
  result := ObSeries = osSfr;
end;

function TMf6CalibrationObs.StoreSftOb: Boolean;
begin
  result := ObSeries = osSft;
end;

function TMf6CalibrationObs.StoreUzfOb: Boolean;
begin
  result := ObSeries = osUzf;
end;

function TMf6CalibrationObs.StoreUztOb: Boolean;
begin
  result := ObSeries = osUzt;
end;

{ TMf6CalibrationObservations }

function TMf6CalibrationObservations.Add: TMf6CalibrationObs;
begin
  result := inherited Add as TMf6CalibrationObs;
end;

procedure TMf6CalibrationObservations.Assign(Source: TPersistent);
begin
  if Source is TMf6CalibrationObservations then
  begin
    MultiLayer := TMf6CalibrationObservations(Source).MultiLayer;
  end;
  inherited;
end;

constructor TMf6CalibrationObservations.Create(
  InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
begin
  inherited Create(TMf6CalibrationObs, InvalidateModelEvent, ScreenObject);
end;

function TMf6CalibrationObservations.GetCalibItem(
  Index: Integer): TMf6CalibrationObs;
begin
  result := inherited Items[Index] as TMf6CalibrationObs;
end;

function TMf6CalibrationObservations.GetGwtObs(Species: Integer): TObGwts;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if (Item.ObSeries = osGwt) and (Item.SpeciesIndex = Species) then
    begin
      Include(result, Item.GwtOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetLakObs: TLakObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osLak then
    begin
      Include(result, Item.LakOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetLktObs(Species: Integer): TLktObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if (Item.ObSeries = osLkt) and (Item.SpeciesIndex = Species) then
    begin
      Include(result, Item.LktOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetMawObs: TMawObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osMaw then
    begin
      Include(result, Item.MawOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetMwtObs(Species: Integer): TMwtObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osMwt then
    begin
      Include(result, Item.MwtOB);
    end;
  end;
end;

function TMf6CalibrationObservations.GetObGenerals: TObGenerals;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osGeneral then
    begin
      Include(result, Item.ObGeneral);
    end;
  end;
end;

function TMf6CalibrationObservations.GetSfrObs: TSfrObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osSfr then
    begin
      Include(result, Item.SfrOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetSftObs(Species: Integer): TSftObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if (Item.ObSeries = osSft) and (Item.SpeciesIndex = Species) then
    begin
      Include(result, Item.SftOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetSubObsSet: TSubObsSet;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osCSub then
    begin
      Include(result, Item.CSubOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetUzfObs: TUzfObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osUzf then
    begin
      Include(result, Item.UzfOb);
    end;
  end;
end;

function TMf6CalibrationObservations.GetUztObs(Species: Integer): TUztObs;
var
  Index: Integer;
  Item: TMf6CalibrationObs;
begin
  result := [];
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.ObSeries = osUzt then
    begin
      Include(result, Item.UztOb);
    end;
  end;
end;

function TMf6CalibrationObservations.IndexOfTimeAndType(ATime: double;
  ObGeneralType: TObGeneral): integer;
var
  Index: Integer;
  ObItem: TMf6CalibrationObs;
begin
  result := -1;
  for Index := 0 to Count - 1 do
  begin
    ObItem := Items[Index];
    if (ObItem.Time = ATime) and (ObItem.ObSeries = osGeneral)
      and (ObItem.ObGeneral = ObGeneralType) then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

procedure TMf6CalibrationObservations.SetCalibItem(Index: Integer;
  const Value: TMf6CalibrationObs);
begin
  inherited Items[Index] := Value;
end;

procedure TMf6CalibrationObservations.SetMultiLayer(const Value: Boolean);
begin
  if FMultiLayer <> Value then
  begin
    FMultiLayer := Value;
    InvalidateModel;
  end;
end;

function TMf6CalibrationObservations.UsesMawConnectionNumber(
  ConnectionNumber: Integer; AnObsType: TMwtOb): Boolean;
var
  ItemIndex: Integer;
  AnItem: TMf6CalibrationObs;
begin
  result := False;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex];
    if (AnItem.ObSeries = osMwt) and (AnItem.MwtOb = AnObsType)
      and (ConnectionNumber = AnItem.MawConnectionNumber) then
    begin
      result := True;
      break;
    end;
  end;
end;

function TMf6CalibrationObservations.UsesMawConnectionNumber(
  ConnectionNumber: Integer; AnObsType: TMawOb): Boolean;
var
  ItemIndex: Integer;
  AnItem: TMf6CalibrationObs;
begin
  result := False;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex];
    if (AnItem.ObSeries = osMaw) and (AnItem.MawOb = AnObsType)
      and (ConnectionNumber = AnItem.MawConnectionNumber) then
    begin
      result := True;
      break;
    end;
  end;
end;

initialization
  InitializeSeriesNames;
  InitializeObGenNames;
  InitializeObConcNames;

finalization
  ObsSeriesNames.Free;
  ObGenNames.Free;
  ObConcNames.Free;

end.
