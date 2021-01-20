unit Modflow6ObsUnit;

interface

uses
  System.Classes, GoPhastTypes, System.SysUtils, ModflowMawUnit,
  ModflowSfr6Unit, ModflowLakMf6Unit, ModflowUzfMf6Unit,
  ModflowCsubUnit, PestObsUnit, FormulaManagerUnit, System.Generics.Collections;

type
  TGwFlowOb = (gfoNearestNeighbor, gfoAllNeighbors, gfoAbove, gfoBelow);
  TGwFlowObs = set of TGwFlowOb;

  TObGeneral = (ogHead, ogDrawdown, ogCHD, ogDrain, ogWell, ogGHB, ogRiv,
    ogRch, ogEVT, ogMvr, ogUndefined);
  TObGenerals = set of TObGeneral;

  TObSeries = (osGeneral, osMaw, osSfr, osLak, osUzf, osCSub);

  TMf6CalibrationObs = class(TCustomTimeObservationItem)
  private
    FUzfOb: TUzfOb;
    FSfrOb: TSfrOb;
    FCSubOb: TCSubOb;
    FObSeries: TObSeries;
    FMawOb: TMawOb;
    FLakOb: TLakOb;
    FObGeneral: TObGeneral;
//    FWeightFormula: TFormulaObject;
    FInterpObsNames: TStringList;
    FMawConnectionNumber: Integer;
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
//    function CreateFormulaObject: TFormulaObject;
//    procedure UpdateFormula(Value: string; Position: integer;
//      var FormulaObject: TFormulaObject);
    procedure SetMawConnectionNumber(const Value: Integer);
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
    property ObGeneral: TObGeneral read FObGeneral write SetObGeneral stored StoreObGeneral;
    property MawOb: TMawOb read FMawOb write SetMawOb stored StoreMawOb;
    property SfrOb: TSfrOb read FSfrOb write SetSfrOb stored StoreSfrOb;
    property LakOb: TLakOb read FLakOb write SetLakOb stored StoreLakOb;
    property UzfOb: TUzfOb read FUzfOb write SetUzfOb stored StoreUzfOb;
    property CSubOb: TCSubOb read FCSubOb write SetCSubOb stored StoreCSubOb;
    property MawConnectionNumber: Integer read FMawConnectionNumber
      write SetMawConnectionNumber;
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
    property Items[Index: Integer]: TMf6CalibrationObs read GetCalibItem
      write SetCalibItem; default;
    function UsesMawConnectionNumber(ConnectionNumber: Integer;
      AnObsType: TMawOb): Boolean;
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
    property ScreenObject: TObject read FScreenObject;
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
    property CSubDelayCells: TIntegerCollection read FCSubDelayCells
      write SetCSubDelayCells;
    property StoredUzfObsDepthFraction: TRealStorage
      read FStoredUzfObsDepthFraction write SetStoredUzfObsDepthFraction;
    property CalibrationObservations: TMf6CalibrationObservations
      read FCalibrationObservations write SetCalibrationObservations
  {$IFNDEF PEST}
    stored False
  {$ELSE}
    stored StoreCalibObs
  {$ENDIF}
      ;
//    property Used: Boolean read GetUsed write SetUsed;// stored False;
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
function GenObToString(const GenOb: TObGeneral): string;
Procedure FillObGenSeriesNames(AList: TStrings);

function TryGetObsSeries(const SeriesName: string; var ObSeries: TObSeries): Boolean;
function ObsSeriesToString(const ObSeries: TObSeries): string;

procedure GlobalRemoveMf6CalibrationObsSubscription(Sender: TObject; Subject: TObject;
  const AName: string);
procedure GlobalRestoreMf6CalibrationObsSubscription(Sender: TObject; Subject: TObject;
  const AName: string);

implementation

uses
  System.Character, frmGoPhastUnit, RbwParser;

const
  ObGenName: array[TObGeneral] of string = ('Head', 'Drawdown', 'CHD', 'Drain', 'Well', 'GHB', 'Riv',
    'Rch', 'EVT', 'Mvr', 'undefined');

  ObSeriesName: array[TObSeries] of string = ('General', 'Maw', 'Sfr', 'Lak', 'Uzf', 'CSub');

var
  ObGenNames: TStringList;
  ObsSeriesNames: TStringList;

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

function GenObToString(const GenOb: TObGeneral): string;
begin
  result := ObGenName[GenOb];
end;

Procedure FillObGenSeriesNames(AList: TStrings);
begin
//  AList.Clear;
//  AList.Add(ObGenNames[Ord(ogHead)]);
//  AList.Add(ObGenNames[Ord(ogDrawdown)]);
  AList.Assign(ObGenNames);
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
    General := SourceObs.General + CalibrationObservations.ObGenerals;

    GroundwaterFlowObs := SourceObs.GroundwaterFlowObs;
    GwFlowObsChoices := SourceObs.GwFlowObsChoices;

    MawObs := SourceObs.MawObs + CalibrationObservations.MawObs;
    SfrObs := SourceObs.SfrObs + CalibrationObservations.SfrObs;
    LakObs := SourceObs.LakObs + CalibrationObservations.LakObs;
    UzfObs := SourceObs.UzfObs + CalibrationObservations.UzfObs;
    SourceObs.CSubObs.CSubObsSet
      := SourceObs.CSubObs.CSubObsSet + CalibrationObservations.SubObsSet;
    CSubObs := SourceObs.CSubObs;

    CSubDelayCells := SourceObs.CSubDelayCells;
    UzfObsDepthFraction := SourceObs.UzfObsDepthFraction;

    SfrObsLocation := SourceObs.SfrObsLocation;
    UzfObsDepthFraction := SourceObs.UzfObsDepthFraction;

    CalibrationObservations := SourceObs.CalibrationObservations;
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

function TModflow6Obs.GetGhbFlowObs: Boolean;
begin
  result := ogGHB in General;
end;

function TModflow6Obs.GetHeadObs: Boolean;
begin
  result := ogHead in General;
end;

function TModflow6Obs.GetLakObs: TLakObs;
begin
  result := FLakObs + CalibrationObservations.LakObs;
end;

function TModflow6Obs.GetMawObs: TMawObs;
begin
  result := FMawObs + CalibrationObservations.MawObs;
end;

function TModflow6Obs.GetName: string;
begin
  if FName <> '' then
  begin
    if FName[1] in ['0'..'9'] then
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

function TModflow6Obs.GetToMvrFlowObs: Boolean;
begin
  result := ogMvr in General;
end;

function TModflow6Obs.GetUsed: Boolean;
begin
  result := (General <> []) or {HeadObs or DrawdownObs or} GroundwaterFlowObs {or ChdFlowObs
    or DrnFlowObs or GhbFlowObs or RivFlowObs or WelFlowObs or RchFlowObs
    or EvtFlowObs  or ToMvrFlowObs} or (MawObs <> []) or (SfrObs <> [])
    or (LakObs <> []) or (UzfObs <> []) or (CSubObs.CSubObsSet <> [])
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
//  SetBooleanProperty(FChdFlowObs, Value);
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
//  SetBooleanProperty(FDrawdownObs, Value);
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
//  SetBooleanProperty(FDrnFlowObs, Value);
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
//  SetBooleanProperty(FEvtFlowObs, Value);
end;

procedure TModflow6Obs.SetGeneral(const Value: TObGenerals);
begin
  if FGeneral <> Value then
  begin
    FGeneral := Value;
    InvalidateModel;
  end;
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
//  SetBooleanProperty(FGhbFlowObs, Value);
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

procedure TModflow6Obs.SetMawObs(const Value: TMawObs);
begin
  if FMawObs <> Value then
  begin
    FMawObs := Value;
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
//  SetBooleanProperty(FRchFlowObs, Value);
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
//  SetBooleanProperty(FRivFlowObs, Value);
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
//  SetBooleanProperty(FToMvrFlowObs, Value);
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
//  SetBooleanProperty(FWelFlowObs, Value);
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
//    WeightFormula := ObsSource.WeightFormula;
    MawConnectionNumber := ObsSource.MawConnectionNumber;
  end;
  inherited;
end;

constructor TMf6CalibrationObs.Create(Collection: TCollection);
begin
  inherited;
//  CreateFormulaObjects;
  FInterpObsNames := TStringList.Create;
end;

//function TMf6CalibrationObs.CreateFormulaObject: TFormulaObject;
//begin
//  result := frmGoPhast.PhastModel.FormulaManager.Add;
//  result.Parser := frmGoPhast.PhastModel.rpThreeDFormulaCompiler;
//  result.AddSubscriptionEvents(
//    GlobalRemoveMf6CalibrationObsSubscription,
//    GlobalRestoreMf6CalibrationObsSubscription, self);
//end;

//procedure TMf6CalibrationObs.CreateFormulaObjects;
//begin
////  FWeightFormula := CreateFormulaObject;
//end;

destructor TMf6CalibrationObs.Destroy;
begin
  FInterpObsNames.Free;
//  RemoveFormulaObjects;
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
    else
      begin
        Assert(False);
      end;
  end;
end;

//function TMf6CalibrationObs.GetUsed: Boolean;
//begin
//  result := False;
//end;

//function TMf6CalibrationObs.GetWeightFormula: string;
//begin
//  Result := FWeightFormula.Formula;
////  ResetItemObserver(EndHeadPosition);
//end;

function TMf6CalibrationObs.ObservationType: string;
begin
  result := 'MODFLOW 6 Observation'
end;

//procedure TMf6CalibrationObs.RemoveFormulaObjects;
//begin
////  frmGoPhast.PhastModel.FormulaManager.Remove(FWeightFormula,
////    GlobalRemoveMf6CalibrationObsSubscription,
////    GlobalRestoreMf6CalibrationObsSubscription, self);
//end;

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

procedure TMf6CalibrationObs.SetMawOb(const Value: TMawOb);
begin
  if FMawOb <> Value then
  begin
    FMawOb := Value;
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

//procedure TMf6CalibrationObs.SetUsed(const Value: Boolean);
//begin
//
//end;

procedure TMf6CalibrationObs.SetUzfOb(const Value: TUzfOb);
begin
  if FUzfOb <> Value then
  begin
    FUzfOb := Value;
    InvalidateModel;
  end;
end;

//procedure TMf6CalibrationObs.SetWeightFormula(const Value: string);
//begin
//  UpdateFormula(Value, 0, FWeightFormula);
//end;

function TMf6CalibrationObs.StoreCSubOb: Boolean;
begin
  result := ObSeries = osCSub;
end;

function TMf6CalibrationObs.StoreLakOb: Boolean;
begin
  result := ObSeries = osLak;
end;

function TMf6CalibrationObs.StoreMawOb: Boolean;
begin
  result := ObSeries = osMaw;
end;

function TMf6CalibrationObs.StoreObGeneral: Boolean;
begin
  result := ObSeries = osGeneral;
end;

function TMf6CalibrationObs.StoreSfrOb: Boolean;
begin
  result := ObSeries = osSfr;
end;

function TMf6CalibrationObs.StoreUzfOb: Boolean;
begin
  result := ObSeries = osUzf;
end;

//procedure TMf6CalibrationObs.UpdateFormula(Value: string; Position: integer;
//  var FormulaObject: TFormulaObject);
////var
////  ParentModel: TPhastModel;
////  Compiler: TRbwParser;
////  LocalObserver: TObserver;
//begin
//  if FormulaObject.Formula <> Value then
//  begin
////    ParentModel := Model as TPhastModel;
////    if ParentModel <> nil then
////    begin
////      Compiler := ParentModel.rpThreeDFormulaCompiler;
////      LocalObserver := Observer[Position];
////      UpdateFormulaDependencies(FormulaObject.Formula, Value, LocalObserver,
////        Compiler);
////    end;
//    InvalidateModel;
//    if not(csDestroying in frmGoPhast.PhastModel.ComponentState) and
//      not frmGoPhast.PhastModel.Clearing then
//    begin
//      frmGoPhast.PhastModel.FormulaManager.ChangeFormula(FormulaObject, Value,
//        frmGoPhast.PhastModel.rpThreeDFormulaCompiler,
//        GlobalRemoveMf6CalibrationObsSubscription, GlobalRestoreMf6CalibrationObsSubscription, self);
//    end;
//  end;
//end;

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

finalization
  ObsSeriesNames.Free;
  ObGenNames.Free;

end.
