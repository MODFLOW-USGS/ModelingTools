unit Modflow6ObsUnit;

interface

uses
  System.Classes, GoPhastTypes, System.SysUtils, ModflowMawUnit,
  ModflowSfr6Unit, ModflowLakMf6Unit, ModflowUzfMf6Unit,
  ModflowCsubUnit;

type
  TGwFlowOb = (gfoNearestNeighbor, gfoAllNeighbors, gfoAbove, gfoBelow);
  TGwFlowObs = set of TGwFlowOb;

  TModflow6Obs = class(TGoPhastPersistent)
  private
    FHeadObs: Boolean;
    FGroundwaterFlowObs: Boolean;
    FGwFlowObsChoices: TGwFlowObs;
    FDrawdownObs: Boolean;
    FUsed: Boolean;
    FName: string;
    FChdFlowObs: Boolean;
    FDrnFlowObs: Boolean;
    FWelFlowObs: Boolean;
    FGhbFlowObs: Boolean;
    FRivFlowObs: Boolean;
    FRchFlowObs: Boolean;
    FEvtFlowObs: Boolean;
    FMawObs: TMawObs;
    FSfrObs: TSfrObs;
    FLakObs: TLakObs;
    FSfrObsLocation: TSfrObsLocation;
    FToMvrFlowObs: Boolean;
    FStoredUzfObsDepthFraction: TRealStorage;
    FUzfObs: TUzfObs;
    FCSubObs: TCSubObs;
    procedure SetDrawdownObs(const Value: Boolean);
    procedure SetGroundwaterFlowObs(const Value: Boolean);
    procedure SetGwFlowObsChoices(const Value: TGwFlowObs);
    procedure SetHeadObs(const Value: Boolean);
    procedure SetUsed(const Value: Boolean);
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
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property UzfObsDepthFraction: double read GetUzfObsDepthFraction
      write SetUzfObsDepthFraction;
  published
    property Name: string read FName write SetName;
    property Used: Boolean read FUsed write SetUsed;
    property HeadObs: Boolean read FHeadObs write SetHeadObs;
    property DrawdownObs: Boolean read FDrawdownObs write SetDrawdownObs;
    property GroundwaterFlowObs: Boolean read FGroundwaterFlowObs
      write SetGroundwaterFlowObs;
    property GwFlowObsChoices: TGwFlowObs read FGwFlowObsChoices
      write SetGwFlowObsChoices;
    property ChdFlowObs: Boolean read FChdFlowObs write SetChdFlowObs;
    property DrnFlowObs: Boolean read FDrnFlowObs write SetDrnFlowObs;
    property GhbFlowObs: Boolean read FGhbFlowObs write SetGhbFlowObs;
    property RivFlowObs: Boolean read FRivFlowObs write SetRivFlowObs;
    property WelFlowObs: Boolean read FWelFlowObs write SetWelFlowObs;
    property RchFlowObs: Boolean read FRchFlowObs write SetRchFlowObs;
    property EvtFlowObs: Boolean read FEvtFlowObs write SetEvtFlowObs;
    property ToMvrFlowObs: Boolean read FToMvrFlowObs write SetToMvrFlowObs;
    property MawObs: TMawObs read FMawObs write SetMawObs;
    property SfrObs: TSfrObs read FSfrObs write SetSfrObs;
    property LakObs: TLakObs read FLakObs write SetLakObs;
    property SfrObsLocation: TSfrObsLocation read FSfrObsLocation write SetSfrObsLocation;
    property UzfObs: TUzfObs read FUzfObs write SetUzfObs;
    property CSubObs: TCSubObs read FCSubObs write SetCSubObs;
    property StoredUzfObsDepthFraction: TRealStorage read FStoredUzfObsDepthFraction write SetStoredUzfObsDepthFraction;
  end;

implementation

uses
  System.Character;

{ TModflow6Obs }

procedure TModflow6Obs.Assign(Source: TPersistent);
var
  SourceObs: TModflow6Obs;
begin
  if Source is TModflow6Obs then
  begin
    SourceObs := TModflow6Obs(Source);
    Used := SourceObs.Used;
    Name := SourceObs.Name;

    HeadObs := SourceObs.HeadObs;
    DrawdownObs := SourceObs.DrawdownObs;
    GroundwaterFlowObs := SourceObs.GroundwaterFlowObs;
    GwFlowObsChoices := SourceObs.GwFlowObsChoices;

    ChdFlowObs := SourceObs.ChdFlowObs;
    DrnFlowObs := SourceObs.DrnFlowObs;
    GhbFlowObs := SourceObs.GhbFlowObs;
    RivFlowObs := SourceObs.RivFlowObs;
    WelFlowObs := SourceObs.WelFlowObs;
    RchFlowObs := SourceObs.RchFlowObs;
    EvtFlowObs := SourceObs.EvtFlowObs;
    ToMvrFlowObs := SourceObs.ToMvrFlowObs;

    MawObs := SourceObs.MawObs;
    SfrObs := SourceObs.SfrObs;
    LakObs := SourceObs.LakObs;
    UzfObs := SourceObs.UzfObs;
    CSubObs := SourceObs.CSubObs;
    UzfObsDepthFraction := SourceObs.UzfObsDepthFraction;

    SfrObsLocation := SourceObs.SfrObsLocation;
    UzfObsDepthFraction := SourceObs.UzfObsDepthFraction;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflow6Obs.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FGwFlowObsChoices := [gfoNearestNeighbor];
  FStoredUzfObsDepthFraction := TRealStorage.Create;
  FStoredUzfObsDepthFraction.OnChange := OnInvalidateModel;
end;

destructor TModflow6Obs.Destroy;
begin
  FStoredUzfObsDepthFraction.Free;
  inherited;
end;

function TModflow6Obs.GetUzfObsDepthFraction: double;
begin
  result := FStoredUzfObsDepthFraction.Value;
end;

procedure TModflow6Obs.SetChdFlowObs(const Value: Boolean);
begin
  SetBooleanProperty(FChdFlowObs, Value);
end;

procedure TModflow6Obs.SetCSubObs(const Value: TCSubObs);
begin
  if FCSubObs <> Value then
  begin
    FCSubObs := Value;
    InvalidateModel;
  end;
end;

procedure TModflow6Obs.SetDrawdownObs(const Value: Boolean);
begin
  SetBooleanProperty(FDrawdownObs, Value);
end;

procedure TModflow6Obs.SetDrnFlowObs(const Value: Boolean);
begin
  SetBooleanProperty(FDrnFlowObs, Value);
end;

procedure TModflow6Obs.SetEvtFlowObs(const Value: Boolean);
begin
  SetBooleanProperty(FEvtFlowObs, Value);
end;

procedure TModflow6Obs.SetGhbFlowObs(const Value: Boolean);
begin
  SetBooleanProperty(FGhbFlowObs, Value);
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
  SetBooleanProperty(FHeadObs, Value);
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
  if Length(Value) > 40 then
  begin
    Value := Copy(Value, 1, 40);
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
  SetBooleanProperty(FRchFlowObs, Value);
end;

procedure TModflow6Obs.SetRivFlowObs(const Value: Boolean);
begin
  SetBooleanProperty(FRivFlowObs, Value);
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
  SetBooleanProperty(FToMvrFlowObs, Value);
end;

procedure TModflow6Obs.SetUsed(const Value: Boolean);
begin
  SetBooleanProperty(FUsed, Value);
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
  SetBooleanProperty(FWelFlowObs, Value);
end;

end.
