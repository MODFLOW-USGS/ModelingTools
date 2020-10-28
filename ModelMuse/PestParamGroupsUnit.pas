unit PestParamGroupsUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TIncrementType = (icRelative, icAbsolute, icRelativeToMax);
  TForceCentral = (fcAlways2, fcAlways3, fcAlways5, fcSwitch, fcSwitch5);
  TDerivativeMethod3 = (dm3Parabolic, dm3BestFit, dm3OutsidePoints);
  TDerivativeMethod5 = (dm5MinimumVariance, dm5MaxPrecision);
  TSplitAction = (saSmaller, saZero, saPrevious);

  TPestParamGroup = class(TPhastCollectionItem)
  private
    FUseSplitSlopeAnalysis: Boolean;
    FStoredMinParamIncrement: TRealStorage;
    FStoredParamIncrementMultiplier: TRealStorage;
    FParamGroupName: string;
    FDM3: TDerivativeMethod3;
    FSplitAction: TSplitAction;
    FDM5: TDerivativeMethod5;
    FStoredSplitThreshold: TRealStorage;
    FStoredRelSlopeDif: TRealStorage;
    FForceCentral: TForceCentral;
    FStoredParamIncrement: TRealStorage;
    FIncrementType: TIncrementType;
    function GetMinParamIncrement: double;
    function GetParamIncrement: double;
    function GetParamIncrementMultiplier: Double;
    procedure SetDM3(const Value: TDerivativeMethod3);
    procedure SetDM5(const Value: TDerivativeMethod5);
    procedure SetForceCentral(const Value: TForceCentral);
    procedure SetIncrementType(const Value: TIncrementType);
    procedure SetMinParamIncrement(const Value: double);
    procedure SetParamGroupName(const Value: string);
    procedure SetParamIncrement(const Value: double);
    procedure SetParamIncrementMultiplier(const Value: Double);
    procedure SetSplitAction(const Value: TSplitAction);
    procedure SetStoredMinParamIncrement(const Value: TRealStorage);
    procedure SetStoredParamIncrement(const Value: TRealStorage);
    procedure SetStoredParamIncrementMultiplier(const Value: TRealStorage);
    procedure SetStoredRelSlopeDif(const Value: TRealStorage);
    procedure SetStoredSplitThreshold(const Value: TRealStorage);
    procedure SetUseSplitSlopeAnalysis(const Value: Boolean);
    function GetRelSlopeDif: double;
    function GetSplitThreshold: double;
    procedure SetRelSlopeDif(const Value: double);
    procedure SetSplitThreshold(const Value: double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeVariables;
    // DERINC
    property ParamIncrement: double read GetParamIncrement
      write SetParamIncrement;
    // DERINCLB
    property MinParamIncrement: double read GetMinParamIncrement
      write SetMinParamIncrement;
    // DERINCMUL
    property ParamIncrementMultiplier: Double read GetParamIncrementMultiplier
      write SetParamIncrementMultiplier;
    // SPLITHRESH,
    property SplitThreshold: double read GetSplitThreshold write SetSplitThreshold;
    // SPLITRELDIFF
    property RelSlopeDif: double read GetRelSlopeDif write SetRelSlopeDif;
    // SPLITACTION
  published
    // PARGPNME 6 characters or less.
    property ParamGroupName: string read FParamGroupName write SetParamGroupName;
    // INCTYP
    property IncrementType: TIncrementType read FIncrementType write SetIncrementType;
    // DERINC
    property StoredParamIncrement: TRealStorage read FStoredParamIncrement write SetStoredParamIncrement;
    // DERINCLB
    property StoredMinParamIncrement: TRealStorage read FStoredMinParamIncrement write SetStoredMinParamIncrement;
    // FORCEN
    property ForceCentral: TForceCentral read FForceCentral write SetForceCentral;
    // DERINCMUL
    property StoredParamIncrementMultiplier: TRealStorage read FStoredParamIncrementMultiplier write SetStoredParamIncrementMultiplier;
    // DERMTHD
    property DM3: TDerivativeMethod3 read FDM3 write SetDM3;
    // DERMTHD
    property DM5: TDerivativeMethod5 read FDM5 write SetDM5;
    // SPLITHRESH,
    property UseSplitSlopeAnalysis: Boolean read FUseSplitSlopeAnalysis write SetUseSplitSlopeAnalysis;
    // SPLITHRESH,
    property StoredSplitThreshold: TRealStorage read FStoredSplitThreshold write SetStoredSplitThreshold;
    // SPLITRELDIFF
    property StoredRelSlopeDif: TRealStorage read FStoredRelSlopeDif write SetStoredRelSlopeDif;
    // SPLITACTION
    property SplitAction: TSplitAction read FSplitAction write SetSplitAction;
  end;

  TPestParamGroups = class(TPhastCollection)
  private
    function GetParamGroup(Index: Integer): TPestParamGroup;
    procedure SetParamGroup(Index: Integer; const Value: TPestParamGroup);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    function Add: TPestParamGroup;
    property Items[Index: Integer]: TPestParamGroup read GetParamGroup
      write SetParamGroup; default;
  end;

implementation


{ TPestParamGroup }

procedure TPestParamGroup.Assign(Source: TPersistent);
var
  ParamGroup: TPestParamGroup;
begin
  if Source is TPestParamGroup then
  begin
    ParamGroup := TPestParamGroup(Source);
    ParamGroupName := ParamGroup.ParamGroupName;
    IncrementType := ParamGroup.IncrementType;
    ParamIncrement := ParamGroup.ParamIncrement;
    MinParamIncrement := ParamGroup.MinParamIncrement;
    ForceCentral := ParamGroup.ForceCentral;
    ParamIncrementMultiplier := ParamGroup.ParamIncrementMultiplier;
    DM3 := ParamGroup.DM3;
    DM5 := ParamGroup.DM5;
    UseSplitSlopeAnalysis := ParamGroup.UseSplitSlopeAnalysis;
    SplitThreshold := ParamGroup.SplitThreshold;
    RelSlopeDif := ParamGroup.RelSlopeDif;
    SplitAction := ParamGroup.SplitAction;
  end
  else
  begin
    inherited;
  end;

end;

constructor TPestParamGroup.Create(Collection: TCollection);
begin
  inherited;
  FStoredParamIncrement := TRealStorage.Create;
  FStoredMinParamIncrement := TRealStorage.Create;
  FStoredParamIncrementMultiplier := TRealStorage.Create;
  FStoredSplitThreshold := TRealStorage.Create;
  FStoredRelSlopeDif := TRealStorage.Create;

  FStoredParamIncrement.OnChange := OnInvalidateModel;
  FStoredMinParamIncrement.OnChange := OnInvalidateModel;
  FStoredParamIncrementMultiplier.OnChange := OnInvalidateModel;
  FStoredSplitThreshold.OnChange := OnInvalidateModel;
  FStoredRelSlopeDif.OnChange := OnInvalidateModel;
end;

destructor TPestParamGroup.Destroy;
begin
  FStoredParamIncrement.Free;
  FStoredMinParamIncrement.Free;
  FStoredParamIncrementMultiplier.Free;
  FStoredSplitThreshold.Free;
  FStoredRelSlopeDif.Free;
  inherited;
end;

function TPestParamGroup.GetMinParamIncrement: double;
begin
  result := StoredMinParamIncrement.Value;
end;

function TPestParamGroup.GetParamIncrement: double;
begin
  result := StoredParamIncrement.Value;
end;

function TPestParamGroup.GetParamIncrementMultiplier: Double;
begin
  result := StoredParamIncrementMultiplier.Value;
end;

function TPestParamGroup.GetRelSlopeDif: double;
begin
  result := StoredRelSlopeDif.Value;
end;

function TPestParamGroup.GetSplitThreshold: double;
begin
  result := StoredSplitThreshold.Value;
end;

procedure TPestParamGroup.InitializeVariables;
begin
  ParamIncrement := 0.01;
  ParamIncrementMultiplier := 1.5;
  ForceCentral := fcSwitch;
  DM3 := dm3Parabolic;
  SplitThreshold := 1E-4;
  RelSlopeDif := 0.5;
  SplitThreshold := 1E-4;
  SplitAction := saSmaller;
end;

procedure TPestParamGroup.SetDM3(const Value: TDerivativeMethod3);
begin
  if FDM3 <> Value then
  begin
    FDM3 := Value;
    InvalidateModel;
  end;
end;

procedure TPestParamGroup.SetDM5(const Value: TDerivativeMethod5);
begin
  if FDM5 <> Value then
  begin
    FDM5 := Value;
    InvalidateModel;
  end;
end;

procedure TPestParamGroup.SetForceCentral(const Value: TForceCentral);
begin
  if FForceCentral <> Value then
  begin
    FForceCentral := Value;
    InvalidateModel;
  end;
end;

procedure TPestParamGroup.SetIncrementType(const Value: TIncrementType);
begin
  if FIncrementType <> Value then
  begin
    FIncrementType := Value;
    InvalidateModel;
  end;
end;

procedure TPestParamGroup.SetMinParamIncrement(const Value: double);
begin
  StoredMinParamIncrement.Value := Value;
end;

procedure TPestParamGroup.SetParamGroupName(const Value: string);
begin
  SetStringProperty(FParamGroupName, Value);
end;

procedure TPestParamGroup.SetParamIncrement(const Value: double);
begin
  StoredParamIncrement.Value := Value;
end;

procedure TPestParamGroup.SetParamIncrementMultiplier(const Value: Double);
begin
  StoredParamIncrementMultiplier.Value := Value;
end;

procedure TPestParamGroup.SetRelSlopeDif(const Value: double);
begin
  StoredRelSlopeDif.Value := Value;
end;

procedure TPestParamGroup.SetSplitAction(const Value: TSplitAction);
begin
  if FSplitAction <> Value then
  begin
    FSplitAction := Value;
    InvalidateModel;
  end;
end;

procedure TPestParamGroup.SetSplitThreshold(const Value: double);
begin
  StoredSplitThreshold.Value := Value;
  end;

procedure TPestParamGroup.SetStoredMinParamIncrement(const Value: TRealStorage);
begin
  FStoredMinParamIncrement.Assign(Value);
end;

procedure TPestParamGroup.SetStoredParamIncrement(const Value: TRealStorage);
begin
  FStoredParamIncrement.Assign(Value);
end;

procedure TPestParamGroup.SetStoredParamIncrementMultiplier(
  const Value: TRealStorage);
begin
  FStoredParamIncrementMultiplier.Assign(Value);
end;

procedure TPestParamGroup.SetStoredRelSlopeDif(const Value: TRealStorage);
begin
  FStoredRelSlopeDif.Assign(Value);
end;

procedure TPestParamGroup.SetStoredSplitThreshold(const Value: TRealStorage);
begin
  FStoredSplitThreshold.Assign(Value);
end;

procedure TPestParamGroup.SetUseSplitSlopeAnalysis(const Value: Boolean);
begin
  SetBooleanProperty(FUseSplitSlopeAnalysis, Value);
end;

{ TPestParamGroups }

function TPestParamGroups.Add: TPestParamGroup;
begin
  result := inherited Add as TPestParamGroup;
end;

constructor TPestParamGroups.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TPestParamGroup, InvalidateModelEvent);
end;

function TPestParamGroups.GetParamGroup(Index: Integer): TPestParamGroup;
begin
  result := inherited Items[Index] as TPestParamGroup;
end;

procedure TPestParamGroups.SetParamGroup(Index: Integer;
  const Value: TPestParamGroup);
begin
  inherited Items[Index] := Value;
end;

end.
