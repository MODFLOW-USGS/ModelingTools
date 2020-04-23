unit SutraPestObsUnit;

interface

uses
  System.Classes, PestObsUnit;

type
  TSutraObsItem = class(TCustomTimeObservationItem)
  private
    FObsType: string;
    procedure SetObsType(const Value: string);
  protected
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    procedure Assign(Source: TPersistent); override;
    function ObservationType: string; override;
    function Units: string; override;
  published
    property ObsType: string read FObsType write SetObsType stored True;
    property GUID;
  end;

  TSutraPestObservations = class(TCustomComparisonCollection)
  private
    function GetSutraPestItem(Index: Integer): TSutraObsItem;
    procedure SetSutraPestItem(Index: Integer; const Value: TSutraObsItem);
    function GetUsed: Boolean;
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TSutraObsItem read GetSutraPestItem
      write SetSutraPestItem; default;
    function Add: TSutraObsItem;
    property Used: Boolean read GetUsed;
  end;

var
  SutraObsTypes: TStringList;

implementation

procedure InitializeSutraObsTypes;
begin
  SutraObsTypes := TStringList.Create;
  SutraObsTypes.Add('Lake stage');
  SutraObsTypes.Add('Fluid flow at specified pressure nodes');
  SutraObsTypes.Add('Conc/temp at specified pressure nodes');
  SutraObsTypes.Add('Conc/temp at specified flow nodes');
  SutraObsTypes.Add('Fluid flow at generalized flow nodes');
  SutraObsTypes.Add('Conc/temp at generalized flow nodes');
end;

{ TSutraObsItem }

procedure TSutraObsItem.Assign(Source: TPersistent);
var
  SutraObsItem: TSutraObsItem;
begin
  if Source is TSutraObsItem then
  begin
    SutraObsItem := TSutraObsItem(Source);
    ObsType := SutraObsItem.ObsType;
  end;
  inherited;

end;

function TSutraObsItem.GetObsTypeIndex: Integer;
begin
  result := SutraObsTypes.IndexOf(ObsType);
end;

function TSutraObsItem.GetObsTypeString: string;
begin
  Result := ObsType;
end;

function TSutraObsItem.ObservationType: string;
begin
  result := ObsType;
end;

procedure TSutraObsItem.SetObsType(const Value: string);
begin
  if FObsType <> Value then
  begin
    BeginUpdate;
    try
      FObsType := Value;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSutraObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraObsTypes[Value];

end;

procedure TSutraObsItem.SetObsTypeString(const Value: string);
begin
  ObsType := Value;
end;

function TSutraObsItem.Units: string;
begin
  result := 'L';
end;

{ TSutraPestObservations }

function TSutraPestObservations.Add: TSutraObsItem;
begin
  result := inherited Add as TSutraObsItem
end;

constructor TSutraPestObservations.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TSutraObsItem, InvalidateModelEvent, ScreenObject);
end;

function TSutraPestObservations.GetSutraPestItem(Index: Integer): TSutraObsItem;
begin
  result := inherited Items[Index] as TSutraObsItem;
end;

function TSutraPestObservations.GetUsed: Boolean;
begin
  result := Count > 0;
end;

procedure TSutraPestObservations.SetSutraPestItem(Index: Integer;
  const Value: TSutraObsItem);
begin
  Items[Index] := Value;
end;

initialization

finalization
  SutraObsTypes.Free;

end.
