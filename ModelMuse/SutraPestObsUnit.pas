unit SutraPestObsUnit;

interface

uses
  System.Classes, PestObsUnit, FluxObservationUnit, GoPhastTypes;

type
  TCustomSutraObsItem = class(TCustomTimeObservationItem)
  private
    FObsType: string;
    procedure SetObsType(const Value: string);
  protected
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
  public
    procedure Assign(Source: TPersistent); override;
    function ObservationType: string; override;
  published
    property ObsType: string read FObsType write SetObsType stored True;
    property GUID;
  end;

  TCustomSutraObservations = class(TCustomComparisonCollection)
  private
    FModel: TBaseModel;
    function GetUsed: Boolean;
  public
    constructor Create(ItemClass: TCollectionItemClass;
      Model: TBaseModel; ScreenObject: TObject);
    property Model: TBaseModel read FModel;
    property Used: Boolean read GetUsed;
  end;
  
  TSutraStateObsItem = class(TCustomSutraObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraStateObservations = class(TCustomSutraObservations)
  private
    function GetSutraStateObsItem(Index: Integer): TSutraStateObsItem;
    procedure SetSutraStateObsItem(Index: Integer; const Value: TSutraStateObsItem);
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    property Items[Index: Integer]: TSutraStateObsItem read GetSutraStateObsItem
      write SetSutraStateObsItem; default;
    function Add: TSutraStateObsItem;
    function HasNonLakeBoundary: Boolean;
  end;

  TCustomFluxObsItem = class(TCustomSutraObsItem)
  private
    // See @link(ObservationFactors).
    FObservationFactors: TObservationFactors;
    // See @link(ObservationFactors).
    procedure SetObservationFactors(const Value: TObservationFactors);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // @name adds a new @link(TObservationFactor) to @link(ObservationFactors)
    // and makes ScreenObject its @link(TObservationFactor.ScreenObject).
    // If ScreenObject has already been added, it will be skipped.
    function AddObject(ScreenObject: TObject): integer;
    // @name removes the @link(TObservationFactor) from
    // @link(ObservationFactors) that has ScreenObject as its
    // @link(TObservationFactor.ScreenObject).
    procedure RemoveObject(ScreenObject: TObject);
    // @name calls @link(TObservationFactors.Loaded ObservationFactors.Loaded).
    procedure Loaded;
    // @name calls @link(TObservationFactors.EliminatedDeletedScreenObjects
    // ObservationFactors.EliminatedDeletedScreenObjects).
    procedure EliminatedDeletedScreenObjects;
  published
    property ObservationFactors: TObservationFactors read FObservationFactors
      write SetObservationFactors;
  end;

  TCustomSutraFluxObservations = class(TCustomSutraObservations)
  public
    procedure Loaded;
  end;

  TSutraFlFluxObsItem = class(TCustomFluxObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraFlFluxObservations = class(TCustomSutraFluxObservations)
  private
    function GetSutraFlFluxObsItem(Index: Integer): TSutraFlFluxObsItem;
    procedure SetSutraFlFluxObsItem(Index: Integer; const Value: TSutraFlFluxObsItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSutraFlFluxObsItem read GetSutraFlFluxObsItem
      write SetSutraFlFluxObsItem; default;
    function Add: TSutraFlFluxObsItem;
  end;

  TSutraFlFluxObservationGroup = class(TPhastCollectionItem)
  private
    FObsGroup: TSutraFlFluxObservations;
    function GetModel: TBaseModel;
    procedure SetObsGroup(const Value: TSutraFlFluxObservations);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Model: TBaseModel read GetModel;
    procedure Loaded;
  published
    property ObsGroup: TSutraFlFluxObservations read FObsGroup write SetObsGroup;
  end;

  TSutraFlFluxObservationGroups = class(TPhastCollection)
  private
    FModel: TBaseModel;
    function GetSutraFlFluxObsGroup(
      Index: Integer): TSutraFlFluxObservationGroup;
    procedure SetSutraFlFluxObsGroup(Index: Integer;
      const Value: TSutraFlFluxObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    property Model: TBaseModel read FModel;
    procedure Loaded;
    property Items[Index: Integer]: TSutraFlFluxObservationGroup read GetSutraFlFluxObsGroup
      write SetSutraFlFluxObsGroup; default;
    function Add: TSutraFlFluxObservationGroup;
  end;
  
  TSutraUFluxObsItem = class(TCustomFluxObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraUFluxObservations = class(TCustomSutraFluxObservations)
  private
    function GetSutraUFluxObsItem(Index: Integer): TSutraUFluxObsItem;
    procedure SetSutraUFluxObsItem(Index: Integer; const Value: TSutraUFluxObsItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSutraUFluxObsItem read GetSutraUFluxObsItem
      write SetSutraUFluxObsItem; default;
    function Add: TSutraUFluxObsItem;
  end;

  TSutraUFluxObservationGroup = class(TPhastCollectionItem)
  private
    FObsGroup: TSutraUFluxObservations;
    function GetModel: TBaseModel;
    procedure SetObsGroup(const Value: TSutraUFluxObservations);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Model: TBaseModel read GetModel;
    procedure Loaded;
  published
    property ObsGroup: TSutraUFluxObservations read FObsGroup write SetObsGroup;
  end;

  TSutraUFluxObservationGroups = class(TPhastCollection)
  private
    FModel: TBaseModel;
    function GetSutraUFluxObsGroup(Index: Integer): TSutraUFluxObservationGroup;
    procedure SetSutraUFluxObsGroup(Index: Integer;
      const Value: TSutraUFluxObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    property Model: TBaseModel read FModel;
    procedure Loaded;
    property Items[Index: Integer]: TSutraUFluxObservationGroup read GetSutraUFluxObsGroup
      write SetSutraUFluxObsGroup; default;
    function Add: TSutraUFluxObservationGroup;
  end;
  
var
  SutraStateObsTypes: TStringList;
  SutraFlFluxObsTypes: TStringList;
  SutraUFluxObsTypes: TStringList;

const
  StrPressure = 'Pressure';
  StrConcOrTemp = 'Conc or Temp';
  StrSaturation = 'Saturation';
  StrLakeStage = 'Lake stage';

implementation

uses
  PhastModelUnit, ScreenObjectUnit;

procedure InitializeSutraObsTypes;
begin
  SutraStateObsTypes := TStringList.Create;
  SutraStateObsTypes.Add(StrPressure); // single node, use OBC
  SutraStateObsTypes.Add(StrConcOrTemp);  // single node, use OBC
  SutraStateObsTypes.Add(StrSaturation);  // single node, use OBC
//  SutraStateObsTypes.Add('Fluid flow rate at specified pressure nodes'); // Units = Mass/sec, Add selected nodes, use .bcop file
//  SutraStateObsTypes.Add('Fluid flow rate at generalized flow nodes'); // Units = Mass/sec, Add selected nodes, Use .bcopg file
//  SutraStateObsTypes.Add('Resultant U rate at specified pressure nodes'); // Units = solute mass/sec, Add selected nodes, use .bcop file
//  SutraStateObsTypes.Add('Resultant U rate at specified flow nodes'); // Units = solute mass/sec, Add selected nodes, use .bcof file
//  SutraStateObsTypes.Add('Resultant U rate at generalized flow nodes'); // Units = solute mass/sec, Add selected nodes, Use .bcopg file
//  SutraStateObsTypes.Add('Resultant U rate at specified U nodes'); // Units = solute mass/sec, Add selected nodes, Use .bcou file
//  SutraStateObsTypes.Add('Resultant U rate at generalized U nodes'); // Units = solute mass/sec, Add selected nodes, Use .bcoug file
  SutraStateObsTypes.Add(StrLakeStage);  // single node, make NLAKPR = 1, read from .lkst

  SutraFlFluxObsTypes := TStringList.Create;
  SutraFlFluxObsTypes.Add('Fluid flow rate'); // Units = Mass/sec, Add selected nodes
  SutraFlFluxObsTypes.Add('Resultant U rate'); // Units = solute mass/sec, Add selected nodes

  SutraUFluxObsTypes := TStringList.Create;
  SutraUFluxObsTypes.Add('Resultant U rate'); // Units = solute mass/sec, Add selected nodes
end;

{ TSutraStateObsItem }

procedure TCustomSutraObsItem.Assign(Source: TPersistent);
var
  SutraObsItem: TCustomSutraObsItem;
begin
  if Source is TCustomSutraObsItem then
  begin
    SutraObsItem := TCustomSutraObsItem(Source);
    ObsType := SutraObsItem.ObsType;
  end;
  inherited;
end;

function TSutraStateObsItem.GetObsTypeIndex: Integer;
begin
  result := SutraStateObsTypes.IndexOf(ObsType);
end;

function TCustomSutraObsItem.GetObsTypeString: string;
begin
  Result := ObsType;
end;

function TCustomSutraObsItem.ObservationType: string;
begin
  result := ObsType;
end;

procedure TCustomSutraObsItem.SetObsType(const Value: string);
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

procedure TSutraStateObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraStateObsTypes[Value];
end;

procedure TCustomSutraObsItem.SetObsTypeString(const Value: string);
begin
  ObsType := Value;
end;

function TSutraStateObsItem.Units: string;
begin
  result := 'L';
end;

{ TSutraStateObservations }

function TSutraStateObservations.Add: TSutraStateObsItem;
begin
  result := inherited Add as TSutraStateObsItem
end;

constructor TSutraStateObservations.Create(Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited Create(TSutraStateObsItem, Model, ScreenObject);
end;

function TSutraStateObservations.GetSutraStateObsItem(Index: Integer): TSutraStateObsItem;
begin
  result := inherited Items[Index] as TSutraStateObsItem;
end;

function TSutraStateObservations.HasNonLakeBoundary: Boolean;
var
  Index: Integer;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].ObsType <> StrLakeStage then
    begin
      result := True;
      Exit;
    end;
  end;
end;

constructor TCustomSutraObservations.Create(ItemClass: TCollectionItemClass;
  Model: TBaseModel; ScreenObject: TObject);
var
  FInvalidateModelEvent: TNotifyEvent;  
begin
  if Model = nil then
  begin
    FInvalidateModelEvent := nil;
  end
  else
  begin
    FInvalidateModelEvent := (Model as TCustomModel).Invalidate;
  end;
  inherited Create(ItemClass, FInvalidateModelEvent, ScreenObject);
end;

function TCustomSutraObservations.GetUsed: Boolean;
begin
  result := Count > 0;
end;

procedure TSutraStateObservations.SetSutraStateObsItem(Index: Integer;
  const Value: TSutraStateObsItem);
begin
  Items[Index] := Value;
end;

{ TSutraFlFluxObsItem }

function TSutraFlFluxObsItem.GetObsTypeIndex: Integer;
begin
  Result := SutraFlFluxObsTypes.IndexOf(ObsType)
end;

procedure TSutraFlFluxObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraFlFluxObsTypes[Value];

end;

function TSutraFlFluxObsItem.Units: string;
begin
  Result := '';
end; 
        
{ TSutraFlFluxObservations }

function TSutraFlFluxObservations.Add: TSutraFlFluxObsItem;
begin
  result := inherited Add as TSutraFlFluxObsItem;
end;

constructor TSutraFlFluxObservations.Create(Model: TBaseModel);
begin
  inherited Create(TSutraFlFluxObsItem, Model, nil);
end;

function TSutraFlFluxObservations.GetSutraFlFluxObsItem(
  Index: Integer): TSutraFlFluxObsItem;
begin
  Result := inherited Items[Index] as TSutraFlFluxObsItem
end;

procedure TSutraFlFluxObservations.SetSutraFlFluxObsItem(Index: Integer;
  const Value: TSutraFlFluxObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TSutraUFluxObsItem }

function TSutraUFluxObsItem.GetObsTypeIndex: Integer;
begin
  result := SutraUFluxObsTypes.IndexOf(ObsType)
end;

procedure TSutraUFluxObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraUFluxObsTypes[Value];
end;

function TSutraUFluxObsItem.Units: string;
begin
  result := '';
end;

{ TSutraUFluxObservations }

function TSutraUFluxObservations.Add: TSutraUFluxObsItem;
begin
  result := inherited Add as TSutraUFluxObsItem;
end;

constructor TSutraUFluxObservations.Create(Model: TBaseModel);
begin
  inherited Create(TSutraUFluxObsItem, Model, nil);
end;

function TSutraUFluxObservations.GetSutraUFluxObsItem(
  Index: Integer): TSutraUFluxObsItem;
begin
  Result := inherited Items[Index] as TSutraUFluxObsItem;
end;

procedure TSutraUFluxObservations.SetSutraUFluxObsItem(Index: Integer;
  const Value: TSutraUFluxObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TCustomFluxObsItem }

function TCustomFluxObsItem.AddObject(ScreenObject: TObject): integer;
var
  Item: TObservationFactor;
begin
  Assert(ScreenObject is TScreenObject);
  result := ObservationFactors.IndexOfScreenObject(ScreenObject);
  if result < 0 then
  begin
    Item := ObservationFactors.Add;
    Item.ScreenObject := ScreenObject;
    InvalidateModel;
    result := ObservationFactors.Count - 1;
  end;
end;

procedure TCustomFluxObsItem.Assign(Source: TPersistent);
var
  SourceItem: TCustomFluxObsItem;
begin
  if Source is TCustomFluxObsItem then
  begin
    SourceItem := TCustomFluxObsItem(Source);
    ObservationFactors := SourceItem.ObservationFactors;
  end;
  inherited;
end;

constructor TCustomFluxObsItem.Create(Collection: TCollection);
begin
  inherited;
  FObservationFactors:= TObservationFactors.Create(
    (Collection as TCustomSutraObservations).Model);
end;

destructor TCustomFluxObsItem.Destroy;
begin
  FObservationFactors.Free;
  inherited;
end;

procedure TCustomFluxObsItem.EliminatedDeletedScreenObjects;
begin
  ObservationFactors.EliminatedDeletedScreenObjects;
end;

procedure TCustomFluxObsItem.Loaded;
begin
  ObservationFactors.Loaded;
end;

procedure TCustomFluxObsItem.RemoveObject(ScreenObject: TObject);
begin
  Assert(ScreenObject is TScreenObject);
  Index := ObservationFactors.IndexOfScreenObject(ScreenObject);
  if Index >= 0 then
  begin
    ObservationFactors.Delete(Index);
  end;
  InvalidateModel;
end;

procedure TCustomFluxObsItem.SetObservationFactors(
  const Value: TObservationFactors);
begin
  FObservationFactors.Assign(Value);
end;

{ TCustomSutraFluxObservations }

procedure TCustomSutraFluxObservations.Loaded;
var
  Index: integer;
begin
  for Index := 0 to Count - 1 do
  begin
    (Items[Index] as TCustomFluxObsItem).Loaded;
  end;
end;

{ TSutraFlFluxObservationGroup }

procedure TSutraFlFluxObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraFlFluxObservationGroup then
  begin  
    ObsGroup := TSutraFlFluxObservationGroup(Source).ObsGroup;
  end    
  else  
  begin  
    inherited;
  end;    

end;

constructor TSutraFlFluxObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup := TSutraFlFluxObservations.Create(Model);
end;

destructor TSutraFlFluxObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

function TSutraFlFluxObservationGroup.GetModel: TBaseModel;
begin
  Result := (Collection as TSutraFlFluxObservationGroups).Model;
end;

procedure TSutraFlFluxObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraFlFluxObservationGroup.SetObsGroup(
  const Value: TSutraFlFluxObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TSutraFlFluxObservationGroups }

function TSutraFlFluxObservationGroups.Add: TSutraFlFluxObservationGroup;
begin
  result := inherited Add as TSutraFlFluxObservationGroup;
end;

constructor TSutraFlFluxObservationGroups.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if FModel <> nil then
  begin
    InvalidateModelEvent := FModel.Invalidate;
  end
  else
  begin
    InvalidateModelEvent := nil;
  end;
  inherited Create(TSutraFlFluxObservationGroup, InvalidateModelEvent);
end;

function TSutraFlFluxObservationGroups.GetSutraFlFluxObsGroup(
  Index: Integer): TSutraFlFluxObservationGroup;
begin
  result := inherited Items[Index] as TSutraFlFluxObservationGroup;
end;

procedure TSutraFlFluxObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraFlFluxObservationGroups.SetSutraFlFluxObsGroup(Index: Integer;
  const Value: TSutraFlFluxObservationGroup);
begin
  inherited Items[Index] := Value;
end;

{ TSutraUFluxObservationGroup }

procedure TSutraUFluxObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraUFluxObservationGroup then
  begin
    ObsGroup := TSutraUFluxObservationGroup(Source).ObsGroup;
  end;
  inherited;
end;

constructor TSutraUFluxObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup := TSutraUFluxObservations.Create(Model);
end;

destructor TSutraUFluxObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

function TSutraUFluxObservationGroup.GetModel: TBaseModel;
begin
  result := (Collection as TSutraUFluxObservationGroups).Model;
end;

procedure TSutraUFluxObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraUFluxObservationGroup.SetObsGroup(
  const Value: TSutraUFluxObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TSutraUFluxObservationGroups }

function TSutraUFluxObservationGroups.Add: TSutraUFluxObservationGroup;
begin
  Result := inherited Add as TSutraUFluxObservationGroup;
end;

constructor TSutraUFluxObservationGroups.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if FModel <> nil then
  begin
    InvalidateModelEvent := FModel.Invalidate;
  end
  else
  begin
    InvalidateModelEvent := nil;
  end;
  inherited Create(TSutraUFluxObservationGroup, InvalidateModelEvent);
end;

function TSutraUFluxObservationGroups.GetSutraUFluxObsGroup(
  Index: Integer): TSutraUFluxObservationGroup;
begin
  result := inherited Items[Index] as TSutraUFluxObservationGroup
end;

procedure TSutraUFluxObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraUFluxObservationGroups.SetSutraUFluxObsGroup(Index: Integer;
  const Value: TSutraUFluxObservationGroup);
begin
  inherited Items[Index] := Value;
end;

initialization
  InitializeSutraObsTypes;

finalization
  SutraStateObsTypes.Free;
  SutraFlFluxObsTypes.Free;
  SutraUFluxObsTypes.Free;

end.
