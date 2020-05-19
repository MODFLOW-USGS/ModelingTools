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
    FScheduleName: string;
    function GetSutraStateObsItem(Index: Integer): TSutraStateObsItem;
    procedure SetSutraStateObsItem(Index: Integer; const Value: TSutraStateObsItem);
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    property Items[Index: Integer]: TSutraStateObsItem read GetSutraStateObsItem
      write SetSutraStateObsItem; default;
    function Add: TSutraStateObsItem;
    function HasNonLakeBoundary: Boolean;
    function HasLakeBoundary: Boolean;
    property ScheduleName: string read FScheduleName write FScheduleName;
  end;

  TCustomFluxObsItem = class(TCustomSutraObsItem);

  TCustomSutraFluxObservations = class(TCustomSutraObservations)
  private
    FObservationName: string;
    FObservationFactors: TObservationFactors;
    procedure SetObservationName(const Value: string);
    procedure SetObservationFactors(const Value: TObservationFactors);
    procedure StopTalkingToAnyOne;
  public
    constructor Create(ItemClass: TCollectionItemClass;
      Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    // @name calls @link(TObservationFactors.Loaded ObservationFactors.Loaded).
    procedure Loaded;
    procedure Assign(Source: TPersistent); override;
    // @name adds a new @link(TObservationFactor) to @link(ObservationFactors)
    // and makes ScreenObject its @link(TObservationFactor.ScreenObject).
    // If ScreenObject has already been added, it will be skipped.
    function AddObject(ScreenObject: TObject): integer;
    // @name removes the @link(TObservationFactor) from
    // @link(ObservationFactors) that has ScreenObject as its
    // @link(TObservationFactor.ScreenObject).
    procedure RemoveObject(ScreenObject: TObject);
    // @name calls @link(TObservationFactors.EliminatedDeletedScreenObjects
    // ObservationFactors.EliminatedDeletedScreenObjects).
    procedure EliminatedDeletedScreenObjects;
    function HasObsIndex(ObservationIndex: Integer): Boolean;
  published
    property ObservationName: string read FObservationName
      write SetObservationName;
    property ObservationFactors: TObservationFactors read FObservationFactors
      write SetObservationFactors;
  end;

  TCustomSutraFluxObservationGroup = class(TPhastCollectionItem)
  private
    function GetModel: TBaseModel;
  protected
    function GetObservationGroup: TCustomSutraFluxObservations; virtual; abstract;
  public
    property ObservationGroup: TCustomSutraFluxObservations
      read GetObservationGroup;
    property Model: TBaseModel read GetModel;
  end;

  TSutraSpecPressObsItem = class(TCustomFluxObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraSpecPressureObservations = class(TCustomSutraFluxObservations)
  private
    function GetSutraSpecPressObsItem(Index: Integer): TSutraSpecPressObsItem;
    procedure SetSutraSpecPressObsItem(Index: Integer;
      const Value: TSutraSpecPressObsItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSutraSpecPressObsItem read GetSutraSpecPressObsItem
      write SetSutraSpecPressObsItem; default;
    function Add: TSutraSpecPressObsItem;
  end;

  TSutraSpecPressureObservationGroup = class(TCustomSutraFluxObservationGroup)
  private
    FObsGroup: TSutraSpecPressureObservations;
//    function GetModel: TBaseModel;
    procedure SetObsGroup(const Value: TSutraSpecPressureObservations);
  protected
    function GetObservationGroup: TCustomSutraFluxObservations; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
//    property Model: TBaseModel read GetModel;
    procedure Loaded;
  published
    property ObsGroup: TSutraSpecPressureObservations read FObsGroup write SetObsGroup;
  end;

  TCustomSutraFluxObservationGroups = class(TPhastCollection)
  private
    FModel: TBaseModel;
    procedure StopTalkingToAnyOne;
  public
    constructor Create(ItemClass: TCollectionItemClass;
      Model: TBaseModel);
    property Model: TBaseModel read FModel;
    procedure Remove(Item: TCustomSutraFluxObservationGroup);
  end;

  TSutraSpecPressureObservationGroups = class(TCustomSutraFluxObservationGroups)
  private
    function GetSutraSpecPresObsGroup(
      Index: Integer): TSutraSpecPressureObservationGroup;
    procedure SetSutraFlFluxObsGroup(Index: Integer;
      const Value: TSutraSpecPressureObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    procedure Loaded;
    property Items[Index: Integer]: TSutraSpecPressureObservationGroup
      read GetSutraSpecPresObsGroup
      write SetSutraFlFluxObsGroup; default;
    function Add: TSutraSpecPressureObservationGroup;
  end;

  TSutraFluidFlowObsItem = class(TCustomFluxObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraFluidFlowObservations = class(TCustomSutraFluxObservations)
  private
    function GetSutraFluidFlowObsItem(Index: Integer): TSutraFluidFlowObsItem;
    procedure SetSutraFluidFlowObsItem(Index: Integer; const Value: TSutraFluidFlowObsItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSutraFluidFlowObsItem read GetSutraFluidFlowObsItem
      write SetSutraFluidFlowObsItem; default;
    function Add: TSutraFluidFlowObsItem;
  end;

  TSutraFluidFlowObservationGroup = class(TCustomSutraFluxObservationGroup)
  private
    FObsGroup: TSutraFluidFlowObservations;
//    function GetModel: TBaseModel;
    procedure SetObsGroup(const Value: TSutraFluidFlowObservations);
  protected
    function GetObservationGroup: TCustomSutraFluxObservations; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
//    property Model: TBaseModel read GetModel;
    procedure Loaded;
  published
    property ObsGroup: TSutraFluidFlowObservations read FObsGroup write SetObsGroup;
  end;

  TSutraFluidFlowObservationGroups = class(TCustomSutraFluxObservationGroups)
  private
    function GetSutraFluidFlowObsGroup(Index: Integer): TSutraFluidFlowObservationGroup;
    procedure SetSutraFluidFlowObsGroup(Index: Integer;
      const Value: TSutraFluidFlowObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    procedure Loaded;
    property Items[Index: Integer]: TSutraFluidFlowObservationGroup
      read GetSutraFluidFlowObsGroup
      write SetSutraFluidFlowObsGroup; default;
    function Add: TSutraFluidFlowObservationGroup;
  end;

  TSutraSpecConcObsItem = class(TCustomFluxObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraSpecConcObservations = class(TCustomSutraFluxObservations)
  private
    function GetSutraSpecConcObsItem(Index: Integer): TSutraSpecConcObsItem;
    procedure SetSutraSpecConcObsItem(Index: Integer; const Value: TSutraSpecConcObsItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSutraSpecConcObsItem read GetSutraSpecConcObsItem
      write SetSutraSpecConcObsItem; default;
    function Add: TSutraSpecConcObsItem;
  end;

  TSutraSpecConcObservationGroup = class(TCustomSutraFluxObservationGroup)
  private
    FObsGroup: TSutraSpecConcObservations;
    procedure SetObsGroup(const Value: TSutraSpecConcObservations);
  protected
    function GetObservationGroup: TCustomSutraFluxObservations; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property ObsGroup: TSutraSpecConcObservations read FObsGroup write SetObsGroup;
  end;

  TSutraSpecConcObservationGroups = class(TCustomSutraFluxObservationGroups)
  private
    function GetSutraSpecConcObsGroup(Index: Integer): TSutraSpecConcObservationGroup;
    procedure SetSutraSpecConcObsGroup(Index: Integer;
      const Value: TSutraSpecConcObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    procedure Loaded;
    property Items[Index: Integer]: TSutraSpecConcObservationGroup
      read GetSutraSpecConcObsGroup
      write SetSutraSpecConcObsGroup; default;
    function Add: TSutraSpecConcObservationGroup;
  end;

  TSutraGenTransObsItem = class(TCustomFluxObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(const Value: Integer); override;
  public
    function Units: string; override;
  end;

  TSutraGenTransObservations = class(TCustomSutraFluxObservations)
  private
    function GetSutraGenTranObsItem(Index: Integer): TSutraGenTransObsItem;
    procedure SetSutraGenTranObsItem(Index: Integer; const Value: TSutraGenTransObsItem);
  public
    Constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TSutraGenTransObsItem read GetSutraGenTranObsItem
      write SetSutraGenTranObsItem; default;
    function Add: TSutraGenTransObsItem;
  end;

  TSutraGenTransObservationGroup = class(TCustomSutraFluxObservationGroup)
  private
    FObsGroup: TSutraGenTransObservations;
    procedure SetObsGroup(const Value: TSutraGenTransObservations);
  protected
    function GetObservationGroup: TCustomSutraFluxObservations; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property ObsGroup: TSutraGenTransObservations read FObsGroup write SetObsGroup;
  end;

  TSutraGenTransObservationGroups = class(TCustomSutraFluxObservationGroups)
  private
    function GetSutraGetTransObsGroup(Index: Integer): TSutraGenTransObservationGroup;
    procedure SetSutraGetTransObsGroup(Index: Integer;
      const Value: TSutraGenTransObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    procedure Loaded;
    property Items[Index: Integer]: TSutraGenTransObservationGroup
      read GetSutraGetTransObsGroup
      write SetSutraGetTransObsGroup; default;
    function Add: TSutraGenTransObservationGroup;
  end;

  TSutraGenPressObservations = class(TSutraSpecPressureObservations);

  TSutraGenPressureObservationGroup = class(TCustomSutraFluxObservationGroup)
  private
    FObsGroup: TSutraGenPressObservations;
    procedure SetObsGroup(const Value: TSutraGenPressObservations);
  protected
    function GetObservationGroup: TCustomSutraFluxObservations; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property ObsGroup: TSutraGenPressObservations read FObsGroup write SetObsGroup;
  end;

  TSutraGenPressureObservationGroups = class(TCustomSutraFluxObservationGroups)
  private
    function GetSutraGetPressObsGroup(Index: Integer): TSutraGenPressureObservationGroup;
    procedure SetSutraGetPressObsGroup(Index: Integer;
      const Value: TSutraGenPressureObservationGroup);
  public
    constructor Create(Model: TBaseModel);
    procedure Loaded;
    property Items[Index: Integer]: TSutraGenPressureObservationGroup
      read GetSutraGetPressObsGroup
      write SetSutraGetPressObsGroup; default;
    function Add: TSutraGenPressureObservationGroup;
  end;

  TSutraFluxObs = class(TGoPhastPersistent)
  private
    FSpecPres: TSutraSpecPressureObservationGroups;
    FFluidFlow: TSutraFluidFlowObservationGroups;
    FSpecConc: TSutraSpecConcObservationGroups;
    FGenFlow: TSutraGenPressureObservationGroups;
    FGenTrans: TSutraGenTransObservationGroups;
    procedure SetSpecPres(const Value: TSutraSpecPressureObservationGroups);
    procedure SetFluidFlow(const Value: TSutraFluidFlowObservationGroups);
    procedure SetSpecConc(const Value: TSutraSpecConcObservationGroups);
    procedure SetGenFlow(const Value: TSutraGenPressureObservationGroups);
    procedure SetGenTrans(const Value: TSutraGenTransObservationGroups);
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
    procedure StopTalkingToAnyOne;
    procedure Clear;
  published
    property SpecPres: TSutraSpecPressureObservationGroups read FSpecPres write SetSpecPres;
    property FluidFlow: TSutraFluidFlowObservationGroups read FFluidFlow write SetFluidFlow;
    property SpecConc: TSutraSpecConcObservationGroups read FSpecConc write SetSpecConc;
    property GenFlow: TSutraGenPressureObservationGroups read FGenFlow write SetGenFlow;
    property GenTrans: TSutraGenTransObservationGroups read FGenTrans write SetGenTrans;
  end;

var
  SutraStateObsTypes: TStringList;
  SutraSpecPressureObsTypes: TStringList;
  SutraSpecFluidFlowObsTypes: TStringList;
  SutraSpecConcObsTypes: TStringList;
  SutraGenUObsTypes: TStringList;

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

  SutraSpecPressureObsTypes := TStringList.Create;
  SutraSpecPressureObsTypes.Add('Fluid flow rate'); // Units = Mass/sec, Add selected nodes
  SutraSpecPressureObsTypes.Add('Conc/Temperature'); // Units = Mass/sec, Add selected nodes
  SutraSpecPressureObsTypes.Add('Resultant mass/energy rate'); // Units = solute mass/sec, Add selected nodes

  SutraSpecFluidFlowObsTypes := TStringList.Create;
  SutraSpecFluidFlowObsTypes.Add('Solute conc/temperature'); // Units = solute mass/sec, Add selected nodes
  SutraSpecFluidFlowObsTypes.Add('Resultant mass/energy rate'); // Units = solute mass/sec, Add selected nodes

  SutraSpecConcObsTypes := TStringList.Create;
  SutraSpecConcObsTypes.Add('Resultant mass/energy rate'); // Units = solute mass/sec, Add selected nodes

  SutraGenUObsTypes := TStringList.Create;
  SutraGenUObsTypes.Add('Resultant mass/energy rate'); // Units = solute mass/sec, Add selected nodes
  SutraGenUObsTypes.Add('Calculated conc/temp'); // Units = solute mass/sec, Add selected nodes
end;

{ TCustomSutraObsItem }

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

function TSutraStateObservations.HasLakeBoundary: Boolean;
var
  Index: Integer;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].ObsType = StrLakeStage then
    begin
      result := True;
      Exit;
    end;
  end;
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

{ TSutraSpecPressObsItem }

function TSutraSpecPressObsItem.GetObsTypeIndex: Integer;
begin
  Result := SutraSpecPressureObsTypes.IndexOf(ObsType)
end;

procedure TSutraSpecPressObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraSpecPressureObsTypes[Value];

end;

function TSutraSpecPressObsItem.Units: string;
begin
  Result := '';
end;

{ TSutraSpecPressureObservations }

function TSutraSpecPressureObservations.Add: TSutraSpecPressObsItem;
begin
  result := inherited Add as TSutraSpecPressObsItem;
end;

constructor TSutraSpecPressureObservations.Create(Model: TBaseModel);
begin
  inherited Create(TSutraSpecPressObsItem, Model, nil);
end;

function TSutraSpecPressureObservations.GetSutraSpecPressObsItem(
  Index: Integer): TSutraSpecPressObsItem;
begin
  Result := inherited Items[Index] as TSutraSpecPressObsItem
end;

procedure TSutraSpecPressureObservations.SetSutraSpecPressObsItem(Index: Integer;
  const Value: TSutraSpecPressObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TSutraFluidFlowObsItem }

function TSutraFluidFlowObsItem.GetObsTypeIndex: Integer;
begin
  result := SutraSpecFluidFlowObsTypes.IndexOf(ObsType)
end;

procedure TSutraFluidFlowObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraSpecFluidFlowObsTypes[Value];
end;

function TSutraFluidFlowObsItem.Units: string;
begin
  result := '';
end;

{ TSutraFluidFlowObservations }

function TSutraFluidFlowObservations.Add: TSutraFluidFlowObsItem;
begin
  result := inherited Add as TSutraFluidFlowObsItem;
end;

constructor TSutraFluidFlowObservations.Create(Model: TBaseModel);
begin
  inherited Create(TSutraFluidFlowObsItem, Model, nil);
end;

function TSutraFluidFlowObservations.GetSutraFluidFlowObsItem(
  Index: Integer): TSutraFluidFlowObsItem;
begin
  Result := inherited Items[Index] as TSutraFluidFlowObsItem;
end;

procedure TSutraFluidFlowObservations.SetSutraFluidFlowObsItem(Index: Integer;
  const Value: TSutraFluidFlowObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TCustomSutraFluxObservations }

function TCustomSutraFluxObservations.AddObject(ScreenObject: TObject): integer;
begin
  result := ObservationFactors.AddObject(ScreenObject);
end;

procedure TCustomSutraFluxObservations.Assign(Source: TPersistent);
var
  ObsSource: TCustomSutraFluxObservations;
begin
  if Source is TCustomSutraFluxObservations then
  begin
    ObsSource := TCustomSutraFluxObservations(Source);
    ObservationName := ObsSource.ObservationName;
    ObservationFactors := ObsSource.ObservationFactors;
  end;
  inherited;

end;

constructor TCustomSutraFluxObservations.Create(ItemClass: TCollectionItemClass;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;
  FObservationFactors:= TObservationFactors.Create(Model);
end;

destructor TCustomSutraFluxObservations.Destroy;
begin
  FObservationFactors.Free;
  inherited;
end;

procedure TCustomSutraFluxObservations.EliminatedDeletedScreenObjects;
begin
  ObservationFactors.EliminatedDeletedScreenObjects;
end;

function TCustomSutraFluxObservations.HasObsIndex(
  ObservationIndex: Integer): Boolean;
var
  Index: Integer;
  Item: TCustomFluxObsItem;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index] as  TCustomFluxObsItem;
    if Item.ObsTypeIndex = ObservationIndex then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TCustomSutraFluxObservations.Loaded;
//var
//  Index: integer;
begin
  ObservationFactors.Loaded;
//  for Index := 0 to Count - 1 do
//  begin
//    (Items[Index] as TCustomFluxObsItem).Loaded;
//  end;
end;

procedure TCustomSutraFluxObservations.RemoveObject(ScreenObject: TObject);
var
  Index: Integer;
begin
  Assert(ScreenObject is TScreenObject);
  Index := ObservationFactors.IndexOfScreenObject(ScreenObject);
  if Index >= 0 then
  begin
    ObservationFactors.Delete(Index);
  end;
  InvalidateModel;
end;

{ TSutraSpecPressureObservationGroup }

procedure TSutraSpecPressureObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraSpecPressureObservationGroup then
  begin
    ObsGroup := TSutraSpecPressureObservationGroup(Source).ObsGroup;
  end
  else
  begin
    inherited;
  end;

end;

constructor TSutraSpecPressureObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup := TSutraSpecPressureObservations.Create(Model);
end;

destructor TSutraSpecPressureObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

//function TSutraSpecPressureObservationGroup.GetModel: TBaseModel;
//begin
//  Result := (Collection as TSutraSpecPressureObservationGroups).Model;
//end;

function TSutraSpecPressureObservationGroup.GetObservationGroup: TCustomSutraFluxObservations;
begin
  result := ObsGroup;
end;

procedure TSutraSpecPressureObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraSpecPressureObservationGroup.SetObsGroup(
  const Value: TSutraSpecPressureObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TSutraSpecPressureObservationGroups }

function TSutraSpecPressureObservationGroups.Add: TSutraSpecPressureObservationGroup;
begin
  result := inherited Add as TSutraSpecPressureObservationGroup;
end;

constructor TSutraSpecPressureObservationGroups.Create(Model: TBaseModel);
begin
  inherited Create(TSutraSpecPressureObservationGroup, Model);
end;

function TSutraSpecPressureObservationGroups.GetSutraSpecPresObsGroup(
  Index: Integer): TSutraSpecPressureObservationGroup;
begin
  result := inherited Items[Index] as TSutraSpecPressureObservationGroup;
end;

procedure TSutraSpecPressureObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraSpecPressureObservationGroups.SetSutraFlFluxObsGroup(Index: Integer;
  const Value: TSutraSpecPressureObservationGroup);
begin
  inherited Items[Index] := Value;
end;

{ TSutraFluidFlowObservationGroup }

procedure TSutraFluidFlowObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraFluidFlowObservationGroup then
  begin
    ObsGroup := TSutraFluidFlowObservationGroup(Source).ObsGroup;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraFluidFlowObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup := TSutraFluidFlowObservations.Create(Model);
end;

destructor TSutraFluidFlowObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

//function TSutraFluidFlowObservationGroup.GetModel: TBaseModel;
//begin
//  result := (Collection as TSutraFluidFlowObservationGroups).Model;
//end;

function TSutraFluidFlowObservationGroup.GetObservationGroup: TCustomSutraFluxObservations;
begin
  Result := ObsGroup;
end;

procedure TSutraFluidFlowObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraFluidFlowObservationGroup.SetObsGroup(
  const Value: TSutraFluidFlowObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TSutraUFluxObservationGroups }

function TSutraFluidFlowObservationGroups.Add: TSutraFluidFlowObservationGroup;
begin
  Result := inherited Add as TSutraFluidFlowObservationGroup;
end;

constructor TSutraFluidFlowObservationGroups.Create(Model: TBaseModel);
begin
  inherited Create(TSutraFluidFlowObservationGroup, Model);
end;

function TSutraFluidFlowObservationGroups.GetSutraFluidFlowObsGroup(
  Index: Integer): TSutraFluidFlowObservationGroup;
begin
  result := inherited Items[Index] as TSutraFluidFlowObservationGroup
end;

procedure TSutraFluidFlowObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraFluidFlowObservationGroups.SetSutraFluidFlowObsGroup(Index: Integer;
  const Value: TSutraFluidFlowObservationGroup);
begin
  inherited Items[Index] := Value;
end;

{ TSutraFluxObs }

procedure TSutraFluxObs.Assign(Source: TPersistent);
var
  FluxObsSource: TSutraFluxObs;
begin
  if Source is TSutraFluxObs then
  begin
    FluxObsSource := TSutraFluxObs(Source);
    SpecPres := FluxObsSource.SpecPres;
    FluidFlow := FluxObsSource.FluidFlow;
    SpecConc := FluxObsSource.SpecConc;
    GenFlow := FluxObsSource.GenFlow;
    GenTrans := FluxObsSource.GenTrans;
  end
  else
  begin
    inherited;
  end;

end;

procedure TSutraFluxObs.Clear;
begin
  SpecPres.Clear;
  FluidFlow.Clear;
  SpecConc.Clear;
  GenFlow.Clear;
  GenTrans.Clear;
end;

constructor TSutraFluxObs.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(InvalidateModelEvent);
  FSpecPres:= TSutraSpecPressureObservationGroups.Create(Model);
  FFluidFlow:= TSutraFluidFlowObservationGroups.Create(Model);
  FSpecConc := TSutraSpecConcObservationGroups.Create(Model);
  FGenFlow := TSutraGenPressureObservationGroups.Create(Model);
  FGenTrans := TSutraGenTransObservationGroups.Create(Model);
end;

destructor TSutraFluxObs.Destroy;
begin
  FGenTrans.Free;
  FGenFlow.Free;
  FSpecConc.Free;
  FSpecPres.Free;
  FFluidFlow.Free;
  inherited;
end;

procedure TSutraFluxObs.Loaded;
begin
  SpecPres.Loaded;
  FluidFlow.Loaded;
  SpecConc.Loaded;
  GenFlow.Loaded;
  GenTrans.Loaded;
end;

procedure TSutraFluxObs.SetSpecConc(
  const Value: TSutraSpecConcObservationGroups);
begin
  FSpecConc.Assign(Value);
end;

procedure TSutraFluxObs.SetSpecPres(
  const Value: TSutraSpecPressureObservationGroups);
begin
  FSpecPres.Assign(Value);
end;

procedure TSutraFluxObs.StopTalkingToAnyOne;
begin
  SpecPres.StopTalkingToAnyOne;
  FluidFlow.StopTalkingToAnyOne;
  SpecConc.StopTalkingToAnyOne;
  GenFlow.StopTalkingToAnyOne;
  GenTrans.StopTalkingToAnyOne;
end;

procedure TSutraFluxObs.SetFluidFlow(const Value: TSutraFluidFlowObservationGroups);
begin
  FFluidFlow.Assign(Value);
end;

procedure TSutraFluxObs.SetGenFlow(
  const Value: TSutraGenPressureObservationGroups);
begin
  FGenFlow.Assign( Value);
end;

procedure TSutraFluxObs.SetGenTrans(
  const Value: TSutraGenTransObservationGroups);
begin
  FGenTrans.Assign( Value);
end;

procedure TCustomSutraFluxObservations.SetObservationFactors(
  const Value: TObservationFactors);
begin
  FObservationFactors.Assign( Value);
end;

procedure TCustomSutraFluxObservations.SetObservationName(const Value: string);
begin
  FObservationName := Value;
end;

procedure TCustomSutraFluxObservations.StopTalkingToAnyOne;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to ObservationFactors.Count - 1 do
  begin
    ObservationFactors[ItemIndex].Factor := '1';
  end;
end;

{ TCustomSutraFluxObservationGroups }

constructor TCustomSutraFluxObservationGroups.Create(ItemClass: TCollectionItemClass;
  Model: TBaseModel);
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
  inherited Create(ItemClass, InvalidateModelEvent);
end;

procedure TCustomSutraFluxObservationGroups.Remove(
  Item: TCustomSutraFluxObservationGroup);
var
  Index: integer;
  AnItem: TCustomSutraFluxObservationGroup;
begin
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index] as TCustomSutraFluxObservationGroup;
    if AnItem = Item then
    begin
      Delete(Index);
      break;
    end;
  end;
end;

procedure TCustomSutraFluxObservationGroups.StopTalkingToAnyOne;
var
  ItemIndex: Integer;
begin
  for ItemIndex := 0 to Count - 1 do
  begin
    (Items[ItemIndex] as TCustomSutraFluxObservationGroup).
      ObservationGroup.StopTalkingToAnyOne;
  end;
end;

{ TSutraSpecConcObsItem }

function TSutraSpecConcObsItem.GetObsTypeIndex: Integer;
begin
  result := SutraSpecConcObsTypes.IndexOf(ObsType)
end;

procedure TSutraSpecConcObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraSpecConcObsTypes[Value];
end;

function TSutraSpecConcObsItem.Units: string;
begin
  result := ''
end;

{ TSutraSpecConcObservations }

function TSutraSpecConcObservations.Add: TSutraSpecConcObsItem;
begin
  result := inherited Add as TSutraSpecConcObsItem
end;

constructor TSutraSpecConcObservations.Create(Model: TBaseModel);
begin
  inherited Create(TSutraSpecConcObsItem, Model, nil);
end;

function TSutraSpecConcObservations.GetSutraSpecConcObsItem(
  Index: Integer): TSutraSpecConcObsItem;
begin
  result := inherited Items[Index] as TSutraSpecConcObsItem;
end;

procedure TSutraSpecConcObservations.SetSutraSpecConcObsItem(Index: Integer;
  const Value: TSutraSpecConcObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TSutraSpecConcObservationGroup }

procedure TSutraSpecConcObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraSpecConcObservationGroup then
  begin
    ObsGroup := TSutraSpecConcObservationGroup(Source).ObsGroup;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraSpecConcObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup := TSutraSpecConcObservations.Create(Model);
end;

destructor TSutraSpecConcObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

function TSutraSpecConcObservationGroup.GetObservationGroup: TCustomSutraFluxObservations;
begin
  Result := ObsGroup;
end;

procedure TSutraSpecConcObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraSpecConcObservationGroup.SetObsGroup(
  const Value: TSutraSpecConcObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TCustomSutraFluxObservationGroup }

function TCustomSutraFluxObservationGroup.GetModel: TBaseModel;
begin
  Result := (Collection as TCustomSutraFluxObservationGroups).Model;
end;

{ TSutraSpecConcObservationGroups }

function TSutraSpecConcObservationGroups.Add: TSutraSpecConcObservationGroup;
begin
 result := inherited Add as TSutraSpecConcObservationGroup;
end;

constructor TSutraSpecConcObservationGroups.Create(Model: TBaseModel);
begin
  inherited Create(TSutraSpecConcObservationGroup, Model);
end;

function TSutraSpecConcObservationGroups.GetSutraSpecConcObsGroup(
  Index: Integer): TSutraSpecConcObservationGroup;
begin
  Result := inherited Items[Index] as TSutraSpecConcObservationGroup;
end;

procedure TSutraSpecConcObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraSpecConcObservationGroups.SetSutraSpecConcObsGroup(
  Index: Integer; const Value: TSutraSpecConcObservationGroup);
begin
  inherited Items[Index] := Value;
end;


{ TSutraGenTransObsItem }

function TSutraGenTransObsItem.GetObsTypeIndex: Integer;
begin
  result := SutraGenUObsTypes.IndexOf(ObsType)
end;

procedure TSutraGenTransObsItem.SetObsTypeIndex(const Value: Integer);
begin
  ObsType := SutraGenUObsTypes[Value];
end;

function TSutraGenTransObsItem.Units: string;
begin
  result := ''
end;

{ TSutraGenTransObservations }

function TSutraGenTransObservations.Add: TSutraGenTransObsItem;
begin
  result := inherited Add as TSutraGenTransObsItem
end;

constructor TSutraGenTransObservations.Create(Model: TBaseModel);
begin
  inherited Create(TSutraGenTransObsItem, Model, nil);
end;

function TSutraGenTransObservations.GetSutraGenTranObsItem(
  Index: Integer): TSutraGenTransObsItem;
begin
  result := inherited Items[Index] as TSutraGenTransObsItem;
end;

procedure TSutraGenTransObservations.SetSutraGenTranObsItem(Index: Integer;
  const Value: TSutraGenTransObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TSutraGenTransObservationGroup }

procedure TSutraGenTransObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraGenTransObservationGroup then
  begin
    ObsGroup := TSutraGenTransObservationGroup(Source).ObsGroup;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraGenTransObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup := TSutraGenTransObservations.Create(Model);
end;

destructor TSutraGenTransObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

function TSutraGenTransObservationGroup.GetObservationGroup: TCustomSutraFluxObservations;
begin
  Result := ObsGroup;
end;

procedure TSutraGenTransObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraGenTransObservationGroup.SetObsGroup(
  const Value: TSutraGenTransObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TSutraGenTransObservationGroups }

function TSutraGenTransObservationGroups.Add: TSutraGenTransObservationGroup;
begin
 result := inherited Add as TSutraGenTransObservationGroup;
end;

constructor TSutraGenTransObservationGroups.Create(Model: TBaseModel);
begin
  inherited Create(TSutraGenTransObservationGroup, Model);
end;

function TSutraGenTransObservationGroups.GetSutraGetTransObsGroup(
  Index: Integer): TSutraGenTransObservationGroup;
begin
  Result := inherited Items[Index] as TSutraGenTransObservationGroup;
end;

procedure TSutraGenTransObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraGenTransObservationGroups.SetSutraGetTransObsGroup(
  Index: Integer; const Value: TSutraGenTransObservationGroup);
begin
  inherited Items[Index] := Value;
end;



{ TSutraGenPressureObservationGroup }

procedure TSutraGenPressureObservationGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraGenPressureObservationGroup then
  begin
    ObsGroup := TSutraGenPressureObservationGroup(Source).ObsGroup;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraGenPressureObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FObsGroup:= TSutraGenPressObservations.Create(Model);
end;

destructor TSutraGenPressureObservationGroup.Destroy;
begin
  FObsGroup.Free;
  inherited;
end;

function TSutraGenPressureObservationGroup.GetObservationGroup: TCustomSutraFluxObservations;
begin
  result := FObsGroup;
end;

procedure TSutraGenPressureObservationGroup.Loaded;
begin
  ObsGroup.Loaded;
end;

procedure TSutraGenPressureObservationGroup.SetObsGroup(
  const Value: TSutraGenPressObservations);
begin
  FObsGroup.Assign(Value);
end;

{ TSutraGenPressureObservationGroups }

function TSutraGenPressureObservationGroups.Add: TSutraGenPressureObservationGroup;
begin
  result := inherited Add as TSutraGenPressureObservationGroup
end;

constructor TSutraGenPressureObservationGroups.Create(Model: TBaseModel);
begin
  inherited Create(TSutraGenPressureObservationGroup, Model);
end;

function TSutraGenPressureObservationGroups.GetSutraGetPressObsGroup(
  Index: Integer): TSutraGenPressureObservationGroup;
begin
  Result := inherited Items[Index] as TSutraGenPressureObservationGroup;
end;

procedure TSutraGenPressureObservationGroups.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSutraGenPressureObservationGroups.SetSutraGetPressObsGroup(
  Index: Integer; const Value: TSutraGenPressureObservationGroup);
begin
  inherited Items[Index] := Value;
end;

initialization

InitializeSutraObsTypes;

finalization
  SutraStateObsTypes.Free;
  SutraSpecPressureObsTypes.Free;
  SutraSpecFluidFlowObsTypes.Free;
  SutraSpecConcObsTypes.Free;
  SutraGenUObsTypes.Free;

end.
