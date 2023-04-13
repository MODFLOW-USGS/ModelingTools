unit ModflowRipPlantGroupsUnit;

interface

uses
  System.Classes, GoPhastTypes, OrderedCollectionUnit, FormulaManagerUnit,
  SubscriptionUnit, System.Generics.Collections, RbwParser, System.SysUtils;

const
  FormulaCount = 4;
  ExtinctDepthPosition = 0;
  ActiveRootDepthPosition = 1;
  MaxETPosition = 2;
  SatExtinctionEvapPosition = 3;

type
  TRipETSegment = class(TPhastCollectionItem)
  private
    FStoredFluxSegment: TRealStorage;
    FStoredActiveRootDepth: TRealStorage;
    procedure SetStoredActiveRootDepth(const Value: TRealStorage);
    procedure SetStoredFluxSegment(const Value: TRealStorage);
    function GetActiveRootDepth: double;
    function GetFluxSegment: double;
    procedure SetActiveRootDepth(const Value: double);
    procedure SetFluxSegment(const Value: double);
  protected
    function IsSame(AnotherItem: TRipETSegment): boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
		// fdh: dimensionless active root depth segment (eqn. 5.1, dimensionless).
    property ActiveRootDepth: double read GetActiveRootDepth
      write SetActiveRootDepth;
    // fdR: fdR is the dimensionless flux segment (eqn. 5.2, dimensionless).
    property FluxSegment: double read GetFluxSegment write SetFluxSegment;
  published
		// fdh: dimensionless active root depth segment (eqn. 5.1, dimensionless).
    property StoredActiveRootDepth: TRealStorage read FStoredActiveRootDepth
      write SetStoredActiveRootDepth;
    // fdR: fdR is the dimensionless flux segment (eqn. 5.2, dimensionless).
    property StoredFluxSegment: TRealStorage read FStoredFluxSegment
      write SetStoredFluxSegment;
  end;

  TRipETSegments = class(TPhastCollection)
  private
    function GetItem(index: integer): TRipETSegment;
    procedure SetItem(index: integer; const Value: TRipETSegment);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    property Items[index: integer]: TRipETSegment read GetItem
      write SetItem; default;
    function IsSame(AnotherSegments: TRipETSegments): boolean;
    function Add: TRipETSegment;
    function SumOfSegmentsEqualsOne: boolean;
    function MaximumRateEqualsOne: boolean;
  end;

  TRipPlantGroup = class(TFormulaOrderedItem)
  private
    FName: string;
    FSaturatedExtinctionDepth: TFormulaObject;
    FActiveRootDepth: TFormulaObject;
    FMaxET: TFormulaObject;
    FSatExtinctionEvap: TFormulaObject;
    FETSegments: TRipETSegments;
    FID: integer;
    FObserverList: TObjectList<TObserver>;
    function GetRipFormula(Index: integer): string;
    procedure SetRipFormula(Index: integer; const Value: string);
    procedure SetETSegments(const Value: TRipETSegments);
    procedure SetName(const Value: string);
    function GetActiveRootDepth: string;
    function GetMaxET: string;
    function GetSatExtinctionEvap: string;
    procedure SetActiveRootDepth(const Value: string);
    procedure SetMaxET(const Value: string);
    procedure SetSatExtinctionEvap(const Value: string);
    procedure SetID(const Value: integer);
    procedure SetSaturatedExtinctionDepth(const Value: string);
    function GetSaturatedExtinctionDepth: string;
    procedure CreateFormulaObjects;
    function CreateFormulaObject(Orientation:
      TDataSetOrientation): TFormulaObject;
    procedure ResetItemObserver(Index: integer);
    procedure RemoveFormulaObjects;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
    procedure UpdateFormulaDependencies(OldFormula: string;
      var NewFormula: string; Observer: TObserver;
      Compiler: TRbwParser); override;
    function GetScreenObject: TObject; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property RipFormula[Index: integer]: string read GetRipFormula
      write SetRipFormula;
    procedure StopTalkingToAnyone;
  published
    // RIMNM: Name (24 characters)
    property Name: string read FName write SetName;
    // Sxd: Saturated Extinction Depth
    property SaturatedExtinctionDepth: string read GetSaturatedExtinctionDepth
      write SetSaturatedExtinctionDepth;
    // Ard: Active root depth
    property ActiveRootDepth: string read GetActiveRootDepth
      write SetActiveRootDepth;
    // Rmax: Max ET flux
    property MaxET: string read GetMaxET write SetMaxET;
    // Rsxd: "transpiration canopy flux at the saturated extinction depth or
    // the maximum evaporation rate"
    property SatExtinctionEvap: string read GetSatExtinctionEvap
      write SetSatExtinctionEvap;
    // fdh and fdR
    property ETSegments: TRipETSegments read FETSegments write SetETSegments;
    // @name is used as a permanent identifier of an instance of @classname.
    property ID: integer read FID write SetID;

  end;

  TRipPlantGroups = class(TEnhancedOrderedCollection)
  private
    FMaxID: Integer;
    FErrorStrings: TStrings;
    function GetItems(Index: Integer): TRipPlantGroup;
    procedure SetItems(Index: Integer; const Value: TRipPlantGroup);
    procedure SetMaxID(const Value: Integer);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    destructor Destroy; override;
    property Items[Index: Integer]: TRipPlantGroup read GetItems
      write SetItems; default;
    function Add: TRipPlantGroup;
    procedure Assign(Source: TPersistent); override;
    function IndexOfName(AName: string): Integer;
    // The result is owned by this @classname.
    function PlantGroupWithBadDepthSegments: TStrings;
    // The result is owned by this @classname.
    function PlantGroupWithBadRateSegments: TStrings;
    procedure StopTalkingToAnyone;
  published
    property MaxID: Integer read FMaxID write SetMaxID;
  end;

implementation

uses
  frmGoPhastUnit, PhastModelUnit, ScreenObjectUnit;

{ TRipETSegment }

procedure TRipETSegment.Assign(Source: TPersistent);
var
  RipSource: TRipETSegment;
begin
  if Source is TRipETSegment then
  begin
    RipSource := TRipETSegment(Source);
    StoredActiveRootDepth := RipSource.StoredActiveRootDepth;
    StoredFluxSegment := RipSource.StoredFluxSegment;
  end
  else
  begin
    inherited;
  end;
end;

constructor TRipETSegment.Create(Collection: TCollection);
begin
  inherited;
  FStoredFluxSegment := TRealStorage.Create;
  FStoredActiveRootDepth := TRealStorage.Create;
  FStoredFluxSegment.OnChange := OnInvalidateModel;
  FStoredActiveRootDepth.OnChange := OnInvalidateModel;
end;

destructor TRipETSegment.Destroy;
begin
  FStoredActiveRootDepth.Free;
  FStoredFluxSegment.Free;
  inherited;
end;

function TRipETSegment.GetActiveRootDepth: double;
begin
  result := StoredActiveRootDepth.Value;
end;

function TRipETSegment.GetFluxSegment: double;
begin
  result := StoredFluxSegment.Value;
end;

function TRipETSegment.IsSame(AnotherItem: TRipETSegment): boolean;
begin
  result := (ActiveRootDepth = AnotherItem.ActiveRootDepth)
    and (FluxSegment = AnotherItem.FluxSegment);
end;

procedure TRipETSegment.SetActiveRootDepth(const Value: double);
begin
  StoredActiveRootDepth.Value := Value;
end;

procedure TRipETSegment.SetFluxSegment(const Value: double);
begin
  StoredFluxSegment.Value := Value;
end;

procedure TRipETSegment.SetStoredActiveRootDepth(const Value: TRealStorage);
begin
  FStoredActiveRootDepth.Assign(Value);
end;

procedure TRipETSegment.SetStoredFluxSegment(const Value: TRealStorage);
begin
  FStoredFluxSegment.Assign(Value);
end;

{ TRipETSegments }

function TRipETSegments.Add: TRipETSegment;
begin
  result := inherited Add as TRipETSegment;
end;

constructor TRipETSegments.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TRipETSegment, InvalidateModelEvent);
end;

function TRipETSegments.GetItem(index: integer): TRipETSegment;
begin
  result := inherited Items[index] as TRipETSegment;
end;

function TRipETSegments.IsSame(AnotherSegments: TRipETSegments): boolean;
var
  index: Integer;
begin
  Result := Count = AnotherSegments.Count;
  if Result then
  begin
    for index := 0 to Count - 1 do
    begin
      result := Items[index].IsSame(AnotherSegments.Items[index]);
      if not Result then
      begin
        Exit;
      end;
    end;
  end;
end;

function TRipETSegments.MaximumRateEqualsOne: boolean;
var
  SegmentIndex: Integer;
  Total: double;
  Max: double;
begin
  Total := 0;
  Max := 0;
  for SegmentIndex := 0 to Count - 1 do
  begin
    Total := Total + Items[SegmentIndex].FluxSegment;
    if Max < Total then
    begin
      Max := Total;
    end;
  end;
  result := (Max > 0.999) and (Max < 1.001);
end;

procedure TRipETSegments.SetItem(index: integer; const Value: TRipETSegment);
begin
  inherited Items[index] := Value;
end;

function TRipETSegments.SumOfSegmentsEqualsOne: boolean;
var
  SegmentIndex: Integer;
  Total: double;
begin
  Total := 0;
  for SegmentIndex := 0 to Count - 1 do
  begin
    Total := Total + Items[SegmentIndex].ActiveRootDepth;
  end;
  result := (Total > 0.999) and (Total < 1.001);
end;

{ TRipPlantGroup }

procedure TRipPlantGroup.Assign(Source: TPersistent);
var
  SourcePlantGroup: TRipPlantGroup;
begin
  if Source is TRipPlantGroup then
  begin
    SourcePlantGroup := TRipPlantGroup(Source);
    ID := SourcePlantGroup.ID;
    Name := SourcePlantGroup.Name;
    SaturatedExtinctionDepth := SourcePlantGroup.SaturatedExtinctionDepth;
    ActiveRootDepth := SourcePlantGroup.ActiveRootDepth;
    MaxET := SourcePlantGroup.MaxET;
    SatExtinctionEvap := SourcePlantGroup.SatExtinctionEvap;
    ETSegments := SourcePlantGroup.ETSegments;
  end;
  inherited;
end;

constructor TRipPlantGroup.Create(Collection: TCollection);
var
  Index: integer;
  Observer: TObserver;
begin
  inherited;
  CreateFormulaObjects;
  FObserverList := TObjectList<TObserver>.Create;
  for Index := 0 to FormulaCount - 1 do
  begin
    Observer := TObserver.Create(nil);
    FObserverList.Add(Observer);
  end;
//  AssignObserverEvents(Collection);

  FETSegments := TRipETSegments.Create(OnInvalidateModelEvent);
  OnRemoveSubscription := GlobalRemoveFormulaObjectSubscription;
  OnRestoreSubscription := GlobalRestoreFormulaObjectSubscription;
end;

function TRipPlantGroup.CreateFormulaObject(
  Orientation: TDataSetOrientation): TFormulaObject;
begin
  result := CreateBlockFormulaObject(Orientation);
  result.AddSubscriptionEvents(
    GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
end;

procedure TRipPlantGroup.CreateFormulaObjects;
begin
  FSaturatedExtinctionDepth := CreateFormulaObject(dso3D);
  FActiveRootDepth := CreateFormulaObject(dso3D);
  FMaxET := CreateFormulaObject(dso3D);
  FSatExtinctionEvap := CreateFormulaObject(dso3D);
end;

destructor TRipPlantGroup.Destroy;
begin
  inherited;
  FETSegments.Free;
  FObserverList.Free;
  RemoveFormulaObjects;
end;

function TRipPlantGroup.GetActiveRootDepth: string;
begin
  Result := FActiveRootDepth.Formula;
  ResetItemObserver(ActiveRootDepthPosition);
end;

function TRipPlantGroup.GetSaturatedExtinctionDepth: string;
begin
  Result := FSaturatedExtinctionDepth.Formula;
  ResetItemObserver(ExtinctDepthPosition);
end;

function TRipPlantGroup.GetScreenObject: TObject;
begin
  result := nil;
end;

function TRipPlantGroup.GetMaxET: string;
begin
  Result := FMaxET.Formula;
  ResetItemObserver(MaxETPosition);
end;

function TRipPlantGroup.GetObserver(Index: Integer): TObserver;
begin
  Result := FObserverList[Index];
end;

function TRipPlantGroup.GetRipFormula(Index: integer): string;
begin
  case Index of
    ExtinctDepthPosition:
      result := SaturatedExtinctionDepth;
    ActiveRootDepthPosition:
      result := ActiveRootDepth;
    MaxETPosition:
      result := MaxET;
    SatExtinctionEvapPosition:
      result := SatExtinctionEvap;
    else Assert(False);
  end;
end;

function TRipPlantGroup.GetSatExtinctionEvap: string;
begin
  Result := FSatExtinctionEvap.Formula;
  ResetItemObserver(SatExtinctionEvapPosition);
end;

function TRipPlantGroup.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TRipPlantGroup;
begin
  result := (AnotherItem is TRipPlantGroup);
  if result then
  begin
    SourceItem := TRipPlantGroup(AnotherItem);
    result := (ID = SourceItem.ID)
      and (Name = SourceItem.Name)
      and (SaturatedExtinctionDepth = SourceItem.SaturatedExtinctionDepth)
      and (ActiveRootDepth = SourceItem.ActiveRootDepth)
      and (MaxET = SourceItem.MaxET)
      and (SatExtinctionEvap = SourceItem.SatExtinctionEvap)
      and ETSegments.IsSame(SourceItem.ETSegments)
  end;
end;

procedure TRipPlantGroup.RemoveFormulaObjects;
var
  FormulaManager: TFormulaManager;
begin
  FormulaManager := frmGoPhast.PhastModel.FormulaManager;
  FormulaManager.Remove(FSaturatedExtinctionDepth, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FActiveRootDepth, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FMaxET, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
  FormulaManager.Remove(FSatExtinctionEvap, GlobalRemoveFormulaObjectSubscription,
    GlobalRestoreFormulaObjectSubscription, self);
end;

procedure TRipPlantGroup.ResetItemObserver(Index: integer);
var
  Observer: TObserver;
begin
  Observer := FObserverList[Index];
  Observer.UpToDate := True;
end;

procedure TRipPlantGroup.SetSatExtinctionEvap(const Value: string);
begin
  UpdateFormulaBlocks(Value, SatExtinctionEvapPosition, FSatExtinctionEvap);
end;

procedure TRipPlantGroup.SetActiveRootDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, ActiveRootDepthPosition, FActiveRootDepth);
end;

procedure TRipPlantGroup.SetETSegments(const Value: TRipETSegments);
begin
  FETSegments.Assign(Value)
end;

procedure TRipPlantGroup.SetSaturatedExtinctionDepth(const Value: string);
begin
  UpdateFormulaBlocks(Value, ExtinctDepthPosition, FSaturatedExtinctionDepth);
end;

procedure TRipPlantGroup.StopTalkingToAnyone;
var
  ObserverIndex: Integer;
begin
  for ObserverIndex := 0 to FObserverList.Count - 1 do
  begin
    FObserverList[ObserverIndex].StopTalkingToAnyone;
  end;
end;

procedure TRipPlantGroup.SetID(const Value: integer);
begin
  FID := Value;
end;

procedure TRipPlantGroup.SetMaxET(const Value: string);
begin
  UpdateFormulaBlocks(Value, MaxETPosition, FMaxET);
end;

procedure TRipPlantGroup.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
end;

procedure TRipPlantGroup.SetRipFormula(Index: integer; const Value: string);
begin
  case Index of
    ExtinctDepthPosition:
      SaturatedExtinctionDepth := Value;
    ActiveRootDepthPosition:
      ActiveRootDepth := Value;
    MaxETPosition:
      MaxET := Value;
    SatExtinctionEvapPosition:
      SatExtinctionEvap := Value;
    else Assert(False);
  end;
end;

procedure TRipPlantGroup.UpdateFormulaDependencies(OldFormula: string;
  var NewFormula: string; Observer: TObserver; Compiler: TRbwParser);
var
  OldUses: TStringList;
  NewUses: TStringList;
  Position: integer;
  DS: TObserver;
  Index: integer;
  procedure CompileFormula(var AFormula: string; UsesList: TStringList);
  begin
    if AFormula <> '' then
    begin
      try
        Compiler.Compile(AFormula);
        UsesList.Assign(Compiler.CurrentExpression.VariablesUsed);
      except
        on E: ERbwParserError do
        begin
        end;
      end;
    end;
  end;

begin
  OldFormula := Trim(OldFormula);
  NewFormula := Trim(NewFormula);
  if OldFormula = NewFormula then
  begin
    Exit;
  end;
  if (frmGoPhast.PhastModel <> nil) and
    ((frmGoPhast.PhastModel.ComponentState * [csLoading, csReading]) <> []) then
  begin
    Exit;
  end;
  OldUses := TStringList.Create;
  NewUses := TStringList.Create;
  try
    CompileFormula(OldFormula, OldUses);
    CompileFormula(NewFormula, NewUses);
    for Index := OldUses.Count - 1 downto 0 do
    begin
      Position := NewUses.IndexOf(OldUses[Index]);
      if Position >= 0 then
      begin
        OldUses.Delete(Index);
        NewUses.Delete(Position);
      end;
    end;
    for Index := 0 to OldUses.Count - 1 do
    begin
      DS := frmGoPhast.PhastModel.GetObserverByName(OldUses[Index]);
      Assert(DS <> nil);
      DS.StopsTalkingTo(Observer);
    end;
    for Index := 0 to NewUses.Count - 1 do
    begin
      DS := frmGoPhast.PhastModel.GetObserverByName(NewUses[Index]);
      Assert(DS <> nil);
      DS.TalksTo(Observer);
    end;
  finally
    NewUses.Free;
    OldUses.Free;
  end;
end;

{ TRipPlantGroups }

function TRipPlantGroups.Add: TRipPlantGroup;
begin
  result := inherited Add as TRipPlantGroup;
  Inc(FMaxID);
  result.ID := MaxID
end;

procedure TRipPlantGroups.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TRipPlantGroups then
  begin
    MaxID := TRipPlantGroups(Source).MaxID;
  end;
end;

constructor TRipPlantGroups.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TRipPlantGroup, Model);
  FErrorStrings := TStringList.Create;
end;

destructor TRipPlantGroups.Destroy;
begin
  FErrorStrings.Free;
  inherited;
end;

function TRipPlantGroups.GetItems(Index: Integer): TRipPlantGroup;
begin
  result := inherited Items[Index] as TRipPlantGroup;
end;

function TRipPlantGroups.IndexOfName(AName: string): Integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if Items[index].Name = AName then
    begin
      result := index;
      Break;
    end;
  end;
end;

function TRipPlantGroups.PlantGroupWithBadDepthSegments: TStrings;
var
  PlantGroupIndex: Integer;
  APlantGroup: TRipPlantGroup;
begin
  FErrorStrings.Clear;
  for PlantGroupIndex := 0 to Count - 1 do
  begin
    APlantGroup := Items[PlantGroupIndex];
    if not APlantGroup.ETSegments.SumOfSegmentsEqualsOne then
    begin
      FErrorStrings.Add(APlantGroup.Name);
    end;
  end;
  result := FErrorStrings;
end;

function TRipPlantGroups.PlantGroupWithBadRateSegments: TStrings;
var
  PlantGroupIndex: Integer;
  APlantGroup: TRipPlantGroup;
begin
  FErrorStrings.Clear;
  for PlantGroupIndex := 0 to Count - 1 do
  begin
    APlantGroup := Items[PlantGroupIndex];
    if not APlantGroup.ETSegments.MaximumRateEqualsOne then
    begin
      FErrorStrings.Add(APlantGroup.Name);
    end;
  end;
  result := FErrorStrings;
end;

procedure TRipPlantGroups.SetItems(Index: Integer; const Value: TRipPlantGroup);
begin
  inherited Items[Index] := Value;
end;

procedure TRipPlantGroups.SetMaxID(const Value: Integer);
begin
  FMaxID := Value;
end;

procedure TRipPlantGroups.StopTalkingToAnyone;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].StopTalkingToAnyone;
  end;
end;

end.
