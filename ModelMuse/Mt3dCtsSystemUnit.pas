unit Mt3dCtsSystemUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit,
  System.Generics.Collections, Mt3dmsChemUnit, FormulaManagerUnit;

type
  // ITRTINJ
  TTreatmentDistribution = (tlNone, tlUniform, tlIndividual);
  // IOPTINJ
  TTreatmentOption = (toPercentage, toConcentrationChange, toMass, toConcentration);

  TListOfObjects = TList<TObject>;

  // @name allows the users to change the injection or extraction wells over
  // time but typically, the same wells will be used in all included times.
  TCtsObjectItem = class(TCustomModflowBoundaryItem)
  private
    FExtractionWellList: TListOfObjects;
    FInjectionWellList: TListOfObjects;
    FExtractionWellNames: TStringList;
    FInjectionWellNames: TStringList;
    function GetExtractionWellObjects: TStrings;
    function GetInjectionWellObjects: TStrings;
    procedure SetExtractionWellObjects(const Value: TStrings);
    procedure SetInjectionWellObjects(const Value: TStrings);
    procedure Loaded(Model: TBaseModel);
    procedure WellsAssigned(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddExtractionWell(AScreenObject: TObject);
    procedure RemoveExtractionWell(AScreenObject: TObject);
    procedure AddInjectionWell(AScreenObject: TObject);
    procedure RemoveInjectionWell(AScreenObject: TObject);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  published
    // data set 4
    property ExtractionWellObjects: TStrings read GetExtractionWellObjects
      write SetExtractionWellObjects;
    // data set 8
    property InjectionWellObjects: TStrings read GetInjectionWellObjects
      write SetInjectionWellObjects;
  end;

  TCtsObjectCollection = class(TCustomNonSpatialBoundColl)
  private
    function GetItem(Index: Integer): TCtsObjectItem;
    procedure SetItem(Index: Integer; const Value: TCtsObjectItem);
    procedure Loaded(Model: TBaseModel);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    function Add: TCtsObjectItem;
    property Items[Index: Integer]: TCtsObjectItem read GetItem write SetItem; default;
  end;

  // One @name is used for each injection well object for each chemical
  // component. The formula for the treatment value is specified in the
  // inherited @link(Value) property.
  TInjectionOptionItem = class(TStringConcValueItem)
  private
    FTreatmentOption: TTreatmentOption;
    procedure SetTreatmentOption(const Value: TTreatmentOption);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    // Data set 6 IOPTINJ
    property TreatmentOption: TTreatmentOption read FTreatmentOption
      write SetTreatmentOption;
  end;

  // @name is a collection of @link(TInjectionOptionItem)s.
  // There is one @link(TInjectionOptionItem) for each chemical species.
  TInjectionOptionCollection = class(TStringConcCollection)
  private
    function GetItem(Index: Integer): TInjectionOptionItem;
    procedure SetItem(Index: Integer; const Value: TInjectionOptionItem);
  protected

  public
    constructor Create(Model: TBaseModel; ScreenObject: TObject;
      Mt3dmsConcCollection: TCollection);
    function Add: TInjectionOptionItem;
    property Items[Index: Integer]: TInjectionOptionItem read GetItem write SetItem; default;
  end;

  // @name allows the treatment to change over time.
  TCtsInjectionTimeItem = class(TCustomModflowBoundaryItem)
  private
    FInjectionOptions: TInjectionOptionCollection;
    procedure SetInjectionOptions(const Value: TInjectionOptionCollection);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
//    procedure Loaded(Model: TBaseModel);
  published
    // Data set 6 if time variable
    property InjectionOptions: TInjectionOptionCollection read FInjectionOptions
      write SetInjectionOptions;
  end;

  TCtsInjectionTimeCollection = class(TCustomNonSpatialBoundColl)
  private
//    FUseDefaultInjectionOptions: boolean;
//    FInjectionObject: TObject;
//    FInjectionObjectName: string;
    function GetItem(Index: Integer): TCtsInjectionTimeItem;
    procedure SetItem(Index: Integer; const Value: TCtsInjectionTimeItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TCtsInjectionTimeItem read GetItem
      write SetItem; default;
  published
  end;

  // @name is used only if @link(TTreatmentDistribution) is tlIndividual
  TIndividualWellInjectionItem = class(TOrderedItem)
  private
    FUseDefaultInjectionOptions: boolean;
    FInjectionObject: TObject;
    FInjectionObjectName: string;
    FInjections: TCtsInjectionTimeCollection;
    function GetInjectionWellObjectName: string;
    procedure SetInjectionObject(const Value: TObject);
    procedure SetInjectionWellObjectName(const Value: string);
    procedure SetUseDefaultInjectionOptions(const Value: boolean);
    procedure Loaded(Model: TBaseModel);
    procedure SetInjections(const Value: TCtsInjectionTimeCollection);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    property InjectionObject: TObject read FInjectionObject
      write SetInjectionObject;
  published
    property InjectionWellObjectName: string read GetInjectionWellObjectName
      write SetInjectionWellObjectName;
    property UseDefaultInjectionOptions: boolean
      read FUseDefaultInjectionOptions write SetUseDefaultInjectionOptions;
    property Injections: TCtsInjectionTimeCollection read FInjections write SetInjections;
  end;

  TIndividualWellInjectionCollection = class(TOrderedCollection)
  private
    procedure Loaded(Model: TBaseModel);
    function GetItem(Index: Integer): TIndividualWellInjectionItem;
    procedure SetItem(Index: Integer;
      const Value: TIndividualWellInjectionItem);
  public
    constructor Create(Model: TBaseModel);
    property Items[Index: Integer]: TIndividualWellInjectionItem read GetItem
      write SetItem; default;
  end;

  TCtsExternalFlowsItem = class(TCustomModflowBoundaryItem)
  private
    FInflowConcentrations: TStringConcCollection;
    FInflowFormula: TFormulaObject;
    FOutflowFormula: TFormulaObject;
    function GetInflow: string;
    function GetOutflow: string;
    procedure SetInflow(const Value: string);
    procedure SetInflowConcentrations(const Value: TStringConcCollection);
    procedure SetOutflow(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure CreateFormulaObjects; override;
    function BoundaryFormulaCount: integer; override;

    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
    // See @link(BoundaryFormula).
    function GetBoundaryFormula(Index: integer): string; override;
    // See @link(BoundaryFormula).
    procedure SetBoundaryFormula(Index: integer; const Value: string); override;
  public
    const InflowPosition = 0;
    const OutflowPosition = 1;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // Data set 9 QOUTCTS
    property Outflow: string read GetOutflow write SetOutflow;
    // Data set 5 QINCTS
    property Inflow: string read GetInflow write SetInflow;
    // Data set 5 CINCTS
    property InflowConcentrations: TStringConcCollection read FInflowConcentrations
      write SetInflowConcentrations;
  end;

  TCtsExternalFlowsCollection = class(TCustomNonSpatialBoundColl)
  private
    function GetItem(Index: Integer): TCtsExternalFlowsItem;
    procedure SetItem(Index: Integer; const Value: TCtsExternalFlowsItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TCtsExternalFlowsItem read GetItem write SetItem; default;
  end;

  TCtsSystem = class(TModflowScreenObjectProperty)
  private
    FTreatmentDistribution: TTreatmentDistribution;
    FDefaultInjectionOptions: TCtsInjectionTimeCollection;
    FCtsObjects: TCtsObjectCollection;
    FExternalFlows: TCtsExternalFlowsCollection;
    FInjections: TIndividualWellInjectionCollection;
    FName: string;
    procedure SetCtsObjects(const Value: TCtsObjectCollection);
    procedure SetDefaultInjectionOptions(
      const Value: TCtsInjectionTimeCollection);
    procedure SetExternalFlows(const Value: TCtsExternalFlowsCollection);
    procedure SetTreatmentDistribution(const Value: TTreatmentDistribution);
    function IsSame(AnotherCtsSystem: TCtsSystem): boolean;
    procedure SetInjections(const Value: TIndividualWellInjectionCollection);
    procedure SetName(const Value: string);
  protected
    function BoundaryObserverPrefix: string; override;
  public
    Constructor Create(Model: TBaseModel; ScreenObject: TObject);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
    function Used: boolean; override;
  published
    // data set 3 ITRTINJ
    property TreatmentDistribution: TTreatmentDistribution
      read FTreatmentDistribution write SetTreatmentDistribution;
    // data set 6
    property DefaultInjectionOptions: TCtsInjectionTimeCollection
      read FDefaultInjectionOptions write SetDefaultInjectionOptions;
    // data sets 4 and 8
    property CtsObjects: TCtsObjectCollection read FCtsObjects
      write SetCtsObjects;
    // data sets 5 and 9
    property ExternalFlows: TCtsExternalFlowsCollection read FExternalFlows
      write SetExternalFlows;
    property Injections: TIndividualWellInjectionCollection read FInjections write SetInjections;
    property Name: string read FName write SetName;
  end;

  TCtsSystemItem = class(TOrderedItem)
  private
    FCtsSystem: TCtsSystem;
    procedure SetCtsSystem(const Value: TCtsSystem);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
  published
    property CtsSystem: TCtsSystem read FCtsSystem write SetCtsSystem;
  end;

  TCtsSystemCollection = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TCtsSystemItem;
    procedure SetItem(Index: Integer; const Value: TCtsSystemItem);
  public
    constructor Create(Model: TBaseModel);
    function Add: TCtsSystemItem;
    property Items[Index: Integer]: TCtsSystemItem read GetItem write SetItem; default;
  end;

implementation

uses
  ScreenObjectUnit, PhastModelUnit, frmGoPhastUnit;

{ TCtsObjectItem }

procedure TCtsObjectItem.AddExtractionWell(AScreenObject: TObject);
begin
  FExtractionWellList.Add(AScreenObject)
end;

procedure TCtsObjectItem.AddInjectionWell(AScreenObject: TObject);
begin
  FInjectionWellList.Add(AScreenObject)
end;

procedure TCtsObjectItem.Assign(Source: TPersistent);
var
  CstSource: TCtsObjectItem;
begin
  if Source is TCtsObjectItem then
  begin
    CstSource := TCtsObjectItem(Source);
    ExtractionWellObjects := CstSource.ExtractionWellObjects;
    InjectionWellObjects := CstSource.InjectionWellObjects;
  end;
  inherited;
end;

constructor TCtsObjectItem.Create(Collection: TCollection);
begin
  inherited;
  FExtractionWellList := TListOfObjects.Create;
  FInjectionWellList := TListOfObjects.Create;
  FExtractionWellNames := TStringList.Create;
  FInjectionWellNames := TStringList.Create;
  FExtractionWellNames.OnChange := WellsAssigned;
  FInjectionWellNames.OnChange := WellsAssigned;
end;

destructor TCtsObjectItem.Destroy;
begin
  FExtractionWellList.Free;
  FInjectionWellList.Free;
  FExtractionWellNames.Free;
  FInjectionWellNames.Free;
  inherited;
end;

function TCtsObjectItem.GetExtractionWellObjects: TStrings;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  FExtractionWellNames.Clear;
  for Index := 0 to FExtractionWellList.Count - 1 do
  begin
    AScreenObject := FExtractionWellList[Index] as TScreenObject;
    if not AScreenObject.Deleted then
    begin
      FExtractionWellNames.AddObject(AScreenObject.Name, AScreenObject)
    end;
  end;
  result := FExtractionWellNames;
end;

function TCtsObjectItem.GetInjectionWellObjects: TStrings;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  FInjectionWellNames.Clear;
  for Index := 0 to FInjectionWellList.Count - 1 do
  begin
    AScreenObject := FInjectionWellList[Index] as TScreenObject;
    if not AScreenObject.Deleted then
    begin
      FInjectionWellNames.AddObject(AScreenObject.Name, AScreenObject)
    end;
  end;
  result := FInjectionWellNames;
end;

function TCtsObjectItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCtsObjectItem;
  LocalObjects: TStrings;
  OtherObjects: TStrings;
  Index: integer;
begin
  Result := (AnotherItem is TCtsObjectItem) and inherited IsSame(AnotherItem);
  if Result then
  begin
    OtherItem := TCtsObjectItem(AnotherItem);
    LocalObjects := ExtractionWellObjects;
    OtherObjects := OtherItem.ExtractionWellObjects;
    Result := LocalObjects.Count = OtherObjects.Count;
    if result then
    begin
      for Index := 0 to LocalObjects.Count - 1 do
      begin
        result := LocalObjects[Index] = OtherObjects[Index];
        if not Result then
        begin
          Exit;
        end;
      end;
      LocalObjects := InjectionWellObjects;
      OtherObjects := OtherItem.InjectionWellObjects;
      Result := LocalObjects.Count = OtherObjects.Count;
      if result then
      begin
        for Index := 0 to LocalObjects.Count - 1 do
        begin
          result := LocalObjects[Index] = OtherObjects[Index];
          if not Result then
          begin
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

procedure TCtsObjectItem.Loaded(Model: TBaseModel);
var
  LocalModel: TPhastModel;
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  LocalModel := Model as TPhastModel;
  FExtractionWellNames.OnChange := nil;
  FInjectionWellNames.OnChange := nil;
  try
    FExtractionWellList.Clear;
    for Index := 0 to FExtractionWellNames.Count - 1 do
    begin
      AScreenObject := LocalModel.GetScreenObjectByName(FExtractionWellNames[Index]);
      AddExtractionWell(AScreenObject);
    end;
    FInjectionWellList.Clear;
    for Index := 0 to FInjectionWellNames.Count - 1 do
    begin
      AScreenObject := LocalModel.GetScreenObjectByName(FInjectionWellNames[Index]);
      AddInjectionWell(AScreenObject);
    end;
  finally
    FExtractionWellNames.OnChange := WellsAssigned;
    FInjectionWellNames.OnChange := WellsAssigned;
  end;
end;

procedure TCtsObjectItem.RemoveExtractionWell(AScreenObject: TObject);
begin
  FExtractionWellList.Remove(AScreenObject);
end;

procedure TCtsObjectItem.RemoveInjectionWell(AScreenObject: TObject);
begin
  FInjectionWellList.Remove(AScreenObject);
end;

procedure TCtsObjectItem.SetExtractionWellObjects(const Value: TStrings);
begin
  FExtractionWellNames.Assign(Value);
end;

procedure TCtsObjectItem.SetInjectionWellObjects(const Value: TStrings);
begin
  FInjectionWellNames.Assign(Value);
end;

procedure TCtsObjectItem.WellsAssigned(Sender: TObject);
var
  AScreenObject: TObject;
  Index: Integer;
begin
  if Sender = FExtractionWellNames then
  begin
    FExtractionWellList.Clear;
    for Index := 0 to FExtractionWellNames.Count - 1 do
    begin
      AScreenObject := FExtractionWellNames.Objects[Index];
      if AScreenObject <> nil then
      begin
        AddExtractionWell(AScreenObject);
      end;
    end;
  end
  else if Sender = FInjectionWellNames then
  begin
    FInjectionWellList.Clear;
    for Index := 0 to FInjectionWellNames.Count - 1 do
    begin
      AScreenObject := FInjectionWellNames.Objects[Index];
      if AScreenObject <> nil then
      begin
        AddInjectionWell(AScreenObject);
      end;
    end;
  end
  else
  begin
    Assert(False);
  end;
end;

{ TInjectionOptionItem }

procedure TInjectionOptionItem.Assign(Source: TPersistent);
var
  InjSource: TInjectionOptionItem;
begin
  if Source is TInjectionOptionItem then
  begin
    InjSource :=  TInjectionOptionItem(Source);
    TreatmentOption := InjSource.TreatmentOption;
  end;
  inherited;
end;

function TInjectionOptionItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TInjectionOptionItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    result := TreatmentOption = TInjectionOptionItem(AnotherItem).TreatmentOption;
  end;
end;

procedure TInjectionOptionItem.SetTreatmentOption(
  const Value: TTreatmentOption);
begin
  if FTreatmentOption <> Value then
  begin
    FTreatmentOption := Value;
    InvalidateModel;
  end;
end;

{ TCtsInjectionTimeItem }

procedure TCtsInjectionTimeItem.Assign(Source: TPersistent);
var
  InjTimeItem: TCtsInjectionTimeItem;
begin
  if  Source is TCtsInjectionTimeItem then
  begin
    InjTimeItem := TCtsInjectionTimeItem(Source);
    InjectionOptions := InjTimeItem.InjectionOptions;
  end;
  inherited;
end;

constructor TCtsInjectionTimeItem.Create(Collection: TCollection);
begin
  inherited;
  FInjectionOptions := TInjectionOptionCollection.Create(Model, ScreenObject, Collection);
end;

destructor TCtsInjectionTimeItem.Destroy;
begin
  FInjectionOptions.Free;
  inherited;
end;

function TCtsInjectionTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCtsInjectionTimeItem;
begin
  result := (AnotherItem is TCtsInjectionTimeItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TCtsInjectionTimeItem(AnotherItem);
    result := InjectionOptions.IsSame(OtherItem.InjectionOptions);
  end;
end;

procedure TCtsInjectionTimeItem.SetInjectionOptions(
  const Value: TInjectionOptionCollection);
begin
  FInjectionOptions.Assign(Value);
end;

{ TCtsExternalFlowsItem }

procedure TCtsExternalFlowsItem.Assign(Source: TPersistent);
var
  ExternalFlowsItem: TCtsExternalFlowsItem;
begin
  if Source is TCtsExternalFlowsItem then
  begin
    ExternalFlowsItem := TCtsExternalFlowsItem(Source);
    Outflow := ExternalFlowsItem.Outflow;
    Inflow := ExternalFlowsItem.Inflow;
    InflowConcentrations := ExternalFlowsItem.InflowConcentrations;
  end;
  inherited;
end;

procedure TCtsExternalFlowsItem.AssignObserverEvents(Collection: TCollection);
begin
  inherited;

end;

function TCtsExternalFlowsItem.BoundaryFormulaCount: integer;
begin
  result := 2;
end;

constructor TCtsExternalFlowsItem.Create(Collection: TCollection);
begin
  inherited;
  FInflowConcentrations := TStringConcCollection.Create(Model, ScreenObject,
    Collection);
end;

procedure TCtsExternalFlowsItem.CreateFormulaObjects;
begin
  inherited;
  FInflowFormula := CreateFormulaObject(dso3D);
  FOutflowFormula := CreateFormulaObject(dso3D);
end;

destructor TCtsExternalFlowsItem.Destroy;
begin
  FInflowConcentrations.Free;
  Inflow := '0';
  Outflow := '0';
  inherited;
end;

function TCtsExternalFlowsItem.GetBoundaryFormula(Index: integer): string;
begin
  case Index of
    InflowPosition:
      begin
        result := Inflow
      end;
    OutflowPosition:
      begin
        result := Outflow
      end;
    else
      Assert(False);
  end;
end;

function TCtsExternalFlowsItem.GetInflow: string;
begin
  Result := FInflowFormula.Formula;
  ResetItemObserver(InflowPosition);
end;

function TCtsExternalFlowsItem.GetOutflow: string;
begin
  Result := FOutflowFormula.Formula;
  ResetItemObserver(OutflowPosition);
end;

procedure TCtsExternalFlowsItem.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
  if (Sender = FInflowFormula) then
  begin
    List.Add(FObserverList[InflowPosition]);
  end
  else if (Sender = FOutflowFormula) then
  begin
    List.Add(FObserverList[OutflowPosition]);
  end
  else
  begin
    Assert(False);
  end;
end;

function TCtsExternalFlowsItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  ExtItem: TCtsExternalFlowsItem;
begin
  result := (AnotherItem is TCtsExternalFlowsItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    ExtItem := TCtsExternalFlowsItem(AnotherItem);
    result := (Outflow = ExtItem.Outflow)
      and (Inflow = ExtItem.Inflow)
      and InflowConcentrations.IsSame(ExtItem.InflowConcentrations);
  end;
end;

procedure TCtsExternalFlowsItem.RemoveFormulaObjects;
begin
  frmGoPhast.PhastModel.FormulaManager.Remove(FInflowFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);
  frmGoPhast.PhastModel.FormulaManager.Remove(FOutflowFormula,
    GlobalRemoveModflowBoundaryItemSubscription,
    GlobalRestoreModflowBoundaryItemSubscription, self);

end;

procedure TCtsExternalFlowsItem.SetBoundaryFormula(Index: integer;
  const Value: string);
begin
  case Index of
    InflowPosition:
      begin
        Inflow := Value;
      end;
    OutflowPosition:
      begin
        Outflow := Value;
      end;
    else
      Assert(False);
  end;
end;

procedure TCtsExternalFlowsItem.SetInflow(const Value: string);
begin
  UpdateFormula(Value, InflowPosition, FInflowFormula);
end;

procedure TCtsExternalFlowsItem.SetInflowConcentrations(const Value: TStringConcCollection);
begin
  FInflowConcentrations.Assign(Value);
end;

procedure TCtsExternalFlowsItem.SetOutflow(const Value: string);
begin
  UpdateFormula(Value, OutflowPosition, FOutflowFormula);
end;

{ TCtsObjectCollection }

function TCtsObjectCollection.Add: TCtsObjectItem;
begin
  result := inherited Add as TCtsObjectItem;
end;

function TCtsObjectCollection.GetItem(Index: Integer): TCtsObjectItem;
begin
  result := inherited GetItem(Index) as TCtsObjectItem;
end;

class function TCtsObjectCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCtsObjectItem;
end;

procedure TCtsObjectCollection.Loaded(Model: TBaseModel);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded(Model);
  end;
end;

procedure TCtsObjectCollection.SetItem(Index: Integer;
  const Value: TCtsObjectItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCtsInjectionTimeCollection }

function TCtsInjectionTimeCollection.GetItem(
  Index: Integer): TCtsInjectionTimeItem;
begin
  result := inherited Items[Index] as TCtsInjectionTimeItem;
end;

class function TCtsInjectionTimeCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCtsInjectionTimeItem;
end;

procedure TCtsInjectionTimeCollection.SetItem(Index: Integer;
  const Value: TCtsInjectionTimeItem);
begin
  inherited Items[Index] := Value;
end;

{ TCtsExternalFlowsCollection }

function TCtsExternalFlowsCollection.GetItem(
  Index: Integer): TCtsExternalFlowsItem;
begin
  result := inherited Items[index] as TCtsExternalFlowsItem;
end;

class function TCtsExternalFlowsCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TCtsExternalFlowsItem
end;

procedure TCtsExternalFlowsCollection.SetItem(Index: Integer;
  const Value: TCtsExternalFlowsItem);
begin
  inherited Items[index] := Value;
end;

{ TCtsSystem }

procedure TCtsSystem.Assign(Source: TPersistent);
var
  CtsSystem: TCtsSystem;
begin
  if Source is TCtsSystem then
  begin
    CtsSystem := TCtsSystem(Source);
    TreatmentDistribution := CtsSystem.TreatmentDistribution;
    DefaultInjectionOptions := CtsSystem.DefaultInjectionOptions;
    CtsObjects := CtsSystem.CtsObjects;
    ExternalFlows := CtsSystem.ExternalFlows;
    Injections := CtsSystem.Injections;
    Name := CtsSystem.Name;
  end;
  inherited;
end;

function TCtsSystem.BoundaryObserverPrefix: string;
begin
  result := 'CTS_';
end;

constructor TCtsSystem.Create(Model: TBaseModel; ScreenObject: TObject);
begin
  FDefaultInjectionOptions := TCtsInjectionTimeCollection.Create(Self, Model, ScreenObject);
  FCtsObjects := TCtsObjectCollection.Create(Self, Model, ScreenObject);
  FExternalFlows := TCtsExternalFlowsCollection.Create(Self, Model, ScreenObject);
  FInjections := TIndividualWellInjectionCollection.Create(Model);
end;

destructor TCtsSystem.Destroy;
begin
  FInjections.Free;
  FExternalFlows.Free;
  FCtsObjects.Free;
  FDefaultInjectionOptions.Free;
  inherited;
end;

function TCtsSystem.IsSame(AnotherCtsSystem: TCtsSystem): boolean;
begin
  Result := (TreatmentDistribution = AnotherCtsSystem.TreatmentDistribution)
    and DefaultInjectionOptions.IsSame(AnotherCtsSystem.DefaultInjectionOptions)
    and CtsObjects.IsSame(AnotherCtsSystem.CtsObjects)
    and ExternalFlows.IsSame(AnotherCtsSystem.ExternalFlows);
end;

procedure TCtsSystem.Loaded;
begin
  FCtsObjects.Loaded(ParentModel);
  FInjections.Loaded(ParentModel);
end;

procedure TCtsSystem.SetCtsObjects(const Value: TCtsObjectCollection);
begin
  FCtsObjects.Assign(Value);
end;

procedure TCtsSystem.SetDefaultInjectionOptions(
  const Value: TCtsInjectionTimeCollection);
begin
  FDefaultInjectionOptions.Assign(Value);
end;

procedure TCtsSystem.SetExternalFlows(const Value: TCtsExternalFlowsCollection);
begin
  FExternalFlows.Assign(Value);
end;

procedure TCtsSystem.SetInjections(const Value: TIndividualWellInjectionCollection);
begin
  FInjections.Assign(Value);
end;

procedure TCtsSystem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
    InvalidateModel;
  end;
end;

procedure TCtsSystem.SetTreatmentDistribution(
  const Value: TTreatmentDistribution);
begin
  FTreatmentDistribution := Value;
end;

function TCtsSystem.Used: boolean;
begin
  result := True;
end;

{ TInjectionOptionCollection }

function TInjectionOptionCollection.Add: TInjectionOptionItem;
begin
  Result := inherited Add as TInjectionOptionItem;
end;

constructor TInjectionOptionCollection.Create(Model: TBaseModel;
  ScreenObject: TObject; Mt3dmsConcCollection: TCollection);
begin
  inherited Create(TInjectionOptionItem, Model, ScreenObject, Mt3dmsConcCollection);
end;

function TInjectionOptionCollection.GetItem(
  Index: Integer): TInjectionOptionItem;
begin
  result := inherited Items[Index] as TInjectionOptionItem;
end;

procedure TInjectionOptionCollection.SetItem(Index: Integer;
  const Value: TInjectionOptionItem);
begin
  inherited Items[Index] := Value;
end;

{ TCtsSystemItem }

procedure TCtsSystemItem.Assign(Source: TPersistent);
begin
  if Source is TCtsSystemItem then
  begin
    CtsSystem := TCtsSystemItem(Source).CtsSystem;
  end;
  inherited;

end;

constructor TCtsSystemItem.Create(Collection: TCollection);
begin
  inherited;
  FCtsSystem := TCtsSystem.Create(Model, nil);
end;

destructor TCtsSystemItem.Destroy;
begin
  FCtsSystem.Free;
  inherited;
end;

function TCtsSystemItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TCtsSystemItem);
  if result then
  begin
    Result := CtsSystem.IsSame(TCtsSystemItem(AnotherItem).CtsSystem)
  end;
end;

procedure TCtsSystemItem.Loaded;
begin
  FCtsSystem.Loaded;
end;

procedure TCtsSystemItem.SetCtsSystem(const Value: TCtsSystem);
begin
  FCtsSystem.Assign(Value);
end;

{ TCtsSystemCollection }

function TCtsSystemCollection.Add: TCtsSystemItem;
begin
  result := inherited Add as TCtsSystemItem
end;

constructor TCtsSystemCollection.Create(Model: TBaseModel);
begin
  inherited Create(TCtsSystemItem, Model);
end;

function TCtsSystemCollection.GetItem(Index: Integer): TCtsSystemItem;
begin
  result := inherited Items[index] as TCtsSystemItem
end;

procedure TCtsSystemCollection.SetItem(Index: Integer;
  const Value: TCtsSystemItem);
begin
  inherited Items[index] := Value;
end;

{ TIndividualWellInjectionItem }

procedure TIndividualWellInjectionItem.Assign(Source: TPersistent);
var
  IndWellItem: TIndividualWellInjectionItem;
begin
  if Source is TIndividualWellInjectionItem then
  begin
    IndWellItem := TIndividualWellInjectionItem(Source);
    InjectionWellObjectName := IndWellItem.InjectionWellObjectName;
    UseDefaultInjectionOptions := IndWellItem.UseDefaultInjectionOptions;
    Injections := IndWellItem.Injections;
  end;
  inherited;

end;

constructor TIndividualWellInjectionItem.Create(Collection: TCollection);
begin
  inherited;
  FInjections := TCtsInjectionTimeCollection.Create(nil, Model, nil);
end;

destructor TIndividualWellInjectionItem.Destroy;
begin
  FInjections.Free;
  inherited;
end;

function TIndividualWellInjectionItem.GetInjectionWellObjectName: string;
begin
  if FInjectionObject <> nil then
  begin
    result := (FInjectionObject as TScreenObject).Name;
  end
  else
  begin
    result := FInjectionObjectName;
  end;
end;

function TIndividualWellInjectionItem.IsSame(
  AnotherItem: TOrderedItem): boolean;
var
  InjWellItem: TIndividualWellInjectionItem;
begin
  result := (AnotherItem is TIndividualWellInjectionItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
    InjWellItem := TIndividualWellInjectionItem(AnotherItem);
    result :=
      (InjectionWellObjectName = InjWellItem.InjectionWellObjectName)
      and (UseDefaultInjectionOptions = InjWellItem.UseDefaultInjectionOptions)
      and (Injections.IsSame(InjWellItem.Injections));
  end;
end;

procedure TIndividualWellInjectionItem.Loaded(Model: TBaseModel);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Model as TPhastModel;
  InjectionObject := LocalModel.GetScreenObjectByName(InjectionWellObjectName);
end;

procedure TIndividualWellInjectionItem.SetInjectionObject(const Value: TObject);
begin
  if Value <> nil then
  begin
    Assert(Value is TScreenObject);
    FInjectionObjectName := TScreenObject(Value).Name;
  end;
  FInjectionObject := Value;
end;

procedure TIndividualWellInjectionItem.SetInjections(
  const Value: TCtsInjectionTimeCollection);
begin
  FInjections.Assign(Value);
end;

procedure TIndividualWellInjectionItem.SetInjectionWellObjectName(
  const Value: string);
begin
  if FInjectionObjectName <> Value then
  begin
    FInjectionObjectName := Value;
    if (FInjectionObjectName <> '') and (Model <> nil) then
    begin
      InjectionObject := (Model as TCustomModel).
        GetScreenObjectByName(FInjectionObjectName)
    end;
  end;
end;

procedure TIndividualWellInjectionItem.SetUseDefaultInjectionOptions(
  const Value: boolean);
begin
  SetBooleanProperty(FUseDefaultInjectionOptions, Value);
end;

{ TIndividualWellInjectionCollection }

constructor TIndividualWellInjectionCollection.Create(Model: TBaseModel);
begin
  inherited Create(TIndividualWellInjectionItem, Model);
end;

function TIndividualWellInjectionCollection.GetItem(
  Index: Integer): TIndividualWellInjectionItem;
begin
  result := inherited Items[index] as TIndividualWellInjectionItem
end;

procedure TIndividualWellInjectionCollection.Loaded(Model: TBaseModel);
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[Index].Loaded(Model);
  end;
end;

procedure TIndividualWellInjectionCollection.SetItem(Index: Integer;
  const Value: TIndividualWellInjectionItem);
begin
  inherited Items[index] := Value;
end;

end.
