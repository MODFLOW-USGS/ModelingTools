unit Mt3dCtsSystemUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit,
  System.Generics.Collections, Mt3dmsChemUnit;

type
  // ITRTINJ
  TTreatmentDistribution = (tlNone, tlUniform, tlIndividual);
  // IOPTINJ
  TTreatmentOption = (toPercentage, toConcentrationChange, toMass, toConcentration);

{
  TStringConcValueItem = class(TFormulaOrderedItem)
  private
    FValue: TFormulaObject;
    FObserver: TObserver;
    FName: string;
    procedure SetValue(const Value: string);
    function StringCollection: TStringConcCollection;
    function GetValue: string;
    procedure RemoveSubscription(Sender: TObject; const AName: string);
    procedure RestoreSubscription(Sender: TObject; const AName: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function GetObserver(Index: Integer): TObserver; override;
    function GetScreenObject: TObject; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Value: string read GetValue write SetValue;
    property Name: string read FName write FName;
  end;
}

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
    property ExtractionWellObjects: TStrings read GetExtractionWellObjects
      write SetExtractionWellObjects;
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
//    constructor Create(Boundary: TModflowScreenObjectProperty;
//      Model: TBaseModel; ScreenObject: TObject); override;
    function Add: TCtsObjectItem;
    property Items[Index: Integer]: TCtsObjectItem read GetItem write SetItem; default;
  end;

  // One @name is used for each injection well object for each chemical component.
  // The formula for the treatment value is specified in the
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
    property TreatmentOption: TTreatmentOption read FTreatmentOption write SetTreatmentOption;
  end;

  // @name is a collection of @link(TInjectionOptionItem)s.
  // There is one @link(TInjectionOptionItem) for each chemical species.
  TInjectionOptionCollection = class(TStringConcCollection)
  protected
  public
    constructor Create(Model: TBaseModel; ScreenObject: TObject;
      Mt3dmsConcCollection: TCollection);
  end;

  // @name allows the treatment to change over time.
  TCtsInjectionTimeItem = class(TCustomModflowBoundaryItem)
  private
    FUseDefaultInjectionOptions: boolean;
    FInjectionOptions: TInjectionOptionCollection;
    FInjectionObject: TObject;
    FInjectionObjectName: string;
    function GetInjectionWellObjectName: string;
    procedure SetInjectionOptions(const Value: TInjectionOptionCollection);
    procedure SetInjectionWellObjectName(const Value: string);
    procedure SetUseDefaultInjectionOptions(const Value: boolean);
    procedure SetInjectionObject(const Value: TObject);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property InjectionObject: TObject read FInjectionObject write SetInjectionObject;
  published
    property InjectionOptions: TInjectionOptionCollection read FInjectionOptions
      write SetInjectionOptions;
    // @name is used only if @link(TTreatmentDistribution) is tlIndividual
    property InjectionWellObjectName: string read GetInjectionWellObjectName
      write SetInjectionWellObjectName;
    property UseDefaultInjectionOptions: boolean
      read FUseDefaultInjectionOptions write SetUseDefaultInjectionOptions;
  end;

  TCtsInjectionTimeCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
//    constructor Create(Boundary: TModflowScreenObjectProperty;
//      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TCtsExternalFlowsItem = class(TCustomModflowBoundaryItem)
  private
    FInflowConcentrations: TStringConcCollection;
    function GetInflow: string;
    function GetOutflow: string;
    procedure SetInflow(const Value: string);
    procedure SetInflowConcentrations(const Value: TStringConcCollection);
    procedure SetOutflow(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Outflow: string read GetOutflow write SetOutflow;
    property Inflow: string read GetInflow write SetInflow;
    property InflowConcentrations: TStringConcCollection read FInflowConcentrations
      write SetInflowConcentrations;
  end;

  TCtsExternalFlowsCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TCtsSystem = class(TGoPhastPersistent)
  private
    FTreatmentDistribution: TTreatmentDistribution;
    FDefaultInjectionOptions: TInjectionOptionCollection;
    FCtsObjects: TCtsObjectCollection;
    FExternalFlows: TCtsExternalFlowsCollection;
    procedure SetCtsObjects(const Value: TCtsObjectCollection);
    procedure SetDefaultInjectionOptions(
      const Value: TInjectionOptionCollection);
    procedure SetExternalFlows(const Value: TCtsExternalFlowsCollection);
    procedure SetTreatmentDistribution(const Value: TTreatmentDistribution);
  public
    procedure Assign(Source: TPersistent); override;
    procedure Loaded(Model: TBaseModel);
  published
    property TreatmentDistribution: TTreatmentDistribution
      read FTreatmentDistribution write SetTreatmentDistribution;
    property DefaultInjectionOptions: TInjectionOptionCollection
      read FDefaultInjectionOptions write SetDefaultInjectionOptions;
    property CtsObjects: TCtsObjectCollection read FCtsObjects
      write SetCtsObjects;
    property ExternalFlows: TCtsExternalFlowsCollection read FExternalFlows
      write SetExternalFlows;
  end;

implementation

uses
  ScreenObjectUnit, PhastModelUnit;

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
//    TreatmentValue := InjSource.TreatmentValue;
  end;
  inherited;
end;

//function TInjectionOptionItem.GetTreatmentValue: string;
//begin
//
//end;

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
    UseDefaultInjectionOptions := InjTimeItem.UseDefaultInjectionOptions;
    if InjTimeItem.InjectionObject <> nil then
    begin
      InjectionObject := InjTimeItem.InjectionObject;
    end
    else
    begin
      InjectionWellObjectName := InjTimeItem.InjectionWellObjectName
    end;
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

function TCtsInjectionTimeItem.GetInjectionWellObjectName: string;
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

function TCtsInjectionTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCtsInjectionTimeItem;
begin
  result := (AnotherItem is TCtsInjectionTimeItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    OtherItem := TCtsInjectionTimeItem(AnotherItem);
    result := InjectionOptions.IsSame(OtherItem.InjectionOptions)
      and (InjectionWellObjectName = OtherItem.InjectionWellObjectName)
      and (UseDefaultInjectionOptions = OtherItem.UseDefaultInjectionOptions)
  end;
end;

procedure TCtsInjectionTimeItem.SetInjectionObject(const Value: TObject);
begin
  if Value <> nil then
  begin
    Assert(Value is TScreenObject);
    FInjectionObjectName := TScreenObject(Value).Name;
  end;
  FInjectionObject := Value;
end;

procedure TCtsInjectionTimeItem.SetInjectionOptions(
  const Value: TInjectionOptionCollection);
begin
  FInjectionOptions.Assign(Value);
end;

procedure TCtsInjectionTimeItem.SetInjectionWellObjectName(const Value: string);
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

procedure TCtsInjectionTimeItem.SetUseDefaultInjectionOptions(
  const Value: boolean);
begin
  SetBooleanProperty(FUseDefaultInjectionOptions, Value);
end;

{ TCtsExternalFlowsItem }

procedure TCtsExternalFlowsItem.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TCtsExternalFlowsItem.Create(Collection: TCollection);
begin
  inherited;
  FInflowConcentrations := TStringConcCollection.Create(Model, ScreenObject,
    Collection);
end;

destructor TCtsExternalFlowsItem.Destroy;
begin
  FInflowConcentrations.Free;
  inherited;
end;

function TCtsExternalFlowsItem.GetInflow: string;
begin

end;

function TCtsExternalFlowsItem.GetOutflow: string;
begin

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

procedure TCtsExternalFlowsItem.SetInflow(const Value: string);
begin

end;

procedure TCtsExternalFlowsItem.SetInflowConcentrations(const Value: TStringConcCollection);
begin

end;

procedure TCtsExternalFlowsItem.SetOutflow(const Value: string);
begin

end;

{ TCtsObjectCollection }

function TCtsObjectCollection.Add: TCtsObjectItem;
begin
  result := inherited Add as TCtsObjectItem;
end;

//constructor TCtsObjectCollection.Create(Boundary: TModflowScreenObjectProperty;
//  Model: TBaseModel; ScreenObject: TObject);
//begin
//  inherited;
//
//end;

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

{ TInjectionOptionCollection }

//constructor TInjectionOptionCollection.Create(Model: TBaseModel);
//begin
//  inherited Create(TInjectionOptionItem, Model);
//end;

{ TCtsInjectionTimeCollection }

//constructor TCtsInjectionTimeCollection.Create(
//  Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
//  ScreenObject: TObject);
//begin
//  inherited;
//
//end;

class function TCtsInjectionTimeCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCtsInjectionTimeItem;
end;

{ TCtsExternalFlowsCollection }

constructor TCtsExternalFlowsCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;

end;

class function TCtsExternalFlowsCollection.ItemClass: TBoundaryItemClass;
begin
  Result := TCtsExternalFlowsItem
end;

{ TCtsSystem }

procedure TCtsSystem.Assign(Source: TPersistent);
begin
  inherited;

end;

procedure TCtsSystem.Loaded(Model: TBaseModel);
begin
  FCtsObjects.Loaded(Model);
end;

procedure TCtsSystem.SetCtsObjects(const Value: TCtsObjectCollection);
begin
  FCtsObjects := Value;
end;

procedure TCtsSystem.SetDefaultInjectionOptions(
  const Value: TInjectionOptionCollection);
begin
  FDefaultInjectionOptions := Value;
end;

procedure TCtsSystem.SetExternalFlows(const Value: TCtsExternalFlowsCollection);
begin
  FExternalFlows := Value;
end;

procedure TCtsSystem.SetTreatmentDistribution(
  const Value: TTreatmentDistribution);
begin
  FTreatmentDistribution := Value;
end;

{ TInjectionOptionCollection }

constructor TInjectionOptionCollection.Create(Model: TBaseModel;
  ScreenObject: TObject; Mt3dmsConcCollection: TCollection);
begin
  inherited Create(TInjectionOptionItem, Model, ScreenObject, Mt3dmsConcCollection);
end;

end.
