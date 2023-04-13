unit Mt3dCtsSystemUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit,
  System.Generics.Collections, Mt3dmsChemUnit, FormulaManagerUnit,
  ScreenObjectUnit;

type
  // ITRTINJ
  TTreatmentDistribution = (tlNone, tlUniform, tlIndividual);
  // IOPTINJ
  TTreatmentOption = (toPercentage, toConcentrationChange, toMass, toConcentration);

  TListOfObjects = TList<TScreenObject>;

  // @name allows the users to change the injection or extraction wells over
  // time but typically, the same wells will be used in all included times.
  TCtsObjectItem = class(TOrderedItem)
  private
    FExtractionWellList: TListOfObjects;
    FInjectionWellList: TListOfObjects;
    FExtractionWellNames: TStringList;
    FInjectionWellNames: TStringList;
    FStoredStartTime: TRealStorage;
    FStoredEndTime: TRealStorage;
    function GetExtractionWellObjects: TStrings;
    function GetInjectionWellObjects: TStrings;
    procedure SetExtractionWellObjects(const Value: TStrings);
    procedure SetInjectionWellObjects(const Value: TStrings);
    procedure Loaded(Model: TBaseModel);
    procedure WellsAssigned(Sender: TObject);
    procedure SetStoredEndTime(const Value: TRealStorage);
    procedure SetStoredStartTime(const Value: TRealStorage);
    function GetEndTime: Double;
    function GetStartTime: Double;
    procedure SetEndTime(const Value: Double);
    procedure SetStartTime(const Value: Double);
    function GetExtractionWell(Index: Integer): TScreenObject;
    function GetInjectionWell(Index: Integer): TScreenObject;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AddExtractionWell(AScreenObject: TScreenObject);
    procedure RemoveExtractionWell(AScreenObject: TScreenObject);
    procedure AddInjectionWell(AScreenObject: TScreenObject);
    procedure RemoveInjectionWell(AScreenObject: TScreenObject);
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    property StartTime: Double read GetStartTime write SetStartTime;
    property EndTime: Double read GetEndTime write SetEndTime;
    function InjectionWellCount: integer;
    function ExtractionWellCount: integer;
    property InjectionWells[Index: Integer]: TScreenObject read GetInjectionWell;
    property ExtractionWells[Index: Integer]: TScreenObject read GetExtractionWell;
  published
    // data set 4
    property ExtractionWellObjects: TStrings read GetExtractionWellObjects
      write SetExtractionWellObjects;
    // data set 8
    property InjectionWellObjects: TStrings read GetInjectionWellObjects
      write SetInjectionWellObjects;
    property StoredStartTime: TRealStorage read FStoredStartTime write SetStoredStartTime;
    property StoredEndTime: TRealStorage read FStoredEndTime write SetStoredEndTime;
  end;

  TCtsObjectCollection = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TCtsObjectItem;
    procedure SetItem(Index: Integer; const Value: TCtsObjectItem);
    procedure Loaded(Model: TBaseModel);
//    class function ItemClass: TBoundaryItemClass; static;
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    function Add: TCtsObjectItem;
    property Items[Index: Integer]: TCtsObjectItem read GetItem write SetItem; default;
    function GetItemByStartTime(StartTime: Double): TCtsObjectItem;
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
    // Data sets 6 and 8 IOPTINJ
    property TreatmentOption: TTreatmentOption read FTreatmentOption
      write SetTreatmentOption;
  end;

  // @name is a collection of @link(TInjectionOptionItem)s.
  // There is one @link(TInjectionOptionItem) for each chemical species.
  TInjectionOptionCollection = class(TStringConcCollection)
  private
    function GetItem(Index: Integer): TInjectionOptionItem;
    procedure SetItem(Index: Integer; const Value: TInjectionOptionItem);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection; ScreenObject: TObject;
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
    procedure CreateFormulaObjects; override;
    function BoundaryFormulaCount: Integer; override;
    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects; override;
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
    function GetItemByStartTime(StartTime: Double): TCtsInjectionTimeItem;
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
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: Integer]: TIndividualWellInjectionItem read GetItem
      write SetItem; default;
    function GetItemByObjectName(const AName: string): TIndividualWellInjectionItem;
    function Add: TIndividualWellInjectionItem;
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
    function GetItemByStartTime(StartTime: Double): TCtsExternalFlowsItem;
  end;

  TCtsMaxConcItem = class(TCustomModflowBoundaryItem)
  private
    FMaxConcentrations: TStringConcCollection;
    procedure SetMaxConcentrations(const Value: TStringConcCollection);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure CreateFormulaObjects; override;
    function BoundaryFormulaCount: integer; override;

    procedure AssignObserverEvents(Collection: TCollection); override;
    procedure GetPropertyObserver(Sender: TObject; List: TList); override;
    procedure RemoveFormulaObjects;override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // Data set 7 CNTE
    property MaxConcentrations: TStringConcCollection read FMaxConcentrations
      write SetMaxConcentrations;
  end;

  TCtsMaxConcCollection = class(TCustomNonSpatialBoundColl)
  private
    function GetItem(Index: Integer): TCtsMaxConcItem;
    procedure SetItem(Index: Integer; const Value: TCtsMaxConcItem);
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    property Items[Index: Integer]: TCtsMaxConcItem read GetItem write SetItem; default;
    function GetItemByStartTime(StartTime: Double): TCtsMaxConcItem;
  end;

  TCtsSystem = class(TModflowScreenObjectProperty)
  private
    FTreatmentDistribution: TTreatmentDistribution;
    FDefaultInjectionOptions: TCtsInjectionTimeCollection;
    FCtsObjects: TCtsObjectCollection;
    FExternalFlows: TCtsExternalFlowsCollection;
    FInjections: TIndividualWellInjectionCollection;
    FName: string;
    FMaximumAllowedConc: TCtsMaxConcCollection;
    procedure SetCtsObjects(const Value: TCtsObjectCollection);
    procedure SetDefaultInjectionOptions(
      const Value: TCtsInjectionTimeCollection);
    procedure SetExternalFlows(const Value: TCtsExternalFlowsCollection);
    procedure SetTreatmentDistribution(const Value: TTreatmentDistribution);
    function IsSame(AnotherCtsSystem: TCtsSystem): boolean;
    procedure SetInjections(const Value: TIndividualWellInjectionCollection);
    procedure SetName(const Value: string);
    procedure SetMaximumAllowedConc(const Value: TCtsMaxConcCollection);
  protected
    function BoundaryObserverPrefix: string; override;
  public
    Constructor Create(Model: ICustomModelInterfaceForTOrderedCollection; ScreenObject: TObject);
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
    // data set 8
    property Injections: TIndividualWellInjectionCollection read FInjections
      write SetInjections;
    // Data Set 7: Maximum allowed concentrations.
    property MaximumAllowedConc: TCtsMaxConcCollection read FMaximumAllowedConc write SetMaximumAllowedConc;
    property Name: string read FName write SetName;
  end;

  TCtsSystemItem = class(TOrderedItem)
  private
    FCtsSystem: TCtsSystem;
    procedure SetCtsSystem(const Value: TCtsSystem);
    procedure Loaded;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CtsSystem: TCtsSystem read FCtsSystem write SetCtsSystem;
  end;

  TCtsSystemCollection = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TCtsSystemItem;
    procedure SetItem(Index: Integer; const Value: TCtsSystemItem);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    function Add: TCtsSystemItem;
    property Items[Index: Integer]: TCtsSystemItem read GetItem write SetItem; default;
    procedure Loaded;
  end;

implementation

uses
  PhastModelUnit, frmGoPhastUnit;

{ TCtsObjectItem }

procedure TCtsObjectItem.AddExtractionWell(AScreenObject: TScreenObject);
begin
  FExtractionWellList.Add(AScreenObject)
end;

procedure TCtsObjectItem.AddInjectionWell(AScreenObject: TScreenObject);
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
    StartTime := CstSource.StartTime;
    EndTime := CstSource.EndTime;
    ExtractionWellObjects := CstSource.ExtractionWellObjects;
    InjectionWellObjects := CstSource.InjectionWellObjects;
  end;
  inherited;
end;

constructor TCtsObjectItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredEndTime := TRealStorage.Create;
  FStoredStartTime := TRealStorage.Create;
  FStoredEndTime.OnChange := OnInvalidateModelEvent;
  FStoredStartTime.OnChange := OnInvalidateModelEvent;

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
  FStoredStartTime.Free;
  FStoredEndTime.Free;
  inherited;
end;

function TCtsObjectItem.ExtractionWellCount: integer;
begin
  result := FExtractionWellList.Count;
end;

function TCtsObjectItem.GetEndTime: Double;
begin
  result := FStoredEndTime.Value;
end;

function TCtsObjectItem.GetExtractionWell(Index: Integer): TScreenObject;
begin
  result := FExtractionWellList[Index];
end;

function TCtsObjectItem.GetExtractionWellObjects: TStrings;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  if FExtractionWellList.Count > 0 then
  begin
    FExtractionWellNames.OnChange := nil;
    try
      FExtractionWellNames.Clear;
      for Index := 0 to FExtractionWellList.Count - 1 do
      begin
        AScreenObject := FExtractionWellList[Index];
        if not AScreenObject.Deleted then
        begin
          FExtractionWellNames.AddObject(AScreenObject.Name, AScreenObject)
        end;
      end;
    finally
      FExtractionWellNames.OnChange := WellsAssigned;
    end;
  end;
  result := FExtractionWellNames;
end;

function TCtsObjectItem.GetInjectionWell(Index: Integer): TScreenObject;
begin
  result := FInjectionWellList[index];
end;

function TCtsObjectItem.GetInjectionWellObjects: TStrings;
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  if FInjectionWellList.Count > 0 then
  begin
    FInjectionWellNames.OnChange := nil;
    try
      FInjectionWellNames.Clear;
      for Index := 0 to FInjectionWellList.Count - 1 do
      begin
        AScreenObject := FInjectionWellList[Index];
        if not AScreenObject.Deleted then
        begin
          FInjectionWellNames.AddObject(AScreenObject.Name, AScreenObject)
        end;
      end;
    finally
      FInjectionWellNames.OnChange := WellsAssigned;
    end;
  end;
  result := FInjectionWellNames;
end;

function TCtsObjectItem.GetStartTime: Double;
begin
  result := FStoredStartTime.Value;
end;

function TCtsObjectItem.InjectionWellCount: integer;
begin
  result := FInjectionWellList.Count;
end;

function TCtsObjectItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherItem: TCtsObjectItem;
  LocalObjects: TStrings;
  OtherObjects: TStrings;
  Index: integer;
begin
  Result := (AnotherItem is TCtsObjectItem);
  if Result then
  begin
    OtherItem := TCtsObjectItem(AnotherItem);
    Result := (StartTime = OtherItem.StartTime)
      and (EndTime = OtherItem.EndTime);
    if Result then
    begin
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
end;

procedure TCtsObjectItem.Loaded(Model: TBaseModel);
var
  LocalModel: TPhastModel;
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  if Model <> nil then
  begin
    LocalModel := Model as TPhastModel;
    FExtractionWellNames.OnChange := nil;
    FInjectionWellNames.OnChange := nil;
    try
      FExtractionWellList.Clear;
      for Index := 0 to FExtractionWellNames.Count - 1 do
      begin
        AScreenObject := LocalModel.GetScreenObjectByName(FExtractionWellNames[Index]);
        if AScreenObject <> nil then
        begin
          AddExtractionWell(AScreenObject);
        end;
      end;
      FInjectionWellList.Clear;
      for Index := 0 to FInjectionWellNames.Count - 1 do
      begin
        AScreenObject := LocalModel.GetScreenObjectByName(FInjectionWellNames[Index]);
        if AScreenObject <> nil then
        begin
          AddInjectionWell(AScreenObject);
        end;
      end;
    finally
      FExtractionWellNames.OnChange := WellsAssigned;
      FInjectionWellNames.OnChange := WellsAssigned;
    end;
  end;
end;

procedure TCtsObjectItem.RemoveExtractionWell(AScreenObject: TScreenObject);
begin
  FExtractionWellList.Remove(AScreenObject);
end;

procedure TCtsObjectItem.RemoveInjectionWell(AScreenObject: TScreenObject);
begin
  FInjectionWellList.Remove(AScreenObject);
end;

procedure TCtsObjectItem.SetEndTime(const Value: Double);
begin
  FStoredEndTime.Value := Value;
end;

procedure TCtsObjectItem.SetExtractionWellObjects(const Value: TStrings);
begin
  FExtractionWellNames.Assign(Value);
end;

procedure TCtsObjectItem.SetInjectionWellObjects(const Value: TStrings);
begin
  FInjectionWellNames.Assign(Value);
end;

procedure TCtsObjectItem.SetStartTime(const Value: Double);
begin
  FStoredStartTime.Value := Value;
end;

procedure TCtsObjectItem.SetStoredEndTime(const Value: TRealStorage);
begin
  FStoredEndTime.Assign(Value);
end;

procedure TCtsObjectItem.SetStoredStartTime(const Value: TRealStorage);
begin
  FStoredStartTime.Assign(Value);
end;

procedure TCtsObjectItem.WellsAssigned(Sender: TObject);
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  if Sender = FExtractionWellNames then
  begin
    FExtractionWellList.Clear;
    for Index := 0 to FExtractionWellNames.Count - 1 do
    begin
      AScreenObject := FExtractionWellNames.Objects[Index] as TScreenObject;
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
      AScreenObject := FInjectionWellNames.Objects[Index] as TScreenObject;
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

procedure TCtsInjectionTimeItem.AssignObserverEvents(Collection: TCollection);
begin
  inherited;

end;

function TCtsInjectionTimeItem.BoundaryFormulaCount: Integer;
begin
  result := 0;
end;

constructor TCtsInjectionTimeItem.Create(Collection: TCollection);
begin
  inherited;
  FInjectionOptions := TInjectionOptionCollection.Create(Model as TCustomModel, ScreenObject, Collection);
end;

procedure TCtsInjectionTimeItem.CreateFormulaObjects;
begin
  inherited;

end;

destructor TCtsInjectionTimeItem.Destroy;
begin
  FInjectionOptions.Free;
  inherited;
end;

procedure TCtsInjectionTimeItem.GetPropertyObserver(Sender: TObject;
  List: TList);
begin
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

procedure TCtsInjectionTimeItem.RemoveFormulaObjects;
begin
  inherited;

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
  FInflowConcentrations := TStringConcCollection.Create(Model as TCustomModel, ScreenObject,
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
  UpdateFormulaBlocks(Value, InflowPosition, FInflowFormula);
end;

procedure TCtsExternalFlowsItem.SetInflowConcentrations(const Value: TStringConcCollection);
begin
  FInflowConcentrations.Assign(Value);
end;

procedure TCtsExternalFlowsItem.SetOutflow(const Value: string);
begin
  UpdateFormulaBlocks(Value, OutflowPosition, FOutflowFormula);
end;

{ TCtsObjectCollection }

function TCtsObjectCollection.Add: TCtsObjectItem;
begin
  result := inherited Add as TCtsObjectItem;
end;

constructor TCtsObjectCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TCtsObjectItem, Model);
end;

function TCtsObjectCollection.GetItem(Index: Integer): TCtsObjectItem;
begin
  result := inherited GetItem(Index) as TCtsObjectItem;
end;

function TCtsObjectCollection.GetItemByStartTime(
  StartTime: Double): TCtsObjectItem;
var
  index: Integer;
  TimeItem: TCtsObjectItem;
begin
  result := nil;
  for index := 0 to Count - 1 do
  begin
    TimeItem := Items[index];
    if (TimeItem.StartTime <= StartTime) and (StartTime < TimeItem.EndTime) then
    begin
      result := TimeItem;
      break;
    end;
  end;
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

function TCtsInjectionTimeCollection.GetItemByStartTime(
  StartTime: Double): TCtsInjectionTimeItem;
var
  ItemIndex: Integer;
  AnItem: TCtsInjectionTimeItem;
begin
  result := nil;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex];
    if (AnItem.StartTime <= StartTime) and (StartTime < AnItem.EndTime) then
    begin
      result := AnItem;
      Break;
    end;
  end;
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

function TCtsExternalFlowsCollection.GetItemByStartTime(
  StartTime: Double): TCtsExternalFlowsItem;
var
  ItemIndex: Integer;
  AnItem: TCtsExternalFlowsItem;
begin
  result := nil;
  for ItemIndex := 0 to Count - 1 do
  begin
    AnItem := Items[ItemIndex];
    if (AnItem.StartTime <= StartTime) and (StartTime < AnItem.EndTime) then
    begin
      result := AnItem;
      Break;
    end;
  end;
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
    MaximumAllowedConc := CtsSystem.MaximumAllowedConc;
    Name := CtsSystem.Name;
  end
  else
  begin
    inherited;
  end;
end;

function TCtsSystem.BoundaryObserverPrefix: string;
begin
  result := 'CTS_';
end;

constructor TCtsSystem.Create(Model: ICustomModelInterfaceForTOrderedCollection; ScreenObject: TObject);
begin
  inherited Create(Model, ScreenObject);
  FDefaultInjectionOptions := TCtsInjectionTimeCollection.Create(Self, Model, ScreenObject);
  FCtsObjects := TCtsObjectCollection.Create(Model as TCustomModel);
  FExternalFlows := TCtsExternalFlowsCollection.Create(Self, Model, ScreenObject);
  FInjections := TIndividualWellInjectionCollection.Create(Model as TCustomModel);
  FMaximumAllowedConc := TCtsMaxConcCollection.Create(self, Model, ScreenObject);
end;

destructor TCtsSystem.Destroy;
begin
  FMaximumAllowedConc.Free;
  FInjections.Free;
  FExternalFlows.Free;
  FCtsObjects.Free;
  FDefaultInjectionOptions.Free;
  inherited;
end;

function TCtsSystem.IsSame(AnotherCtsSystem: TCtsSystem): boolean;
begin
  Result := (Name = AnotherCtsSystem.Name)
    and (TreatmentDistribution = AnotherCtsSystem.TreatmentDistribution)
    and DefaultInjectionOptions.IsSame(AnotherCtsSystem.DefaultInjectionOptions)
    and CtsObjects.IsSame(AnotherCtsSystem.CtsObjects)
    and ExternalFlows.IsSame(AnotherCtsSystem.ExternalFlows)
    and Injections.IsSame(AnotherCtsSystem.Injections)
    and MaximumAllowedConc.IsSame(AnotherCtsSystem.MaximumAllowedConc);
end;

procedure TCtsSystem.Loaded;
begin
  FCtsObjects.Loaded(ParentModel as TCustomModel);
  FInjections.Loaded(ParentModel as TCustomModel);
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

procedure TCtsSystem.SetMaximumAllowedConc(const Value: TCtsMaxConcCollection);
begin
  FMaximumAllowedConc.Assign(Value);
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
  if FTreatmentDistribution <> Value then
  begin
    FTreatmentDistribution := Value;
    InvalidateModel;
  end;
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

constructor TInjectionOptionCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection;
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
  FCtsSystem := TCtsSystem.Create(Model as TCustomModel, nil);
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

constructor TCtsSystemCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TCtsSystemItem, Model);
end;

function TCtsSystemCollection.GetItem(Index: Integer): TCtsSystemItem;
begin
  result := inherited Items[index] as TCtsSystemItem
end;

procedure TCtsSystemCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
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
  FInjections := TCtsInjectionTimeCollection.Create(nil, Model as TCustomModel, nil);
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
  result := (AnotherItem is TIndividualWellInjectionItem);
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

function TIndividualWellInjectionCollection.Add: TIndividualWellInjectionItem;
begin
  result := inherited Add as TIndividualWellInjectionItem;
end;

constructor TIndividualWellInjectionCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TIndividualWellInjectionItem, Model);
end;

function TIndividualWellInjectionCollection.GetItem(
  Index: Integer): TIndividualWellInjectionItem;
begin
  result := inherited Items[index] as TIndividualWellInjectionItem
end;

function TIndividualWellInjectionCollection.GetItemByObjectName(
  const AName: string): TIndividualWellInjectionItem;
var
  ItemIndex: Integer;
begin
  result := nil;
  for ItemIndex := 0 to Count - 1 do
  begin
    if Items[ItemIndex].InjectionWellObjectName = AName then
    begin
      result := Items[ItemIndex];
      break;
    end;
  end;
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

{ TCtsMaxConcItem }

procedure TCtsMaxConcItem.Assign(Source: TPersistent);
begin
  if Source is TCtsMaxConcItem then
  begin
    MaxConcentrations := TCtsMaxConcItem(Source).MaxConcentrations;
  end;
  inherited;
end;

procedure TCtsMaxConcItem.AssignObserverEvents(Collection: TCollection);
begin
  inherited;

end;

function TCtsMaxConcItem.BoundaryFormulaCount: integer;
begin
  result := 0;
end;

constructor TCtsMaxConcItem.Create(Collection: TCollection);
begin
  inherited;
  FMaxConcentrations := TStringConcCollection.Create(Model as TCustomModel, ScreenObject,
    Collection);
end;

procedure TCtsMaxConcItem.CreateFormulaObjects;
begin
  inherited;

end;

destructor TCtsMaxConcItem.Destroy;
begin
  FMaxConcentrations.Free;
  inherited;
end;

procedure TCtsMaxConcItem.GetPropertyObserver(Sender: TObject; List: TList);
begin
  inherited;

end;

function TCtsMaxConcItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := (AnotherItem is TCtsMaxConcItem) and inherited IsSame(AnotherItem);
  if result then
  begin
    result := MaxConcentrations.IsSame(
      TCtsMaxConcItem(AnotherItem).MaxConcentrations);
  end;
end;

procedure TCtsMaxConcItem.RemoveFormulaObjects;
begin
  inherited;

end;

procedure TCtsMaxConcItem.SetMaxConcentrations(
  const Value: TStringConcCollection);
begin
  FMaxConcentrations.Assign(Value);
end;

{ TCtsMaxConcCollection }

function TCtsMaxConcCollection.GetItem(Index: Integer): TCtsMaxConcItem;
begin
  result := inherited Items[index] as TCtsMaxConcItem;
end;

function TCtsMaxConcCollection.GetItemByStartTime(
  StartTime: Double): TCtsMaxConcItem;
var
  index: Integer;
  TimeItem: TCtsMaxConcItem;
begin
  result := nil;
  for index := 0 to Count - 1 do
  begin
    TimeItem := Items[index];
    if (TimeItem.StartTime <= StartTime) and (StartTime < TimeItem.EndTime) then
    begin
      result := TimeItem;
      break;
    end;
  end;
end;

class function TCtsMaxConcCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCtsMaxConcItem;
end;

procedure TCtsMaxConcCollection.SetItem(Index: Integer;
  const Value: TCtsMaxConcItem);
begin
  inherited Items[index] := Value;
end;

end.
