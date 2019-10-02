unit Mt3dCtsSystemUnit;

interface

uses
  System.Classes, ModflowBoundaryUnit, GoPhastTypes, OrderedCollectionUnit;

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


  // @name allows the users to change the injection or extraction wells over
  // time but typically, the same wells will be used in all included times.
  TCtsObjectItem = class(TCustomModflowBoundaryItem)
  private
    function GetExtractionWellObjects: TStrings;
    function GetInjectionWellObjects: TStrings;
    procedure SetExtractionWellObjects(const Value: TStrings);
    procedure SetInjectionWellObjects(const Value: TStrings);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ExtractionWellObjects: TStrings read GetExtractionWellObjects write SetExtractionWellObjects;
    property InjectionWellObjects: TStrings read GetInjectionWellObjects write SetInjectionWellObjects;
  end;

  TCtsObjectCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  // One name is used for each injection well object for each chemical component
  TInjectionOptionItem = class(TCollectionItem)
  private
    FTreatmentOption: TTreatmentOption;
    function GetTreatmentValue: string;
    procedure SetTreatmentOption(const Value: TTreatmentOption);
    procedure SetTreatmentValue(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property TreatmentOption: TTreatmentOption read FTreatmentOption write SetTreatmentOption;
    // @name is the formula used to calculate the treatment value.
    property TreatmentValue: string read GetTreatmentValue write SetTreatmentValue;
  end;

  // @name is a collection of @link(TInjectionOptionItem)s.
  // There is one @link(TInjectionOptionItem) for each chemical species.
  TInjectionOptionCollection = class(TOrderedCollection)
  protected
  public
    constructor Create(Model: TBaseModel);
  end;

  // @name allows the treatment to change over time.
  TCtsInjectionTimeItem = class(TCustomModflowBoundaryItem)
  private
    FUseDefaultInchectionOptions: boolean;
    FInjectionOptions: TInjectionOptionCollection;
    function GetInjectionWellObjectName: string;
    procedure SetInjectionOptions(const Value: TInjectionOptionCollection);
    procedure SetInjectionWellObjectName(const Value: string);
    procedure SetUseDefaultInchectionOptions(const Value: boolean);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property InjectionOptions: TInjectionOptionCollection read FInjectionOptions write SetInjectionOptions;
    // @name is used only if @link(TTreatmentDistribution) is tlIndividual
    property InjectionWellObjectName: string read GetInjectionWellObjectName write SetInjectionWellObjectName;
    property UseDefaultInchectionOptions: boolean read FUseDefaultInchectionOptions write SetUseDefaultInchectionOptions;
  end;

  TCtsInjectionTimeCollection = class(TCustomNonSpatialBoundColl)
  protected
    class function ItemClass: TBoundaryItemClass; override;
  public
    constructor Create(Boundary: TModflowScreenObjectProperty;
      Model: TBaseModel; ScreenObject: TObject); override;
  end;

  TCtsExternalFlowsItem = class(TCustomModflowBoundaryItem)
  private
    function GetInflow: string;
    function GetInflowConcentrations: TStrings;
    function GetOutflow: string;
    procedure SetInflow(const Value: string);
    procedure SetInflowConcentrations(const Value: TStrings);
    procedure SetOutflow(const Value: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Outflow: string read GetOutflow write SetOutflow;
    property Inflow: string read GetInflow write SetInflow;
    property InflowConcentrations: TStrings read GetInflowConcentrations write SetInflowConcentrations;
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
  published
    property TreatmentDistribution: TTreatmentDistribution read FTreatmentDistribution write SetTreatmentDistribution;
    property DefaultInjectionOptions: TInjectionOptionCollection read FDefaultInjectionOptions write SetDefaultInjectionOptions;
    property CtsObjects: TCtsObjectCollection read FCtsObjects write SetCtsObjects;
    property ExternalFlows: TCtsExternalFlowsCollection read FExternalFlows write SetExternalFlows;
  end;

implementation

{ TCtsObjectItem }

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

function TCtsObjectItem.GetExtractionWellObjects: TStrings;
begin

end;

function TCtsObjectItem.GetInjectionWellObjects: TStrings;
begin

end;

procedure TCtsObjectItem.SetExtractionWellObjects(const Value: TStrings);
begin

end;

procedure TCtsObjectItem.SetInjectionWellObjects(const Value: TStrings);
begin

end;

{ TInjectionOptionItem }

procedure TInjectionOptionItem.Assign(Source: TPersistent);
begin
  inherited;

end;

function TInjectionOptionItem.GetTreatmentValue: string;
begin

end;

procedure TInjectionOptionItem.SetTreatmentOption(
  const Value: TTreatmentOption);
begin
  FTreatmentOption := Value;
end;

procedure TInjectionOptionItem.SetTreatmentValue(const Value: string);
begin

end;

{ TCtsInjectionTimeItem }

procedure TCtsInjectionTimeItem.Assign(Source: TPersistent);
begin
  inherited;

end;

function TCtsInjectionTimeItem.GetInjectionWellObjectName: string;
begin

end;

procedure TCtsInjectionTimeItem.SetInjectionOptions(
  const Value: TInjectionOptionCollection);
begin
  FInjectionOptions := Value;
end;

procedure TCtsInjectionTimeItem.SetInjectionWellObjectName(const Value: string);
begin

end;

procedure TCtsInjectionTimeItem.SetUseDefaultInchectionOptions(
  const Value: boolean);
begin
  FUseDefaultInchectionOptions := Value;
end;

{ TCtsExternalFlowsItem }

procedure TCtsExternalFlowsItem.Assign(Source: TPersistent);
begin
  inherited;

end;

function TCtsExternalFlowsItem.GetInflow: string;
begin

end;

function TCtsExternalFlowsItem.GetInflowConcentrations: TStrings;
begin

end;

function TCtsExternalFlowsItem.GetOutflow: string;
begin

end;

procedure TCtsExternalFlowsItem.SetInflow(const Value: string);
begin

end;

procedure TCtsExternalFlowsItem.SetInflowConcentrations(const Value: TStrings);
begin

end;

procedure TCtsExternalFlowsItem.SetOutflow(const Value: string);
begin

end;

{ TCtsObjectCollection }

constructor TCtsObjectCollection.Create(Boundary: TModflowScreenObjectProperty;
  Model: TBaseModel; ScreenObject: TObject);
begin
  inherited;

end;

class function TCtsObjectCollection.ItemClass: TBoundaryItemClass;
begin
  result := TCtsObjectItem;
end;

{ TInjectionOptionCollection }

constructor TInjectionOptionCollection.Create(Model: TBaseModel);
begin
  inherited Create(TInjectionOptionItem, Model);
end;

{ TCtsInjectionTimeCollection }

constructor TCtsInjectionTimeCollection.Create(
  Boundary: TModflowScreenObjectProperty; Model: TBaseModel;
  ScreenObject: TObject);
begin
  inherited;

end;

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

end.
