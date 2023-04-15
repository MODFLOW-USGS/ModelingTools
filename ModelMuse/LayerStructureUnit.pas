unit LayerStructureUnit;

interface

uses OrderedCollectionUnit, Classes, GoPhastTypes, SubscriptionUnit,
  ModflowSubsidenceDefUnit, SysUtils, OrderedCollectionInterfaceUnit;

type
  TIntTypeMethod = (itmLaytype, itmLayavg, itmLayvka);
  TFloatTypeMethod = (ftmTrpy, ftmTrtp, ftmTrpv, ftmDmcoef);

  EInvalidLayer = class(Exception);

  TLayerCollection = class;

  TLayerFraction = class(TCollectionItem)
  private
    FFraction: real;
    procedure InvalidateModel;
    procedure SetFraction(const Value: real);
    function IsSame(AnotherLayerFraction: TLayerFraction): boolean;
  public
    function Collection: TLayerCollection;
    procedure Assign(Source: TPersistent); override;
  published
    property Fraction: real read FFraction write SetFraction;
  end;

  TCustomLayerGroup = class;
  TLayerGroup = class;
  TGrowItem = class;
  TGrowthControls = class;

  TLayerCollection  = class(TCollection)
  Private
    FLayerGroup: TGrowthControls;
    procedure InvalidateModel;
    procedure Sort;
    function GetItem(Index: Integer): TLayerFraction;
    procedure SetItem(Index: Integer; const Value: TLayerFraction);
  public
    function IsSame(AnotherLayerCollection: TLayerCollection): boolean;
    procedure Assign(Source: TPersistent); override;
    constructor Create(LayerGroup: TGrowthControls);
    destructor Destroy; override;
    property Items[Index: Integer]: TLayerFraction read GetItem
      write SetItem; default;
  end;

  TCustomLayerStructure = class;
  TLayerStructure = class;

  TGrowthControls = class(TGoPhastPersistent)
  private
    FGrowthMethod: TGrowthMethod;
    procedure SetGrowthMethod(const Value: TGrowthMethod);
    procedure SetGrowthRate(const Value: real);
  protected
    FGrowthRate: real;
    {@name defines the layer or layers in @classname.}
    FLayerCollection: TLayerCollection;
    function StoreLayerCollection: boolean; virtual;
    procedure SetLayerCollection(const Value: TLayerCollection); virtual;
    function IsSame(AnotherGrowControls : TGrowthControls): boolean;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
//    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function LayerCount: Integer; virtual;
  published
    {
    When a layer group is split into more than one layer, @name defines
    how the thickness of those layers is specified.
    }
    property GrowthMethod: TGrowthMethod read FGrowthMethod
      write SetGrowthMethod;
    {
    When @link(GrowthMethod) is  @link(TGrowthMethod gmUp),
    @link(TGrowthMethod gmDown),
    @link(TGrowthMethod gmMiddle), or @link(TGrowthMethod gmEdge),
    @name is used to help define
    how the thickness of those layers is calculated.
    }
    property GrowthRate: real read FGrowthRate write SetGrowthRate;
    { @name defines the layer or layers in @classname.}
    property LayerCollection: TLayerCollection read FLayerCollection
      write SetLayerCollection stored StoreLayerCollection;
  end;

  TGrowItem = class(TOrderedItem)
  private
    FGrowthControls: TGrowthControls;
    procedure SetGrowthMethod(const Value: TGrowthMethod);
    procedure SetGrowthRate(const Value: real);
    function GetGrowthMethod: TGrowthMethod;
    function GetGrowthRate: real;
    function GetLayerCollection: TLayerCollection;
    procedure SetGrowthControls(const Value: TGrowthControls);
  protected
    function StoreLayerCollection: boolean; virtual;
    procedure SetLayerCollection(const Value: TLayerCollection); virtual;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function LayerCount: Integer; virtual;
  published
    {
    When a layer group is split into more than one layer, @name defines
    how the thickness of those layers is specified.
    }
    property GrowthMethod: TGrowthMethod read GetGrowthMethod
      write SetGrowthMethod stored False;
    {
    When @link(GrowthMethod) is  @link(TGrowthMethod gmUp),
    @link(TGrowthMethod gmDown),
    @link(TGrowthMethod gmMiddle), or @link(TGrowthMethod gmEdge),
    @name is used to help define
    how the thickness of those layers is calculated.
    }
    property GrowthRate: real read GetGrowthRate write SetGrowthRate stored False;
    { @name defines the layer or layers in @classname.}
    property LayerCollection: TLayerCollection read GetLayerCollection
      write SetLayerCollection stored False;
    property GrowthControls: TGrowthControls read FGrowthControls
      write SetGrowthControls;
  end;

  TCustomLayerGroup = class(TGrowItem)
  private
    FDataArrayName: string;
    FAquiferName: string;
    FAquiferDisplayName: string;
    procedure SetDataArrayName(const NewName: string);
    procedure SetAquiferName(const Value: string);
    function EvalAt: TEvaluatedAt; virtual;
    function GetSimulated: boolean; virtual;
    procedure SetSimulated(const Value: boolean); virtual;
    procedure UpdateDataArray(const NewName, NewDisplayName: string);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure SetTopDisplayName(Model: TBaseModel);
    function GetRunTimeSimulated: boolean;
  protected
    function Collection: TCustomLayerStructure;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Loaded; virtual;
    function ShouldCreateDataArray: Boolean; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    property RunTimeSimulated: boolean read GetRunTimeSimulated;
  published
    property DataArrayName: string read FDataArrayName write SetDataArrayName;
    property AquiferName: string read FAquiferName write SetAquiferName;
    property Simulated: boolean read GetSimulated write SetSimulated;
  end;

  TConduitLayerItem = class(TOrderedItem)
  private
    FStoredLowerCriticalReynoldsNumber: TRealStorage;
    FStoredHigherCriticalReynoldsNumber: TRealStorage;
    FStoredVoid: TRealStorage;
    FIsConduitLayer: boolean;
    function GetHigherCriticalReynoldsNumber: Double;
    function GetLowerCriticalReynoldsNumber: Double;
    function GetVoid: Double;
    procedure SetHigherCriticalReynoldsNumber(const Value: Double);
    procedure SetIsConduitLayer(const Value: boolean);
    procedure SetLowerCriticalReynoldsNumber(const Value: Double);
    procedure SetStoredHigherCriticalReynoldsNumber(const Value: TRealStorage);
    procedure SetStoredLowerCriticalReynoldsNumber(const Value: TRealStorage);
    procedure SetStoredVoid(const Value: TRealStorage);
    procedure SetVoid(const Value: Double);
    procedure ValueChanged(Sender: TObject);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Void: Double read GetVoid Write SetVoid;
    property LowerCriticalReynoldsNumber: Double
      read GetLowerCriticalReynoldsNumber Write SetLowerCriticalReynoldsNumber;
    property HigherCriticalReynoldsNumber: Double
      read GetHigherCriticalReynoldsNumber Write SetHigherCriticalReynoldsNumber;
  published
    property IsConduitLayer: boolean read FIsConduitLayer write SetIsConduitLayer;
    property StoredVoid: TRealStorage read FStoredVoid write SetStoredVoid;
    property StoredLowerCriticalReynoldsNumber: TRealStorage read FStoredLowerCriticalReynoldsNumber write SetStoredLowerCriticalReynoldsNumber;
    property StoredHigherCriticalReynoldsNumber: TRealStorage read FStoredHigherCriticalReynoldsNumber write SetStoredHigherCriticalReynoldsNumber;
  end;

  TConduitLayerCollection = class(TOrderedCollection)
  private
    function GetItem(index: Integer): TConduitLayerItem;
    procedure SetItem(index: Integer; const Value: TConduitLayerItem);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[index: Integer] : TConduitLayerItem read GetItem
      write SetItem; default;
  end;

  TLayerGroup = class(TCustomLayerGroup)
  private
    FSimulated: boolean;
    FAquiferType: integer;
    FInterblockTransmissivityMethod: integer;
    FVerticalHydraulicConductivityMethod: integer;
    FUseStartingHeadForSaturatedThickness: boolean;
    FHorizontalAnisotropy: double;
    FSubNoDelayBedLayers: TSubNoDelayBedLayers;
    FSubDelayBedLayers: TSubDelayBedLayers;
    FWaterTableLayers: TWaterTableLayers;
    FMt3dmsHorzTransDisp: TRealCollection;
    FMt3dmsDiffusionCoef: TRealCollection;
    FMt3dmsVertTransDisp: TRealCollection;
    FConduitLayers: TConduitLayerCollection;
    procedure SetSimulated(const Value: boolean); override;
    procedure SetAquiferType(const Value: integer);
    procedure SetInterblockTransmissivityMethod(const Value: integer);
    procedure SetVerticalHydraulicConductivityMethod(const Value: integer);
    procedure SetUseStartingHeadForSaturatedThickness(const Value: boolean);
    function GetSimulated: boolean; override;
    procedure SetHorizontalAnisotropy(const Value: double);
    procedure SetSubDelayBedLayers(const Value: TSubDelayBedLayers);
    procedure SetSubNoDelayBedLayers(const Value: TSubNoDelayBedLayers);
    function SubsidenceLayerCount(SubLayers: TCustomSubLayer): integer;
    procedure SetWaterTableLayers(const Value: TWaterTableLayers);
    procedure UpdateChildModels(PriorCount: Integer);
    procedure SetMt3dmsDiffusionCoef(const Value: TRealCollection);
    procedure SetMt3dmsHorzTransDisp(const Value: TRealCollection);
    procedure SetMt3dmsVertTransDisp(const Value: TRealCollection);
    procedure SetConduitLayers(const Value: TConduitLayerCollection);
  protected
    procedure Loaded; override;
    procedure SetLayerCollection(const Value: TLayerCollection); override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    function StoreLayerCollection: boolean; override;
    function ShouldCreateDataArray: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
    function LayerCount: integer; override;
    function ModflowLayerCount: integer;
    procedure WriteLAYCB(const DiscretizationWriter: TObject);
    function SubsidenceDefined: boolean;
    function SwtDefined: boolean;
    function DelayCount: integer;
    function NoDelayCount: integer;
    function WaterTableCount: integer;
    procedure UpdateRequiredDataArrays;
  published
    { @name can take on the following values:
     @unorderedlist(
       @item(0, confined)
       @item(1, convertible in LPF and HUF, Unconfined in BCF)
       @item(2, limited convertible in BCF with constant transmissivity)
       @item(3, fully convertible in BCF with variable transmissivity)
      )
      2 and 3 are not defined for LPF and HUF.
    }
    property AquiferType: integer read FAquiferType write SetAquiferType;
    // @name represents the first digit of Ltype in the BCF package
    // and LAYAVG in the LPF package. @name is not used in the HUF package.
    // However, Ltype and LAYAVG are not defined in exactly the same way
    // Ltype = 1 in BCF means use an arithmetic mean.  There is no such
    // option in LPF. Options 2 and 3 in BCF correspond to 1 and 2 in LPF.
    property InterblockTransmissivityMethod: integer
      read FInterblockTransmissivityMethod
      write SetInterblockTransmissivityMethod;
    property VerticalHydraulicConductivityMethod: integer
      read FVerticalHydraulicConductivityMethod
      write SetVerticalHydraulicConductivityMethod;
    property UseStartingHeadForSaturatedThickness: boolean
      read FUseStartingHeadForSaturatedThickness
      write SetUseStartingHeadForSaturatedThickness;
    // TRPY in the BCF package.
    property HorizontalAnisotropy: double read FHorizontalAnisotropy
      write SetHorizontalAnisotropy;
    property SubNoDelayBedLayers: TSubNoDelayBedLayers
      read FSubNoDelayBedLayers write SetSubNoDelayBedLayers;
    property SubDelayBedLayers: TSubDelayBedLayers
      read FSubDelayBedLayers write SetSubDelayBedLayers;
    property WaterTableLayers: TWaterTableLayers read FWaterTableLayers
      write SetWaterTableLayers;
    property Mt3dmsHorzTransDisp: TRealCollection read FMt3dmsHorzTransDisp
      write SetMt3dmsHorzTransDisp;
    property Mt3dmsVertTransDisp: TRealCollection read FMt3dmsVertTransDisp
      write SetMt3dmsVertTransDisp;
    property Mt3dmsDiffusionCoef: TRealCollection read FMt3dmsDiffusionCoef
      write SetMt3dmsDiffusionCoef;
    property ConduitLayers: TConduitLayerCollection read FConduitLayers
      write SetConduitLayers;
  end;

  TSutraLayerGroup = class(TCustomLayerGroup)
  private
    FStoredMinThickness: TRealStorage;
    procedure SetStoredMinThickness(const Value: TRealStorage);
    function GetMinThickness: Double;
    procedure SetMinThickness(const Value: Double);
  protected
    function EvalAt: TEvaluatedAt; override;
    function ShouldCreateDataArray: Boolean; override;
    procedure InvalidateModel; override;
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure MinThicknessChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property MinThickness: Double read GetMinThickness write SetMinThickness;
  published
    property StoredMinThickness: TRealStorage read FStoredMinThickness
      write SetStoredMinThickness;
  end;

  TCustomLayerStructure = class(TLayerOwnerCollection)
  public
    function LayerCount: integer; virtual;
    procedure Loaded; virtual;
  end;

  TLayerStructure = class(TCustomLayerStructure)
  Private
    FSimulatedNotifier: TObserver;
    FAquiferTypeNotifier: TObserver;
    function GetLayerGroup(const Index: integer): TLayerGroup;
    function IntegerArray(Method: TIntTypeMethod): TOneDIntegerArray;
    function FloatArray(Method: TFloatTypeMethod): TOneDRealArray;
  public
    function Last: TLayerGroup;
    function NonSimulatedLayersPresent: boolean;
    procedure AssignAssociatedInputDataSets;
    procedure Assign(Source: TPersistent);override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    destructor Destroy; override;
    property LayerGroups[const Index: integer]: TLayerGroup
      read GetLayerGroup; default;
    procedure Loaded; override;
    function ModflowLayerCount: integer;
    function ModflowConfiningBedCount: integer;
    procedure WriteLAYCB(const DiscretizationWriter: TObject);
    function ModflowLayerBottomDescription(const LayerID: integer): string;
    // @name returns true if a layer in the grid is simulated
    // LayerID is zero-based.
    function IsLayerSimulated(const LayerID: integer): boolean;
    function IsLayerConfined(const LayerID: integer): boolean;
    Function Laytyp: TOneDIntegerArray;
    Function Layavg: TOneDIntegerArray;
    function Chani: TOneDIntegerArray;
    Function Layvka: TOneDIntegerArray;
    function Trpy: TOneDRealArray;
    // @name converts a MODFLOW model layer (starting at 1) to the
    // appropriate index in a 3D data array;
    Function ModflowLayerToDataSetLayer(ModflowLayer: integer): integer;
    function DataSetLayerToModflowLayer(DataSetLayer: integer): integer;
    //  @name returns the @link(TLayerGroup) that contains Layer.
    // Layer is zero-based.
    function GetLayerGroupByLayer(const Layer: integer): TLayerGroup;
    property SimulatedNotifier: TObserver read FSimulatedNotifier;
    property AquiferTypeNotifier: TObserver read FAquiferTypeNotifier;
    procedure StopTalkingToAnyone;
    function SubsidenceDefined: boolean;
    function SwtDefined: boolean;
    function DelayCount: integer;
    function NoDelayCount: integer;
    function WaterTableCount: integer;
    Function TRPT: TOneDRealArray;
    function TRPV: TOneDRealArray;
    Function DMCOEF: TOneDRealArray;
    procedure UpdateRequiredDataArrays;
  end;

  TSutraLayerStructure = class(TCustomLayerStructure)
  private
    function GetLayerGroup(const Index: integer): TSutraLayerGroup;
  public
    function LayerCount: integer; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property LayerGroups[const Index: integer]: TSutraLayerGroup
      read GetLayerGroup; default;
    function NodeLayerCount: Integer;
    function ElementLayerCount: Integer;
  end;

resourcestring
  StrLayerDefinition = 'Layer Definition';

implementation

uses Math, RbwParser, PhastModelUnit, DataSetUnit,
  ModflowDiscretizationWriterUnit, SutraMeshUnit, DataSetNamesUnit;

const
  KBottom = '_Bottom';

resourcestring
  StrBottom = KBottom;

procedure TLayerGroup.Assign(Source: TPersistent);
var
  AnotherLayerGroup: TLayerGroup;
begin
  // if Assign is updated, update IsSame too.
  inherited;
  AnotherLayerGroup := Source as TLayerGroup;
  if not IsSame(AnotherLayerGroup) then
  begin
    AquiferType := AnotherLayerGroup.AquiferType;
    Simulated := AnotherLayerGroup.Simulated;
    InterblockTransmissivityMethod :=
      AnotherLayerGroup.InterblockTransmissivityMethod;
    VerticalHydraulicConductivityMethod :=
      AnotherLayerGroup.VerticalHydraulicConductivityMethod;
    UseStartingHeadForSaturatedThickness :=
      AnotherLayerGroup.UseStartingHeadForSaturatedThickness;
    HorizontalAnisotropy := AnotherLayerGroup.HorizontalAnisotropy;
    SubNoDelayBedLayers := AnotherLayerGroup.SubNoDelayBedLayers;
    SubDelayBedLayers := AnotherLayerGroup.SubDelayBedLayers;
    WaterTableLayers := AnotherLayerGroup.WaterTableLayers;
    Mt3dmsHorzTransDisp := AnotherLayerGroup.Mt3dmsHorzTransDisp;
    Mt3dmsVertTransDisp := AnotherLayerGroup.Mt3dmsVertTransDisp;
    Mt3dmsDiffusionCoef := AnotherLayerGroup.Mt3dmsDiffusionCoef;
    ConduitLayers := AnotherLayerGroup.ConduitLayers;
  end;
end;

constructor TLayerGroup.Create(Collection: TCollection);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  inherited;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := (Model as TCustomModel).DoInvalidate;
  end;
  FMt3dmsHorzTransDisp := TRealCollection.Create(InvalidateModelEvent);
  FMt3dmsHorzTransDisp.InitialValue := 0.1;
  FMt3dmsVertTransDisp := TRealCollection.Create(InvalidateModelEvent);
  FMt3dmsVertTransDisp.InitialValue := 0.01;
  FMt3dmsDiffusionCoef := TRealCollection.Create(InvalidateModelEvent);
  FMt3dmsDiffusionCoef.InitialValue := 0;
  FSubNoDelayBedLayers := TSubNoDelayBedLayers.Create(Model as TCustomModel);
  FSubDelayBedLayers := TSubDelayBedLayers.Create(Model as TCustomModel);
  FWaterTableLayers := TWaterTableLayers.Create(Model as TCustomModel);
  FConduitLayers := TConduitLayerCollection.Create(Model as TCustomModel);
//  AquiferName := 'New Layer Group';
//  AquiferName := '';
  FHorizontalAnisotropy := 1;
  FSimulated := True;
end;

function TLayerGroup.DelayCount: integer;
begin
  Result := SubsidenceLayerCount(SubDelayBedLayers);
end;

destructor TLayerGroup.Destroy;
var
  Model: TPhastModel;
  DataArray: TDataArray;
  ChildIndex: Integer;
//  ChildModel: TChildModel;
  OtherGroup: TLayerGroup;
  Discretization: TChildDiscretizationCollection;
  DisIndex: Integer;
  DisItem: TChildDiscretization;
  ChildModel: TChildModel;
begin
  FConduitLayers.Free;
  FWaterTableLayers.Free;
  FSubDelayBedLayers.Free;
  FSubNoDelayBedLayers.Free;
  if Collection.Model <> nil then
  begin
    Model := Collection.Model as TPhastModel;
    if not (csDestroying in Model.ComponentState) and not Model.Clearing then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(DataArrayName);
      if DataArray <> nil then
      begin
        DataArray.Lock := [];
      end;
      for ChildIndex := 0 to Model.ChildModels.Count - 1 do
      begin
        ChildModel := Model.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          Discretization := ChildModel.Discretization;
          if Discretization.BottomLayerGroup = self then
          begin
            if Index > 1 then
            begin
              OtherGroup := Collection.Items[Index-1] as TLayerGroup;
              Discretization.BottomLayerInUnit := OtherGroup.LayerCount-1;
            end
            else if Collection.Count > Index+1 then
            begin
              OtherGroup := Collection.Items[Index+1] as TLayerGroup;
              Discretization.BottomLayerInUnit := 0;
            end
            else
            begin
              OtherGroup := nil;
            end;
            Discretization.BottomLayerGroup := OtherGroup;
            for DisIndex := Discretization.Count - 1 downto 0 do
            begin
              DisItem := Discretization[DisIndex];
              if DisItem.LayerGroup = self then
              begin
                Discretization.Delete(DisIndex);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  FMt3dmsDiffusionCoef.Free;
  FMt3dmsVertTransDisp.Free;
  FMt3dmsHorzTransDisp.Free;
  inherited;
end;

function TLayerGroup.GetSimulated: boolean;
var
  PhastModel: TPhastModel;
begin
  result := FSimulated;
  if not result then
  begin
    PhastModel := Model as TPhastModel;
    if (PhastModel <> nil)
      and (PhastModel.ModflowPackages <> nil)
      and (PhastModel.ModflowPackages.HufPackage <> nil)
      and PhastModel.ModflowPackages.HufPackage.IsSelected then
    begin
      result := True;
    end;
  end;
end;

function TLayerGroup.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherLayerGroup : TLayerGroup;
begin
  result := inherited;
  if result then
  begin
    AnotherLayerGroup := AnotherItem as TLayerGroup;
    result := (AnotherLayerGroup.Simulated = Simulated)
      and (AnotherLayerGroup.AquiferType = AquiferType)
      and (AnotherLayerGroup.InterblockTransmissivityMethod =
        InterblockTransmissivityMethod)
      and (AnotherLayerGroup.VerticalHydraulicConductivityMethod =
        VerticalHydraulicConductivityMethod)
      and (AnotherLayerGroup.UseStartingHeadForSaturatedThickness =
        UseStartingHeadForSaturatedThickness)
      and (AnotherLayerGroup.HorizontalAnisotropy = HorizontalAnisotropy)
      and AnotherLayerGroup.SubNoDelayBedLayers.IsSame(SubNoDelayBedLayers)
      and AnotherLayerGroup.SubDelayBedLayers.IsSame(SubDelayBedLayers)
      and AnotherLayerGroup.WaterTableLayers.IsSame(WaterTableLayers)
      and AnotherLayerGroup.Mt3dmsHorzTransDisp.IsSame(Mt3dmsHorzTransDisp)
      and AnotherLayerGroup.Mt3dmsVertTransDisp.IsSame(Mt3dmsVertTransDisp)
      and AnotherLayerGroup.Mt3dmsDiffusionCoef.IsSame(Mt3dmsDiffusionCoef)
      and AnotherLayerGroup.ConduitLayers.IsSame(ConduitLayers)
  end;
end;

function TLayerGroup.LayerCount: integer;
begin
  if RunTimeSimulated then
  begin
    result := inherited;
  end
  else
  begin
    result := 1;
  end;
end;

procedure TLayerGroup.Loaded;
begin
  inherited;
  SubNoDelayBedLayers.Loaded;
  SubDelayBedLayers.Loaded;
  WaterTableLayers.Loaded;
end;

function TLayerGroup.ModflowLayerCount: integer;
begin
  if RunTimeSimulated then
  begin
    result := LayerCollection.Count + 1;
  end
  else
  begin
    result := 0;
  end;
end;

function TLayerGroup.NoDelayCount: integer;
begin
  Result := SubsidenceLayerCount(SubNoDelayBedLayers);
end;

function TLayerGroup.SubsidenceLayerCount(SubLayers: TCustomSubLayer): integer;
var
  Index: Integer;
  UseIndex: Integer;
  Item: TCustomSubLayerItem;
begin
  result := 0;
  if RunTimeSimulated then
  begin
    if LayerCount > 1 then
    begin
      for Index := 0 to SubLayers.Count - 1 do
      begin
        Item := SubLayers.Items[Index] as TCustomSubLayerItem;
        if Item.UseInAllLayers then
        begin
          Inc(result, LayerCount);
        end
        else
        begin
          for UseIndex := 1 to LayerCount do
          begin
            if Item.UsedLayers.GetItemByLayerNumber(UseIndex) <> nil then
            begin
              Inc(result);
            end;
          end;
        end;
      end;
    end
    else
    begin
      result := SubLayers.Count;
    end;
  end;
end;

procedure TLayerGroup.SetAquiferType(const Value: integer);
var
  Notifier: TObserver;
begin
  if FAquiferType <> Value then
  begin
    Assert(Value  in [0..3]);
    FAquiferType := Value;
    Notifier := (Collection as TLayerStructure).AquiferTypeNotifier;
    Notifier.UpToDate := False;
    Notifier.UpToDate := True;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetConduitLayers(const Value: TConduitLayerCollection);
begin
  FConduitLayers.Assign(Value);
end;

procedure TLayerGroup.SetHorizontalAnisotropy(const Value: double);
begin
  if FHorizontalAnisotropy <> Value then
  begin
    FHorizontalAnisotropy := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetInterblockTransmissivityMethod(const Value: integer);
begin
  if FInterblockTransmissivityMethod <> Value then
  begin
    FInterblockTransmissivityMethod := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetLayerCollection(const Value: TLayerCollection);
var
  PriorCount: integer;
begin
  PriorCount := LayerCollection.Count;
  inherited;
  UpdateChildModels(PriorCount);
end;

procedure TLayerGroup.SetMt3dmsDiffusionCoef(const Value: TRealCollection);
begin
  FMt3dmsDiffusionCoef.Assign(Value);
end;

procedure TLayerGroup.SetMt3dmsHorzTransDisp(const Value: TRealCollection);
begin
  FMt3dmsHorzTransDisp.Assign(Value);
end;

procedure TLayerGroup.SetMt3dmsVertTransDisp(const Value: TRealCollection);
begin
  FMt3dmsVertTransDisp.Assign(Value);
end;

procedure TLayerGroup.SetSimulated(const Value: boolean);
var
  PhastModel: TPhastModel;
  Notifier: TObserver;
begin
  if FSimulated <> Value then
  begin
    FSimulated := Value;
    PhastModel := Model as TPhastModel;
    if PhastModel <> nil then
    begin
      PhastModel.InvalidateModflowBoundaries;
    end;
    Notifier := (Collection as TLayerStructure).SimulatedNotifier;
    Notifier.UpToDate := False;
    Notifier.UpToDate := True;
    UpdateChildModels(LayerCollection.Count);
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetSubDelayBedLayers(const Value: TSubDelayBedLayers);
begin
  FSubDelayBedLayers.Assign(Value);
end;

procedure TLayerGroup.SetSubNoDelayBedLayers(const Value: TSubNoDelayBedLayers);
begin
  FSubNoDelayBedLayers.Assign(Value)
end;

procedure TLayerGroup.SetUseStartingHeadForSaturatedThickness(
  const Value: boolean);
begin
  if FUseStartingHeadForSaturatedThickness <> Value then
  begin
    FUseStartingHeadForSaturatedThickness := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetVerticalHydraulicConductivityMethod(
  const Value: integer);
begin
  if FVerticalHydraulicConductivityMethod <> Value then
  begin
    FVerticalHydraulicConductivityMethod := Value;
    InvalidateModel;
  end;
end;

procedure TLayerGroup.SetWaterTableLayers(const Value: TWaterTableLayers);
begin
  FWaterTableLayers.Assign(Value);
end;

function TLayerGroup.ShouldCreateDataArray: Boolean;
begin
  result := Model.ModelSelection in ModflowSelection;
end;

function TLayerGroup.StoreLayerCollection: boolean;
begin
  result := FSimulated;
end;

function TLayerGroup.SubsidenceDefined: boolean;
begin
  result := RunTimeSimulated and
    ((SubNoDelayBedLayers.Count > 0) or (SubDelayBedLayers.Count > 0));
end;

function TLayerGroup.SwtDefined: boolean;
begin
  result := RunTimeSimulated and
    (WaterTableLayers.Count > 0) ;
end;

function TLayerGroup.WaterTableCount: integer;
begin
  result := SubsidenceLayerCount(WaterTableLayers);
end;

procedure TLayerGroup.UpdateChildModels(PriorCount: Integer);
var
  PriorGroup: TLayerGroup;
  Discretization: TChildDiscretizationCollection;
  ChildItem: TChildModelItem;
  ChildIndex: Integer;
  PhastModel: TPhastModel;
begin
  PhastModel := Model as TPhastModel;
  if (PhastModel <> nil) and (PriorCount <> LayerCollection.Count) then
  begin
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildItem := PhastModel.ChildModels[ChildIndex];
      if ChildItem.ChildModel <> nil then
      begin
        Discretization := ChildItem.ChildModel.Discretization;
        if (Discretization.BottomLayerGroup = self) then
        begin
          if not RunTimeSimulated then
          begin
            PriorGroup := Collection.Items[Index - 1] as TLayerGroup;
            Discretization.BottomLayerGroup := PriorGroup;
            Discretization.BottomLayerInUnit := PriorGroup.LayerCount - 1;
          end
          else if (Discretization.BottomLayerInUnit = PriorCount) then
          begin
            Discretization.BottomLayerInUnit := LayerCount - 1;
          end
          else if Discretization.BottomLayerInUnit > LayerCount - 1 then
          begin
            Discretization.BottomLayerInUnit := LayerCount - 1;
          end;
        end;
        Discretization.SortAndDeleteExtraItems;
      end;
    end;
  end;
end;

procedure TLayerGroup.UpdateRequiredDataArrays;
begin
  SubNoDelayBedLayers.UpdateRequiredDataArrays;
  SubDelayBedLayers.UpdateRequiredDataArrays;
end;

procedure TLayerGroup.WriteLAYCB(const DiscretizationWriter: TObject);
var
  DisWriter: TModflowDiscretizationWriter;
  Index: integer;
begin
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  if RunTimeSimulated then
  begin
    for Index := 0 to LayerCollection.Count-1 do
    begin
      DisWriter.WriteInteger(0);
    end;
  end
  else
  begin
    DisWriter.WriteInteger(1);
  end;
end;

{ TLayerStructure }
procedure TLayerStructure.Assign(Source: TPersistent);
var
  PhastModel: TPhastModel;
  Index: Integer;
  Child: TChildModelItem;
begin
  inherited;
  if Model <> nil then
  begin
    PhastModel := Model as TPhastModel;
    if PhastModel.DisvUsed then
    begin
      PhastModel.DisvGrid.LayerCount := LayerCount;
      PhastModel.ModflowGrid.LayerCount := LayerCount;
    end
    else
    begin
      PhastModel.Grid.LayerCount := LayerCount;
    end;
    for Index := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      Child := PhastModel.ChildModels[Index];
      if Child.ChildModel <> nil then
      begin
        Child.ChildModel.UpdateLayerCount;
      end;
    end;
    PhastModel.InvalidateMfEtsEvapLayer(self);
    PhastModel.InvalidateMfEvtEvapLayer(self);
    PhastModel.InvalidateMfRchLayer(self);
    AssignAssociatedInputDataSets;
  end;
end;

function TLayerStructure.Chani: TOneDIntegerArray;
var
  Index: Integer;
begin
  SetLength(result, ModflowLayerCount);
  for Index := 0 to Length(result) - 1 do
  begin
    result[Index] := -1;
  end;
end;

constructor TLayerStructure.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TLayerGroup, Model);
  FSimulatedNotifier := TObserver.Create(nil);
  FSimulatedNotifier.Name := 'Simulated_Notifier';
  FAquiferTypeNotifier := TObserver.Create(nil);
  FAquiferTypeNotifier.Name := 'AquiferType_Notifier'
end;

function TLayerStructure.GetLayerGroup(const Index: integer): TLayerGroup;
begin
  result := Items[Index] as TLayerGroup;
end;

function TLayerStructure.GetLayerGroupByLayer(
  const Layer: integer): TLayerGroup;
var
  HufUnitIndex: Integer;
  HufUnit: TLayerGroup;
  LocalLayerCount : integer;
begin
  Assert(Layer >= 0);
  result := nil;
  LocalLayerCount := 0;
  for HufUnitIndex := 1 to Count - 1 do
  begin
    HufUnit := LayerGroups[HufUnitIndex];
    LocalLayerCount := LocalLayerCount + HufUnit.LayerCount;
    if LocalLayerCount > Layer then
    begin
      result := HufUnit;
      Exit;
    end;
  end;
end;

function TLayerStructure.IsLayerConfined(const LayerID: integer): boolean;
var
  LayerGroup: TLayerGroup;
  LayerGroupIndex: integer;
  LayerCount: integer;
begin
  { TODO -cMODFLOW 6 : This may need to change for MF 6 }
  Assert(LayerID >= 0);
  LayerCount := 0;
  result := False;
  for LayerGroupIndex := 1 to Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    LayerCount := LayerCount + LayerGroup.LayerCount;
    if LayerCount >= LayerID+1 then
    begin
      result := LayerGroup.AquiferType = 0;
      Exit;
    end;
  end;
  Assert(False);
end;

function TLayerStructure.IsLayerSimulated(const LayerID: integer): boolean;
var
  LayerGroup: TLayerGroup;
  LayerGroupIndex: integer;
  LayerCount: integer;
begin
  Assert(Model <> nil);
  if Model.ModelSelection = msModflow2015 then
  begin
    result := True;
    Exit;
  end;
  Assert(LayerID >= 0);
  LayerCount := 0;
  result := False;
  for LayerGroupIndex := 1 to Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    LayerCount := LayerCount + LayerGroup.LayerCount;
    if LayerCount >= LayerID+1 then
    begin
      result := LayerGroup.RunTimeSimulated;
      Exit;
    end;
  end;
  Assert(False);
end;

function TLayerStructure.IntegerArray(Method: TIntTypeMethod):
  TOneDIntegerArray;
var
  LayerCount: integer;
  GroupIndex: Integer;
  Group: TLayerGroup;
  LayerIndex: Integer;
  MFLayIndex: integer;
  PhastModel: TPhastModel;
begin
  PhastModel := Model as TPhastModel;
  LayerCount := ModflowLayerCount;
  SetLength(result, LayerCount);
  MFLayIndex := 0;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    if Group.RunTimeSimulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount - 1 do
      begin
        Assert(MFLayIndex < LayerCount);
        case Method of
          itmLaytype:
            begin
              result[MFLayIndex] := Group.AquiferType;
              if (Group.AquiferType = 1) and
                Group.UseStartingHeadForSaturatedThickness
                and PhastModel.ModflowPackages.LpfPackage.isSelected
                and PhastModel.ModflowPackages.LpfPackage.UseSaturatedThickness then
              begin
                result[MFLayIndex] := -1;
              end;
              if PhastModel.ModflowPackages.BcfPackage.isSelected then
              begin
                if (MFLayIndex > 0) and (result[MFLayIndex] = 1) then
                begin
                  result[MFLayIndex] := 3;
                end;
                result[MFLayIndex] := result[MFLayIndex]
                  + Group.InterblockTransmissivityMethod*10;
              end;
            end;
          itmLayavg: result[MFLayIndex] := Group.InterblockTransmissivityMethod;
          itmLayvka: result[MFLayIndex] :=
            Group.VerticalHydraulicConductivityMethod;
          else Assert(False);
        end;
        Inc(MFLayIndex);
      end;
    end;
  end;
  Assert(MFLayIndex = LayerCount);
end;

function TLayerStructure.Last: TLayerGroup;
begin
  result := inherited Last as TLayerGroup;
end;

function TLayerStructure.Layavg: TOneDIntegerArray;
begin
  result := IntegerArray(itmLayavg);
end;

function TLayerStructure.Laytyp: TOneDIntegerArray;
begin
  result := IntegerArray(itmLaytype);
end;

function TLayerStructure.Layvka: TOneDIntegerArray;
begin
  result := IntegerArray(itmLayvka);
end;

procedure TLayerStructure.Loaded;
begin
  inherited;
  AssignAssociatedInputDataSets;
end;

function TLayerStructure.ModflowLayerBottomDescription(
  const LayerID: integer): string;
var
  LayerGroup: TLayerGroup;
  LayerGroupIndex: integer;
  LayerCount: integer;
  LayerNumber: integer;
begin
  Assert(LayerID >= 0);
  LayerCount := 0;
  result := '';
  LayerNumber := LayerID+1;
  for LayerGroupIndex := 1 to Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    LayerCount := LayerCount + LayerGroup.LayerCount;
    if LayerCount >= LayerID+1 then
    begin
      result := LayerGroup.AquiferName;
      if LayerGroup.LayerCollection.Count > 0 then
      begin
        result := result + ' Layer ' + IntToStr(LayerNumber);
      end;
      Exit;
    end;
    LayerNumber := LayerID+1 - LayerCount;
  end;
end;

function TLayerStructure.ModflowConfiningBedCount: integer;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  result := 0;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TLayerGroup;
    if not LayerGroup.RunTimeSimulated then
    begin
      Inc(result);
    end;
  end;
end;

function TLayerStructure.ModflowLayerCount: integer;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  result := 0;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TLayerGroup;
    result := result + LayerGroup.ModflowLayerCount;
  end;
end;

function TLayerStructure.DataSetLayerToModflowLayer(
  DataSetLayer: integer): integer;
var
  GroupIndex: Integer;
  Group: TLayerGroup;
  GridLayerCount: integer;
begin
  GridLayerCount := 0;
  result := 0;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    if Group.RunTimeSimulated then
    begin
      GridLayerCount := GridLayerCount + Group.ModflowLayerCount;
      result := result + Group.ModflowLayerCount;
    end
    else
    begin
      GridLayerCount := GridLayerCount + 1;
    end;
    if GridLayerCount >= DataSetLayer then
    begin
      result := result - (GridLayerCount-DataSetLayer)+1;
      Exit;
    end;
  end;
  Assert(False);
end;

function TLayerStructure.DelayCount: integer;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Group := LayerGroups[Index];
    result := result + Group.DelayCount;
  end;
end;

destructor TLayerStructure.Destroy;
begin
  FAquiferTypeNotifier.Free;
  FSimulatedNotifier.Free;
  inherited;
end;

function TLayerStructure.DMCOEF: TOneDRealArray;
begin
  Result := FloatArray(ftmDmcoef);
end;

//function TLayerStructure.First: TLayerGroup;
//begin
//  result := inherited First as TLayerGroup;
//end;

function TLayerStructure.FloatArray(Method: TFloatTypeMethod): TOneDRealArray;
var
  LayerCount: integer;
  GroupIndex: Integer;
  Group: TLayerGroup;
  LayerIndex: Integer;
  MFLayIndex: integer;
begin
  LayerCount := ModflowLayerCount;
  SetLength(result, LayerCount);
  MFLayIndex := 0;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    if Group.RunTimeSimulated then
    begin
      for LayerIndex := 0 to Group.ModflowLayerCount - 1 do
      begin
        Assert(MFLayIndex < LayerCount);
        case Method of
          ftmTrpy:
            begin
              result[MFLayIndex] := Group.HorizontalAnisotropy;
            end;
          ftmTrtp:
            begin
              while LayerIndex >= Group.Mt3dmsHorzTransDisp.Count do
              begin
                Group.Mt3dmsHorzTransDisp.Add;
                if LayerIndex > 0 then
                begin
                  Group.Mt3dmsHorzTransDisp[LayerIndex].Assign(Group.Mt3dmsHorzTransDisp[LayerIndex-1]);
                end;
              end;
              result[MFLayIndex] := Group.Mt3dmsHorzTransDisp[LayerIndex].Value;
            end;
          ftmTrpv:
            begin
              while LayerIndex >= Group.Mt3dmsVertTransDisp.Count do
              begin
                Group.Mt3dmsVertTransDisp.Add;
                if LayerIndex > 0 then
                begin
                  Group.Mt3dmsVertTransDisp[LayerIndex].Assign(Group.Mt3dmsVertTransDisp[LayerIndex-1]);
                end;
              end;
              result[MFLayIndex] := Group.Mt3dmsVertTransDisp[LayerIndex].Value;
            end;
          ftmDmcoef:
            begin
              while LayerIndex >= Group.Mt3dmsDiffusionCoef.Count do
              begin
                Group.Mt3dmsDiffusionCoef.Add;
                if LayerIndex > 0 then
                begin
                  Group.Mt3dmsDiffusionCoef[LayerIndex].Assign(Group.Mt3dmsDiffusionCoef[LayerIndex-1]);
                end;
              end;
              result[MFLayIndex] := Group.Mt3dmsDiffusionCoef[LayerIndex].Value;
            end
          else Assert(False);
        end;
        Inc(MFLayIndex);
      end;
    end;
  end;
  Assert(MFLayIndex = LayerCount);
end;

procedure TLayerStructure.AssignAssociatedInputDataSets;
var
  Index: Integer;
  PhastModel: TPhastModel;
  Name: string;
  DataSet: TDataArray;
begin
  PhastModel := Model as TPhastModel;
  Assert(PhastModel <> nil);
  for Index := 0 to Count - 1 do
  begin
    Name := LayerGroups[Index].DataArrayName;
    DataSet := PhastModel.DataArrayManager.GetDataSetByName(Name);
    Assert(DataSet <> nil);
    if Index = 0 then
    begin
      DataSet.AssociatedDataSets := 'MODFLOW DIS: Top';
    end
    else
    begin
      DataSet.AssociatedDataSets := 'MODFLOW DIS: BOTM';
    end;
  end;
end;

function TLayerStructure.ModflowLayerToDataSetLayer(
  ModflowLayer: integer): integer;
var
  GroupIndex: Integer;
  Group: TLayerGroup;
  LayerCount: integer;
begin
  if (ModflowLayer < 1) or (ModflowLayer > ModflowLayerCount) then
  begin
    raise EInvalidLayer.Create('Invalid layer number');
  end;
  LayerCount := 0;
  result := -1;
  for GroupIndex := 1 to Count - 1 do
  begin
    Group := LayerGroups[GroupIndex];
    LayerCount := LayerCount + Group.ModflowLayerCount;
    if Group.RunTimeSimulated then
    begin
      result := result + Group.ModflowLayerCount;
    end
    else
    begin
      result := result + 1;
    end;

    if LayerCount >= ModflowLayer then
    begin
      result := result - (LayerCount - ModflowLayer);
      Exit;
    end;
  end;
  Assert(False);
end;

function TLayerStructure.NoDelayCount: integer;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Group := LayerGroups[Index];
    result := result + Group.NoDelayCount;
  end;
end;

function TLayerStructure.NonSimulatedLayersPresent: boolean;
var
  Index: Integer;
  LayerGroup: TLayerGroup;
begin
  result := false;
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TLayerGroup;
    if not LayerGroup.RunTimeSimulated then
    begin
      result := True;
      Exit;
    end;
  end;
end;

procedure TLayerStructure.StopTalkingToAnyone;
begin
  SimulatedNotifier.StopTalkingToAnyone;
  AquiferTypeNotifier.StopTalkingToAnyone;
end;

function TLayerStructure.SubsidenceDefined: boolean;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Group := LayerGroups[Index];
    result := Group.SubsidenceDefined;
    if result then
    begin
      Exit;
    end;
  end;
end;

function TLayerStructure.SwtDefined: boolean;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  result := False;
  for Index := 0 to Count - 1 do
  begin
    Group := LayerGroups[Index];
    result := Group.SwtDefined;
    if result then
    begin
      Exit;
    end;
  end;
end;

function TLayerStructure.TRPT: TOneDRealArray;
begin
  Result := FloatArray(ftmTrtp);
end;

function TLayerStructure.TRPV: TOneDRealArray;
begin
  Result := FloatArray(ftmTrpv);
end;

function TLayerStructure.Trpy: TOneDRealArray;
begin
  result := FloatArray(ftmTrpy);
end;

procedure TLayerStructure.UpdateRequiredDataArrays;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  for Index := 0 to Count - 1 do
  begin
    Group := LayerGroups[Index];
    Group.UpdateRequiredDataArrays;
  end;
end;

function TLayerStructure.WaterTableCount: integer;
var
  Index: Integer;
  Group: TLayerGroup;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Group := LayerGroups[Index];
    result := result + Group.WaterTableCount;
  end;
end;

procedure TLayerStructure.WriteLAYCB(const DiscretizationWriter: TObject);
var
  Index: Integer;
  LayerGroup: TLayerGroup;
  DisWriter: TModflowDiscretizationWriter;
  ItemIndex: Integer;
begin
  DisWriter := DiscretizationWriter as TModflowDiscretizationWriter;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
//    LayerGroup.WriteLAYCB(DisWriter);
    LayerGroup := Items[Index] as TLayerGroup;
    if LayerGroup.RunTimeSimulated then
    begin
      for ItemIndex := 0 to LayerGroup.LayerCollection.Count-1 do
      begin
        DisWriter.WriteInteger(0);
      end;
      if Index < Count - 1 then
      begin
        LayerGroup := Items[Index+1] as TLayerGroup;
        if LayerGroup.RunTimeSimulated then
        begin
          DisWriter.WriteInteger(0);
        end
        else
        begin
          DisWriter.WriteInteger(1);
        end;
      end
      else
      begin
        DisWriter.WriteInteger(0);
      end;
    end;
  end;
  DisWriter.WriteString(' # LAYCB');
  DisWriter.NewLine;
end;

{ TLayerFraction }

procedure TLayerFraction.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  Fraction := (Source as TLayerFraction).Fraction;
end;

function TLayerFraction.Collection: TLayerCollection;
begin
  result := inherited Collection as TLayerCollection;
end;

procedure TLayerFraction.InvalidateModel;
begin
  Collection.InvalidateModel;
end;

function TLayerFraction.IsSame(AnotherLayerFraction: TLayerFraction): boolean;
begin
  result := Fraction = AnotherLayerFraction.Fraction;
end;

procedure TLayerFraction.SetFraction(const Value: real);
begin
  if FFraction <> Value then
  begin
    FFraction := Value;
    InvalidateModel;
  end;
end;

{ TLayerCollection }

procedure TLayerCollection.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if not IsSame(Source as TLayerCollection) then
  begin
    inherited;
  end;
  Sort;
end;

constructor TLayerCollection.Create(LayerGroup: TGrowthControls);
begin
  inherited Create(TLayerFraction);
  Assert(LayerGroup <> nil);
  FLayerGroup := LayerGroup;
  InvalidateModel;
end;

destructor TLayerCollection.Destroy;
begin
  InvalidateModel;
  inherited;
end;

function TLayerCollection.GetItem(Index: Integer): TLayerFraction;
begin
  Result := inherited Items[Index] as TLayerFraction;
end;

procedure TLayerCollection.InvalidateModel;
begin
  FLayerGroup.InvalidateModel;
end;

function TLayerCollection.IsSame(
  AnotherLayerCollection: TLayerCollection): boolean;
var
  Index: Integer;
begin
  result := Count = AnotherLayerCollection.Count;
  if result then
  begin
    for Index := 0 to Count - 1 do
    begin
      result := Items[Index].IsSame(
        AnotherLayerCollection.Items[Index]);
      if not result then Exit;
    end;
  end;
end;

function CompareLayerFractions(Item1, Item2: Pointer): Integer;
var
  Frac1, Frac2: TLayerFraction;
begin
  Frac1 := Item1;
  Frac2 := Item2;
  result := Sign(Frac2.Fraction - Frac1.Fraction);
end;

procedure TLayerCollection.SetItem(Index: Integer;
  const Value: TLayerFraction);
begin
  inherited Items[Index] := Value;
end;

procedure TLayerCollection.Sort;
var
  List: TList;
  Index: integer;
  Item: TLayerFraction;
begin
  List := TList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      List.Add(Items[Index]);
    end;
    List.Sort(CompareLayerFractions);
    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      Item.Index := Index;
    end;
  finally
    List.Free;
  end;
end;

procedure TCustomLayerGroup.Assign(Source: TPersistent);
var
  AnotherLayerGroup: TCustomLayerGroup;
begin
  // if Assign is updated, update IsSame too.
  inherited;
  AnotherLayerGroup := Source as TCustomLayerGroup;
  if not IsSame(AnotherLayerGroup) then
  begin
    // It is important for AquiferName to be assigned after DataArrayName
    // because if AquiferName has changed, DataArrayName will
    // need to be changed too and that change should not be overwritten
    // by another assignment to DataArrayName.
    if AnotherLayerGroup.DataArrayName <> '' then
    begin
      DataArrayName := AnotherLayerGroup.DataArrayName;
    end;
    AquiferName := AnotherLayerGroup.AquiferName;
//    GrowthMethod := AnotherLayerGroup.GrowthMethod;
//    GrowthRate := AnotherLayerGroup.GrowthRate;
//    LayerCollection := AnotherLayerGroup.LayerCollection;
  end;
end;

function TCustomLayerGroup.Collection: TCustomLayerStructure;
begin
  result := inherited Collection as TCustomLayerStructure;
end;

function TCustomLayerGroup.EvalAt: TEvaluatedAt;
begin
  result := eaBlocks
end;

function TCustomLayerGroup.GetRunTimeSimulated: boolean;
begin
  if (Model <> nil) and (Model.ModelSelection = msModflow2015) then
  begin
    result := True;
  end
  else
  begin
    result := Simulated;
  end;
end;

function TCustomLayerGroup.GetSimulated: boolean;
begin
  result := True;
end;

procedure TCustomLayerGroup.SetDataArrayName(const NewName: string);
begin
  if FDataArrayName <> NewName then
  begin
    UpdateDataArray(NewName, FAquiferDisplayName);
    FDataArrayName := NewName;
    InvalidateModel;
  end;
end;

procedure TGrowthControls.Assign(Source: TPersistent);
var
  AnotherGrowthControls: TGrowthControls;
begin
  // if Assign is updated, update IsSame too.
//  inherited;
  AnotherGrowthControls := Source as TGrowthControls;
  if not IsSame(AnotherGrowthControls) then
  begin
    GrowthMethod := AnotherGrowthControls.GrowthMethod;
    GrowthRate := AnotherGrowthControls.GrowthRate;
    LayerCollection := AnotherGrowthControls.LayerCollection;
  end;
end;

constructor TGrowthControls.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
//  if Model = nil then
//  begin
//    inherited Create(nil);
//  end
//  else
//  begin
//    inherited Create(Model.Invalidate);
//  end;
  FLayerCollection:= TLayerCollection.Create(self);
  FGrowthRate := 1.2;
  InvalidateModel;
end;

destructor TGrowthControls.Destroy;
begin
  FLayerCollection.Free;
  InvalidateModel;
  inherited;
end;

function TGrowthControls.IsSame(AnotherGrowControls : TGrowthControls): boolean;
begin
  result := (AnotherGrowControls.GrowthMethod = GrowthMethod)
    and (AnotherGrowControls.GrowthRate = GrowthRate)
    and AnotherGrowControls.LayerCollection.IsSame(LayerCollection)
end;

procedure TGrowItem.Assign(Source: TPersistent);
var
  SourceItem: TGrowItem;
begin
  if Source is TGrowItem then
  begin
    SourceItem := TGrowItem(Source);
    GrowthControls := SourceItem.GrowthControls;
  end;
  inherited;

end;

constructor TGrowItem.Create(Collection: TCollection);
begin
  inherited;
  FGrowthControls := TGrowthControls.Create(OnInvalidateModelEvent);
end;

destructor TGrowItem.Destroy;
begin
  FGrowthControls.Free;
  inherited;
end;

function TGrowItem.GetGrowthMethod: TGrowthMethod;
begin
  result := FGrowthControls.GrowthMethod;
end;

function TGrowItem.GetGrowthRate: real;
begin
  result := FGrowthControls.GrowthRate;
end;

function TGrowItem.GetLayerCollection: TLayerCollection;
begin
  result := FGrowthControls.LayerCollection;
end;

function TGrowItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherGrowItem : TGrowItem;
begin
  AnotherGrowItem := AnotherItem as TGrowItem;
  result := (AnotherGrowItem.GrowthMethod = GrowthMethod)
    and (AnotherGrowItem.GrowthRate = GrowthRate)
    and AnotherGrowItem.LayerCollection.IsSame(LayerCollection)
end;

function TCustomLayerGroup.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnotherLayerGroup : TCustomLayerGroup;
begin
  AnotherLayerGroup := AnotherItem as TCustomLayerGroup;
  result := inherited IsSame(AnotherLayerGroup)
    and (AnotherLayerGroup.AquiferName = AquiferName)
    and (AnotherLayerGroup.DataArrayName = DataArrayName)
end;

procedure TCustomLayerGroup.SetTopDisplayName(Model: TBaseModel);
begin
  //      FAquiferDisplayName := FDataArrayName;
  if FDAtaArrayName = kModelTop then
  begin
    FAquiferDisplayName := StrModelTop
  end
  else if FDAtaArrayName = kSUTRAMeshTop then
  begin
    FAquiferDisplayName := StrSUTRAMeshTop
  end
  else if FDAtaArrayName = '' then
  begin
    FAquiferDisplayName := '';
  end
  else
  begin
    Assert(False);
  end;
//  case Model.ModelSelection of
//    msUndefined, msPhast, msModflow, msModflowLGR,msModflowNWT:
//      FAquiferDisplayName := StrModelTop;
//    {$IFDEF SUTRA}
//    msSutra: FAquiferDisplayName := StrSUTRAMeshTop;
//    {$ENDIF}
//    else Assert(False);
//  end;
end;

procedure TCustomLayerGroup.UpdateDataArray(const NewName, NewDisplayName: string);
var
  Model: TPhastModel;
  DataArray: TDataArray;
  UnitAbove: TCustomLayerGroup;
  NewFormula: string;
  UnitBelow: TCustomLayerGroup;
begin
  if Collection.Model <> nil then
  begin
    Model := Collection.Model as TPhastModel;
    if not (csLoading in Model.ComponentState) then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(FDataArrayName);
      if DataArray <> nil then
      begin
        Model.RenameDataArray(DataArray, NewName, NewDisplayName);
      end
      else
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(NewName);
      end;
      if (DataArray = nil) and ShouldCreateDataArray then
      begin
        // create a new data array.
        // First get formula for new layer.
        if Collection.Count = 1 then
        begin
          NewFormula := '0';
        end
        else if Index <= 0 then
        begin
          NewFormula := '0.';
          // No unit can be inserted above the top of the model.
//          Assert(False);
        end
        else if Index = Collection.Count - 1 then
        begin
          UnitAbove := Collection.Items[Index - 1] as TCustomLayerGroup;
          NewFormula := UnitAbove.DataArrayName + ' - 1';
        end
        else
        begin
          UnitAbove := Collection.Items[Index - 1] as TCustomLayerGroup;
          UnitBelow := Collection.Items[Index + 1] as TCustomLayerGroup;
          NewFormula := '(' + UnitAbove.DataArrayName + ' + ' + UnitBelow.DataArrayName + ') / 2';
        end;
        // create new data array.
        DataArray := Model.DataArrayManager.CreateNewDataArray(TDataArray,
           NewName, NewFormula, NewDisplayName,
           [dcName, dcType, dcOrientation, dcEvaluatedAt], rdtDouble, EvalAt,
           dsoTop, StrLayerDefinition);
        DataArray.OnDataSetUsed := Model.ModelLayerDataArrayUsed;
        Collection.AddOwnedDataArray(DataArray);
      end;
      if DataArray <> nil then
      begin
        Model.TopGridObserver.TalksTo(DataArray);
        DataArray.TalksTo(Model.ThreeDGridObserver);
        Model.ThreeDGridObserver.StopsTalkingTo(DataArray);
        Model.UpdateDataArrayDimensions(DataArray);
        DataArray.Classification := StrLayerDefinition;
      end;
    end;
    // DataArray.UpdateDimensions(Model.Grid.LayerCount,
    // Model.Grid.RowCount, Model.Grid.ColumnCount);
  end;
end;

procedure TCustomLayerGroup.Loaded;
var
  Model: TPhastModel;
  DataArray: TDataArray;
begin
  Model := Collection.Model as TPhastModel;
//  DataArray := Model.DataArrayManager.GetDataSetByName(FDataArrayName);
//  if ShouldCreateDataArray and (DataArray = nil) then
  begin
    if Index = 0 then
    begin
      SetTopDisplayName(Model);
    end
    else
    begin
      FAquiferDisplayName := GenerateNewRoot(FAquiferName + StrBottom);
    end;
    UpdateDataArray(DataArrayName, FAquiferDisplayName);
    DataArray := Model.DataArrayManager.GetDataSetByName(FDataArrayName);
    if ShouldCreateDataArray then
    begin
      Assert(DataArray <> nil);
    end;
  end;
  if DataArray <> nil then
  begin
    Model.TopGridObserver.TalksTo(DataArray);
    DataArray.TalksTo(Model.ThreeDGridObserver);
    Model.ThreeDGridObserver.StopsTalkingTo(DataArray);
  end;
end;

procedure TCustomLayerGroup.SetAquiferName(const Value: string);
var
  LocalDataArrayName: string;
begin
  Assert(Value <> '');
  if FAquiferName <> Value then
  begin
    if Collection.Model <> nil then
    begin
      if UpperCase(FAquiferName) = UpperCase(Value) then
      begin
        // Change case of the data set
        LocalDataArrayName := StringReplace(DataArrayName,
          GenerateNewRoot(FAquiferName), GenerateNewRoot(Value), []);
        FAquiferDisplayName := LocalDataArrayName;
        DataArrayName := LocalDataArrayName;
      end
      else
      begin
        if Index = 0 then
        begin
          LocalDataArrayName := GenerateNewName(Value);
//          FAquiferDisplayName := LocalDataArrayName;
          SetTopDisplayName(Collection.Model as TCustomModel);
//          FAquiferDisplayName := ModelTopDisplayName;
          DataArrayName := LocalDataArrayName;
        end
        else
        begin
          LocalDataArrayName := GenerateNewRoot(Value + KBottom);
          FAquiferDisplayName := GenerateNewRoot(Value + StrBottom);
          DataArrayName := LocalDataArrayName;
        end;
      end;
    end;
    FAquiferName := Value;
    InvalidateModel;
  end;
end;

procedure TCustomLayerGroup.SetSimulated(const Value: boolean);
begin
  // do nothing
end;

function TCustomLayerGroup.ShouldCreateDataArray: Boolean;
begin
  result := True;
end;

function TCustomLayerStructure.LayerCount: integer;
var
  Index: integer;
  LayerGroup: TCustomLayerGroup;
begin
  Result := 0;
  // Skip the top of the model: it doesn't count.
  for Index := 1 to Count - 1 do
  begin
    LayerGroup := Items[Index] as TCustomLayerGroup;
    Result := Result + LayerGroup.LayerCount;
  end;
end;

procedure TCustomLayerStructure.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    (Items[Index] as TCustomLayerGroup).Loaded;
  end;
end;

{ TSutraLayerStructure }

constructor TSutraLayerStructure.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TSutraLayerGroup, Model);
end;

function TSutraLayerStructure.ElementLayerCount: Integer;
begin
  if (Model as TPhastModel).SutraMesh.MeshType in [mt2D, mtProfile] then
  begin
    result := 1;
  end
  else
  begin
    result := inherited LayerCount;
  end;
end;

function TSutraLayerStructure.GetLayerGroup(
  const Index: integer): TSutraLayerGroup;
begin
  result := Items[Index] as TSutraLayerGroup;
end;

function TSutraLayerStructure.LayerCount: integer;
begin
  if (Model is TPhastModel) then
  begin
    if TPhastModel(Model).Mesh = nil then
    begin
      result := 0
    end
    else if TPhastModel(Model).SutraMesh.MeshType in [mt2D, mtProfile] then
    begin
      result := 1;
    end
    else
    begin
      result := inherited;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TSutraLayerStructure.NodeLayerCount: Integer;
begin
  if (Model as TPhastModel).Mesh = nil then
  begin
    result := 0;
  end
  else if (Model as TPhastModel).SutraMesh.MeshType in [mt2D, mtProfile] then
  begin
    result := 1;
  end
  else
  begin
    result := inherited LayerCount + 1;
  end;
end;

{ TSutraLayerGroup }

procedure TSutraLayerGroup.Assign(Source: TPersistent);
begin
  if Source is TSutraLayerGroup then
  begin
    MinThickness := TSutraLayerGroup(Source).MinThickness;
  end;
  inherited;
end;

constructor TSutraLayerGroup.Create(Collection: TCollection);
begin
  inherited;
  FStoredMinThickness := TRealStorage.Create;
  FStoredMinThickness.Value := 1e-3;
  FStoredMinThickness.OnChange := MinThicknessChanged;
end;

destructor TSutraLayerGroup.Destroy;
begin
  FStoredMinThickness.Free;
  inherited;
end;

function TSutraLayerGroup.EvalAt: TEvaluatedAt;
begin
  result := eaNodes;
end;

function TSutraLayerGroup.GetMinThickness: Double;
begin
  Result := FStoredMinThickness.Value;
end;

procedure TSutraLayerGroup.InvalidateModel;
var
  Mesh: TSutraMesh3D;
begin
  inherited;
  if Model <> nil then
  begin
    Mesh := (Model as TCustomModel).SutraMesh;
    if Mesh <> nil then
    begin
      Mesh.InvalidatePolygons;
      Mesh.ElevationsNeedUpdating := True
    end;
  end;
end;

function TSutraLayerGroup.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  Result := (AnotherItem is TSutraLayerGroup) and inherited IsSame(AnotherItem);
  if result then
  begin
    Result := MinThickness = TSutraLayerGroup(AnotherItem).MinThickness;
  end;
end;

procedure TSutraLayerGroup.MinThicknessChanged(Sender: TObject);
begin
  InvalidateModel;
end;

procedure TSutraLayerGroup.SetMinThickness(const Value: Double);
begin
  FStoredMinThickness.Value := Value;
end;

procedure TSutraLayerGroup.SetStoredMinThickness(const Value: TRealStorage);
begin
  FStoredMinThickness.Assign(Value);
end;

function TSutraLayerGroup.ShouldCreateDataArray: Boolean;
var
  LocalModel: TCustomModel;
begin
  Result := (Model.ModelSelection in SutraSelection);
  if result then
  begin
    LocalModel := Model as TCustomModel;
    result := (LocalModel.SutraMesh <> nil) and
      (LocalModel.SutraMesh.MeshType = mt3D);
  end;
end;

procedure TGrowthControls.SetGrowthMethod(const Value: TGrowthMethod);
begin
  if FGrowthMethod <> Value then
  begin
    FGrowthMethod := Value;
    InvalidateModel;
  end;
end;

procedure TGrowthControls.SetGrowthRate(const Value: real);
begin
  if FGrowthRate <> Value then
  begin
    FGrowthRate := Value;
    InvalidateModel;
  end;
end;

function TGrowthControls.StoreLayerCollection: boolean;
begin
  Result := True;
end;

procedure TGrowthControls.SetLayerCollection(const Value: TLayerCollection);
begin
  FLayerCollection.Assign(Value);
end;

function TGrowthControls.LayerCount: Integer;
begin
  Result := LayerCollection.Count + 1;
end;

function TGrowItem.LayerCount: Integer;
begin
  result := FGrowthControls.LayerCount
end;

procedure TGrowItem.SetGrowthControls(const Value: TGrowthControls);
begin
  FGrowthControls.Assign(Value);
end;

procedure TGrowItem.SetGrowthMethod(const Value: TGrowthMethod);
begin
  FGrowthControls.GrowthMethod := Value;
end;

procedure TGrowItem.SetGrowthRate(const Value: real);
begin
  FGrowthControls.GrowthRate := Value;
end;

procedure TGrowItem.SetLayerCollection(const Value: TLayerCollection);
begin
  FGrowthControls.LayerCollection := Value;
end;

function TGrowItem.StoreLayerCollection: boolean;
begin
  result := True;
end;

{ TConduitLayerItem }

procedure TConduitLayerItem.Assign(Source: TPersistent);
var
  SourceItem: TConduitLayerItem;
begin
  if Source is TConduitLayerItem then
  begin
    SourceItem := TConduitLayerItem(Source);
    IsConduitLayer := SourceItem.IsConduitLayer;
    Void := SourceItem.Void;
    LowerCriticalReynoldsNumber := SourceItem.LowerCriticalReynoldsNumber;
    HigherCriticalReynoldsNumber := SourceItem.HigherCriticalReynoldsNumber;
  end;
  inherited;
end;

constructor TConduitLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredLowerCriticalReynoldsNumber := TRealStorage.Create;
  FStoredHigherCriticalReynoldsNumber := TRealStorage.Create;
  FStoredVoid := TRealStorage.Create;
  LowerCriticalReynoldsNumber := 2000;
  HigherCriticalReynoldsNumber := 4000;
  Void := 0.25;

  FStoredLowerCriticalReynoldsNumber.OnChange := ValueChanged;
  FStoredHigherCriticalReynoldsNumber.OnChange := ValueChanged;
  FStoredVoid.OnChange := ValueChanged;
end;

destructor TConduitLayerItem.Destroy;
begin
  FStoredVoid.Free;
  FStoredHigherCriticalReynoldsNumber.Free;
  FStoredLowerCriticalReynoldsNumber.Free;
  inherited;
end;

function TConduitLayerItem.GetHigherCriticalReynoldsNumber: Double;
begin
  result := StoredHigherCriticalReynoldsNumber.Value;
end;

function TConduitLayerItem.GetLowerCriticalReynoldsNumber: Double;
begin
  result := StoredLowerCriticalReynoldsNumber.Value;
end;

function TConduitLayerItem.GetVoid: Double;
begin
  result := StoredVoid.Value;
end;

function TConduitLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SourceItem: TConduitLayerItem;
begin
  result := AnotherItem is TConduitLayerItem;
  if result then
  begin
    SourceItem := TConduitLayerItem(AnotherItem);
    result := (IsConduitLayer = SourceItem.IsConduitLayer)
      and (Void = SourceItem.Void)
      and (LowerCriticalReynoldsNumber = SourceItem.LowerCriticalReynoldsNumber)
      and (HigherCriticalReynoldsNumber = SourceItem.HigherCriticalReynoldsNumber);
  end;
end;

procedure TConduitLayerItem.SetHigherCriticalReynoldsNumber(
  const Value: Double);
begin
  StoredHigherCriticalReynoldsNumber.Value := Value;
end;

procedure TConduitLayerItem.SetIsConduitLayer(const Value: boolean);
begin
  SetBooleanProperty(FIsConduitLayer, Value);
end;

procedure TConduitLayerItem.SetLowerCriticalReynoldsNumber(const Value: Double);
begin
  StoredLowerCriticalReynoldsNumber.Value := Value;
end;

procedure TConduitLayerItem.SetStoredHigherCriticalReynoldsNumber(
  const Value: TRealStorage);
begin
  FStoredHigherCriticalReynoldsNumber.Assign(Value);
end;

procedure TConduitLayerItem.SetStoredLowerCriticalReynoldsNumber(
  const Value: TRealStorage);
begin
  FStoredLowerCriticalReynoldsNumber.Assign(Value);
end;

procedure TConduitLayerItem.SetStoredVoid(const Value: TRealStorage);
begin
  FStoredVoid.Assign(Value);
end;

procedure TConduitLayerItem.SetVoid(const Value: Double);
begin
  StoredVoid.Value := Value;
end;

procedure TConduitLayerItem.ValueChanged(Sender: TObject);
begin
  InvalidateModel
end;

{ TConduitLayerCollection }

constructor TConduitLayerCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TConduitLayerItem, Model);
end;

function TConduitLayerCollection.GetItem(index: Integer): TConduitLayerItem;
begin
  result := inherited Items[index] as TConduitLayerItem;
end;

procedure TConduitLayerCollection.SetItem(index: Integer;
  const Value: TConduitLayerItem);
begin
  inherited Items[index] := Value;
end;

end.
