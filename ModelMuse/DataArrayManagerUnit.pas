unit DataArrayManagerUnit;

interface

uses
  DataSetUnit, GoPhastTypes,
  SutraMeshUnit, System.Classes, CrossSectionUnit, System.Contnrs, RbwParser,
  HashTableFacadeUnit, PhastDataSets, System.SysUtils, AbstractGridUnit,
  System.Math, ModflowPackagesUnit, Mt3dmsChemSpeciesUnit, ModelMuseInterfaceUnit;

type
  ICustomModelForDataArrayManager = interface(IModelMuseModel)
    function GetThreeDDataSet: TDataArray;
    function GetFrontDataSet: TDataArray;
    function GetSideDataSet: TDataArray;
    function GetTopDataSet: TDataArray;
    function GetFrontContourDataSet: TDataArray;
    function GetSideContourDataSet: TDataArray;
    function GetThreeDContourDataSet: TDataArray;
    function GetTopContourDataSet: TDataArray;
    function GetModflowPackages: TModflowPackages;
    function GetMobileComponents: TMobileChemSpeciesCollection;
    function GetSutraMesh: TSutraMesh3D;
    function GetOnActiveDataSetChanged: TNotifyEvent;
    function GetOnNodeActiveDataSetChanged: TNotifyEvent;
    function GetZetaUsed: TObjectUsedEvent;
    function GetSftUsed: TObjectUsedEvent;
    function GetGwtUztUsed: TObjectUsedEvent;
    function GetActiveUsed: TObjectUsedEvent;
    function GetInitializeActiveDataArrayWithCellSizeObjects: TNotifyEvent;
    function GetDoShouldActiveBeSetByCellSize: TCheckUsageEvent;
    function GetAquiferPropertiesUsed: TObjectUsedEvent;
    function GetKyUsed: TObjectUsedEvent;
    function GetDetermineKyFromAnisotropy: TNotifyEvent;
    function GetShouldKyBeDeterminedFromAnisotropy: TCheckUsageEvent;
    function GetKzUsed: TObjectUsedEvent;
    function GetDetermineKzFromAnisotropy: TNotifyEvent;
    function GetShouldKzBeDeterminedFromAnisotropy: TCheckUsageEvent;
    function GetPorosityUsed: TObjectUsedEvent;

    procedure SetFrontDataSet(const Value: TDataArray);
    procedure SetSideDataSet(const Value: TDataArray);
    procedure SetTopDataSet(const Value: TDataArray);
    procedure SetThreeDDataSet(const Value: TDataArray);
    procedure SetThreeDContourDataSet(const Value: TDataArray);
    procedure SetTopContourDataSet(const Value: TDataArray);
    procedure SetFrontContourDataSet(const Value: TDataArray);
    procedure SetSideContourDataSet(const Value: TDataArray);
    procedure SetModflowPackages(const Value: TModflowPackages);
    procedure SetMobileComponents(const Value: TMobileChemSpeciesCollection);
    procedure SetSutraMesh(const Value: TSutraMesh3D);

    procedure CreateVariables(const DataSet: TDataArray);
    procedure UpdateDataArrayDimensions(DataArray: TDataArray);
    function DoSwiUsed(Sender: TObject): boolean;
    function GetSwiUsed: TObjectUsedEvent;
    property SwiUsed: TObjectUsedEvent read GetSwiUsed;
    function NumberOfMt3dChemComponents: integer;
    procedure DoOnActiveDataSetChanged(Sender: TObject);

    property ThreeDDataSet: TDataArray read GetThreeDDataSet
      write SetThreeDDataSet;
    property TopDataSet: TDataArray read GetTopDataSet
      write SetTopDataSet;
    property FrontDataSet: TDataArray read GetFrontDataSet
      write SetFrontDataSet;
    property SideDataSet: TDataArray read GetSideDataSet
      write SetSideDataSet;
    property ThreeDContourDataSet: TDataArray read GetThreeDContourDataSet
      write SetThreeDContourDataSet;
    property TopContourDataSet: TDataArray read GetTopContourDataSet
      write SetTopContourDataSet;
    property FrontContourDataSet: TDataArray read GetFrontContourDataSet
      write SetFrontContourDataSet;
    property SideContourDataSet: TDataArray read GetSideContourDataSet
      write SetSideContourDataSet;
    property ModflowPackages: TModflowPackages read GetModflowPackages
      write SetModflowPackages;
    property MobileComponents: TMobileChemSpeciesCollection
      read GetMobileComponents write SetMobileComponents;
    property SutraMesh: TSutraMesh3D read GetSutraMesh write SetSutraMesh;
    property OnActiveDataSetChanged: TNotifyEvent read GetOnActiveDataSetChanged;
    property OnNodeActiveDataSetChanged: TNotifyEvent read GetOnNodeActiveDataSetChanged;
    property ZetaUsed: TObjectUsedEvent read GetZetaUsed;
    property SftUsed: TObjectUsedEvent read GetSftUsed;
    property GwtUztUsed: TObjectUsedEvent read GetGwtUztUsed;
    property ActiveUsed: TObjectUsedEvent read GetActiveUsed;
    property InitializeActiveDataArrayWithCellSizeObjects: TNotifyEvent
      read GetInitializeActiveDataArrayWithCellSizeObjects;
    property ShouldActiveBeSetByCellSize: TCheckUsageEvent
      read GetDoShouldActiveBeSetByCellSize;
    property AquiferPropertiesUsed: TObjectUsedEvent read GetAquiferPropertiesUsed;
    property KyUsed: TObjectUsedEvent read GetKyUsed;
    property DetermineKyFromAnisotropy: TNotifyEvent read GetDetermineKyFromAnisotropy;
    property ShouldKyBeDeterminedFromAnisotropy: TCheckUsageEvent read GetShouldKyBeDeterminedFromAnisotropy;
    property KzUsed: TObjectUsedEvent read GetKzUsed;
    property DetermineKzFromAnisotropy: TNotifyEvent read GetDetermineKzFromAnisotropy;
    property ShouldKzBeDeterminedFromAnisotropy: TCheckUsageEvent read GetShouldKzBeDeterminedFromAnisotropy;
    property PorosityUsed:  TObjectUsedEvent read GetPorosityUsed;
    function GetSpecificStorageUsed: TObjectUsedEvent;
    property SpecificStorageUsed: TObjectUsedEvent read GetSpecificStorageUsed;
    function GetLongitudinalDispersionUsed: TObjectUsedEvent;
    property LongitudinalDispersionUsed: TObjectUsedEvent read GetLongitudinalDispersionUsed;
    function GetHorizontalTransverseDispersionUsed: TObjectUsedEvent;
    property HorizontalTransverseDispersionUsed: TObjectUsedEvent read GetHorizontalTransverseDispersionUsed;
    function GetVerticalTransverseDispersionUsed: TObjectUsedEvent;
    property VerticalTransverseDispersionUsed: TObjectUsedEvent read GetVerticalTransverseDispersionUsed;
    function GetInitialHeadUsed: TObjectUsedEvent;
    property InitialHeadUsed: TObjectUsedEvent read GetInitialHeadUsed;
    function GetInitialWaterTableUsed: TObjectUsedEvent;
    property InitialWaterTableUsed: TObjectUsedEvent read GetInitialWaterTableUsed;
    function GetChemistryUsed: TObjectUsedEvent;
    property ChemistryUsed: TObjectUsedEvent read GetChemistryUsed;
    function GetEquilibriumPhasesUsed: TObjectUsedEvent;
    property EquilibriumPhasesUsed: TObjectUsedEvent read GetEquilibriumPhasesUsed;
    function GetSurfacesUsed: TObjectUsedEvent;
    property SurfacesUsed: TObjectUsedEvent read GetSurfacesUsed;
    function GetExchangeUsed: TObjectUsedEvent;
    property ExchangeUsed: TObjectUsedEvent read GetExchangeUsed;
    function GetGasPhaseUsed: TObjectUsedEvent;
    property GasPhaseUsed: TObjectUsedEvent read GetGasPhaseUsed;
    function GetSolidSolutionUsed: TObjectUsedEvent;
    property SolidSolutionUsed: TObjectUsedEvent read GetSolidSolutionUsed;
    function GetReservoirLayerUsed: TObjectUsedEvent;
    property ReservoirLayerUsed: TObjectUsedEvent read GetReservoirLayerUsed;
    function GetReservoirPackageUsed: TObjectUsedEvent;
    property ReservoirPackageUsed: TObjectUsedEvent read GetReservoirPackageUsed;
    function GetKineticsUsed: TObjectUsedEvent;
    property KineticsUsed: TObjectUsedEvent read GetKineticsUsed;
    function GetLakePackageUsed: TObjectUsedEvent;
    property LakePackageUsed: TObjectUsedEvent read GetLakePackageUsed;
    function GetGroundSurfaceUsed: TObjectUsedEvent;
    property GroundSurfaceUsed: TObjectUsedEvent read GetGroundSurfaceUsed;
    function GetModflowUsed: TObjectUsedEvent;
    property ModflowUsed: TObjectUsedEvent read GetModflowUsed;
    function GetUzfPackageUsed: TObjectUsedEvent;
    property UzfPackageUsed: TObjectUsedEvent read GetUzfPackageUsed;
    function GetRouteUzfDischarge: TObjectUsedEvent;
    property RouteUzfDischarge: TObjectUsedEvent read GetRouteUzfDischarge;
    function GetUzfUnsatVertKUsed: TObjectUsedEvent;
    property UzfUnsatVertKUsed: TObjectUsedEvent read GetUzfUnsatVertKUsed;
    function GetUzfInitialInfiltrationUsed: TObjectUsedEvent;
    property UzfInitialInfiltrationUsed: TObjectUsedEvent read GetUzfInitialInfiltrationUsed;
    function GetUzfResidualWaterContentUsed: TObjectUsedEvent;
    property UzfResidualWaterContentUsed: TObjectUsedEvent read GetUzfResidualWaterContentUsed;
    function GetUzfSurfKUsed: TObjectUsedEvent;
    property UzfSurfKUsed: TObjectUsedEvent read GetUzfSurfKUsed;
    function GetModflowInitialHeadUsed: TObjectUsedEvent;
    property ModflowInitialHeadUsed: TObjectUsedEvent read GetModflowInitialHeadUsed;
    function GetConfiningBedKzUsed: TObjectUsedEvent;
    property ConfiningBedKzUsed: TObjectUsedEvent read GetConfiningBedKzUsed;
    function GetVerticalAnisotropyUsed: TObjectUsedEvent;
    property VerticalAnisotropyUsed: TObjectUsedEvent read GetVerticalAnisotropyUsed;
    function GetHorizontalAnisotropyUsed: TObjectUsedEvent;
    property HorizontalAnisotropyUsed: TObjectUsedEvent read GetHorizontalAnisotropyUsed;
    function GetSpecificYieldUsed: TObjectUsedEvent;
    property SpecificYieldUsed: TObjectUsedEvent read GetSpecificYieldUsed;
    function GetWetDryUsed: TObjectUsedEvent;
    property WetDryUsed: TObjectUsedEvent read GetWetDryUsed;
    function GetModpathZonesNeeded: TObjectUsedEvent;
    property ModpathZonesNeeded: TObjectUsedEvent read GetModpathZonesNeeded;
    function GetHufReferenceSurfaceNeeded: TObjectUsedEvent;
    property HufReferenceSurfaceNeeded: TObjectUsedEvent read GetHufReferenceSurfaceNeeded;
    function GetBcfUsed: TObjectUsedEvent;
    property BcfUsed: TObjectUsedEvent read GetBcfUsed;
    function GetConfinedStorageCoefUsed: TObjectUsedEvent;
    property ConfinedStorageCoefUsed: TObjectUsedEvent read GetConfinedStorageCoefUsed;
    function GetHufSelected: TObjectUsedEvent;
    property HufSelected: TObjectUsedEvent read GetHufSelected;
    function GetOptionalDataSet: TObjectUsedEvent;
    property OptionalDataSet: TObjectUsedEvent read GetOptionalDataSet;
    function GetHufStorageUsed: TObjectUsedEvent;
    property HufStorageUsed: TObjectUsedEvent read GetHufStorageUsed;
    function GetZoneBudgetSelected: TObjectUsedEvent;
    property ZoneBudgetSelected: TObjectUsedEvent read GetZoneBudgetSelected;
    function GetSwtSelected: TObjectUsedEvent;
    property SwtSelected: TObjectUsedEvent read GetSwtSelected;
    function GetSwtOffsetsUsed: TObjectUsedEvent;
    property SwtOffsetsUsed: TObjectUsedEvent read GetSwtOffsetsUsed;
    function GetSwtSpecifiedUsed: TObjectUsedEvent;
    property SwtSpecifiedUsed: TObjectUsedEvent read GetSwtSpecifiedUsed;
    function GetMt3dMS_StrictUsed: TObjectUsedEvent;
    property Mt3dMS_StrictUsed: TObjectUsedEvent read GetMt3dMS_StrictUsed;
    function GetMt3dMSBulkDensityUsed: TObjectUsedEvent;
    property Mt3dMSBulkDensityUsed: TObjectUsedEvent read GetMt3dMSBulkDensityUsed;
    function GetMt3dMSImmobPorosityUsed: TObjectUsedEvent;
    property Mt3dMSImmobPorosityUsed: TObjectUsedEvent read GetMt3dMSImmobPorosityUsed;
    function GetModpathBudgetNeeded: TObjectUsedEvent;
    property ModpathBudgetNeeded: TObjectUsedEvent read GetModpathBudgetNeeded;
    function GetModpathRetardationNeeded: TObjectUsedEvent;
    property ModpathRetardationNeeded: TObjectUsedEvent read GetModpathRetardationNeeded;
    function GetSutraUsed: TObjectUsedEvent;
    property SutraUsed: TObjectUsedEvent read GetSutraUsed;
    function GetSutraThicknessUsed: TObjectUsedEvent;
    property SutraThicknessUsed: TObjectUsedEvent read GetSutraThicknessUsed;
    function DoSutraUnsatRegionUsed(Sender: TObject): boolean;
    function GetSutraUnsatRegionUsed: TObjectUsedEvent;
    property SutraUnsatRegionUsed: TObjectUsedEvent read GetSutraUnsatRegionUsed;
    function GetSutraPermeabilityUsed: TObjectUsedEvent;
    property SutraPermeabilityUsed: TObjectUsedEvent read GetSutraPermeabilityUsed;
    function GetSutraMiddlePermeabilityUsed: TObjectUsedEvent;
    property SutraMiddlePermeabilityUsed: TObjectUsedEvent read GetSutraMiddlePermeabilityUsed;
    function GetSutraHydraulicConductivityUsed: TObjectUsedEvent;
    property SutraHydraulicConductivityUsed: TObjectUsedEvent read GetSutraHydraulicConductivityUsed;
    function GetSutraMiddleHydraulicConductivityUsed: TObjectUsedEvent;
    property SutraMiddleHydraulicConductivityUsed: TObjectUsedEvent read GetSutraMiddleHydraulicConductivityUsed;
    function GetSutra3DModel: TObjectUsedEvent;
    property Sutra3DModel: TObjectUsedEvent read GetSutra3DModel;
    function GetSutraConcentrationUsed: TObjectUsedEvent;
    property SutraConcentrationUsed: TObjectUsedEvent read GetSutraConcentrationUsed;
    function GetSutraTemperatureUsed: TObjectUsedEvent;
    property SutraTemperatureUsed: TObjectUsedEvent read GetSutraTemperatureUsed;
    function GetSoilIDUsed: TObjectUsedEvent;
    property SoilIDUsed: TObjectUsedEvent read GetSoilIDUsed;
    function GetCfpPipesSelected: TObjectUsedEvent;
    property CfpPipesSelected: TObjectUsedEvent read GetCfpPipesSelected;
    function GetSwiObsUsed: TObjectUsedEvent;
    property SwiObsUsed: TObjectUsedEvent read GetSwiObsUsed;
    function GetSwrSelected: TObjectUsedEvent;
    property SwrSelected: TObjectUsedEvent read GetSwrSelected;
    function GetFootprintSelected: TObjectUsedEvent;
    property FootprintSelected: TObjectUsedEvent read GetFootprintSelected;
    function GetModflow6Selected: TObjectUsedEvent;
    property Modflow6Selected: TObjectUsedEvent read GetModflow6Selected;
    function GetStorageSelected: TObjectUsedEvent;
    property StorageSelected: TObjectUsedEvent read GetStorageSelected;
    function GetSutraLakeUsed: TObjectUsedEvent;
    property SutraLakeUsed: TObjectUsedEvent read GetSutraLakeUsed;
    function GetAssignFootprintBoundarydWithdrawal: TNotifyEvent;
    property AssignFootprintBoundarydWithdrawal: TNotifyEvent read GetAssignFootprintBoundarydWithdrawal;
    function GetUseFootprintWells: TCheckUsageEvent;
    property UseFootprintWells: TCheckUsageEvent read GetUseFootprintWells;
    function GetNpfUsed: TObjectUsedEvent;
    property NpfUsed: TObjectUsedEvent read GetNpfUsed;
    function GetSfrMf6Selected: TObjectUsedEvent;
    property SfrMf6Selected: TObjectUsedEvent read GetSfrMf6Selected;
    function GetMawSelected: TObjectUsedEvent;
    property MawSelected: TObjectUsedEvent read GetMawSelected;
    function GetLakMf6Selected: TObjectUsedEvent;
    property LakMf6Selected: TObjectUsedEvent read GetLakMf6Selected;
    function GetUzfMf6PackageUsed: TObjectUsedEvent;
    property UzfMf6PackageUsed: TObjectUsedEvent read GetUzfMf6PackageUsed;
    function GetSutraLakeBottomUsed: TObjectUsedEvent;
    property SutraLakeBottomUsed: TObjectUsedEvent read GetSutraLakeBottomUsed;
    function GetHorizAnisotropyMf6Used: TObjectUsedEvent;
    property HorizAnisotropyMf6Used: TObjectUsedEvent read GetHorizAnisotropyMf6Used;
    function GetVertAnisotropyMf6Used: TObjectUsedEvent;
    property VertAnisotropyMf6Used: TObjectUsedEvent read GetVertAnisotropyMf6Used;
    function GetCSubInitialElasticStorageUsed: TObjectUsedEvent;
    property CSubInitialElasticStorageUsed: TObjectUsedEvent read GetCSubInitialElasticStorageUsed;
    function GetCSubInitialRecompressionIndexUsed: TObjectUsedEvent;
    property CSubInitialRecompressionIndexUsed: TObjectUsedEvent read GetCSubInitialRecompressionIndexUsed;
    function GetCSubDataSetsUsed: TObjectUsedEvent;
    property CSubDataSetsUsed: TObjectUsedEvent read GetCSubDataSetsUsed;
    function GetMt3d_LktIsSelected: TObjectUsedEvent;
    property Mt3d_LktIsSelected: TObjectUsedEvent read GetMt3d_LktIsSelected;
    function GetSutra4Used: TObjectUsedEvent;
    property Sutra4Used: TObjectUsedEvent read GetSutra4Used;
    function GetSutra4EnergyUsed: TObjectUsedEvent;
    property Sutra4EnergyUsed: TObjectUsedEvent read GetSutra4EnergyUsed;
    function GetSutra4EnergyOrSorptionUsed: TObjectUsedEvent;
    property Sutra4EnergyOrSorptionUsed: TObjectUsedEvent read GetSutra4EnergyOrSorptionUsed;
    function GetSutra4ProductionUsed: TObjectUsedEvent;
    property Sutra4ProductionUsed: TObjectUsedEvent read GetSutra4ProductionUsed;
    function GetSutra4FreezingUsed: TObjectUsedEvent;
    property Sutra4FreezingUsed: TObjectUsedEvent read GetSutra4FreezingUsed;
    function GetGwtDispUsed: TObjectUsedEvent;
    property GwtDispUsed: TObjectUsedEvent read GetGwtDispUsed;
    function GetSeparatedLongitudinalDispersionUsed: TObjectUsedEvent;
    property SeparatedLongitudinalDispersionUsed: TObjectUsedEvent read GetSeparatedLongitudinalDispersionUsed;
    function GetSeparatedHorizontalTransverseDispersionUsed: TObjectUsedEvent;
    property SeparatedHorizontalTransverseDispersionUsed: TObjectUsedEvent
       read GetSeparatedHorizontalTransverseDispersionUsed;
    function GetFarmProcess4SteadyFarmsUsed: TObjectUsedEvent;
    property FarmProcess4SteadyFarmsUsed: TObjectUsedEvent read GetFarmProcess4SteadyFarmsUsed;
    function GetAssignModflow6LakeDisplayArrays: TNotifyEvent;
    property AssignModflow6LakeDisplayArrays: TNotifyEvent read GetAssignModflow6LakeDisplayArrays;
    function GetSutra4SoluteUsed: TObjectUsedEvent;
    property Sutra4SoluteUsed: TObjectUsedEvent read GetSutra4SoluteUsed;
    function GetFarmProcess4SteadyCropsUsed: TObjectUsedEvent;
    property FarmProcess4SteadyCropsUsed: TObjectUsedEvent read GetFarmProcess4SteadyCropsUsed;
    function GetFarmProcess4SteadyRefETUsed: TObjectUsedEvent;
    property FarmProcess4SteadyRefETUsed: TObjectUsedEvent read GetFarmProcess4SteadyRefETUsed;
    function GetFarmProcess4SteadyPrecipUsed: TObjectUsedEvent;
    property FarmProcess4SteadyPrecipUsed: TObjectUsedEvent read GetFarmProcess4SteadyPrecipUsed;
    function GetFarmProcess4SteadArrayEfficiencyUsed: TObjectUsedEvent;
    property FarmProcess4SteadArrayEfficiencyUsed: TObjectUsedEvent read GetFarmProcess4SteadArrayEfficiencyUsed;
    function GetFarmProcess4SteadArrayEfficiencyImprovementUsed: TObjectUsedEvent;
    property FarmProcess4SteadArrayEfficiencyImprovementUsed: TObjectUsedEvent
      read GetFarmProcess4SteadArrayEfficiencyImprovementUsed;
    function GetFarmProcess4SteadArrayBareRunoffFractionUsed: TObjectUsedEvent;
    property FarmProcess4SteadArrayBareRunoffFractionUsed: TObjectUsedEvent
      read GetFarmProcess4SteadArrayBareRunoffFractionUsed;
    function GetFarmProcess4SteadArrayBarePrecipitationConsumptionFractionUsed: TObjectUsedEvent;
    property FarmProcess4SteadArrayBarePrecipitationConsumptionFractionUsed: TObjectUsedEvent
      read GetFarmProcess4SteadArrayBarePrecipitationConsumptionFractionUsed;
    function GetFarmProcess4SteadArrayAddedDemandRunoffSplitUsed: TObjectUsedEvent;
    property FarmProcess4SteadArrayAddedDemandRunoffSplitUsed: TObjectUsedEvent
      read GetFarmProcess4SteadArrayAddedDemandRunoffSplitUsed;
    function GetCapillaryFringeUsed: TObjectUsedEvent;
    property CapillaryFringeUsed: TObjectUsedEvent read GetCapillaryFringeUsed;
    function GetSurfaceKUsed: TObjectUsedEvent;
    property SurfaceKUsed: TObjectUsedEvent read GetSurfaceKUsed;
    function GetPotentialEvapBareUsed: TObjectUsedEvent;
    property PotentialEvapBareUsed: TObjectUsedEvent read GetPotentialEvapBareUsed;
    function GetDirectRechargeUsed: TObjectUsedEvent;
    property DirectRechargeUsed: TObjectUsedEvent read GetDirectRechargeUsed;
    function GetPrecipPotConsumptionUsed: TObjectUsedEvent;
    property PrecipPotConsumptionUsed: TObjectUsedEvent read GetPrecipPotConsumptionUsed;
    function GetNrdInfilLocationUsed: TObjectUsedEvent;
    property NrdInfilLocationUsed: TObjectUsedEvent read GetNrdInfilLocationUsed;
    function GetCropCoefficientUsed: TObjectUsedEvent;
    property CropCoefficientUsed: TObjectUsedEvent read GetCropCoefficientUsed;
    function GetLandUseAreaFractionUsed: TObjectUsedEvent;
    property LandUseAreaFractionUsed: TObjectUsedEvent read GetLandUseAreaFractionUsed;
    function GetConsumptiveUseUsed: TObjectUsedEvent;
    property ConsumptiveUseUsed: TObjectUsedEvent read GetConsumptiveUseUsed;
    function GetIrrigationUsed: TObjectUsedEvent;
    property IrrigationUsed: TObjectUsedEvent read GetIrrigationUsed;
    function GetRootDepthUsed: TObjectUsedEvent;
    property RootDepthUsed: TObjectUsedEvent read GetRootDepthUsed;
    function GetGwRootInteractionUsed: TObjectUsedEvent;
    property GwRootInteractionUsed: TObjectUsedEvent read GetGwRootInteractionUsed;
    function GetTranspirationFractionUsed: TObjectUsedEvent;
    property TranspirationFractionUsed: TObjectUsedEvent read GetTranspirationFractionUsed;
    function GetEvaporationIrrigationFractionUsed: TObjectUsedEvent;
    property EvaporationIrrigationFractionUsed: TObjectUsedEvent read GetEvaporationIrrigationFractionUsed;
    function GetFractionOfPrecipToSurfaceWaterUsed: TObjectUsedEvent;
    property FractionOfPrecipToSurfaceWaterUsed: TObjectUsedEvent read GetFractionOfPrecipToSurfaceWaterUsed;
    function GetFractionOfIrrigToSurfaceWaterUsed: TObjectUsedEvent;
    property FractionOfIrrigToSurfaceWaterUsed: TObjectUsedEvent read GetFractionOfIrrigToSurfaceWaterUsed;
    function GetAddedDemandUsed: TObjectUsedEvent;
    property AddedDemandUsed: TObjectUsedEvent read GetAddedDemandUsed;
    function GetCropHasSalinityDemandUsed: TObjectUsedEvent;
    property CropHasSalinityDemandUsed: TObjectUsedEvent read GetCropHasSalinityDemandUsed;
    function GetLandUseCellsToPrintUsed: TObjectUsedEvent;
    property LandUseCellsToPrintUsed: TObjectUsedEvent read GetLandUseCellsToPrintUsed;
    procedure RemoveVariables(const DataSet: TDataArray); overload;
    procedure RemoveVariables(const DataSetName: String;
      Orientation: TDataSetOrientation; EvaluatedAt: TEvaluatedAt); overload;
    function GetCrossSection: TCrossSection;
    procedure SetCrossSection(const Value: TCrossSection);
    property CrossSection: TCrossSection read GetCrossSection write SetCrossSection;
    function GetGrid: TCustomModelGrid;
    property Grid: TCustomModelGrid read GetGrid;
  end;


  TDataSetCreationData = record
    DataSetType: TDataArrayType;
    Orientation: TDataSetOrientation;
    DataType: TRbwDataType;
    Name: string;
    DisplayName: string;
    Formula: string;
    Classification: String;
    DataSetNeeded: TObjectUsedEvent;
    DataSetShouldBeCreated: TObjectUsedEvent;
    Lock: TDataLock;
    CheckMax: boolean;
    CheckMin: boolean;
    Max: double;
    Min: double;
    EvaluatedAt: TEvaluatedAt;
    AssociatedDataSets: string;
    AngleType: TAngleType;
    Visible: boolean;
    OnInitialize: TNotifyEvent;
    OnShouldUseOnInitialize: TCheckUsageEvent;
  end;


  {
    @name manages the creation of @link(TDataArray)s. The @link(TDataArray)s
    are defined in @link(DefinePackageDataArrays) and actually created in
    @link(CreateInitialDataSets).
  }
  TDataArrayManager = class(TObject, ISimpleDataArrayManager)
  private
    FCustomModel: ICustomModelForDataArrayManager;
    // @name is used to store the @link(TDataArray)s in the model that
    // are defined throughout the grid.  An example is the data set for
    // the hydraulic conductivity in the X direction.
    FDataSets: TObjectList;
    FDataSetsToCache: TList;
    // @name is used to store @link(TDataArray)s that are related to
    // boundary conditions but which do not vary with time.
    FBoundaryDataSets: TObjectList;
    FDataSetLookUpList: THashTableFacade;
    FRiverDataSets: TList;
    FStoreCachedData: boolean;
    // @name is used to store @link(TDataArray)s that have been deleted
    // so that they can be restored later.
    FDeletedDataSets: TList;
    FLongitudinalDispersivityIndex: Integer;
    FPorosityIndex: Integer;
    FDataSetNames: TStringList;
    FTransverseDispersivityIndex: Integer;
    // See @link(DataSetCount).
    function GetDataSetCount: integer;
    // See @link(DataSets).
    function GetDataSet(const Index: integer): TDataArray;
    // See @link(BoundaryDataSetCount).
    function GetBoundaryDataSetCount: integer;
    // See @link(BoundaryDataSets).
    function GetBoundaryDataSets(const Index: integer): TDataArray;
    // @name returns the position of the @link(TDataArray) whose name is
    // DataSetName in List.
    // @name is used in @link(IndexOfBoundaryDataSet)
    // and @link(IndexOfDataSet).
    function IndexOfDataSetInList(DataSetName: string;
      const List: TObjectList): integer;
    function GetDataSetsCapacity: integer;
    procedure SetDataSetsCapacity(const Value: integer);
    procedure Invalidate;
    function GetChildDataArrayManager(Index: integer): TDataArrayManager;
    function GetChildDataArrayManagerCount: integer;
    function LocalCount: integer;
    function GetDataSetNames: TStringList;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  public
    FDataArrayCreationRecords: array of TDataSetCreationData;
    FZetaDataDefinition: TDataSetCreationData;
    FSftInitConc: TDataSetCreationData;
    FSftDispersion: TDataSetCreationData;
    FUztInitConc: TDataSetCreationData;
    procedure Assign(Source: TDataArrayManager);
    procedure AddDataSetToLookUpList(const DataSet: TDataArray);
    Constructor Create(Model: ICustomModelForDataArrayManager);
    Destructor Destroy; override;
    procedure ClearDataSetsToCache;
    procedure ClearAllDataSets;
    // @name holds all the @link(TDataArray)s that are related to the river
    // boundary but which are not transient.
    property RiverDataSets: TList read FRiverDataSets;
    property StoreCachedData: boolean read FStoreCachedData
      write FStoreCachedData;
    // @name indicates the number of @link(TDataArray)s in @name.
    property DataSetCount: integer read GetDataSetCount;
    // @name is used to access the @link(TDataArray)s that are defined
    // throughout the grid.
    property DataSets[const Index: integer]: TDataArray read GetDataSet; default;
    // @name is used to determine the number of @link(TDataArray)s in
    // @link(BoundaryDataSets).  Only data sets that don't vary with
    // time yet are related to boundary conditions are in
    // @link(BoundaryDataSets).
    property BoundaryDataSetCount: integer read GetBoundaryDataSetCount;
    // @name is used to access @link(TDataArray)s in
    // @link(FBoundaryDataSets).  Only data sets that don't vary with
    // time yet are related to boundary conditions are in
    // @link(FBoundaryDataSets).
    property BoundaryDataSets[const Index: integer]: TDataArray
      read GetBoundaryDataSets;
    property DataSetsCapacity: integer read GetDataSetsCapacity
      write SetDataSetsCapacity;
    procedure AddDataSetToCache(DataArray: TDataArray);
    procedure DontCache(DataArray: TDataArray);
    procedure CacheDataArrays;
    // @name creates a new @link(TDataArray) and adds it to @link(DataSets).
    function CreateNewDataArray(const ClassType: TDataArrayType;
      const Name, Formula, DisplayName: string; Lock: TDataLock;
      DataType: TRbwDataType;
      EvaluatedAt: TEvaluatedAt; Orientation: TDataSetOrientation;
      const Classification: string): TDataArray;
    // @name retrieves a @link(TDataArray) from
    // @link(DataSets) based on its name.
    function GetDataSetByName(const DataSetName: string): TDataArray;
    // @name is used to create data sets whenever the required data sets change.
    // @seealso(DefinePackageDataArrays)
    procedure CreateInitialDataSets;
    procedure RemoveDataSetFromLookUpList(const DataSet: TDataArray);
    // @name creates non-transient @link(TDataArray)s for boundary conditions.
    // @name is used to store DataSet in @link(FBoundaryDataSets).
    function AddBoundaryDataSet(const DataSet: TDataArray): Integer;
    // @name creates non-transient @link(TDataArray)s for boundary conditions.
    procedure CreateInitialBoundaryDataSets;
    // @name removes DataSet from @link(DataSets) without freeing it.
    procedure ExtractDataSet(const DataSet: TDataArray);
    // @name returns the position of the @link(TDataArray) in
    // @link(DataSets) whose Name is DataSetName. If none has that
    // name, @name returns -1.
    function IndexOfDataSet(DataSetName: string): integer;
    // @name returns the position of the @link(TDataArray) in
    // @link(BoundaryDataSets) whose Name is DataSetName. If none has that
    // name, @name returns -1.
    function IndexOfBoundaryDataSet(DataSetName: string): integer;
    procedure InvalidateAllDataSets;
    procedure InvalidateAll3DDataSets;
    procedure ClearDeletedDataSets;
    procedure UnlinkDeletedDataSets;
    procedure HandleAddedDataArrays(AddedDataSetList: TList);
    procedure HandleDeletedDataArrays(DeletedDataSetList: TList);
    property ChildDataArrayManagerCount: integer
      read GetChildDataArrayManagerCount;
    property ChildDataArrayManagers[Index: integer]: TDataArrayManager
      read GetChildDataArrayManager;
    procedure UpdateClassifications;
    procedure RemoveDataSet(ADataArray: TDataArray);
    procedure Loaded;
    property DataSetNames: TStringList read GetDataSetNames;
    procedure InvalidateHguFormulaDataSets;
    procedure AddNamesOfAllDataSets(Names: TStringList);
    // This should only be called by a TCustomModel;
    // @name adds DataSet to @link(FDataSets) and calls @link(AddDataSetToLookUpList);
    function AddDataSet(const DataSet: TDataArray): Integer;
    procedure InvalidateDataSetLookupList;
    function DataArrayHeld(DataArray: TDataArray): boolean;
    procedure UpdateDataSetDimensions;
    // @name defines the characteristics of the data sets that should be
    // created under various circumstances.
    // @seealso(CreateInitialDataSets)
    procedure DefinePackageDataArrays;
    procedure ClearPestArrayFileNames;
  end;

var
  ClearingDeletedDataSets: boolean = False;



implementation

uses
  PhastModelUnit, ModflowBoundaryDisplayUnit, DataSetNamesUnit, GIS_Functions;

{ TDataArrayManager }

function TDataArrayManager.AddBoundaryDataSet(
  const DataSet: TDataArray): Integer;
begin
  result := FBoundaryDataSets.Add(DataSet);
  Invalidate;
end;

function TDataArrayManager.AddDataSet(const DataSet: TDataArray): Integer;
begin
  result := FDataSets.Add(DataSet);
  Invalidate;
  AddDataSetToLookUpList(DataSet);
end;

procedure TDataArrayManager.AddDataSetToCache(DataArray: TDataArray);
var
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataArray: TDataArray;
begin
  if FDataSetsToCache.IndexOf(DataArray) < 0 then
  begin
    if (DataArray.Name <> '')
      and not (DataArray is TSparseArrayPhastInterpolationDataSet)
      and not (DataArray is TCustomSparseDataSet)
      then
    begin
      FDataSetsToCache.Add(DataArray);
      if FCustomModel is TPhastModel then
      begin
        LocalModel := TPhastModel(FCustomModel);
        for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
        begin
          ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
          if ChildModel <> nil then
          begin
            ChildDataArray := ChildModel.DataArrayManager.GetDataSetByName(DataArray.Name);
            if (ChildDataArray <> nil) then
            begin
              ChildModel.DataArrayManager.AddDataSetToCache(ChildDataArray);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TDataArrayManager.AddDataSetToLookUpList(const DataSet: TDataArray);
var
  Dummy: pointer;
begin
  if (FDataSetLookUpList <> nil) and (DataSet.Name <> '') then
  begin
    if not FDataSetLookUpList.Search(DataSet.Name, Dummy) then
    begin
      FDataSetLookUpList.Insert(DataSet.Name, DataSet);
    end;
  end;
end;

procedure TDataArrayManager.AddNamesOfAllDataSets(Names: TStringList);
var
  Index: Integer;
  DataSet: TDataArray;
begin
  for Index := 0 to DataSetCount - 1 do
  begin
    DataSet := DataSets[Index];
    Names.Add(DataSet.Name);
  end;

  // Don't allow the name to be the same as a deleted data set.
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
    Names.Add(DataSet.Name);
  end;
end;

procedure TDataArrayManager.Assign(Source: TDataArrayManager);
var
  Index: Integer;
  DataArray: TDataArray;
  SourceDataArray: TDataArray;
  DA_Index: Integer;
begin
  Assert(Source <> self);
  for Index := DataSetCount - 1 downto 0 do
  begin
    DataArray := DataSets[Index];
    if Source.IndexOfDataSet(DataArray.Name) < 0 then
    begin
      FDataSets.Delete(Index);
    end;
  end;
  for Index := BoundaryDataSetCount - 1 downto 0 do
  begin
    DataArray := BoundaryDataSets[Index];
    if Source.IndexOfBoundaryDataSet(DataArray.Name) < 0 then
    begin
      FBoundaryDataSets.Delete(Index);
    end;
  end;
  for Index := 0 to Source.DataSetCount - 1 do
  begin
    SourceDataArray := Source.DataSets[Index];
    DataArray := GetDataSetByName(SourceDataArray.Name);
    if DataArray = nil then
    begin
      DataArray := TDataArrayType(SourceDataArray.ClassType).Create(FCustomModel);
      AddDataSet(DataArray);
    end;
    DataArray.AssignProperties(SourceDataArray);
  end;
  for Index := 0 to Source.BoundaryDataSetCount - 1 do
  begin
    SourceDataArray := Source.BoundaryDataSets[Index];
    DA_Index := IndexOfBoundaryDataSet(SourceDataArray.Name);
    if DA_Index < 0 then
    begin
      DataArray := TDataArrayType(SourceDataArray.ClassType).Create(FCustomModel);
      AddBoundaryDataSet(DataArray);
    end
    else
    begin
      DataArray := BoundaryDataSets[DA_Index];
    end;
    DataArray.AssignProperties(SourceDataArray);
  end;
end;

procedure TDataArrayManager.CacheDataArrays;
var
  Index: Integer;
  DataArray: TDataArray;
  LocalModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  for Index := 0 to FDataSetsToCache.Count - 1 do
  begin
    DataArray := FDataSetsToCache[Index];
    if (FCustomModel.Grid = nil)
      or ((DataArray <> FCustomModel.TopDataSet)
      and (DataArray <> FCustomModel.FrontDataSet)
      and (DataArray <> FCustomModel.SideDataSet)
      and (DataArray <> FCustomModel.ThreeDDataSet)
      and (DataArray <> FCustomModel.Grid.TopContourDataSet)
      and (DataArray <> FCustomModel.Grid.FrontContourDataSet)
      and (DataArray <> FCustomModel.Grid.SideContourDataSet)
      and (DataArray <> FCustomModel.Grid.ThreeDContourDataSet)) then
    begin
      DataArray.CacheData;
    end;
  end;
  FDataSetsToCache.Clear;
  if FCustomModel is TPhastModel then
  begin
    LocalModel := TPhastModel(FCustomModel);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      ChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        ChildModel.DataArrayManager.CacheDataArrays;
      end;
    end;
  end;
end;

procedure TDataArrayManager.ClearAllDataSets;
begin
  FDataSetsToCache.Clear;
  FDataSets.Clear;
  FBoundaryDataSets.Clear;
end;

procedure TDataArrayManager.ClearDataSetsToCache;
begin
  FDataSetsToCache.Clear;
end;

procedure TDataArrayManager.ClearDeletedDataSets;
var
  Index: Integer;
begin
  ClearingDeletedDataSets := True;
  try
    FDeletedDataSets.Clear;
  finally
    ClearingDeletedDataSets := False;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].ClearDeletedDataSets;
  end;
end;

procedure TDataArrayManager.ClearPestArrayFileNames;
var
  DataArrayIndex: Integer;
  ADataArray: TDataArray;
begin
  for DataArrayIndex := 0 to DataSetCount - 1 do
  begin
    ADataArray := DataSets[DataArrayIndex];
    ADataArray.PestArrayFileNames.Clear;
  end;
end;

constructor TDataArrayManager.Create(Model: ICustomModelForDataArrayManager);
begin
  FCustomModel := Model;
  FRiverDataSets := TList.Create;
  FDataSetsToCache:= TList.Create;
  FDataSets := TObjectList.Create;
  FBoundaryDataSets := TObjectList.Create;
  FDeletedDataSets := TObjectList.Create;
  FLongitudinalDispersivityIndex := -1;
  FTransverseDispersivityIndex := -1;
  FPorosityIndex := -1;
end;

procedure TDataArrayManager.CreateInitialBoundaryDataSets;
var
  PhastDataSet: TDataArray;
begin
  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsTopLeakyHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsTopLeakyThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsFrontLeakyHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsFrontLeakyThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsSideLeakyHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsSideLeakyThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverHydraulicConductivity);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '100.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverWidth);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '10.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverDepth);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '10.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TRealSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsRiverBedThickness);
  PhastDataSet.DataType := rdtDouble;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dsoTop;
  PhastDataSet.Formula := '1.';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);
  RiverDataSets.Add(PhastDataSet);

  PhastDataSet := TIntegerSparseDataSet.Create(FCustomModel);
  PhastDataSet.Lock := [dcName, dcType, dcOrientation, dcEvaluatedAt];
  PhastDataSet.UpdateWithName(rsSolutionType);
  PhastDataSet.DataType := rdtInteger;
  PhastDataSet.EvaluatedAt := eaNodes;
  PhastDataSet.Orientation := dso3D;
  PhastDataSet.Formula := '0';
  PhastDataSet.CheckMin := True;
  PhastDataSet.Min := 0;
  AddBoundaryDataSet(PhastDataSet);

end;

procedure TDataArrayManager.CreateInitialDataSets;
var
  DataArray: TDataArray;
  Index: Integer;
  DataSetName: string;
  Orientation: TDataSetOrientation;
  DataType: TRbwDataType;
  ArrayNeeded: TObjectUsedEvent;
  ArrayArrayShouldBeCreated: TObjectUsedEvent;
  NewFormula, Classification: string;
  Lock: TDataLock;
  DisplayName: string;
  AngleType: TAngleType;
  NotifyEvent: TNotifyEvent;
  procedure HandleDataArray(
    const DataSetCreationData: TDataSetCreationData);
  begin
    DataArray := GetDataSetByName(DataSetName);
    Assert(Assigned(ArrayNeeded));
    if DataArray <> nil then
    begin
      DataArray.Name := DataSetName;
      DataArray.DisplayName := DisplayName;
      DataArray.Lock := Lock;
      DataArray.OnDataSetUsed := ArrayNeeded;
      FCustomModel.CreateVariables(DataArray);
      DataArray.AngleType := AngleType;
      DataArray.Classification := Classification;
      DataArray.Visible := DataSetCreationData.Visible;
      DataArray.OnInitialize := DataSetCreationData.OnInitialize;
      DataArray.OnShouldUseOnInitialize := DataSetCreationData.OnShouldUseOnInitialize;
      if ArrayNeeded(DataArray) then
      begin
        if DataArray.Orientation <> Orientation then
        begin
          DataArray.Orientation := Orientation
        end;
        if DataArray.EvaluatedAt <> DataSetCreationData.EvaluatedAt then
        begin
          DataArray.EvaluatedAt := DataSetCreationData.EvaluatedAt
        end;
        if DataArray.DataType <> DataType then
        begin
          DataArray.DataType := DataType
        end;
      end;
    end
    else if ArrayNeeded(DataArray)
      or (Assigned(ArrayArrayShouldBeCreated)
      and ArrayArrayShouldBeCreated(self)) then
    begin
      DataArray := CreateNewDataArray(
        DataSetCreationData.DataSetType, DataSetName, NewFormula,
        DisplayName,
        Lock, DataType, DataSetCreationData.EvaluatedAt,
        Orientation, Classification);
      DataArray.OnDataSetUsed := ArrayNeeded;
      DataArray.Lock := Lock;
      DataArray.CheckMax := DataSetCreationData.CheckMax;
      DataArray.CheckMin := DataSetCreationData.CheckMin;
      DataArray.Max := DataSetCreationData.Max;
      DataArray.Min := DataSetCreationData.Min;
      DataArray.DisplayName := DisplayName;
      DataArray.AngleType := AngleType;
      DataArray.Visible := DataSetCreationData.Visible;
      DataArray.OnInitialize := DataSetCreationData.OnInitialize;
      DataArray.OnShouldUseOnInitialize := DataSetCreationData.OnShouldUseOnInitialize;
    end;
    if DataArray <> nil then
    begin
      FCustomModel.UpdateDataArrayDimensions(DataArray);
      DataArray.AssociatedDataSets := DataSetCreationData.AssociatedDataSets;
      DataArray.Classification := DataSetCreationData.Classification;
    end;
  end;
begin
  // See DefinePackageDataArrays for the definition of the
  // contents of DataArrayCreationRecords.

  if FCustomModel.DoSwiUsed(nil) then
  begin
    for Index := 1 to FCustomModel.ModflowPackages.SwiPackage.NumberOfSurfaces do
    begin
      DataSetName := FZetaDataDefinition.Name + IntToStr(Index);
      DisplayName := FZetaDataDefinition.DisplayName + IntToStr(Index);
      Orientation := FZetaDataDefinition.Orientation;
      DataType := FZetaDataDefinition.DataType;
      ArrayNeeded := FZetaDataDefinition.DataSetNeeded;
      ArrayArrayShouldBeCreated :=
        FZetaDataDefinition.DataSetShouldBeCreated;
      NewFormula := FZetaDataDefinition.Formula;
      Classification := FZetaDataDefinition.Classification;
      Lock := FZetaDataDefinition.Lock;
      AngleType := FZetaDataDefinition.AngleType;
      HandleDataArray(FZetaDataDefinition);
    end;
  end;

  if FCustomModel.SftUsed(nil) then
  begin
    for Index := 1 to FCustomModel.NumberOfMt3dChemComponents do
    begin
      DataSetName := FSftInitConc.Name + IntToStr(Index);
      DisplayName := FSftInitConc.DisplayName + IntToStr(Index);
      Orientation := FSftInitConc.Orientation;
      DataType := FSftInitConc.DataType;
      ArrayNeeded := FSftInitConc.DataSetNeeded;
      ArrayArrayShouldBeCreated :=
        FSftInitConc.DataSetShouldBeCreated;
      NewFormula := FSftInitConc.Formula;
      Classification := FSftInitConc.Classification;
      Lock := FSftInitConc.Lock;
      AngleType := FSftInitConc.AngleType;
      HandleDataArray(FSftInitConc);

      DataSetName := FSftDispersion.Name + IntToStr(Index);
      DisplayName := FSftDispersion.DisplayName + IntToStr(Index);
      Orientation := FSftDispersion.Orientation;
      DataType := FSftDispersion.DataType;
      ArrayNeeded := FSftDispersion.DataSetNeeded;
      ArrayArrayShouldBeCreated :=
        FSftDispersion.DataSetShouldBeCreated;
      NewFormula := FSftDispersion.Formula;
      Classification := FSftDispersion.Classification;
      Lock := FSftDispersion.Lock;
      AngleType := FSftDispersion.AngleType;
      HandleDataArray(FSftDispersion);

    end;
  end;

  if FCustomModel.GwtUztUsed(nil) then
  begin
    for Index := 1 to FCustomModel.MobileComponents.Count do
    begin
      DataSetName := FUztInitConc.Name + IntToStr(Index);
      DisplayName := FUztInitConc.DisplayName + IntToStr(Index);
      Orientation := FUztInitConc.Orientation;
      DataType := FUztInitConc.DataType;
      ArrayNeeded := FUztInitConc.DataSetNeeded;
      ArrayArrayShouldBeCreated :=
        FUztInitConc.DataSetShouldBeCreated;
      NewFormula := FUztInitConc.Formula;
      Classification := FUztInitConc.Classification;
      Lock := FUztInitConc.Lock;
      AngleType := FUztInitConc.AngleType;
      HandleDataArray(FUztInitConc);
    end;
  end;

  for Index := 0 to Length(FDataArrayCreationRecords) - 1 do
  begin
    DataSetName := FDataArrayCreationRecords[Index].Name;
    DisplayName := FDataArrayCreationRecords[Index].DisplayName;
    Orientation := FDataArrayCreationRecords[Index].Orientation;
    DataType := FDataArrayCreationRecords[Index].DataType;
    ArrayNeeded := FDataArrayCreationRecords[Index].DataSetNeeded;
    ArrayArrayShouldBeCreated :=
      FDataArrayCreationRecords[Index].DataSetShouldBeCreated;
    NewFormula := FDataArrayCreationRecords[Index].Formula;
    Classification := FDataArrayCreationRecords[Index].Classification;
    Lock := FDataArrayCreationRecords[Index].Lock;
    AngleType := FDataArrayCreationRecords[Index].AngleType;

    HandleDataArray(FDataArrayCreationRecords[Index]);
//    DataArray := GetDataSetByName(DataSetName);
//    Assert(Assigned(ArrayNeeded));
//    if DataArray <> nil then
//    begin
//      DataArray.Name := DataSetName;
//      DataArray.DisplayName := DisplayName;
//      DataArray.Lock := Lock;
//      DataArray.OnDataSetUsed := ArrayNeeded;
//      FCustomModel.CreateVariables(DataArray);
//      DataArray.AngleType := AngleType;
//      DataArray.Classification := Classification;
//      DataArray.Visible := FDataArrayCreationRecords[Index].Visible;
//    end
//    else if ArrayNeeded(self)
//      or (Assigned(ArrayArrayShouldBeCreated)
//      and ArrayArrayShouldBeCreated(self)) then
//    begin
//      DataArray := CreateNewDataArray(
//        FDataArrayCreationRecords[Index].DataSetType, DataSetName, NewFormula,
//        DisplayName,
//        Lock, DataType, FDataArrayCreationRecords[Index].EvaluatedAt,
//        Orientation, Classification);
//      DataArray.OnDataSetUsed := ArrayNeeded;
//      DataArray.Lock := Lock;
//      DataArray.CheckMax := FDataArrayCreationRecords[Index].CheckMax;
//      DataArray.CheckMin := FDataArrayCreationRecords[Index].CheckMin;
//      DataArray.Max := FDataArrayCreationRecords[Index].Max;
//      DataArray.Min := FDataArrayCreationRecords[Index].Min;
//      DataArray.DisplayName := DisplayName;
//      DataArray.AngleType := AngleType;
//      DataArray.Visible := FDataArrayCreationRecords[Index].Visible;
//    end;
//    if DataArray <> nil then
//    begin
//      FCustomModel.UpdateDataArrayDimensions(DataArray);
////      if FCustomModel.Grid <> nil then
////      begin
////        DataArray.UpdateDimensions(FCustomModel.Grid.LayerCount, FCustomModel.Grid.RowCount,
////          FCustomModel.Grid.ColumnCount);
////      end;
//      DataArray.AssociatedDataSets := FDataArrayCreationRecords[
//        Index].AssociatedDataSets;
//      DataArray.Classification := FDataArrayCreationRecords[Index].Classification;
//    end;
  end;

  DataArray := GetDataSetByName(rsActive);
  if (DataArray <> nil) and not Assigned(DataArray.OnUpToDateSet) then
  begin
    NotifyEvent := FCustomModel.OnActiveDataSetChanged;
    DataArray.OnUpToDateSet := NotifyEvent;
  end;

  DataArray := GetDataSetByName(KNodeActive);
  if (DataArray <> nil) and (FCustomModel.SutraMesh <> nil) then
  begin
    NotifyEvent := FCustomModel.OnNodeActiveDataSetChanged;;
    DataArray.OnUpToDateSet := NotifyEvent;
  end;
end;

function TDataArrayManager.CreateNewDataArray(const ClassType: TDataArrayType;
  const Name, Formula, DisplayName: string; Lock: TDataLock; DataType: TRbwDataType;
  EvaluatedAt: TEvaluatedAt; Orientation: TDataSetOrientation;
  const Classification: string): TDataArray;
var
  ChildManagerIndex: Integer;
  ChildMan: TDataArrayManager;
begin
  result := ClassType.Create(FCustomModel);
  result.Lock := Lock;
  result.UpdateWithName(Name);
  result.DisplayName := DisplayName;
  result.DataType := DataType;
  result.EvaluatedAt := EvaluatedAt;
  result.Orientation := Orientation;
  result.Formula := Formula;
  result.Classification := Classification;
  AddDataSet(result);
  FCustomModel.CreateVariables(result);
  for ChildManagerIndex := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildMan := ChildDataArrayManagers[ChildManagerIndex];
    if ChildMan.GetDataSetByName(Name) = nil then
    begin
      ChildMan.CreateNewDataArray(ClassType, Name, Formula, DisplayName, Lock, DataType,
        EvaluatedAt, Orientation, Classification);
    end;
  end;
end;

function TDataArrayManager.DataArrayHeld(DataArray: TDataArray): boolean;
begin
  result := FDataSets.IndexOf(DataArray) >= 0
end;

resourcestring
  StrAngleCounterclickwise = 'SUTRA Data Set 15B: ANGLE1' + sLineBreak
    + 'Angle counterclockwise from +X direction';
  StrAngleUpward = 'SUTRA Data Set 15B: ANGLE2' + sLineBreak
    + 'Angle upward from the horizontal';
  StrAngleAround = 'SUTRA Data Set 15B: ANGLE3' + SLineBreak
    + 'Angle around PMAX direction';


procedure TDataArrayManager.DefinePackageDataArrays;
  procedure NoCheck(var ARecord: TDataSetCreationData);
  begin
    ARecord.CheckMax := False;
    ARecord.CheckMin := False;
    ARecord.Max := 1;
    ARecord.Min := 0;
  end;
const
  {$IFDEF OWHMV2}
  OWHM4DataSets  = 28;
  {$ELSE}
  OWHM4DataSets  = 0;
  {$ENDIF}
//  GWTDataSets = 5;
{$IFDEF SUTRA4}
  ArrayCount = 172 + OWHM4DataSets;
{$ELSE}
  ArrayCount = 162 + OWHM4DataSets;
{$ENDIF}
var
  Index: integer;
begin
  FZetaDataDefinition.DataSetType := TDataArray;
  FZetaDataDefinition.Orientation := dso3D;
  FZetaDataDefinition.DataType := rdtDouble;
  FZetaDataDefinition.Name := KActive_Surface_Elevation;
  FZetaDataDefinition.DisplayName := StrActiveSurfaceEleva;
  FZetaDataDefinition.Formula := '0';
  FZetaDataDefinition.Classification := StrSWI;
//  AssignDataSetNeeded(FZetaDataDefinition FCustomModel.ZetaUsed);
  FZetaDataDefinition.DataSetNeeded := FCustomModel.ZetaUsed;
  FZetaDataDefinition.Lock := StandardLock;
  FZetaDataDefinition.EvaluatedAt := eaBlocks;
  FZetaDataDefinition.AssociatedDataSets := StrMODFLOWSWIZETA;
  FZetaDataDefinition.Visible := True;
  NoCheck(FZetaDataDefinition);

  FSftInitConc.DataSetType := TModflowBoundaryDisplayDataArray;
  FSftInitConc.Orientation := dso3D;
  FSftInitConc.DataType := rdtDouble;
  FSftInitConc.Name := KSFTInitialConcentra;
  FSftInitConc.DisplayName := StrSFTInitialConcentra;
  FSftInitConc.Formula := '0';
  FSftInitConc.Classification := StrStreamTransport;
  FSftInitConc.DataSetNeeded := FCustomModel.SftUsed;
  FSftInitConc.Lock := StandardLock + [dcFormula];
  FSftInitConc.EvaluatedAt := eaBlocks;
  FSftInitConc.AssociatedDataSets := StrSTRPackageDataSet;
  FSftInitConc.Visible := True;
  NoCheck(FSftInitConc);

  FSftDispersion.DataSetType := TModflowBoundaryDisplayDataArray;
  FSftDispersion.Orientation := dso3D;
  FSftDispersion.DataType := rdtDouble;
  FSftDispersion.Name := KSFTDispersion;
  FSftDispersion.DisplayName := StrSFTDispersion;
  FSftDispersion.Formula := '0';
  FSftDispersion.Classification := StrStreamTransport;
  FSftDispersion.DataSetNeeded := FCustomModel.SftUsed;
  FSftDispersion.Lock := StandardLock + [dcFormula];
  FSftDispersion.EvaluatedAt := eaBlocks;
  FSftDispersion.AssociatedDataSets := StrSTRPackageDataSetDISPSF;
  FSftDispersion.Visible := True;
  NoCheck(FSftDispersion);

  FUztInitConc.DataSetType := TDataArray;
  FUztInitConc.Orientation := dso3D;
  FUztInitConc.DataType := rdtDouble;
  FUztInitConc.Name := KUztInitialConcentration;
  FUztInitConc.DisplayName := StrUztInitialConcentration;
  FUztInitConc.Formula := '0';
  FUztInitConc.Classification := StrUzt;
  FUztInitConc.DataSetNeeded := FCustomModel.GwtUztUsed;
  FUztInitConc.Lock := StandardLock;
  FUztInitConc.EvaluatedAt := eaBlocks;
  FUztInitConc.AssociatedDataSets := StrSTRPackageDataSet;
  FUztInitConc.Visible := True;
  NoCheck(FUztInitConc);

  // Whenever something in this is changed, be sure to update the help too.
  SetLength(FDataArrayCreationRecords, ArrayCount);
  for Index := 0 to ArrayCount - 1 do
  begin
    FDataArrayCreationRecords[Index].Visible := True;
    FDataArrayCreationRecords[Index].OnInitialize := nil;
    FDataArrayCreationRecords[Index].OnShouldUseOnInitialize := nil;
  end;

  Index := 0;

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := rsActive;
  FDataArrayCreationRecords[Index].DisplayName := rsActiveDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'True';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.ActiveUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAactive+ sLineBreak + StrMODFLOWBASIBOUND;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.InitializeActiveDataArrayWithCellSizeObjects;
  FDataArrayCreationRecords[Index].OnShouldUseOnInitialize :=
    FCustomModel.ShouldActiveBeSetByCellSize;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKx;
  FDataArrayCreationRecords[Index].DisplayName := rsKxDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.0001';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.AquiferPropertiesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAKx+ sLineBreak + StrMODFLOWLPFUPWHK
    + sLineBreak + StrMODFLOWBCFTRANHY
    + sLineBreak + StrMODFLOW6NPFK;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKy;
  FDataArrayCreationRecords[Index].DisplayName := rsKyDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsKx;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAKy+ sLineBreak + StrMODFLOWLPFUPWHA
    + sLineBreak + StrMODFLOWHUFAndBCF
    + sLineBreak + StrMODFLOW6NPFK22;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.DetermineKyFromAnisotropy;
  FDataArrayCreationRecords[Index].OnShouldUseOnInitialize :=
    FCustomModel.ShouldKyBeDeterminedFromAnisotropy;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsKz;
  FDataArrayCreationRecords[Index].DisplayName := rsKzDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsKx + ' / 10';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KzUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAKz+ sLineBreak + StrMODFLOWLPFUPWVK
    + sLineBreak + StrMODFLOWBCFVcont
    + sLineBreak + StrMODFLOW6NPFK33;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.DetermineKzFromAnisotropy;
  FDataArrayCreationRecords[Index].OnShouldUseOnInitialize :=
    FCustomModel.ShouldKzBeDeterminedFromAnisotropy;
  Inc(Index);

  FPorosityIndex := Index;
  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsPorosity;
  FDataArrayCreationRecords[Index].DisplayName := rsPorosityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.25';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.PorosityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAporosit
    + sLineBreak + StrMODPATHBasicData
    + sLineBreak + StrMT3DMSBTNPackage
    + sLineBreak + StrMODFLOW6MSTPackag;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsSpecific_Storage;
  FDataArrayCreationRecords[Index].DisplayName := rsSpecific_StorageDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1e-5';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.SpecificStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrUnits1L
    + sLineBreak + StrPHASTMEDIAspecificStorage
    + sLineBreak + StrMODFLOWLPFUPWSs
    + sLineBreak + StrMODFLOWBCFSf1
    + sLineBreak + StrMODFLOWNWTUPWSs
    + sLineBreak + StrMODFLOW6STOSs;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsLong_Dispersivity;
  FDataArrayCreationRecords[Index].DisplayName := rsLong_DispersivityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '10.';
  FLongitudinalDispersivityIndex := Index;
  if (FCustomModel = nil) or (FCustomModel.ModelSelection = msPhast) then
  begin
    FDataArrayCreationRecords[Index].Classification := StrHydrology;
  end
  else
  begin
    FDataArrayCreationRecords[Index].Classification := StrMT3DMS_GWT_Classificaton;
  end;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LongitudinalDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAlongitu
    + sLineBreak + StrMODFLOW6DSPPacka;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsHorizontal_Transv_Dispersivity;
  FDataArrayCreationRecords[Index].DisplayName := rsHorizontal_Transv_DispersivityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FTransverseDispersivityIndex := Index;
  if (FCustomModel = nil) or (FCustomModel.ModelSelection = msPhast) then
  begin
    FDataArrayCreationRecords[Index].Classification := StrHydrology;
  end
  else
  begin
    FDataArrayCreationRecords[Index].Classification := 'GWT';
  end;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.HorizontalTransverseDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAhorizon
    + sLineBreak + StrMODFLOw6DSPPackaath1;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsVertical_Transv_Dispersivity;
  FDataArrayCreationRecords[Index].DisplayName := rsVertical_Transv_DispersivityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1.';
  if (FCustomModel = nil) or (FCustomModel.ModelSelection = msPhast) then
  begin
    FDataArrayCreationRecords[Index].Classification := StrHydrology;
  end
  else
  begin
    FDataArrayCreationRecords[Index].Classification := StrMT3DMS_GWT_Classificaton;
  end;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.VerticalTransverseDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTMEDIAvertica
    + sLineBreak + StrMODFLOw6DSPPackaATV;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsInitial_Head;
  FDataArrayCreationRecords[Index].DisplayName := rsInitial_HeadDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.InitialHeadUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTHEADIChead + sLineBreak + StrSUTRAICSPVEC;
    ;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsInitial_Water_Table;
  FDataArrayCreationRecords[Index].DisplayName := rsInitial_Water_TableDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.InitialWaterTableUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTHEADICwater;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Solution;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_SolutionDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_solution;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Equilibrium_Phases;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_Equilibrium_PhasesDisplayName;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.EquilibriumPhasesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_Equilibrium;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Surface;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_SurfaceDisplayName;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SurfacesUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_surface;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Exchange;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_ExchangeDisplayName;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ExchangeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_exchange;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Gas_Phase;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_Gas_PhaseDisplayName;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.GasPhaseUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_gas_phase;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Solid_Solutions;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_Solid_SolutionsDisplayName;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SolidSolutionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_solid_solutions;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TIntegerPhastDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsChemistry_Initial_Kinetics;
  FDataArrayCreationRecords[Index].DisplayName := rsChemistry_Initial_KineticsDisplayName;
  FDataArrayCreationRecords[Index].Formula := '-1';
  FDataArrayCreationRecords[Index].Classification := StrChemistry;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.KineticsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTCHEMISTRYIC_kinetics;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsPrint_Chemistry;
  FDataArrayCreationRecords[Index].DisplayName := rsPrint_ChemistryDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrOutput;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTPRINTLOCATIO_chemistry;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsPrint_XYZ_Chemistry;
  FDataArrayCreationRecords[Index].DisplayName := rsPrint_XYZ_ChemistryDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrOutput;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ChemistryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrPHASTPRINTLOCATIO_xyz_chemistry;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 0;
  Inc(Index);

  // Reservoir layers
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsResLayer;
  FDataArrayCreationRecords[Index].DisplayName := rsResLayerDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirLayerUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrRESISRESL;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsResBottom;
  FDataArrayCreationRecords[Index].DisplayName := rsResBottomDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrRESBRES;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsResKv;
  FDataArrayCreationRecords[Index].DisplayName := rsResKvDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1E-5.';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrRESHCres;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsResBedThickness;
  FDataArrayCreationRecords[Index].DisplayName := rsResBedThicknessDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FDataArrayCreationRecords[Index].Classification := rsResClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ReservoirPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrRESRbthck;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  // Lake Layers
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsLakeID;
  FDataArrayCreationRecords[Index].DisplayName := rsLakeIDDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakePackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrLAKLKARR;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsLakeLeakance;
  FDataArrayCreationRecords[Index].DisplayName := rsLakeLeakanceDisplayName;
  FDataArrayCreationRecords[Index].Formula := '100.';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakePackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrLAKBDLKNC;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  // UZF layers
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfLandSurface;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfLandSurfaceDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrUzfFmpClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.GroundSurfaceUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrUZFIUZFBNDViaTh;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := rsModflowSpecifiedHead;
  FDataArrayCreationRecords[Index].DisplayName := rsModflowSpecifiedHeadDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModflowUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBASIBOUND;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfLayer;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfLayerDisplayName;
  FDataArrayCreationRecords[Index].Formula :=
    'If((ActiveOnLayer(ElevationToLayer('
      + StrUzfLandSurface + ')) AND NOT SpecifiedHeadOnLayer(ElevationToLayer('
      + StrUzfLandSurface + '))), ElevationToModelLayer('
      + StrUzfLandSurface + '), 0)';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFIUZFBND;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfDischargeRouting;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfDischargeRoutingDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.RouteUzfDischarge;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFIRUNBND;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfVerticalK;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfVerticalKDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1.';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfUnsatVertKUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFVKS;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfBrooksCoreyEpsilon;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfBrooksCoreyEpsilonDisplayName;
  FDataArrayCreationRecords[Index].Formula := '3.5';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFEPS;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfSaturatedWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfSaturatedWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.3';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFTHTS;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfInitialUnsaturatedWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfInitialUnsaturatedWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.3';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfInitialInfiltrationUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFTHTI;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfReisidualWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfReisidualWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfResidualWaterContentUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFTHTR;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfSurfaceK;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfSurfaceKDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.0001';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfSurfKUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFSURFK;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfGage_1_and_2;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfGage_1_and_2DisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFIUZOPT;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrUzfGage3;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfGage3DisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfPackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrUZFIUZOPT;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsModflow_Initial_Head;
  FDataArrayCreationRecords[Index].DisplayName := rsModflow_Initial_HeadDisplayName;
//  FDataArrayCreationRecords[Index].Formula := kModelTop;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModflowInitialHeadUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.ModflowUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBASSTRT;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsModflow_CBKz;
  FDataArrayCreationRecords[Index].DisplayName := rsModflow_CBKzDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsKz;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ConfiningBedKzUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrLPFUPWVKCBBCF;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsVerticalAnisotropy;
  FDataArrayCreationRecords[Index].DisplayName := rsVerticalAnisotropyDisplayName;
  FDataArrayCreationRecords[Index].Formula :=
    'If((' + rsKz + ' = 0.), 0, (' + rsKx + ' / ' + rsKz + '))';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.VerticalAnisotropyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrLPFUPWVKA;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsHorizontalAnisotropy;
  FDataArrayCreationRecords[Index].DisplayName := rsHorizontalAnisotropyDisplayName;
  FDataArrayCreationRecords[Index].Formula :=
    'If((' + rsKx + ' = 0.), 1., (' + rsKy + ' / ' + rsKx + '))';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.HorizontalAnisotropyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrLPFUPWHANI;
//    + sLineBreak + 'MODFLOW-6 NPF: hani';
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsSpecificYield;
  FDataArrayCreationRecords[Index].DisplayName := rsSpecificYieldDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SpecificYieldUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrLPFUPWSy
    + sLineBreak + StrBCFSf1
    + sLineBreak + StrMODFLOW6STOSy;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsWetDryThreshold;
  FDataArrayCreationRecords[Index].DisplayName := rsWetDryThresholdDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrLayerHeight + ' * 0.1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.WetDryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBCFLPFHUFWETDR;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := rsWetDryFlag;
  FDataArrayCreationRecords[Index].DisplayName := rsWetDryFlagDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'IfI(' + rsActive + ', -1, 0)';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.WetDryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := True;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := -1;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBCFLPFHUFWETDR
    + sLineBreak + sLineBreak + StrAValueLess0Indicate
    + sLineBreak + sLineBreak + StrAValueGreat0Indicate
    + sLineBreak + sLineBreak + StrAValueEq0Indicate;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsWetDry;
  FDataArrayCreationRecords[Index].DisplayName := rsWetDryDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsWetDryThreshold
    + ' * ' + rsWetDryFlag;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.WetDryUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBCFLPFHUFWETDR;
  NoCheck(FDataArrayCreationRecords[Index]);
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrModpathZone;
  FDataArrayCreationRecords[Index].DisplayName := StrModpathZoneDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrMODPATH;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathZonesNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := False;
//  FDataArrayCreationRecords[Index].Max := 1;
//  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrMODPATHV5IBOUND
    + sLineBreak + StrMODPATHV6Simulatio;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHufReferenceSurface;
  FDataArrayCreationRecords[Index].DisplayName := StrHufReferenceSurfaceDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded :=
    FCustomModel.HufReferenceSurfaceNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := False;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrMODFLOWKDEPPackag;
  Inc(Index);

  // BCF data sets.
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrTransmissivity;
  FDataArrayCreationRecords[Index].DisplayName := StrTransmissivityDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsKx + ' * ' + StrLayerHeight;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.BcfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBCFTran;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrVerticalConductance;
  FDataArrayCreationRecords[Index].DisplayName := StrVerticalConductanceDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrBcfVCONT;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.BcfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrBCFVCONTDataSet;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrConfinedStorageCoe;
  FDataArrayCreationRecords[Index].DisplayName := StrConfinedStorageCoeDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsSpecific_Storage + ' * ' + StrLayerHeight;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ConfinedStorageCoefUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].CheckMax := False;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Max := 1;
  FDataArrayCreationRecords[Index].Min := 0;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrStorativityDimen
    + sLineBreak + StrBCFSf1
    + sLineBreak + StrMODFLOW6STOSs;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFKxName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFKxNameDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufKx + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrDisplaysHUFKxValu;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFKyName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFKyNameDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufKy + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrDisplaysHUFKyValu;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFInterlayerKz;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFInterlayerKzDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufKz + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrDisplaysHUFKzValu;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFSSName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFSSNameDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufSs + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrDisplaysHUFSSValu;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFAverageSYName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFAverageSYNameDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufAverageSy + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrDisplaysAverageHUF;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFSYName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFSYNameDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufSy + '(' + rsModflow_Initial_Head + ')';
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrDisplaysHUFSyValu;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrHUFSYTPName;
  FDataArrayCreationRecords[Index].DisplayName := StrHUFSYTPNameDisplayName;
  FDataArrayCreationRecords[Index].Formula := StrHufSytp;
  FDataArrayCreationRecords[Index].Classification := StrHUF;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.OptionalDataSet;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.HufStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrDisplaysHUFSYTPVa;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := StrZones;
  FDataArrayCreationRecords[Index].DisplayName := StrZonesDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrZonebudget;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ZoneBudgetSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated :=
    FCustomModel.ZoneBudgetSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrZONEBUDGETZoneArra;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrGeostaticStress;
  FDataArrayCreationRecords[Index].DisplayName := StrGeostaticStressDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWTGL0DataSet4;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrSpecificGravityUns;
  FDataArrayCreationRecords[Index].DisplayName := StrSpecificGravityUnsDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1.7';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWTSGMDataSet5;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrSpecificGravitySat;
  FDataArrayCreationRecords[Index].DisplayName := StrSpecificGravitySatDisplayName;
  FDataArrayCreationRecords[Index].Formula := '2.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWTSGSDataSet6;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrInitialPreOffsets;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialPreOffsetsDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtOffsetsUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtOffsetsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWTPCSOFFDataSe;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrInitialPreconsolida;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialPreconsolidaDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwtSpecifiedUsed;
  FDataArrayCreationRecords[Index].DataSetShouldBeCreated := FCustomModel.SwtSpecifiedUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWTPCSDataSet1;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := STR_MT3DMS_Observation_Locations;
  FDataArrayCreationRecords[Index].DisplayName := STR_MT3DMS_Observation_LocationsDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrMt3dClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMS_StrictUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMT3DMSBasicPackage;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := StrMT3DMSActive;
  FDataArrayCreationRecords[Index].DisplayName := StrMT3DMSActiveDisplayName;
  FDataArrayCreationRecords[Index].Formula := rsActive;
  FDataArrayCreationRecords[Index].Classification := StrMt3dClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMS_StrictUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMT3DMSBasicPackage_ICBUND;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsBulkDensity;
  FDataArrayCreationRecords[Index].DisplayName := rsBulkDensityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '2000000';
  FDataArrayCreationRecords[Index].Classification := StrMt3dClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSBulkDensityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMT3DMSChemicalReac_RHOB;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsImmobPorosity;
  FDataArrayCreationRecords[Index].DisplayName := rsImmobPorosityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := StrMt3dClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMSImmobPorosityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMT3DMSChemicalReac_PRSITY2;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := rsMT3DMS_Layer_Thickness;
  FDataArrayCreationRecords[Index].DisplayName := rsMT3DMS_Layer_ThicknessDisplayName;
  FDataArrayCreationRecords[Index].Formula :=StrLayerHeight;
  FDataArrayCreationRecords[Index].Classification := StrMt3dClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3dMS_StrictUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMT3DMSBasicPackage_DZ;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := KModpathBudget;
  FDataArrayCreationRecords[Index].DisplayName := KModpathBudgetDisplayName;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrMODPATH;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathBudgetNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODPATHSimulationF_27;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KModpathRetardation;
  FDataArrayCreationRecords[Index].DisplayName := KModpathRetardationDisplayName;
  FDataArrayCreationRecords[Index].Formula := '2.';
  FDataArrayCreationRecords[Index].Classification := StrMODPATH;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ModpathRetardationNeeded;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODPATHSimulationF_32_and_33;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KNodalPorosity;
  FDataArrayCreationRecords[Index].DisplayName := StrNodalPorosityDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_Por;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KNodalThickness;
  FDataArrayCreationRecords[Index].DisplayName := StrNodalThicknessDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraThicknessUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_Z;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KUnsatRegionNodes;
  FDataArrayCreationRecords[Index].DisplayName := StrUnsatRegionNodesDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUnsatRegionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_NREG;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KUnsatRegionElements;
  FDataArrayCreationRecords[Index].DisplayName := StrUnsatRegionElementsDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUnsatRegionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_LREG;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMaximumPermeability;
  FDataArrayCreationRecords[Index].DisplayName := StrMaximumPermeability;
  FDataArrayCreationRecords[Index].Formula := '1E-10';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraPermeabilityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_PMAX;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMiddlePermeability;
  FDataArrayCreationRecords[Index].DisplayName := StrMiddlePermeability;
  FDataArrayCreationRecords[Index].Formula := KMaximumPermeability;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraMiddlePermeabilityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_PMID;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMinimumPermeability;
  FDataArrayCreationRecords[Index].DisplayName := StrMinimumPermeability;
  FDataArrayCreationRecords[Index].Formula := KMaximumPermeability;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraPermeabilityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_PMIN;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMaximumK;
  FDataArrayCreationRecords[Index].DisplayName := StrMaximumK;
  FDataArrayCreationRecords[Index].Formula := '1E-3';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraHydraulicConductivityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_PMAX;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrMiddleK;
  FDataArrayCreationRecords[Index].DisplayName := KMiddleK;
  FDataArrayCreationRecords[Index].Formula := KMaximumK;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraMiddleHydraulicConductivityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_PMID;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMinimumK;
  FDataArrayCreationRecords[Index].DisplayName := StrMinimumK;
  FDataArrayCreationRecords[Index].Formula := KMaximumK;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraHydraulicConductivityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_PMIN;
  Inc(Index);


  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KHorizontalAngle;
  FDataArrayCreationRecords[Index].DisplayName := StrHorizontalAngle;
  FDataArrayCreationRecords[Index].AngleType := atDegrees;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcUnits];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrAngleCounterclickwise;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KVerticalAngle;
  FDataArrayCreationRecords[Index].DisplayName := StrVerticalAngle;
  FDataArrayCreationRecords[Index].AngleType := atDegrees;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra3DModel;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcUnits];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrAngleUpward;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KRotationalAngle;
  FDataArrayCreationRecords[Index].DisplayName := StrRotationalAngle;
  FDataArrayCreationRecords[Index].AngleType := atDegrees;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra3DModel;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcUnits];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrAngleAround;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMaxLongitudinalDisp;
  FDataArrayCreationRecords[Index].DisplayName := StrMaxLongitudinalDisp;
  FDataArrayCreationRecords[Index].Formula := '0.5';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_ALMAX;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMidLongitudinalDisp;
  FDataArrayCreationRecords[Index].DisplayName := StrMidLongitudinalDisp;
  FDataArrayCreationRecords[Index].Formula := KMaxLongitudinalDisp;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra3DModel;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_ALMID;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMinLongitudinalDisp;
  FDataArrayCreationRecords[Index].DisplayName := StrMinLongitudinalDisp;
  FDataArrayCreationRecords[Index].Formula := KMaxLongitudinalDisp;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_ALMIN;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMaxTransverseDisp;
  FDataArrayCreationRecords[Index].DisplayName := StrMaxTransverseDisp;
  FDataArrayCreationRecords[Index].Formula := '0.5';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_ATMAX;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMidTransverseDisp;
  FDataArrayCreationRecords[Index].DisplayName := StrMidTransverseDisp;
  FDataArrayCreationRecords[Index].Formula := KMaxTransverseDisp;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra3DModel;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_ATMID;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMinTransverseDisp;
  FDataArrayCreationRecords[Index].DisplayName := StrMinTransverseDisp;
  FDataArrayCreationRecords[Index].Formula := KMaxTransverseDisp;
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_ATMIN;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KInitialPressure;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialPressure;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraPermeabilityUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRAICSDataSet2_PVEC;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KInitialConcentration;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialConcentration;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraConcentrationUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRAICSDataSet3_UVEC;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KInitialTemperature;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialTemperature;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraTemperatureUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRAICSDataSet3_UVEC;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KSoilID;
  FDataArrayCreationRecords[Index].DisplayName := StrSoilID;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SoilIDUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrFMPDataSet8SID;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KPipeDiameter;
  FDataArrayCreationRecords[Index].DisplayName := StrPipeDiameter;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_DIAMETER;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KTortuosity;
  FDataArrayCreationRecords[Index].DisplayName := StrTortuosity;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_TORTUOSITY;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KRoughnessHeight;
  FDataArrayCreationRecords[Index].DisplayName := StrRoughnessHeight;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_RHEIGHT;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLowerCriticalR;
  FDataArrayCreationRecords[Index].DisplayName := StrLowerCriticalR;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_LCRITREY_P;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KUpperCriticalR;
  FDataArrayCreationRecords[Index].DisplayName := StrUpperCriticalR;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_TCRITREY_P;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KPipeConductanceOrPer;
  FDataArrayCreationRecords[Index].DisplayName := StrPipeConductanceOrPer;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_K_EXCHANGE;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KCfpNodeElevation;
  FDataArrayCreationRecords[Index].DisplayName := StrCfpNodeElevation;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_ELEVATION;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KCfpFixedHeads;
  FDataArrayCreationRecords[Index].DisplayName := StrCfpFixedHeads;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrCFPClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CfpPipesSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWCFPDataSet_N_HEAD;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KEffectivePorosity;
  FDataArrayCreationRecords[Index].DisplayName := StrEffectivePorosity;
  FDataArrayCreationRecords[Index].Formula := '0.1';
  FDataArrayCreationRecords[Index].Classification := StrSWI;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwiUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWSWIPackage_SSZ;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KSourceFluidDensityZone;
  FDataArrayCreationRecords[Index].DisplayName := StrSourceFluidDensityZo;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSWI;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwiUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWSWIPackage_ISOURCE;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtString;
  FDataArrayCreationRecords[Index].Name := KSWI_Observation_Name;
  FDataArrayCreationRecords[Index].DisplayName := StrSWIObservationName;
  FDataArrayCreationRecords[Index].Formula := '""';
  FDataArrayCreationRecords[Index].Classification := StrSWI;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwiObsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOWSWIPackage_OBSNAM;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSwrReach;
  FDataArrayCreationRecords[Index].DisplayName := StrSwrReach;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSwrClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwrSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWRProcessDataSet_IRCH4A;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSwrReachGroup;
  FDataArrayCreationRecords[Index].DisplayName := StrSwrReachGroup;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSwrClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwrSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWRProcessDataSet_IRGNUM;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSwrRoutingType;
  FDataArrayCreationRecords[Index].DisplayName := StrSwrRoutingType;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSwrClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwrSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWRProcessDataSet_IROUTETYPE;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSwrReachLength;
  FDataArrayCreationRecords[Index].DisplayName := StrSwrReachLength;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSwrClassifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SwrSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSWRProcessDataSet_RLEN;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KDepthRateIndex;
  FDataArrayCreationRecords[Index].DisplayName := StrDepthRateIndex;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFootprintInputClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FootprintSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrRatePerUnitAreaO;
  FDataArrayCreationRecords[Index].Visible := True;
  FDataArrayCreationRecords[Index].CheckMin := True;
  FDataArrayCreationRecords[Index].Min := 1e-6;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TFootprintWithdrawalDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KWithdrawals;
  FDataArrayCreationRecords[Index].DisplayName := StrWithdrawals;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFootprintInputClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FootprintSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrSpecifiedWithdrawal;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignFootprintBoundarydWithdrawal;
  FDataArrayCreationRecords[Index].OnShouldUseOnInitialize :=
    FCustomModel.UseFootprintWells;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := K_IDOMAIN;
  FDataArrayCreationRecords[Index].DisplayName := StrIDOMAIN;
  FDataArrayCreationRecords[Index].Formula := 'IfI(' + rsActive
    + ', IfI((' + StrLayerHeight + ' > 0.), 1, -1), 0)';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Modflow6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := StrMODFLOW6Structure
    + sLineBreak + Str1ActiveCells
    + sLineBreak + Str0InactiveCells
    + sLineBreak + Str1VerticalPasst
    + sLineBreak + StrTheDefaultFormula;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KCellType;
  FDataArrayCreationRecords[Index].DisplayName := StrCell_Type;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Modflow6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6NPFIcel + sLineBreak
    + Str0ConstantCellThi + sLineBreak
    + Str0CalculatedCell + sLineBreak
    + Str0ConstantOrCalc + sLineBreak
    + Format(StrSeeAlsoTheSDa, [StrConvertible]);
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := KConvertible;
  FDataArrayCreationRecords[Index].DisplayName := StrConvertible;
  FDataArrayCreationRecords[Index].Formula := KCellType + ' > 0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.StorageSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6STO_iconvert;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSUTRAInitialLakeS;
  FDataArrayCreationRecords[Index].DisplayName := StrSUTRAInitialLakeS;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSUTRALake;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraLakeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRA30STGI;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSUTRAInitialLakeU;
  FDataArrayCreationRecords[Index].DisplayName := StrSUTRAInitialLakeU;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSUTRALake;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraLakeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRA30UWI;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSUTRALakeRecharge;
  FDataArrayCreationRecords[Index].DisplayName := StrSUTRALakeRecharge;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSUTRALake;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraLakeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRA30FRRO;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSUTRALakeDischarge;
  FDataArrayCreationRecords[Index].DisplayName := StrSUTRALakeDischarge;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSUTRALake;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraLakeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRA30FDRO;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KXT3DAngle1;
  FDataArrayCreationRecords[Index].DisplayName := StrXT3DAngle1;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.NpfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6NPFAngle1;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KXT3DAngle2;
  FDataArrayCreationRecords[Index].DisplayName := StrXT3DAngle2;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.NpfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6NPFAngle2;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KXT3DAngle3;
  FDataArrayCreationRecords[Index].DisplayName := StrXT3DAngle3;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.NpfUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6NPFAngle3;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KReachLengthSFR;
  FDataArrayCreationRecords[Index].DisplayName := StrReachLengthSFR;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSFRMODFLOW6;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SfrMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSFRMODFLOW6Packa_rlen;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KReachWidthSFR6;
  FDataArrayCreationRecords[Index].DisplayName := StrReachWidthSFR6;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSFRMODFLOW6;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SfrMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSFRMODFLOW6Packa_rwid;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KGradientSFR6;
  FDataArrayCreationRecords[Index].DisplayName := StrGradientSFR6;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSFRMODFLOW6;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SfrMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSFRMODFLOW6Packa_rgrd;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KStreambedTopSFR6;
  FDataArrayCreationRecords[Index].DisplayName := StrStreambedTopSFR6;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSFRMODFLOW6;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SfrMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSFRMODFLOW6Packa_rtp;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KStreambedThicknessSFR6;
  FDataArrayCreationRecords[Index].DisplayName := StrStreambedThicknessSFR6;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSFRMODFLOW6;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SfrMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSFRMODFLOW6Packa_rbth;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KHydraulicConductivitySFR6;
  FDataArrayCreationRecords[Index].DisplayName := StrHydraulicConductivitySFR6;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSFRMODFLOW6;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SfrMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSFRMODFLOW6Packa_rhk;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWRadius;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWRadius;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6Packa_radius;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWBottom;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWBottom;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6Packa_bottom;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWInitialHead;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWInitialHead;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6Packa_strt;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWScreenTop;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWScreenTop;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6CONNE_scrn_top;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWScreenBottom;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWScreenBottom;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6CONNE_scrn_bot;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWSkinK;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWSkinK;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6CONNE_hk_skin;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMAWSkinRadius;
  FDataArrayCreationRecords[Index].DisplayName := StrMAWSkinRadius;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMAW;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.MawSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMAWMODFLOW6CONNE_radius_skin;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := KLakeMf6;
  FDataArrayCreationRecords[Index].DisplayName := StrLakeMf6;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := '';
//    StrMAWMODFLOW6CONNE_radius_skin;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Bed_Leakance;
  FDataArrayCreationRecords[Index].DisplayName := StrLake_Bed_Leakance;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula]; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := LakeMf6BedLeak;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignMOdflow6LakeDisplayArrays;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Bed_Thickness;
  FDataArrayCreationRecords[Index].DisplayName := StrLake_Bed_Thickness;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula]; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := LakeMf6BedLeak;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignMOdflow6LakeDisplayArrays;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Bottom_Elevation;
  FDataArrayCreationRecords[Index].DisplayName := StrLake_Bottom_Elevation;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula]; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := LakeMf6Belev;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignMOdflow6LakeDisplayArrays;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Top_Elevation;
  FDataArrayCreationRecords[Index].DisplayName := StrLake_Top_Elevation;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula]; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := LakeMf6Telev;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignMOdflow6LakeDisplayArrays;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Connection_Length;
  FDataArrayCreationRecords[Index].DisplayName := StrLake_Connection_Length;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula]; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := LakeMf6ConnLen;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignMOdflow6LakeDisplayArrays;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TModflowBoundaryDisplayDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Connection_Width;
  FDataArrayCreationRecords[Index].DisplayName := StrLake_Connection_Width;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := rsLakeClassificaton;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LakMf6Selected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula]; // [dcName, dcType, dcOrientation, dcEvaluatedAt, dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets := LakeMf6ConnWidth;
  FDataArrayCreationRecords[Index].OnInitialize :=
    FCustomModel.AssignMOdflow6LakeDisplayArrays;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfMf6BrooksCoreyEpsilon;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfMf6BrooksCoreyEpsilonDisplayName;
  FDataArrayCreationRecords[Index].Formula := '3.5';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfMf6PackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock  + [dcFormula, dcInterpolation];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacEps;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfMf6InitialUnsaturatedWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfMf6InitialUnsaturatedWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.3';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfMf6PackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock  + [dcFormula, dcInterpolation];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacThti;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfMf6ReisidualWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfMf6ReisidualWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.2';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfMf6PackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula, dcInterpolation];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacThtr;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfMf6SaturatedWaterContent;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfMf6SaturatedWaterContentDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0.3';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfMf6PackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula, dcInterpolation];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacThts;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfMf6SurfaceDepressionDepth;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfMf6SurfaceDepressionDepthDisplayName;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfMf6PackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula, dcInterpolation];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacSurfdep;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := StrUzfMf6VerticalSaturatedK;
  FDataArrayCreationRecords[Index].DisplayName := StrUzfMf6VerticalSaturatedKDisplayName;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := strUzfClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.UzfMf6PackageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula, dcInterpolation];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacVks;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLake_Bottom;
  FDataArrayCreationRecords[Index].DisplayName := StrLakeBottom;
  FDataArrayCreationRecords[Index].Formula := StrSUTRAMeshTop;
  FDataArrayCreationRecords[Index].Classification := StrSUTRALake;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SutraLakeBottomUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRALakeAreaInpu;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KKyOverKx;
  FDataArrayCreationRecords[Index].DisplayName := StrKyOverKxDisplay;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.HorizAnisotropyMf6Used;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6NPFK22;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KKzOverKx;
  FDataArrayCreationRecords[Index].DisplayName := StrKzOverKxDisplay;
  FDataArrayCreationRecords[Index].Formula := '0.1';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.VertAnisotropyMf6Used;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6NPFK33;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KInitialElasticSpec;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialElasticSpec;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CSubInitialElasticStorageUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6CSUBCg;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KInitialElasticReco;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialElasticReco;
  FDataArrayCreationRecords[Index].Formula := '0.005';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CSubInitialRecompressionIndexUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6CSUBCg;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KInitialCoarsePoros;
  FDataArrayCreationRecords[Index].DisplayName := StrInitialCoarsePoros;
  FDataArrayCreationRecords[Index].Formula := '0.25';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CSubDataSetsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6CSUBCgTheta;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KMoistSpecificGravi;
  FDataArrayCreationRecords[Index].DisplayName := StrMoistSpecificGravi;
  FDataArrayCreationRecords[Index].Formula := '1.7';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CSubDataSetsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6CSUBSgm;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSaturatedSpecificG;
  FDataArrayCreationRecords[Index].DisplayName := StrSaturatedSpecificG;
  FDataArrayCreationRecords[Index].Formula := '2.0';
  FDataArrayCreationRecords[Index].Classification := StrSubsidence;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CSubDataSetsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6CSUBSgs;
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TRealSparseDataSet;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLakeTransportConce;
  FDataArrayCreationRecords[Index].DisplayName := StrLakeTransportConce;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrMt3dClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Mt3d_LktIsSelected;
  FDataArrayCreationRecords[Index].Lock := StandardLock + [dcFormula];
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6UZF6PacVks;
  FDataArrayCreationRecords[Index].Visible := False;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := StrNodeActive;
  FDataArrayCreationRecords[Index].DisplayName := KNodeActive;
  FDataArrayCreationRecords[Index].Formula := 'True';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra3DModel;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets := '';
  FDataArrayCreationRecords[Index].Visible := True;
  Inc(Index);

  {$IFDEF SUTRA4}
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSolidMatrixComp;
  FDataArrayCreationRecords[Index].DisplayName := StrSolidMatrixComp;
  FDataArrayCreationRecords[Index].Formula := '1E-8';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4Used;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSolidGrainSpecificHeat;
  FDataArrayCreationRecords[Index].DisplayName := StrSolidGrainSpecificHeat;
  FDataArrayCreationRecords[Index].Formula := '840';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4EnergyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_CS;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSolidGrainDensity;
  FDataArrayCreationRecords[Index].DisplayName := StrSolidGrainDensity;
  FDataArrayCreationRecords[Index].Formula := '2600';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4EnergyOrSorptionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_RHOS;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KZeroOrderProductionRateInLiquid;
  FDataArrayCreationRecords[Index].DisplayName := StrZeroOrderProductionRateInLiquid;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4ProductionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_PRODL0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KZeroOrderProductionRateInImmobile;
  FDataArrayCreationRecords[Index].DisplayName := StrZeroOrderProductionRateInImmobile;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4ProductionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_PRODS0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KFirstOrderProductionRateInLiquid;
  FDataArrayCreationRecords[Index].DisplayName := StrFirstOrderProductionRateInLiquid;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4SoluteUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_PRODL1;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KFirstOrderProductionRateInImmobile;
  FDataArrayCreationRecords[Index].DisplayName := StrFirstOrderProductionRateInImmobile;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4SoluteUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_PRODS1;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KZeroOrderProductionRateInIce;
  FDataArrayCreationRecords[Index].DisplayName := StrZeroOrderProductionRateInIce;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4FreezingUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaNodes;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet14B_PRODI0;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KScaledSolidGrainThermalConductivity;
  FDataArrayCreationRecords[Index].DisplayName := StrScaledSolidGrainThermalConductivity;
  FDataArrayCreationRecords[Index].Formula := '3.5';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4EnergyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_SIGMAS;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KScaledEffectiveAirThermalConductivity;
  FDataArrayCreationRecords[Index].DisplayName := StrScaledEffectiveAirThermalConductivity;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrHydrology;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.Sutra4EnergyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrSUTRADataSet15B_SIGMAA;
  Inc(Index);
  {$ENDIF}

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KDiffusionCoefficien;
  FDataArrayCreationRecords[Index].DisplayName := StrDiffusionCoefficien;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrGWTClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.GwtDispUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6Dispersion_DIFFC;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLongitudinalDispersH;
  FDataArrayCreationRecords[Index].DisplayName := StrLongitudinalDispersH;
  FDataArrayCreationRecords[Index].Formula := '10';
  FDataArrayCreationRecords[Index].Classification := StrGWTClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SeparatedLongitudinalDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6Dispersion_ALH;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLongitudinalDispersV;
  FDataArrayCreationRecords[Index].DisplayName := StrLongitudinalDispersV;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrGWTClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SeparatedLongitudinalDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6Dispersion_ALV;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KHorizontalTransvers;
  FDataArrayCreationRecords[Index].DisplayName := StrHorizontalTransvers;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrGWTClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SeparatedHorizontalTransverseDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6Dispersion_ATH1;
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dso3D;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KVerticalTransverse;
  FDataArrayCreationRecords[Index].DisplayName := StrVerticalTransverse;
  FDataArrayCreationRecords[Index].Formula := '0.1';
  FDataArrayCreationRecords[Index].Classification := StrGWTClassification;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SeparatedHorizontalTransverseDispersionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    StrMODFLOW6Dispersion_ATH2;
  Inc(Index);

  {$IFDEF OWHMV2}
  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KFarmID;
  FDataArrayCreationRecords[Index].DisplayName := StrFarmID;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadyFarmsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, WBS Location';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KLand_Use_ID;
  FDataArrayCreationRecords[Index].DisplayName := StrLand_Use_ID;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadyCropsUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE Location';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KRefET;
  FDataArrayCreationRecords[Index].DisplayName := StrRefET;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadyRefETUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, CLIMATE: REFERENCE_ET';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KPrecipitation;
  FDataArrayCreationRecords[Index].DisplayName := StrPrecipitation;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadyPrecipUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, CLIMATE: PRECIPITATION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KEfficiency;
  FDataArrayCreationRecords[Index].DisplayName := StrEfficiency;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadArrayEfficiencyUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, WBS: EFFICIENCY';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KEfficiencyImprovement;
  FDataArrayCreationRecords[Index].DisplayName := StrEfficiencyImprovement;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadArrayEfficiencyImprovementUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, WBS: EFFICIENCY_IMPROVEMENT';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KBareRunoffFraction;
  FDataArrayCreationRecords[Index].DisplayName := StrBareRunoffFraction;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadArrayBareRunoffFractionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, WBS: BARE_RUNOFF_FRACTION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KBarePrecipitationConsumptionFraction;
  FDataArrayCreationRecords[Index].DisplayName := StrBarePrecipitationConsumptionFraction;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadArrayBarePrecipitationConsumptionFractionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, WBS: BARE_PRECIPITATION_CONSUMPTION_FRACTION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KAddedDemandRunoffSplit;
  FDataArrayCreationRecords[Index].DisplayName := StrAddedDemandRunoffSplit;
  FDataArrayCreationRecords[Index].Formula := '0.1';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FarmProcess4SteadArrayAddedDemandRunoffSplitUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, WBS: ADDED_DEMAND_RUNOFF_SPLIT';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KCapillary_Fringe;
  FDataArrayCreationRecords[Index].DisplayName := StrCapillary_Fringe;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CapillaryFringeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, SOIL: CAPILLARY_FRINGE';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KSurfaceK;
  FDataArrayCreationRecords[Index].DisplayName := StrSurfaceK;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.SurfaceKUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, SOIL: SURFACE_VERTICAL_HYDRAULIC_CONDUCTIVITY';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KPotential_Evap_Bare;
  FDataArrayCreationRecords[Index].DisplayName := StrPotential_Evap_Bare;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.PotentialEvapBareUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, CLIMATE: POTENTIAL_EVAPORATION_BARE';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KDirectRecharge;
  FDataArrayCreationRecords[Index].DisplayName := StrDirectRecharge;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.DirectRechargeUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, CLIMATE: DIRECT_RECHARGE';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KPrecipPotConsumption;
  FDataArrayCreationRecords[Index].DisplayName := StrPrecipPotConsumption;
  FDataArrayCreationRecords[Index].Formula := '0.';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.PrecipPotConsumptionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, CLIMATE: PRECIPITATION_POTENTIAL_CONSUMPTION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KNRD_Infiltration_Location;
  FDataArrayCreationRecords[Index].DisplayName := StrNRD_Infiltration_Location;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.NrdInfilLocationUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, SURFACE_WATER: NRD_INFILTRATION_LOCATION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KCropCoefficient;
  FDataArrayCreationRecords[Index].DisplayName := StrCropCoefficient;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CropCoefficientUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: CROP_COEFFICIENT';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KLandUseAreaFraction;
  FDataArrayCreationRecords[Index].DisplayName := StrLandUseAreaFraction;
  FDataArrayCreationRecords[Index].Formula := '1';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LandUseAreaFractionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: LAND_USE_AREA_FRACTION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KConsumptiveUse;
  FDataArrayCreationRecords[Index].DisplayName := StrConsumptiveUse;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.ConsumptiveUseUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: CONSUMPTIVE_USE';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KIrrigation;
  FDataArrayCreationRecords[Index].DisplayName := StrKIrrigation;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.IrrigationUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: IRRIGATION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KRootDepth;
  FDataArrayCreationRecords[Index].DisplayName := StrRootDepth;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.RootDepthUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: ROOT_DEPTH';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtInteger;
  FDataArrayCreationRecords[Index].Name := KGWRootInteraction;
  FDataArrayCreationRecords[Index].DisplayName := StrGWRootInteraction;
  FDataArrayCreationRecords[Index].Formula := '5';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.GwRootInteractionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: GROUNDWATER_ROOT_INTERACTION' + sLineBreak
    + '0 = No Transpiration' + sLineBreak
    + '1 = No Groundwater Interaction' + sLineBreak
    + '2 = Has Anoxia/Soil Stress Loss, NO Root-Groundwater Uptake' + sLineBreak
    + '3 = Has Root-Groundwater Uptake, NO Anoxia/Soil Stress Loss' + sLineBreak
    + '4 = Has Root-Groundwater Uptake and Soil Stress Loss, NO Anoxia Loss' + sLineBreak
    + '5 = Full Interaction' + sLineBreak
    + 'It is recommended to only use option 1, 3, or 5. If keyword is not specified, the default value is 5.';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KTranspirationFraction;
  FDataArrayCreationRecords[Index].DisplayName := StrTranspirationFraction;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.TranspirationFractionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: TRANSPIRATION_FRACTION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KEvaporationIrrigationFraction;
  FDataArrayCreationRecords[Index].DisplayName := StrEvaporationIrrigationFraction;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.EvaporationIrrigationFractionUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: EVAPORATION_IRRIGATION_FRACTION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KFractionOfPrecipToSurfaceWater;
  FDataArrayCreationRecords[Index].DisplayName := StrFractionOfPrecipToSurfaceWater;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FractionOfPrecipToSurfaceWaterUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: SURFACEWATER_LOSS_FRACTION_PRECIPITATION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KFractionOfIrrigToSurfaceWater;
  FDataArrayCreationRecords[Index].DisplayName := StrFractionOfIrrigToSurfaceWater;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.FractionOfIrrigToSurfaceWaterUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: SURFACEWATER_LOSS_FRACTION_IRRIGATION';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtDouble;
  FDataArrayCreationRecords[Index].Name := KAddedDemand;
  FDataArrayCreationRecords[Index].DisplayName := StrAddedDemand;
  FDataArrayCreationRecords[Index].Formula := '0';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.AddedDemandUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: ADDED_DEMAND';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := KCropHasSalinityDemand;
  FDataArrayCreationRecords[Index].DisplayName := StrCropHasSalinityDemand;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.CropHasSalinityDemandUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, SALINITY_FLUSH_IRRIGATION: CROP_HAS_SALINITY_DEMAND';
  Inc(Index);

  FDataArrayCreationRecords[Index].DataSetType := TDataArray;
  FDataArrayCreationRecords[Index].Orientation := dsoTop;
  FDataArrayCreationRecords[Index].DataType := rdtBoolean;
  FDataArrayCreationRecords[Index].Name := KLandUseCellsToPrint;
  FDataArrayCreationRecords[Index].DisplayName := StrLandUseCellsToPrint;
  FDataArrayCreationRecords[Index].Formula := 'False';
  FDataArrayCreationRecords[Index].Classification := StrFmp2Classifiation;
  FDataArrayCreationRecords[Index].DataSetNeeded := FCustomModel.LandUseCellsToPrintUsed;
  FDataArrayCreationRecords[Index].Lock := StandardLock;
  FDataArrayCreationRecords[Index].EvaluatedAt := eaBlocks;
  FDataArrayCreationRecords[Index].AssociatedDataSets :=
    'MODFLOW-OWHM version 2, LAND_USE: PRINT ROW_COLUMN';
  Inc(Index);


  {$ENDIF}

  // See ArrayCount above.
  Assert(Length(FDataArrayCreationRecords) = Index);
end;

destructor TDataArrayManager.Destroy;
var
  Index: Integer;
  DataSet: TDataArray;
begin
  FDataSetNames.Free;
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
    DataSet.StopTalkingToAnyone;
//    DataSet.SetModelToNil;
  end;
  for Index := 0 to LocalCount - 1 do
  begin
    DataSet := DataSets[Index];
    DataSet.StopTalkingToAnyone;
//    DataSet.SetModelToNil;
  end;
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
//    DataSet.StopTalkingToAnyone;
    DataSet.SetModelToNil;
  end;
  for Index := 0 to LocalCount - 1 do
  begin
    DataSet := DataSets[Index];
//    DataSet.StopTalkingToAnyone;
    DataSet.SetModelToNil;
  end;
  FDeletedDataSets.Free;
  FDataSetLookUpList.Free;
  FBoundaryDataSets.Free;
  FDataSets.Free;
  FDataSetsToCache.Free;
  FRiverDataSets.Free;
  inherited;
end;

procedure TDataArrayManager.DontCache(DataArray: TDataArray);
begin
  FDataSetsToCache.Remove(DataArray);
end;

procedure TDataArrayManager.ExtractDataSet(const DataSet: TDataArray);
begin
  FDataSets.Extract(DataSet);
  Invalidate;
  RemoveDataSetFromLookUpList(DataSet);
end;

function TDataArrayManager.GetBoundaryDataSetCount: integer;
begin
  result := FBoundaryDataSets.Count;
end;

function TDataArrayManager.GetBoundaryDataSets(
  const Index: integer): TDataArray;
begin
  result := FBoundaryDataSets[Index] as TDataArray;
end;

function TDataArrayManager.GetChildDataArrayManager(
  Index: integer): TDataArrayManager;
var
  PhastModel: TPhastModel;
  ChildModel: TChildModel;
begin
  PhastModel := FCustomModel as TPhastModel;
  ChildModel := PhastModel.ChildModels[Index].ChildModel;
  if ChildModel = nil then
  begin
    result := nil;
  end
  else
  begin
    result := ChildModel.DataArrayManager;
  end;
end;

function TDataArrayManager.GetChildDataArrayManagerCount: integer;
var
  PhastModel: TPhastModel;
begin
  if FCustomModel is TPhastModel then
  begin
    PhastModel := TPhastModel(FCustomModel);
//    if PhastModel.LgrUsed then
//    begin
      result := PhastModel.ChildModels.Count;
//    end
//    else
//    begin
//      result := 0;
//    end;
  end
  else
  begin
    result := 0;
  end;
end;

function TDataArrayManager.GetDataSetByName(
  const DataSetName: string): TDataArray;
var
  Index: Integer;
  APointer: pointer;
  DataArray: TDataArray;
begin
  if LocalCount = 0 then
  begin
    result := nil;
    Exit;
  end;
  if FDataSetLookUpList = nil then
  begin
    FDataSetLookUpList := THashTableFacade.Create( Max(211, DataSetCount*2-1));
    FDataSetLookUpList.IgnoreCase := True;
//    FDataSetLookUpList.TableSize := Max(211, DataSetCount*2-1);
    for Index := 0 to LocalCount - 1 do
    begin
      DataArray := DataSets[Index];
      FDataSetLookUpList.Insert(DataArray.Name, DataArray)
    end;
  end;
  if FDataSetLookUpList.Search(DataSetName, APointer) then
  begin
    result := APointer;
  end
  else
  begin
    result := nil;
  end;
end;

procedure TDataArrayManager.Loaded;
var
  DataArrayIndex: Integer;
begin
  for DataArrayIndex := 0 to DataSetCount - 1 do
  begin
    DataSets[DataArrayIndex].Loaded;
  end;
  UpdateClassifications;
end;

function TDataArrayManager.LocalCount: integer;
begin
  if FDataSets = nil then
  begin
    result := 0;
  end
  else
  begin
    result := FDataSets.Count;
  end;
end;

function TDataArrayManager.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TDataArrayManager.GetDataSetCount: integer;
begin
  if (FCustomModel <> nil) and (FCustomModel is TChildModel) then
  begin
    if TChildModel(FCustomModel).ParentModel = nil then
    begin
      result := LocalCount;
    end
    else
    begin
      result := TChildModel(FCustomModel).ParentModel.DataArrayManager.DataSetCount;
    end;
  end
  else
  begin
    result := LocalCount;
  end;
end;

function TDataArrayManager.GetDataSetNames: TStringList;
var
  Index: Integer;
begin
  if FDataSetNames = nil then
  begin
    FDataSetNames := TStringList.Create;
  end
  else
  begin
    FDataSetNames.Clear;
  end;
  for Index := 0 to DataSetCount - 1 do
  begin
    FDataSetNames.Add(DataSets[Index].Name);
  end;
  result := FDataSetNames;
end;

function TDataArrayManager.GetDataSet(const Index: integer): TDataArray;
begin
  result := FDataSets[Index] as TDataArray;
end;

function TDataArrayManager.GetDataSetsCapacity: integer;
begin
  if FDataSets = nil then
  begin
    result := 0;
  end
  else
  begin
    result := FDataSets.Capacity;
  end;
end;

procedure TDataArrayManager.HandleAddedDataArrays(AddedDataSetList: TList);
var
  Index: Integer;
  DataArray: TDataArray;
  ChildIndex: Integer;
  ChildManager: TDataArrayManager;
  NewAddedList: TList;
  DataArrayIndex: Integer;
  ChildDataArray: TDataArray;
  DeletedIndex: Integer;
  TestDataArray: TDataArray;
begin
  for Index := 0 to AddedDataSetList.Count - 1 do
  begin
    DataArray := AddedDataSetList[Index];
    if GetDataSetByName(DataArray.Name) = nil then
    begin
      AddDataSet(DataArray);
      FCustomModel.CreateVariables(DataArray);
    end;
    FDeletedDataSets.Extract(DataArray);
  end;
  for ChildIndex := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildManager := ChildDataArrayManagers[ChildIndex];
    NewAddedList := TList.Create;
    try
      NewAddedList.Capacity := AddedDataSetList.Count;
      for DataArrayIndex := 0 to AddedDataSetList.Count - 1 do
      begin
        DataArray := AddedDataSetList[DataArrayIndex];
        ChildDataArray := ChildManager.GetDataSetByName(DataArray.Name);
        if ChildDataArray = nil then
        begin
          for DeletedIndex := 0 to ChildManager.FDeletedDataSets.Count - 1 do
          begin
            TestDataArray := ChildManager.FDeletedDataSets[DeletedIndex];
            if DataArray.Name = TestDataArray.Name then
            begin
              ChildDataArray := TestDataArray;
              break;
            end;
          end;
        end;
        if ChildDataArray = nil then
        begin
          ChildDataArray := ChildManager.CreateNewDataArray(
            TDataArrayType(DataArray.ClassType),
            DataArray.Name, DataArray.Formula, DataArray.DisplayName,  DataArray.Lock,
            DataArray.DataType, DataArray.EvaluatedAt, DataArray.Orientation,
            DataArray.Classification);
          ChildDataArray.AssignProperties(DataArray);
          ChildManager.FCustomModel.UpdateDataArrayDimensions(ChildDataArray);
//          ChildGrid := ChildManager.FCustomModel.Grid;
//          ChildDataArray.UpdateDimensions(ChildGrid.LayerCount,
//            ChildGrid.RowCount, ChildGrid.ColumnCount);
        end;
        DataArray.TalksTo(ChildDataArray);
        NewAddedList.Add(ChildDataArray);
      end;
      ChildManager.HandleAddedDataArrays(NewAddedList);
    finally
      NewAddedList.Free;
    end;
  end;
end;

procedure TDataArrayManager.HandleDeletedDataArrays(DeletedDataSetList: TList);
var
  Index: Integer;
  DataArray: TDataArray;
  ChildIndex: Integer;
  ChildManager: TDataArrayManager;
  NewDeletedList: TList;
  DataArrayIndex: integer;
  ChildDataArray: TDataArray;
begin
  for Index := DeletedDataSetList.Count - 1 downto 0 do
  begin
    DataArray := DeletedDataSetList[Index];
    FCustomModel.CrossSection.RemoveDataArray(DataArray);
    FCustomModel.RemoveVariables(DataArray);
    ExtractDataSet(DataArray);

  end;
  FDeletedDataSets.Assign(DeletedDataSetList, laOr);
  for ChildIndex := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildManager := ChildDataArrayManagers[ChildIndex];
    NewDeletedList := TList.Create;
    try
      NewDeletedList.Capacity := DeletedDataSetList.Count;
      for DataArrayIndex := 0 to DeletedDataSetList.Count - 1 do
      begin
        DataArray := DeletedDataSetList[DataArrayIndex];
        ChildDataArray := ChildManager.GetDataSetByName(DataArray.Name);
        if ChildDataArray <> nil then
        begin
          NewDeletedList.Add(ChildDataArray);
        end;
      end;
      ChildManager.HandleDeletedDataArrays(NewDeletedList);
    finally
      NewDeletedList.Free;
    end;
  end;
end;

function TDataArrayManager.IndexOfBoundaryDataSet(DataSetName: string): integer;
begin
  result := IndexOfDataSetInList(DataSetName, FBoundaryDataSets);
end;

function TDataArrayManager.IndexOfDataSet(DataSetName: string): integer;
begin
  result := IndexOfDataSetInList(DataSetName, FDataSets);
end;

function TDataArrayManager.IndexOfDataSetInList(DataSetName: string;
  const List: TObjectList): integer;
var
  Index: integer;
  DataSet: TDataArray;
begin
  result := -1;
  DataSetName := UpperCase(DataSetName);
  for Index := 0 to List.Count - 1 do
  begin
    DataSet := List[Index] as TDataArray;
    if UpperCase(DataSet.Name) = DataSetName then
    begin
      result := Index;
      Exit;
    end;
  end;
end;

procedure TDataArrayManager.Invalidate;
begin
  FCustomModel.DoInvalidate(self);
end;

procedure TDataArrayManager.InvalidateAll3DDataSets;
var
  Index: Integer;
  DS: TDataArray;
begin
  for Index := 0 to DataSetCount - 1 do
  begin
    DS := DataSets[Index];
    if DS.Orientation = dso3D then
    begin
      if (DS.Name <> kNodeActive)
        or not (FCustomModel.ModelSelection in SutraSelection)
        or not FCustomModel.SutraMesh.UpdatingElevations then
      begin
        DS.Invalidate;
      end;
    end;
  end;
  for Index := 0 to BoundaryDataSetCount - 1 do
  begin
    DS := BoundaryDataSets[Index];
    if DS.Orientation = dso3D then
    begin
      DS.Invalidate;
    end;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].InvalidateAll3DDataSets;
  end;
end;

procedure TDataArrayManager.InvalidateAllDataSets;
var
  Index: Integer;
  DS: TDataArray;
begin
  for Index := 0 to DataSetCount - 1 do
  begin
    DS := DataSets[Index];
    DS.Invalidate;
  end;
  for Index := 0 to BoundaryDataSetCount - 1 do
  begin
    DS := BoundaryDataSets[Index];
    DS.Invalidate;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].InvalidateAllDataSets;
  end;
end;

procedure TDataArrayManager.InvalidateDataSetLookupList;
begin
  FreeAndNil(FDataSetLookUpList);
end;

procedure TDataArrayManager.InvalidateHguFormulaDataSets;
var
  Index: Integer;
  DS: TDataArray;
  Formula: string;
  HguFunctionNames: TStringList;
  NameIndex: Integer;
begin
  HguFunctionNames := TStringList.Create;
  try
    HguFunctionNames.Add(KHGU_HK);
    HguFunctionNames.Add(KHGU_HANI);
    HguFunctionNames.Add(KHGU_VK);
    HguFunctionNames.Add(KHGU_VANI);
    HguFunctionNames.Add(KHGU_SS);
    HguFunctionNames.Add(KHGU_SY);
    HguFunctionNames.Add(KHGU_KDEP);

    for Index := 0 to DataSetCount - 1 do
    begin
      DS := DataSets[Index];
      Formula := UpperCase(DS.Formula);
      for NameIndex := 0 to HguFunctionNames.Count - 1 do
      begin
        if Pos(HguFunctionNames[NameIndex], Formula) >= 1 then
        begin
          DS.Invalidate;
          break;
        end;
      end;
    end;
    for Index := 0 to BoundaryDataSetCount - 1 do
    begin
      DS := BoundaryDataSets[Index];
      Formula := DS.Formula;
      for NameIndex := 0 to HguFunctionNames.Count - 1 do
      begin
        if Pos(HguFunctionNames[NameIndex], Formula) >= 1 then
        begin
          DS.Invalidate;
          break;
        end;
      end;
    end;
    for Index := 0 to ChildDataArrayManagerCount - 1 do
    begin
      ChildDataArrayManagers[Index].InvalidateHguFormulaDataSets;
    end;
  finally
    HguFunctionNames.Free;
  end;
end;

procedure TDataArrayManager.RemoveDataSet(ADataArray: TDataArray);
var
  LocalModel: TPhastModel;
  AChildModel: TChildModel;
  ChildArray: TDataArray;
  ChildIndex: Integer;
begin

  if FCustomModel is TPhastModel then
  begin
    LocalModel := TPhastModel(FCustomModel);
    for ChildIndex := 0 to LocalModel.ChildModels.Count - 1 do
    begin
      AChildModel := LocalModel.ChildModels[ChildIndex].ChildModel;
      if AChildModel <> nil then
      begin
        ChildArray := AChildModel.DataArrayManager.GetDataSetByName(ADataArray.Name);
        AChildModel.DataArrayManager.RemoveDataSet(ChildArray);
      end;
    end;
  end;
  FDataSets.Remove(ADataArray);
end;

procedure TDataArrayManager.RemoveDataSetFromLookUpList(
  const DataSet: TDataArray);
var
  Dummy: pointer;
begin
  if (FDataSetLookUpList <> nil) and (DataSet.Name <> '') then
  begin
    if FDataSetLookUpList.Search(DataSet.Name, Dummy) then
    begin
      FDataSetLookUpList.Delete(DataSet.Name);
    end;
  end;
end;

procedure TDataArrayManager.SetDataSetsCapacity(const Value: integer);
begin
  if FDataSets <> nil then
  begin
    FDataSets.Capacity := Value;
  end;
end;

procedure TDataArrayManager.UnlinkDeletedDataSets;
var
  Index: Integer;
  DataSet: TDataArray;
  ChildModel: TChildModel;
  ParentDataSet: TDataArray;
begin
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
    DataSet.StopTalkingToAnyone;
    DataSet.SetModelToNil;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    ChildDataArrayManagers[Index].UnlinkDeletedDataSets;
  end;
  if FCustomModel is TChildModel then
  begin
    ChildModel := TChildModel(FCustomModel);
    if ChildModel.ParentModel <> nil then
    begin
      for Index := 0 to DataSetCount - 1 do
      begin
        DataSet := DataSets[Index];
        ParentDataSet := ChildModel.ParentModel.
          DataArrayManager.GetDataSetByName(DataSet.Name);
        if (ParentDataSet <> nil) then
        begin
          ParentDataSet.StopsTalkingTo(DataSet);
        end;
      end;
    end;
  end;
end;

procedure TDataArrayManager.UpdateClassifications;
var
  DataArray: TDataArray;
  Packages: TModflowPackages;
begin
  if FLongitudinalDispersivityIndex >= 0 then
  begin
    if FCustomModel.ModelSelection = msPhast then
    begin
      FDataArrayCreationRecords[FLongitudinalDispersivityIndex].Classification := StrHydrology;
    end
    else
    begin
      FDataArrayCreationRecords[FLongitudinalDispersivityIndex].Classification := StrMT3DMS_GWT_Classificaton;
    end;
    DataArray := GetDataSetByName(FDataArrayCreationRecords[FLongitudinalDispersivityIndex].Name);
    if DataArray <> nil then
    begin
      DataArray.Classification :=
        FDataArrayCreationRecords[FLongitudinalDispersivityIndex].Classification;
    end;
  end;

  if FTransverseDispersivityIndex >= 0 then
  begin
    if FCustomModel.ModelSelection = msPhast then
    begin
      FDataArrayCreationRecords[FTransverseDispersivityIndex].Classification := StrHydrology;
    end
    else
    begin
      FDataArrayCreationRecords[FTransverseDispersivityIndex].Classification := StrGWTClassification;
    end;
    DataArray := GetDataSetByName(FDataArrayCreationRecords[FTransverseDispersivityIndex].Name);
    if DataArray <> nil then
    begin
      DataArray.Classification :=
        FDataArrayCreationRecords[FTransverseDispersivityIndex].Classification;
    end;
  end;

  if FPorosityIndex >= 0 then
  begin
    if FCustomModel.ModelSelection = msPhast then
    begin
      FDataArrayCreationRecords[FPorosityIndex].Classification := StrHydrology;
    end
    else
    begin
      Packages := FCustomModel.ModflowPackages;
      if (Packages.Mt3dBasic.IsSelected or FCustomModel.GwtUsed) and Packages.ModPath.IsSelected then
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrModpath + ' \ ' + StrMT3DMS_GWT_Classificaton;
      end
      else if Packages.Mt3dBasic.IsSelected or FCustomModel.GwtUsed then
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrMT3DMS_GWT_Classificaton;
      end
      else if Packages.ModPath.IsSelected then
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrModpath;
      end
      else
      begin
        FDataArrayCreationRecords[FPorosityIndex].Classification := StrHydrology;
      end;
    end;
    DataArray := GetDataSetByName(FDataArrayCreationRecords[FPorosityIndex].Name);
    if DataArray <> nil then
    begin
      DataArray.Classification :=
        FDataArrayCreationRecords[FPorosityIndex].Classification;
    end;
  end;
end;

procedure TDataArrayManager.UpdateDataSetDimensions;
var
  Index: integer;
  DataSet: TDataArray;
  Grid: TCustomModelGrid;
  Manager: TDataArrayManager;
begin
  for Index := 0 to LocalCount - 1 do
  begin
    DataSet := DataSets[Index];
    FCustomModel.UpdateDataArrayDimensions(DataSet);
  end;
  for Index := 0 to BoundaryDataSetCount - 1 do
  begin
    DataSet := BoundaryDataSets[Index];
    FCustomModel.UpdateDataArrayDimensions(DataSet);
  end;
  for Index := 0 to FDeletedDataSets.Count - 1 do
  begin
    DataSet := FDeletedDataSets[Index];
    FCustomModel.UpdateDataArrayDimensions(DataSet);
  end;
  Grid := FCustomModel.Grid;
  if Grid <> nil then
  begin
    Grid.NeedToRecalculateTopCellColors := True;
    Grid.NeedToRecalculateFrontCellColors := True;
    Grid.NeedToRecalculateSideCellColors := True;
  end;
  for Index := 0 to ChildDataArrayManagerCount - 1 do
  begin
    Manager := ChildDataArrayManagers[Index];
    if Manager <> nil then
    begin
      Manager.UpdateDataSetDimensions;
    end;
  end;
end;



function TDataArrayManager._AddRef: Integer;
begin
  result := 1;
end;

function TDataArrayManager._Release: Integer;
begin
  result := 1;
end;

end.
