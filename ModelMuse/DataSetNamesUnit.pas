unit DataSetNamesUnit;

interface

const
  // @name is the name of the @link(TDataArray) that specifies whether an
  // element in PHAST is active or not.
  rsActive = 'Active';
  K_IDOMAIN = 'IDOMAIN';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the X direction.
  rsKx = 'Kx';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Y direction.
  rsKy = 'Ky';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Z direction.
  rsKz = 'Kz';
  // @name is the name of the @link(TDataArray) that specifies
  // the porosity.
  rsPorosity = 'Porosity';
  // @name is the name of the @link(TDataArray) that specifies
  // the specific storage.
  rsSpecific_Storage = 'Specific_Storage';
  // @name is the name of the @link(TDataArray) that specifies
  // the longitudinal dispersivity.
  rsLong_Dispersivity = 'Longitudinal_Dispersivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the horizontal transverse dispersivity.
  rsHorizontal_Transv_Dispersivity = 'Horizontal_Transverse_Dispersivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the vertical transverse dispersivity.
  rsVertical_Transv_Dispersivity = 'Vertical_Transverse_Dispersivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial head.
  rsInitial_Head = 'Initial_Head';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial water table.
  rsInitial_Water_Table = 'Initial_Water_Table';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solution.
  rsChemistry_Initial_Solution = 'Chemistry_Initial_Solution';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial equilibrium phases.
  rsChemistry_Initial_Equilibrium_Phases =
    'Chemistry_Initial_Equilibrium_Phases';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial surface properties.
  rsChemistry_Initial_Surface = 'Chemistry_Initial_Surface';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial exchange properties.
  rsChemistry_Initial_Exchange = 'Chemistry_Initial_Exchange';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial gas phase properties.
  rsChemistry_Initial_Gas_Phase = 'Chemistry_Initial_Gas_Phase';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solid-solution properties.
  rsChemistry_Initial_Solid_Solutions = 'Chemistry_Initial_Solid_Solutions';
  // @name is the name of the @link(TDataArray) that specifies
  // the initial kinetic properties.
  rsChemistry_Initial_Kinetics = 'Chemistry_Initial_Kinetics';
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print Chemistry" distribution.
  rsPrint_Chemistry = 'Print_Chemistry';
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print XYZ Chemistry" distribution.
  rsPrint_XYZ_Chemistry = 'Print_XYZ_Chemistry';
  // @name indicates the layer number for a MODFLOW reservoir.
  rsResLayer = 'Reservoir_Layer';
  // @name is the elevation of a MODFLOW reservoir.
  rsResBottom = 'Reservoir_Elevation';
  // @name is the hydraulic conductivity of a MODFLOW reservoir.
  rsResKv = 'Reservoir_Hydraulic_Conductivity';
  // @name is the bed thickness of a MODFLOW reservoir.
  rsResBedThickness = 'Reservoir_Bed_Thickness';

  // @name is the lake number of a MODFLOW lake.
  rsLakeID = 'Lake_ID';
  // @name is the leakance of a MODFLOW lake.
  rsLakeLeakance = 'Lakebed_Leakance';

  rsModflowSpecifiedHead = 'Modflow_Specified_Head';

  // @name is the land surface in UZF. It is used to set IUZFBND.
  StrUzfLandSurface = 'Land_Surface';
  // @name set IUZFBND.
  StrUzfLayer = 'UZF_Layer';
  StrUzfDischargeRouting = 'Discharge_Routing';
  StrUzfVerticalK = 'Maximum_Unsaturated_Vertical_K';
  StrUzfBrooksCoreyEpsilon = 'Brooks_Corey_Epsilon';
  StrUzfSaturatedWaterContent = 'Saturated_Water_Content';
  StrUzfInitialUnsaturatedWaterContent = 'Initial_Unsaturated_Water_Content';
  StrUzfReisidualWaterContent = 'Residual_Water_Content';
  StrUzfSurfaceK = 'Uzf_Surficial_Hydraulic_Conductivity';
  StrUzfGage_1_and_2 = 'UZF_Gage_1_and_2';
  StrUzfGage3 = 'UZF_Gage3';

  StrUzfMf6BrooksCoreyEpsilon = 'UZF6_Brooks_Corey_Epsilon';
  StrUzfMf6InitialUnsaturatedWaterContent = 'UZF6_Initial_Unsaturated_Water_Content';
  StrUzfMf6ReisidualWaterContent = 'UZF6_Residual_Water_Content';
  StrUzfMf6SaturatedWaterContent = 'UZF6_Saturated_Water_Content';
  StrUzfMf6SurfaceDepressionDepth = 'UZF6_Surface_Depression_Depth';
  StrUzfMf6VerticalSaturatedK = 'UZF6_Vertical_Saturated_K';


  rsModflow_Initial_Head = 'Modflow_Initial_Head';
  rsModflow_CBKz = 'Confining_Bed_Kz';
  rsVerticalAnisotropy = 'Vertical_Anisotropy';
  rsHorizontalAnisotropy = 'Horizontal_Anisotropy';
  rsSpecificYield = 'Specific_Yield';
  rsWetDryThreshold = 'Wet_Dry_Threshold';
  rsWetDryFlag = 'Wet_Dry_Flag';
  rsWetDry = 'WetDry';

  StrModpathZone = 'Modpath_Zone';
  StrHufReferenceSurface = 'HUF_Reference_Surface';
  StrTransmissivity = 'Transmissivity';
  StrVerticalConductance = 'Vertical_Leakance';
  StrConfinedStorageCoe = 'Confined_Storage_Coefficient';

  StrHUFKxName = 'HUF_Kx';
  StrHUFKyName = 'HUF_Ky';
  StrHUFInterlayerKz = 'HUF_Interlayer_Kz';
  StrHUFSSName = 'HUF_SS';
  StrHUFAverageSYName = 'HUF_Average_SY';
  StrHUFSYName = 'HUF_SY';
  StrHUFSYTPName = 'HUF_SYTP';

  StrZones = 'Zones';
  StrGeostaticStress = 'Geostatic_Stress';
  StrSpecificGravityUns = 'Specific_Gravity_Unsaturated';
  StrSpecificGravitySat = 'Specific_Gravity_Saturated';
  StrInitialPreOffsets = 'Initial_Preconsolidation_Stress_Offset';
  StrInitialPreconsolida = 'Initial_Preconsolidation_Stress';

  STR_MT3DMS_Observation_Locations = 'MT3DMS_Observation_Locations';
  StrMT3DMSActive = 'MT3DMS_Active';
  rsBulkDensity = 'Bulk_Density';
  rsImmobPorosity = 'Immobile_Domain_Porosity';
  rsMT3DMS_Layer_Thickness = 'MT3DMS_Layer_Thickness';

  KModpathBudget = 'Modpath_Budget';
  KModpathRetardation = 'Modpath_Retardation';

  KNodalPorosity = 'Nodal_Porosity';
  KNodalThickness = 'Nodal_Thickness';
  KUnsatRegionNodes = 'Region_Nodes';
//  KUnsatRegionNodes = 'Unsat_Region_Nodes';
  KUnsatRegionElements = 'Region_Elements';
//  KUnsatRegionElements = 'Unsat_Region_Elements';
  KMaximumPermeability = 'Maximum_Permeability';
  KMiddlePermeability = 'Middle_Permeability';
  KMinimumPermeability = 'Minimum_Permeability';
  KMaximumK = 'Maximum_Hydraulic_Conductivity';
  KMiddleK = 'Middle_Hydraulic_Conductivity';
  KMinimumK = 'Minimum_Hydraulic_Conductivity';
  KHorizontalAngle = 'Angle_Horizontal';
  KVerticalAngle = 'Angle_Vertical';
  KRotationalAngle = 'Angle_Rotational';
  KMaxLongitudinalDisp = 'Longitudinal_Dispersivity_Max_Dir';
  KMidLongitudinalDisp = 'Longitudinal_Dispersivity_Mid_Dir';
  KMinLongitudinalDisp = 'Longitudinal_Dispersivity_Min_Dir';
  KMaxTransverseDisp = 'Transverse_Dispersivity_Max_Dir';
  KMidTransverseDisp = 'Transverse_Dispersivity_Mid_Dir';
  KMinTransverseDisp = 'Transverse_Dispersivity_Min_Dir';
  KInitialPressure = 'InitialPressure';
  KInitialConcentration = 'InitialConcentration';
  KInitialTemperature = 'InitialTemperature';
//  KFarmID = 'Farm_ID';
  KSoilID = 'Soil_ID';
  KPipeDiameter = 'PipeDiameter';
  KTortuosity = 'Tortuosity';
  KRoughnessHeight = 'RoughnessHeight';
  KLowerCriticalR = 'LowerCriticalR';
  KUpperCriticalR = 'UpperCriticalR';
  KPipeConductanceOrPer = 'PipeConductanceOrPermeabilty';
  KDrainableStorageWidth = 'DrainableStorageWidth';
  KCfpNodeElevation = 'CfpNodeElevation';
  KCfpFixedHeads = 'CfpFixedHeads';
  KCfpBoundaryType = 'CfpBoundaryType';
  KCfpLimitedFlowValue = 'CfpLimitedFlowValue';
  KCfpWellFlow = 'CfpWellFlow';
//  KCfpWellConductance = 'CfpWellConductance';
  KCfpCauchyHead = 'CfpCauchyHead';
  KCfpCauchyConductivity = 'CfpCauchyConductivity';
  KCfpCauchyLimitedInflow = 'CfpCauchyLimitedInflow';
  KCfpLimitedHead = 'CfpLimitedHead';


  KCfpValue2 = 'CfpValue2';
  KCfpValue3 = 'CfpValue3';
  KActive_Surface_Elevation = 'Active_Surface_Elevation';
  KEffectivePorosity = 'EffectivePorosity';
  KSourceFluidDensityZone = 'SourceFluidDensityZone';
  KSWI_Observation_Name = 'SWI_Observation_Name';
  KSwrReach = 'SWR_Reach_Number';
  KSwrReachGroup = 'SWR_Reach_Group_Number';
  KSwrRoutingType = 'SWR_Routing_Type';
  KSwrReachLength = 'SWR_Reach_Length';
  KReachString = 'Reach';
  KDepthRateIndex = 'DepthRateIndex';
//  KDistributedWithdrawals = 'Distributed_Withdrawals';
  KWithdrawals = 'Withdrawals';
  KCellType = 'Cell_Type';
  KConvertible = 'Convertible';
  KSUTRAInitialLakeS = 'SUTRA_Initial_Lake_Stage';
  KSUTRAInitialLakeU = 'SUTRA_Initial_Lake_U';
  KSUTRALakeRecharge = 'SUTRA_Lake_Recharge_Fraction_Diverted';
  KSUTRALakeDischarge = 'SUTRA_Lake_Discharge_Fraction_Diverted';
  KLake_Bottom = 'Lake_Bottom';
//  KInitialWaterContent = 'InitialWaterContent';
//  KSaturatedThickness = 'SaturatedThickness';
  KXT3DAngle1 = 'XT3D_Angle_1';
  KXT3DAngle2= 'XT3D_Angle_2';
  KXT3DAngle3 = 'XT3D_Angle_3';
//  KFootprint_Code = 'Footprint_Code';
  KSFTInitialConcentra = 'SFT_InitialConcentration';
  KSFTDispersion = 'SFT_Dispersion';
  KLakeTransportConce = 'Lake_Transport_Concentration';
  KNodeActive = 'Active_Node';

  KUztInitialConcentration = 'UZT_Initial_Concentration';

  // Sutra 4 node data sets
  KSolidMatrixComp = 'Solid_Matrix_Compressibility';
  KSolidGrainSpecificHeat = 'Solid_Grain_Specific_Heat';
  KSolidGrainDensity = 'Solid_Grain_Density';
  KZeroOrderProductionRateInLiquid = 'Zero_Order_Production_Rate_In_Liquid';
  KZeroOrderProductionRateInImmobile = 'Zero_Order_Production_Rate_In_Immobile';
  KFirstOrderProductionRateInLiquid = 'First_Order_Production_Rate_In_Liquid';
  KFirstOrderProductionRateInImmobile = 'First_Order_Production_Rate_In_Immobile';
  KZeroOrderProductionRateInIce = 'Zero_Order_Production_Rate_In_Ice';
  // Sutra 4 element data sets
  KScaledSolidGrainThermalConductivity = 'Scaled_Solid_Grain_Thermal_Conductivity';
  KScaledEffectiveAirThermalConductivity = 'Scaled_Effective_Air_Thermal_Conductivity';
  KDiffusionCoefficien = 'Diffusion_Coefficient';
  KVerticalTransverse = 'Vertical_Transverse_Dispersivity_Horizontal_Flow';
  KHorizontalTransvers = 'Horizontal_Transverse_Dispersivity_Horizontal_Flow';
  KLongitudinalDispersV = 'Longitudinal_Dispersivity_Vertical_Flow';
  KLongitudinalDispersH = 'Longitudinal_Dispersivity_Horizontal_Flow';

  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the leaky boundary condition on the top
  // view of the model.
  rsTopLeakyHydraulicConductivity = 'Top_Leaky_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the thickness associated with the leaky boundary condition on the top
  // view of the model.
  rsTopLeakyThickness = 'Top_Leaky_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the leaky boundary condition on the front
  // view of the model.
  rsFrontLeakyHydraulicConductivity = 'Front_Leaky_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the thickness associated with the leaky boundary condition on the front
  // view of the model.
  rsFrontLeakyThickness = 'Front_Leaky_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the leaky boundary condition on the side
  // view of the model.
  rsSideLeakyHydraulicConductivity = 'Side_Leaky_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the thickness associated with the leaky boundary condition on the side
  // view of the model.
  rsSideLeakyThickness = 'Side_Leaky_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity for the river boundary condition.
  rsRiverHydraulicConductivity = 'River_Hydraulic_Conductivity';
  // @name is the name of the @link(TDataArray) that specifies
  // the width for the river boundary condition.
  rsRiverWidth = 'River_Width';
  // @name is the name of the @link(TDataArray) that specifies
  // the depth for the river boundary condition.
  rsRiverDepth = 'River_Depth';
  // @name is the name of the @link(TDataArray) that specifies
  // the bed thickness for the river boundary condition.
  rsRiverBedThickness = 'River_Bed_Thickness';
  // @name is the name of the @link(TDataArray) that specifies
  // what type of solution is occurs with a specified head boundary.
  // (specified solution or associated solution).
  rsSolutionType = 'Specified_Head_Solution_Type';


  StrSpecifiedHead = 'Specified_Head';
  StrSpecifiedHeadSolution = 'Specified_Head_Solution';
  StrTopFluxBoundaryFlux = 'Top_Flux_Boundary_Flux';
  StrFrontFluxBoundaryFlux = 'Front_Flux_Boundary_Flux';
  StrSideFluxBoundaryFlux = 'Side_Flux_Boundary_Flux';
  StrTopFluxBoundaryAssocSoln = 'Top_Flux_Boundary_Associated_Solution';
  StrFrontFluxBoundaryAssocSoln = 'Front_Flux_Boundary_Associated_Solution';
  StrSideFluxBoundaryAssocSoln = 'Side_Flux_Boundary_Associated_Solution';
  StrTopLeakyBoundaryHead = 'Top_Leaky_Boundary_Head';
  StrTopLeakyBoundaryAssocSoln = 'Top_Leaky_Boundary_Associated_Solution';
  StrFrontLeakyBoundaryHead = 'Front_Leaky_Boundary_Head';
  StrFrontLeakyBoundaryAssocSoln = 'Front_Leaky_Boundary_Associated_Solution';
  StrSideLeakyBoundaryHead = 'Side_Leaky_Boundary_Head';
  StrSideLeakyBoundaryAssocSoln = 'Side_Leaky_Boundary_Associated_Solution';
  StrRiverHead = 'River_Head';
  StrRiverAssocSoln = 'River_Associated_Solution';
  StrWellInjectionRate = 'Well_Injection_Rate';
  StrWellSolution = 'Well_Solution';

  // names of @link(TModflowBoundaryDisplayTimeList)s

  //
  StrMODFLOWWellPumping = 'WEL Pumping Rate';
  StrMODFLOWGhbConductance = 'GHB Conductance';
  StrMODFLOWGhbHead = 'GHB Boundary Head';
  StrMODFLOWDrainConductance = 'DRN Conductance';
  StrMODFLOWDrainElevation = 'DRN Elevation';
  StrDrainDDRN = 'Drain DDRN';
  StrDRNDDRN = 'DRN DDRN';
  StrMODFLOWDrainReturnConductance = 'DRT Return Conductance';
  StrMODFLOWDrainReturnElevation = 'DRT Return Elevation';
  StrMODFLOWDrainReturnFraction = 'DRT Return Fraction';
  StrMODFLOWRiverConductance = 'RIV Conductance';
  StrMODFLOWRiverStage = 'RIV Stage';
  StrMODFLOWRiverBottom = 'RIV Bottom';
  StrMODFLOWCHDStartingHead = 'CHD Starting Head';
  StrMODFLOWCHDEndingHead = 'CHD Ending Head';
  StrModflowChdActive = 'CHD Active';
  StrMODFLOWEtsRateFraction = 'ETS Evapotranspiration Rate Fraction';
  StrMODFLOWEtsDepthFraction = 'ETS Evapotranspiration Depth Fraction';
  StrMODFLOWEtsRate = 'ETS Evapotranspiration Rate';
  StrMODFLOWEtsDepth = 'ETS Evapotranspiration Depth';
  StrMODFLOWEtsSurface = 'ETS Evapotranspiration Surface';
  StrMODFLOWEtsLayer = 'ETS Evapotranspiration Layer';

  StrMODFLOWEvtRate = 'EVT Evapotranspiration Rate';
  StrMODFLOWEvtDepth = 'EVT Evapotranspiration Depth';
  StrMODFLOWEvtSurface = 'EVT Evapotranspiration Surface';
  StrMODFLOWEvtLayer = 'EVT Evapotranspiration Layer';
  StrMODFLOWRchRate = 'RCH Rate';
  StrMODFLOWRchLayer = 'RCH Layer';

  StrModflowSfrSegment = 'SFR Segment';
  StrModflowSfrReach = 'SFR Reach';
  StrModflowSfrIcalc = 'SFR ICALC';
  StrModflowSfrReachLength = 'SFR Reach Length';
  StrModflowSfrStreamTop = 'SFR Streambed Top';
  StrModflowSfrStreamSlope = 'SFR Stream Slope';
  StrModflowSfrStreamThickness = 'SFR Streambed Thickness';
  StrModflowSfrStreamK = 'SFR Streambed Kv';
  StrModflowSfrSatWatCont = 'SFR Saturated Volumetric Water Content';
  StrModflowSfrInitWatCont = 'SFR Initial Volumentric Water Content';
  StrModflowSfrBrooksCorey = 'SFR Brooks Corey Exponent';
  StrModflowSfrVertK = 'SFR Max Unsaturated Kz';
  StrModflowSfrDownstreamSegment = 'SFR Outflow Segment';
  StrModflowSfrDiversionSegment = 'SFR Diversion Segment';
  StrModflowSfrIprior = 'SFR Diversion Priority';
  StrModflowSfrFlow = 'SFR Flow';
  StrModflowSfrRunoff = 'SFR Runoff';
  StrModflowSfrPrecipitation = 'SFR Precipitation';
  StrModflowSfrEvapotranspiration = 'SFR Evapotranspiration';
  StrModflowSfrChannelRoughness = 'SFR Channel Roughness';
  StrModflowSfrBankRoughness = 'SFR Bank Roughness';
  StrModflowSfrDepthCoefficient = 'SFR Depth Coefficient';
  StrModflowSfrDepthExponent = 'SFR Depth Exponent';
  StrModflowSfrWidthCoefficient = 'SFR Width Coefficient';
  StrModflowSfrWidthExponent = 'SFR Width Exponent';
  StrModflowSfrUpstreamHydraulicConductivity = 'SFR Upstream Hydraulic Conductivity';
  StrModflowSfrDownstreamHydraulicConductivity = 'SFR Downstream Hydraulic Conductivity';
  StrModflowSfrUpstreamWidth = 'SFR Upstream Width';
  StrModflowSfrDownstreamWidth = 'SFR Downstream Width';
  StrModflowSfrUpstreamThickness = 'SFR Upstream Thickness';
  StrModflowSfrDownstreamThickness = 'SFR Downstream Thickness';
  StrModflowSfrUpstreamElevation = 'SFR Upstream Elevation';
  StrModflowSfrDownstreamElevation = 'SFR Downstream Elevation';
  StrModflowSfrUpstreamDepth = 'SFR Upstream Depth';
  StrModflowSfrDownstreamDepth = 'SFR Downstream Depth';
  StrModflowSfrUpstreamSaturatedWaterContent =
    'SFR Upstream Saturated Water Content';
  StrModflowSfrDownstreamSaturatedWaterContent =
    'SFR Downstream Saturated Water Content';
  StrModflowSfrUpstreamInitialUnsaturatedWaterContent =
    'SFR Upstream Initial Unsaturated Water Content';
  StrModflowSfrDownstreamInitialUnsaturatedWaterContent =
    'SFR Downstream Initial Unsaturated Water Content';
  StrModflowSfrUpstreamBrooksCoreyExponent =
    'SFR Upstream Brooks Corey Exponent';
  StrModflowSfrDownstreamBrooksCoreyExponent =
    'SFR Downstream Brooks Corey Exponent';
  StrModflowSfrUpstreamMaxUnsaturatedKz = 'SFR Upstream Max Unsaturated Kz';
  StrModflowSfrDownstreamMaxUnsaturatedKz = 'SFR Downstream Max Unsaturated Kz';

  StrMAWWellElevation = 'MAW Flowing Well Elevation';
  StrMAWWellConductance = 'MAW Flowing Well Conductance';
  StrMAWWellRedLength = 'MAW Flowing Well Reduction Length';

  StrMAWWellElevationShort = 'MAW FW Elevation';
  StrMAWWellConductanceShort = 'MAW FW Conductance';
  StrMAWWellRedLengthShort = 'MAW FW Reduction Length';


  StrMAWWellRate = 'MAW Well Rate';
  StrMAWWellHead = 'MAW Well Head';
  StrMAWWellLimit = 'MAW Well Limit';
  StrMAWWellMinimumPum = 'MAW Well Minimum Pumping Rate';
  StrMAWWellMaximumPum = 'MAW Well Maximum Pumping Rate';
  StrMAWPumpElevation = 'MAW Pump Elevation';
  StrMAWScalingLength = 'MAW Scaling Length';

  StrModflowStrSegment = 'STR Segment';
  StrModflowStrDownstreamSegment = 'STR Outflow Segment';
  StrModflowStrDiversionSegment = 'STR Diversion Segment';
  StrModflowStrReach = 'STR Reach';
  StrSTRStreamTopElev = 'STR Streambed Top Elevation';
  StrSTRStreamBottomElev = 'STR Streambed Bottom Elevation';
  StrSTRStreamStage = 'STR Stream Head';
  StrSTRStreamConductance = 'STR Streambed Conductance';
  StrSTRStreamFlow = 'STR Flow into Upstream End';
  StrSTRStreamWidth = 'STR Width';
  StrSTRStreamSlope = 'STR Slope';
  StrSTRStreamRoughness = 'STR Roughness';

  StrUzfInfiltrationRate = 'UZF Infiltration Rate';
  StrUzfExtinctionDepth = 'UZF Extinction Depth';
  StrUzfWaterContent = 'UZF Water Content';
  StrUzfEtDemand = 'UZF ET Demand';
  StrMODFLOWHeadObservations = 'Head Observations';
  StrMt3dTobConcObservations = 'Transport Observations';

  StrUzfMf6InfiltrationRate = 'UZF MF6 Infiltration Rate';
  StrUzfMf6PotentialET = 'UZF MF6 Potential ET';
  StrUzfMf6ExtinctionDepth = 'UZF Mf6 Extinction Depth';
  StrUzfMf6ExtinctionWaterContent = 'UZF Mf6 Extinction Water Content';
  StrUzfMf6AirEntryPotential = 'UZF Mf6 Air Entry Potential';
  StrUzfMf6RootPotential = 'UZF Mf6 Root Potential';
  StrUzfMf6RootActivity = 'UZF Mf6 Root Activity';

  StrCSUBStressOffset = 'CSUB Stress Offset';
  StrMVRValue = 'MVR Value';

  StrWellRadius = 'Well Radius';
  StrSkinRadius = 'Skin Radius';
  StrSkinK = 'Skin K';
  StrB = 'B';
  StrC = 'C';
  StrP = 'P';
  StrCellToWellConductance = 'Cell to Well Conductance';
  StrPartialPenetration = 'Partial Penetration Fraction';
  StrMT3DMSSSMConcentra = 'MT3DMS SSM Concentration';
  StrMt3dRechConcentrat = 'UZT Recharge Concentration';
  StrMt3dUnsatConcentrat = 'UZT Unsaturated ET Concentration';
  StrMt3dSatConcentrat = 'UZT Saturated ET Concentration';
  StrMt3dSsmRechConcentrat = 'SSM UZF Recharge Concentration';
  StrMt3dSsmSinkConcentrat = 'SSM UZF Sink Concentration';

  StrFarmEvap = 'Farm Reference Evaportranspiration';
  StrFarmPrecip = 'Farm Precipitation';
  StrFarmCropID = 'Farm Crop ID';
  StrFarmID2 = 'Water Balance Subregion ID';
  StrFarmMaxPumpRate = 'Farm Well Maximum Pumping Rate';
  StrFarmWellFarmID = 'Farm Well Farm-ID';
  StrFarmWellPumpRequired = 'Farm Well Pump Only If Required';
  StrLandUseID = 'Land Use ID';
  StrPotentialEvaporatio = 'Potential Evaporation Bare';
  StrFMP4Efficiency = 'FMP4 Efficiency';
  StrFMP4EfficiencyImpr = 'FMP4 Efficiency Improvement';
  StrFMP4BareRunoffFra = 'FMP4 Bare Runoff Fraction';
  StrFMP4BarePrecipitat = 'FMP4 Bare Precipitation Consumption Fraction';
  StrFMP4AddedDemandRu = 'FMP4 Added Demand Runoff Split';
  StrFMP4DirectRecharge = 'FMP4 Direct Recharge';
  StrFMP4PrecipitationP = 'FMP4 Precipitation Potential Consumption';
  StrFMP4NonRoutedDeli = 'FMP4 Non-Routed Delivery Infiltration Location';
  StrFMP4LandUseAreaF = 'FMP4 Land Use Area Fraction';
  StrFMP4CropCoefficien = 'FMP4 Crop Coefficient';
  StrFMP4ConsumptiveUse = 'FMP4 Consumptive Use';
  StrFMP4Irrigation = 'FMP4 Irrigation';
  StrFMP4RootDepth = 'FMP4 Root Depth';
  StrFMP4TranspirationF = 'FMP4 Transpiration Fraction';
  StrFMP4EvaporationIrr = 'FMP4 Evaporation Irrigation Fraction';
  StrFMP4FractionOfExc = 'FMP4 Fraction of Excess Precip to SW';
  StrFMP4FractionOfExcIrrig = 'FMP4 Fraction of Excess Irrig to SW';
  StrFMP4AddedDemand = 'FMP4 Added Demand';

  StrCfpRecharge = 'Conduit Recharge';
  StrSWR_Rain = 'SWR Rain';
  StrSWR_Evap = 'SWR Evaporation';
  StrSWR_LatInflow = 'SWR Lateral Inflow';
  StrSWR_Stage = 'SWR Stage';
  StrSWR_DirectRunoffReach = 'SWR Direct Runoff Reach';
  StrSWR_DirectRunoffValue = 'SWR Direct Runoff Value';
  StrSWR_Vertical_Offset = 'SWR Vertical Offset';
  StrSWR_BoundaryType = 'SWR Boundary Type';
  StrSWR_GeometryNumber = 'SWR Geometry Number';

  StrMNW1DesiredPumping = 'MNW1 Desired Pumping Rate';
  StrMNW1WaterQuality = 'MNW1 Water Quality';
  StrMNW1WellRadius = 'MNW1 Well Radius';
  StrMNW1Conductance = 'MNW1 Conductance';
  StrMNW1Skin = 'MNW1 Skin';
  StrMNW1LimitingWater = 'MNW1 Limiting Water Level';
  StrMNW1ReferenceEleva = 'MNW1 Reference Elevation';
  StrMNW1WaterQualityG = 'MNW1 Water Quality Group';
  StrMNW1NonlinearLoss = 'MNW1 Nonlinear Loss Coefficient';
  StrMNW1MinimumPumping = 'MNW1 Minimum Pumping Rate';
  StrMNW1ReactivationPu = 'MNW1 Reactivation Pumping Rate';

  StrRipGroundElevation = 'RIP Ground Elevation';

  StrSFR6Inflow = 'SFR6 Inflow';
  StrSFR6Rainfall = 'SFR6 Rainfall';
  StrSFR6Evaporation = 'SFR6 Evaporation';
  StrSFR6Runoff = 'SFR6 Runoff';
  StrSFR6UpstreamFracti = 'SFR6 Upstream Fraction';
  StrSFR6Stage = 'SFR6 Stage';
  StrSFR6Roughness = 'SFR6 Roughness';
  StrSFR6StreamStatus = 'SFR6 Stream Status';
  StrSFR6ReachNumber = 'SFR6 Reach Number';

  KReachLengthSFR = 'SFR6_ReachLength';
  KReachWidthSFR6 = 'SFR6_ReachWidth';
  KGradientSFR6 = 'SFR6_Gradient';
  KStreambedTopSFR6 = 'SFR6_StreambedTop';
  KStreambedThicknessSFR6 = 'SFR6_StreambedThickness';
  KHydraulicConductivitySFR6 = 'SFR6_HydraulicConductivity';
  KMAWRadius = 'MAW_Radius';
  KMAWBottom = 'MAW_Bottom';
  KMAWInitialHead = 'MAW_Initial_Head';
  KMAWScreenTop = 'MAW_Screen_Top';
  KMAWScreenBottom = 'MAW_Screen_Bottom';
  KMAWSkinK = 'MAW_Skin_K';
  KMAWSkinRadius = 'MAW_Skin_Radius';
//  KLakeMf6 = 'LakeMf6';
  KMf6LakeConnectionTypes = 'MF6_Lake_Connection_Types';
  KLake_Bed_Leakance = 'Lake_Bed_Leakance';
  KLake_Bed_Thickness = 'Lake_Bed_Thickness';
  KLake_Bottom_Elevation = 'Lake_Bottom_Elevation';
  KLake_Top_Elevation = 'Lake_Top_Elevation';
  KLake_Connection_Length = 'Lake_Connection_Length';
  KLake_Connection_Width = 'Lake_Connection_Width';
  KKyOverKx = 'Ky_Over_Kx';
  KKzOverKx = 'Kz_Over_Kx';
  KInitialElasticReco = 'Initial_Elastic_Recompression_Index';
  KInitialCoarsePoros = 'Initial_Coarse_Porosity';
  KMoistSpecificGravi = 'Moist_Specific_Gravity';
  KSaturatedSpecificG = 'Saturated_Specific_Gravity';
  KInitialElasticSpec = 'Initial_Elastic_Specific_Storage';
  KFarmID = 'Farm_ID';
  KRefET = 'Reference_ET';
  KPrecipitation = 'Precipitation';
  KLand_Use_ID = 'Land_Use_ID';
  KEfficiency = 'Efficiency';
  KEfficiencyImprovement = 'Efficiency_Improvement';
  KBareRunoffFraction = 'Bare_Runoff_Fraction';
  KAddedDemandRunoffSplit = 'Added_Demand_Runoff_Split';
  KBarePrecipitationConsumptionFraction = 'Bare_Precipitation_Consumption_Fraction';
  KCapillary_Fringe = 'Capillary_Fringe';
  KSurfaceK = 'Surface_Vert_K';
  KPotential_Evap_Bare = 'Potential_Evap_Bare';
  KDirectRecharge = 'Direct_Recharge';
  KPrecipPotConsumption = 'Precip_Pot_Consumption';
  KNRD_Infiltration_Location = 'NRD_Infiltration_Location';
  KCropCoefficient = 'Crop_Coefficient';
  KLandUseAreaFraction = 'Land_Use_Area_Fraction';
  KConsumptiveUse = 'Consumptive_Use';
  KIrrigation = 'Irrigation';
  KRootDepth = 'Root_Depth';
  KGWRootInteraction = 'GW_Root_Interaction';
  KTranspirationFraction = 'Transpiration_Fraction';
  KEvaporationIrrigationFraction = 'Evaporation_Irrigation_Fraction';
  KFractionOfPrecipToSurfaceWater = 'Frac_Unconsumed_Precip_to_SW';
  KFractionOfIrrigToSurfaceWater = 'Frac_Unconsumed_Irrig_to_SW';
  KAddedDemand = 'Added_Demand';
  KCropHasSalinityDemand = 'Crop_Has_Salinity_Demand';
  KLandUseCellsToPrint = 'Land_Use_Cells_ToPrint';
  OldLongDispersivityName = 'Long_Dispersivity';
  OldHorizontal_Transv_Dispersivity = 'Horizontal_Transv_Dispersivity';
  OldVertical_Transv_Dispersivity = 'Vertical_Transv_Dispersivity';
  kHufThickness = '_Thickness';
  StrTransientKx = 'Transient Kx';
  StrTransientKy = 'Transient Ky';
  StrTransientKyKx = 'Transient Ky/Kx';
  StrTransientKz = 'Transient Kz';
  StrTransientKzKx = 'Transient Kz/Kx';
  StrTransientSS = 'Transient SS';
  StrTransientSY = 'Transient SY';

resourcestring
  StrActiveSurfaceEleva = 'Active_Surface_Elevation';
  StrSFTInitialConcentra = KSFTInitialConcentra;
  StrUztInitialConcentration = KUztInitialConcentration;
  StrSWI = 'SWI';
  StrMODFLOWSWIZETA = 'MODFLOW SWI: ZETA';
  StrPHASTMEDIAactive = 'PHAST: MEDIA-active';
  StrMODFLOWBASIBOUND = 'MODFLOW BAS: IBOUND';
  StrPHASTMEDIAKx = 'PHAST: MEDIA-Kx';
  StrMODFLOWLPFUPWHK = 'MODFLOW LPF, UPW: HK';
  StrMODFLOWBCFTRANHY = 'MODFLOW BCF: TRAN,HY';
  StrMODFLOW6NPFK = 'MODFLOW 6 NPF: K';
  StrPHASTMEDIAKy = 'PHAST: MEDIA-Ky';
  StrMODFLOWLPFUPWHA = 'MODFLOW LPF, UPW: HANI';
  StrMODFLOWHUFAndBCF = 'MODFLOW HUF and BCF: (not used)';
  StrMODFLOW6NPFK22 = 'MODFLOW 6 NPF: K22';
  StrPHASTMEDIAKz = 'PHAST: MEDIA-Kz';
  StrMODFLOWLPFUPWVK = 'MODFLOW LPF, UPW: VKA';
  StrMODFLOWBCFVcont = 'MODFLOW BCF: Vcont';
  StrMODFLOW6NPFK33 = 'MODFLOW 6 NPF: K33';
  StrPHASTMEDIAporosit = 'PHAST: MEDIA-porosity';
  StrMODPATHBasicData = 'MODPATH: Basic Data File: Item 7: POR';
  StrMT3DMSBTNPackage = 'MT3DMS BTN Package: PRSITY';
  StrMODPATHVersion7 = 'MODPATH Version 7';
  StrMODFLOW6Structure = 'MODFLOW 6, Structured Discretization package: IDOM' +
  'AIN';
  Str1ActiveCells = '1 = Active cells';
  Str0InactiveCells = '0 = Inactive cells';
  Str1VerticalPasst = '-1 = Vertical pass-through cells';
  StrTheDefaultFormula = 'The default formula sets IDOMAIN to -1 for cells i' +
  'n which the Active data set is true and the layer height is less than or ' +
  'equal to zero.';
  StrMODFLOW6NPFIcel = 'MODFLOW 6, NPF: icelltype';
  Str0ConstantCellThi = '0: constant cell thickness';
  Str0CalculatedCell = '>0: calculated cell thickness';
  Str0ConstantOrCalc = '<0: constant or calculated cell thickness depending ' +
  'on THICKSTRT option';
  StrSpecifiedWithdrawal = 'Specified withdrawals for WellFootprint model (L' +
  '^3/T)';
  StrRatePerUnitAreaO = 'Rate per unit area of distributed withdrawal (L/T)';
  StrUnits1L = 'Units = 1/L';
  StrPHASTMEDIAspecificStorage = 'PHAST: MEDIA-specific_storage';
  StrMODFLOWLPFUPWSs = 'MODFLOW LPF, UPW: Ss';
  StrMODFLOWBCFSf1 = 'MODFLOW BCF: Sf1';
  StrMODFLOWNWTUPWSs = 'MODFLOW-NWT UPW: Ss';
  StrMODFLOW6STOSs = 'MODFLOW 6 STO: ss';
  StrPHASTMEDIAlongitu = 'PHAST: MEDIA-longitudinal_dispersivity; ' + sLineBreak +
  'MT3DMS: Dispersion Package Data Sets C1: AL' + sLineBreak +
  'MODFLOW 6 Dispersion Package Data Set ALH';
  StrPHASTMEDIAhorizon = 'PHAST: MEDIA-horizontal_dispersivity';
  StrPHASTMEDIAvertica = 'PHAST: MEDIA-vertical_dispersivity;' + sLineBreak +
  'MODFLOW 6, Dispersion Package Data Set ATV';
  StrPHASTHEADIChead = 'PHAST: HEAD_IC-head';
  StrSUTRAICSPVEC = 'SUTRA, ICS: PVEC';
  StrPHASTHEADICwater = 'PHAST: HEAD_IC-water_table';
  StrPHASTCHEMISTRYIC_solution = 'PHAST: CHEMISTRY_IC-solution';
  StrPHASTCHEMISTRYIC_Equilibrium = 'PHAST: CHEMISTRY_IC-equilibrium_phases';
  StrPHASTCHEMISTRYIC_surface = 'PHAST: CHEMISTRY_IC-surface';
  StrPHASTCHEMISTRYIC_exchange = 'PHAST: CHEMISTRY_IC-exchange';
  StrPHASTCHEMISTRYIC_gas_phase = 'PHAST: CHEMISTRY_IC-gas_phase';
  StrPHASTCHEMISTRYIC_solid_solutions = 'PHAST: CHEMISTRY_IC-solid_solutions';
  StrPHASTCHEMISTRYIC_kinetics = 'PHAST: CHEMISTRY_IC-kinetics';
  StrPHASTPRINTLOCATIO_chemistry = 'PHAST: PRINT_LOCATIONS-chemistry';
  StrPHASTPRINTLOCATIO_xyz_chemistry = 'PHAST: PRINT_LOCATIONS-xyz_chemistry';
  StrRESISRESL = 'RES: ISRESL (Note: this represents the MODFLOW layer. If the'
  + ' model is a quasi-3D model, the model layer and MODFLOW layer may not be '
  + 'the same.)';
  StrRESBRES = 'RES: BRES';
  StrRESHCres = 'RES: HCres';
  StrRESRbthck = 'RES: Rbthck';
  StrLAKLKARR = 'LAK: LKARR';
  StrLAKBDLKNC = 'LAK: BDLKNC';
  StrUZFIUZFBNDViaTh = 'UZF: IUZFBND (via the formula for ' + StrUzfLayer + ')'
    + '; FMP2: GSURF';
  StrBASIBOUND = 'BAS: IBOUND';
  StrUZFIUZFBND = 'UZF: IUZFBND' + sLineBreak + 'MT3D-USGS UZT: IUZFBND';
  StrUZFIRUNBND = 'UZF: IRUNBND';
  StrUZFVKS = 'UZF: VKS';
  StrUZFEPS = 'UZF: EPS';
  StrUZFTHTS = 'UZF: THTS';
  StrUZFTHTI = 'UZF: THTI';
  StrUZFTHTR = 'UZF: THTR';
  StrUZFSURFK = 'UZF: SURFK';
  StrUZFIUZOPT = 'UZF: IUZOPT';
  StrBASSTRT = 'BAS: STRT';
  StrLPFUPWVKCBBCF = 'LPF, UPW: VKCB; BCF: VCONT';
  StrLPFUPWVKA = 'LPF, UPW: VKA';
  StrLPFUPWHANI = 'LPF, UPW: HANI';
  StrLPFUPWSy = 'LPF, UPW: Sy; ';
  StrBCFSf1 = 'BCF: Sf1';
  StrMODFLOW6STOSy = 'MODFLOW 6, STO: sy';
  StrBCFLPFHUFWETDR = 'BCF, LPF, HUF: WETDRY';
  StrAValueLess0Indicate = 'A value < 0 indicates that only the cell below t' +
  'he dry cell can cause the dry cell to become active again.';
  StrAValueGreat0Indicate = 'A value > 0 indicates that the cell below the d' +
  'ry cell or the cells next to the dry cell can cause the dry cell to becom' +
  'e active again.';
  StrAValueEq0Indicate = 'A value = 0 indicates that the dry cell can not be' +
  'come active again.';
  StrMODPATHV5IBOUND = 'MODPATH v5: IBOUND';
  StrMODPATHV6Simulatio = 'MODPATH v6 Simulation File Item 32: Zones';
  StrMODFLOWKDEPPackag = 'MODFLOW, KDEP package: RS';
  StrBCFTran = 'BCF: Tran';
  StrBCFVCONTDataSet = 'BCF: VCONT';
  StrStorativityDimen = '= Storativity (dimensionless)';
  StrDisplaysHUFKxValu = 'displays HUF Kx values';
  StrDisplaysHUFKyValu = 'displays HUF Ky values';
  StrDisplaysHUFKzValu = 'displays HUF Kz values between one layer and the l' +
  'ayer beneath';
  StrDisplaysHUFSSValu = 'displays HUF SS values';
  StrDisplaysAverageHUF = 'displays average HUF Sy values for a cell';
  StrDisplaysHUFSyValu = 'displays HUF Sy values for a cell';
  StrDisplaysHUFSYTPVa = 'displays HUF SYTP values for the top active cell';
  StrZONEBUDGETZoneArra = 'ZONEBUDGET Zone array (IZONE)';
  StrSWTGL0DataSet4 = 'SWT: GL0 (data set 4)';
  StrSWTSGMDataSet5 = 'SWT: SGM (data set 5)';
  StrSWTSGSDataSet6 = 'SWT: SGS (data set 6)';
  StrSWTPCSOFFDataSe = 'SWT: PCSOFF (data set 14)';
  StrSWTPCSDataSet1 = 'SWT: PCS (data set 15)';
  StrMT3DMSBasicPackage = 'MT3DMS Basic Package Data Sets 18 and 19';
  StrMT3DMSBasicPackage_ICBUND = 'MT3DMS Basic Package Data Sets 12: ICBUND';
  StrMT3DMSChemicalReac_RHOB = 'MT3DMS Chemical Reaction Package Data Set E2' +
  'A: RHOB';
  StrMT3DMSChemicalReac_PRSITY2 = 'MT3DMS Chemical Reaction Package Data Set' +
  ' E2B: PRSITY2';
  StrMT3DMSBasicPackage_DZ = 'MT3DMS Basic package DZ data set 10';
  StrMODPATHSimulationF_27 = 'MODPATH Simulation file, Item 27';
  StrMODPATHSimulationF_32_and_33 = 'MODPATH Simulation file, Items 32 and 3' +
  '3: RetardationFactor and RetardationFactorCB';
  StrSUTRADataSet14B_Por = 'SUTRA Data Set 14B: POR';
  StrSUTRADataSet14B_Z = 'SUTRA Data Set 14B: Z';
  StrSUTRADataSet14B_NREG = 'SUTRA Data Set 14B: NREG';
  StrSUTRADataSet15B_LREG = 'SUTRA Data Set 15B: LREG';
  StrSUTRADataSet15B_PMAX = 'SUTRA Data Set 15B: PMAX';
  StrSUTRADataSet15B_PMID = 'SUTRA Data Set 15B: PMID';
  StrSUTRADataSet15B_PMIN = 'SUTRA Data Set 15B: PMIN';
  StrSUTRADataSet15B_ALMAX = 'SUTRA Data Set 15B: ALMAX';
  StrSUTRADataSet15B_ALMID = 'SUTRA Data Set 15B: ALMID';
  StrSUTRADataSet15B_ALMIN = 'SUTRA Data Set 15B: ALMIN';
  StrSUTRADataSet15B_ATMAX = 'SUTRA Data Set 15B: ATMAX';
  StrSUTRADataSet15B_ATMID = 'SUTRA Data Set 15B: ATMID';
  StrSUTRADataSet15B_ATMIN = 'SUTRA Data Set 15B: ATMIN';
  StrSUTRAICSDataSet2_PVEC = 'SUTRA ICS Data Set 2: PVEC';
  StrSUTRAICSDataSet3_UVEC = 'SUTRA ICS Data Set 3: UVEC';
  StrFMPDataSet8SID = 'FMP Data Set 8: SID';
  StrMODFLOWCFPDataSet_DIAMETER = 'MODFLOW-CFP Data Set 25: DIAMETER';
  StrMODFLOWCFPDataSet_TORTUOSITY = 'MODFLOW-CFP Data Set 25: TORTUOSITY';
  StrMODFLOWCFPDataSet_RHEIGHT = 'MODFLOW-CFP Data Set 25: RHEIGHT';
  StrMODFLOWCFPDataSet_LCRITREY_P = 'MODFLOW-CFP Data Set 25: LCRITREY_P';
  StrMODFLOWCFPDataSet_TCRITREY_P = 'MODFLOW-CFP Data Set 25: TCRITREY_P';
  StrMODFLOWCFPDataSet_ELEVATION = 'MODFLOW-CFP Data Set 12: ELEVATION';
  StrMODFLOWCFPDataSet_N_HEAD = 'MODFLOW-CFP Data Set 27: N_HEAD';
  StrMODFLOWCFPDataSet_K_EXCHANGE = 'MODFLOW-CFP Data Set 25: K_EXCHANGE';
  StrMODFLOWSWIPackage_SSZ = 'MODFLOW, SWI Package: SSZ';
  StrMODFLOWSWIPackage_ISOURCE = 'MODFLOW, SWI Package: ISOURCE';
  StrMODFLOWSWIPackage_OBSNAM = 'MODFLOW, SWI Package: Data Set 8 OBSNAM';
  StrSWRProcessDataSet_IRCH4A = 'SWR Process Data Set 4a: IRCH4A'
    + sLineBreak
    + 'See also: DROMAP2D SWR1 Direct Runoff Subroutine Data Set 2'
    + sLineBreak
    + 'This data set shows the average of the'
    + sLineBreak
    + 'reach numbers in all the cells in a'
    + sLineBreak
    + 'particular row and column.';
  StrSWRProcessDataSet_IRGNUM = 'SWR Process Data Set 4a: IRGNUM';
  StrSWRProcessDataSet_IROUTETYPE = 'SWR Process Data Set 4a: IROUTETYPE';
  StrSWRProcessDataSet_RLEN = 'SWR Process Data Set 4a: RLEN';
  StrMODFLOW6STO_iconvert = 'MODFLOW 6, STO: iconvert' + sLineBreak
    + '0: confined' + sLineBreak
    + '>0: convertible';
  StrSUTRA30STGI = 'SUTRA 3.0: STGI';
  StrSUTRA30UWI = 'SUTRA 3.0: UWI';
  StrSUTRA30FRRO = 'SUTRA 3.0: FRRO';
  StrSUTRA30FDRO = 'SUTRA 3.0: FDRO';
  StrMT3DUSGSUZTWC = 'MT3D-USGS UZT WC';
  StrMT3DUSGSUZTSDH = 'MT3D-USGS UZT SDH';
  StrMODFLOW6NPFAngle1 = 'MODFLOW 6 NPF angle1';
  StrMODFLOW6NPFAngle2 = 'MODFLOW 6 NPF angle2';
  StrMODFLOW6NPFAngle3 = 'MODFLOW 6 NPF angle3';
  StrSFRMODFLOW6 = 'SFR MODFLOW 6';
  StrSFRMODFLOW6Packa_rlen = 'SFR MODFLOW 6, Package Data: rlen';
  StrSFRMODFLOW6Packa_rwid = 'SFR MODFLOW 6, Package Data: rwid';
  StrSFRMODFLOW6Packa_rgrd = 'SFR MODFLOW 6, Package Data: rgrd';
  StrSFRMODFLOW6Packa_rtp = 'SFR MODFLOW 6, Package Data: rtp';
  StrSFRMODFLOW6Packa_rbth = 'SFR MODFLOW 6, Package Data: rbth';
  StrSFRMODFLOW6Packa_rhk = 'SFR MODFLOW 6, Package Data: rhk';
  StrMAW = 'MAW';
  StrMAWMODFLOW6Packa_radius = 'MAW MODFLOW 6, Package Data: radius';
  StrMAWMODFLOW6Packa_bottom = 'MAW MODFLOW 6, Package Data: bottom';
  StrMAWMODFLOW6Packa_strt = 'MAW MODFLOW 6, Package Data: strt';
  StrMAWMODFLOW6CONNE_scrn_top = 'MAW MODFLOW 6, CONNECTIONDATA Data: scrn_t' +
  'op';
  StrMAWMODFLOW6CONNE_scrn_bot = 'MAW MODFLOW 6, CONNECTIONDATA Data: scrn_b' +
  'ot';
  StrMAWMODFLOW6CONNE_hk_skin = 'MAW MODFLOW 6, CONNECTIONDATA Data: hk_skin';
  StrMAWMODFLOW6CONNE_radius_skin = 'MAW MODFLOW 6, CONNECTIONDATA Data: rad' +
  'ius_skin';
  StrInvalidDISVGridFo = 'Invalid DISV grid for MODPATH';
  StrMODFPATHRequiresTh = 'MODFPATH requires that the edges of all cells be ' +
  'at either 0 or 90 degrees.';
  LakeMf6BedLeak = 'MODFLOW 6 Lake package: bedleak';
  LakeMf6Belev = 'MODFLOW 6 Lake package: belev';
  LakeMf6Telev = 'MODFLOW 6 Lake package: telev';
  LakeMf6ConnLen = 'MODFLOW 6 Lake package: connlen';
  LakeMf6ConnWidth = 'MODFLOW 6 Lake package: connwidth';
  StrMODFLOW6UZF6PacEps = 'MODFLOW 6: UZF6, Package Data, eps';
  StrMODFLOW6UZF6PacThti = 'MODFLOW 6: UZF6, Package Data, thti';
  StrMODFLOW6UZF6PacThtr = 'MODFLOW 6: UZF6, Package Data, thtr';
  StrMODFLOW6UZF6PacThts = 'MODFLOW 6: UZF6, Package Data, thts';
  StrMODFLOW6UZF6PacSurfdep = 'MODFLOW 6: UZF6, Package Data, surfdep';
  StrMODFLOW6UZF6PacVks = 'MODFLOW 6: UZF6, Package Data, vks';
  StrIDOMAINSetToZeroOver = 'IDOMAIN set to zero because all overlying layer' +
  's are inactive';
  StrIDOMAINSetToZeroUnder = 'IDOMAIN set to zero because all underlying lay' +
  'ers are inactive';
  StrSTRPackageDataSet = 'STR Package data set 3 CINITSF';
  StrSFTDispersion = KSFTDispersion;
  StrSTRPackageDataSetDISPSF = 'STR Package data set 4 DISPSF';
  StrSUTRALakeAreaInpu = 'SUTRA Lake-Area Input File: ELVLB';
  StrMODFLOWStressPerio = 'MODFLOW Stress periods have not been defined.';
  StrSelectModelMODFLO = 'Select "Model|MODFLOW Time..." to define the stres' +
  's periods.';
  StrKyOverKxDisplay = KKyOverKx;
  StrKzOverKxDisplay = KKzOverKx;
  StrSetByMultiplying = 'Set by multiplying %0:s by %1:s.';
  StrMODFLOW6CSUBCg = 'MODFLOW 6, CSUB: cg_ske_cr';
  StrInitialElasticSpec = KInitialElasticSpec;
  StrInitialElasticReco = KInitialElasticReco;
  StrInitialCoarsePoros = KInitialCoarsePoros;
  StrMoistSpecificGravi = KMoistSpecificGravi;
  StrSaturatedSpecificG = KSaturatedSpecificG;
  StrMODFLOW6CSUBCgTheta = 'MODFLOW 6, CSUB: cg_theta';
  StrMODFLOW6CSUBSgm = 'MODFLOW 6, CSUB: sgm';
  StrMODFLOW6CSUBSgs = 'MODFLOW 6, CSUB: sgs';
  StrSetToTrueBecause = 'Set to True because it is inside %s which sets cell' +
  ' size.';
  StrSetToFalseBecause = 'Set to False because it is not inside any object t' +
  'hat sets cell size.';
  StrSeeAlsoTheSDa = 'See also the "%s" data set for the Storage package.';
  StrExportingMT3DUSGS = 'Exporting MT3D-USGS input files';
  StrExportingMT3DMSInp = 'Exporting MT3DMS input files';
  StrLakeTransportConce = KLakeTransportConce;
  StrMF2005ImporterexeIsNot = 'MF2005_Importer.exe is not MODFLOW. You need ' +
  'to download MODFLOW from the USGS web site and unzip the MODFLOW distribu' +
  'tion file. Once you have done that, select "Model|MODFLOW Program Locatio' +
  'ns" and enter the location of MODFLOW in the dialog box.';
  StrNodeActive = KNodeActive;
  StrNoStressPeriodsWe = 'No stress periods were defined';
  StrStressPeriodAdded = 'No stress periods were defined in the MODFLOW Time' +
  ' dialog box. One has been added automatically.';
  StrS = '"%s" ';
  StrPESTWasNotFoundI = 'PEST was not found in %s';
  StrPESTNotFound = 'PEST not found.';
  StrIllegalGlobalVaria = 'Illegal global variable name';
  StrAGlobalVariableNa = 'A global variable named %s has the same name as a ' +
  'data set. The global variable has been deleted.';
  StrTheFollowingParame = 'The following parameters are not used in any PEST' +
  ' template.';
  StrThePestUtilityFil = 'The Pest Utility file "parrep.exe" not found.';
  StrParrepDoesNotExis = 'Parrep does not exist at "%s".';
  StrParFileNotFound = '.par file not found';
  StrTheParameterFileF = 'The parameter file from a previous estimation run ' +
  'was not found at "%s".';
  StrPESTCHEKNotFound = 'PESTCHEK not found';
  StrNeitherI64pestchek = 'Neither I64pestchek.exe nor pestchek.exe were fou' +
  'nd in %s';
  StrSUPCALCNotFound = 'SUPCALC not found';
  StrNeither164supcalce = 'Neither 164supcalc.exe nor supcalc.exe were found' +
  ' in %s';
  StrParcalcexeAndPica = 'parcalc.exe and picalc.exe must both be present in' +
  ' the PEST directory to run PEST wish SVD Assist. You must install them th' +
  'ere before continuing.';
  StrNoSolverPackageSe = 'No solver package selected';
  StrNoFlowPackageSele = 'No flow package selected';
  StrYouMustSpecifyA = 'You must specify a %s package for this model in the ' +
  'MODFLOW Packages and Progams dialog box.';
  StrSolver = 'solver';
  StrFlow = 'flow';

  // Sutra 4 node data sets
  StrSolidMatrixComp = KSolidMatrixComp;
  StrSolidGrainSpecificHeat = KSolidGrainSpecificHeat;
  StrSolidGrainDensity = KSolidGrainDensity;
  StrZeroOrderProductionRateInLiquid = KZeroOrderProductionRateInLiquid;
  StrZeroOrderProductionRateInImmobile = KZeroOrderProductionRateInImmobile;
  StrFirstOrderProductionRateInLiquid = KFirstOrderProductionRateInLiquid;
  StrFirstOrderProductionRateInImmobile = KFirstOrderProductionRateInImmobile;
  StrZeroOrderProductionRateInIce = KZeroOrderProductionRateInIce;
  // Sutra 4 element data sets
  StrScaledSolidGrainThermalConductivity = KScaledSolidGrainThermalConductivity;
  StrScaledEffectiveAirThermalConductivity = KScaledEffectiveAirThermalConductivity;
  StrDiffusionCoefficien = KDiffusionCoefficien;
  StrVerticalTransverse = KVerticalTransverse;
  StrHorizontalTransvers = KHorizontalTransvers;
  StrLongitudinalDispersV = KLongitudinalDispersV;
  StrLongitudinalDispersH = KLongitudinalDispersH;
  StrMODFLOW6MSTPackag = 'MODFLOW 6 MST Package: POROSITY';
  StrMODFLOW6DSPPacka = 'MODFLOW 6, DSP Package: ALH';
  StrMODFLOw6DSPPackaath1 = 'MODFLOw 6, DSP Package: ATH1';
  StrMODFLOw6DSPPackaATV = 'MODFLOw 6, DSP Package: ATV';
  StrSUTRADataSet14B = 'SUTRA Data Set 14B: COMPMA';
  StrSUTRADataSet14B_CS = 'SUTRA Data Set 14B: CS';
  StrSUTRADataSet14B_RHOS = 'SUTRA Data Set 14B: RHOS';
  StrSUTRADataSet14B_PRODL0 = 'SUTRA Data Set 14B: PRODL0';
  StrSUTRADataSet14B_PRODS0 = 'SUTRA Data Set 14B: PRODS0';
  StrSUTRADataSet14B_PRODL1 = 'SUTRA Data Set 14B: PRODL1';
  StrSUTRADataSet14B_PRODS1 = 'SUTRA Data Set 14B: PRODS1';
  StrSUTRADataSet14B_PRODI0 = 'SUTRA Data Set 14B: PRODI0';
  StrSUTRADataSet15B_SIGMAS = 'SUTRA Data Set 15B: SIGMAS';
  StrSUTRADataSet15B_SIGMAA = 'SUTRA Data Set 15B: SIGMAA';
  StrMODFLOW6Dispersion_DIFFC = 'MODFLOW 6 Dispersion Package: DIFFC';
  StrMODFLOW6Dispersion_ALH = 'MODFLOW 6 Dispersion Package: ALH';
  StrMODFLOW6Dispersion_ALV = 'MODFLOW 6 Dispersion Package: ALV';
  StrMODFLOW6Dispersion_ATH1 = 'MODFLOW 6 Dispersion Package: ATH1';
  StrMODFLOW6Dispersion_ATH2 = 'MODFLOW 6 Dispersion Package: ATH2';
  StrFarmID = KFarmID;
  StrRefET = KRefET;
  StrPrecipitation = KPrecipitation;
  StrLand_Use_ID = KLand_Use_ID;
  StrEfficiency = KEfficiency;
  StrEfficiencyImprovement = KEfficiencyImprovement;
  StrBareRunoffFraction = KBareRunoffFraction;
  StrAddedDemandRunoffSplit = KAddedDemandRunoffSplit;
  StrBarePrecipitationConsumptionFraction = KBarePrecipitationConsumptionFraction;
  StrCapillary_Fringe = KCapillary_Fringe;
  StrSurfaceK = KSurfaceK;
  StrPotential_Evap_Bare = KPotential_Evap_Bare;
  StrDirectRecharge = KDirectRecharge;
  StrPrecipPotConsumption = KPrecipPotConsumption;
  StrNRD_Infiltration_Location = KNRD_Infiltration_Location;
  StrCropCoefficient = KCropCoefficient;
  StrLandUseAreaFraction = KLandUseAreaFraction;
  StrConsumptiveUse = KConsumptiveUse;
  StrKIrrigation = KIrrigation;
  StrRootDepth = KRootDepth;
  StrGWRootInteraction = KGWRootInteraction;
  StrTranspirationFraction = KTranspirationFraction;
  StrEvaporationIrrigationFraction = KEvaporationIrrigationFraction;
  StrFractionOfPrecipToSurfaceWater = KFractionOfPrecipToSurfaceWater;
  StrFractionOfIrrigToSurfaceWater = KFractionOfIrrigToSurfaceWater;
  StrAddedDemand = KAddedDemand;
  StrCropHasSalinityDemand = KCropHasSalinityDemand;
  StrIrrigationEarly = 'The specified times for irrigation types include tim' +
  'es before the beginning of the first stress period.';
  StrIrrigationLate = 'The specified times for irrigation types include time' +
  's after the end of the last stress period.';
  StrLandUseCellsToPrint = KLandUseCellsToPrint;
  StrHufThickness = kHufThickness;
  // @name is the name of the @link(TDataArray) that specifies whether an
  // element in PHAST is active or not.
  rsActiveDisplayName = rsActive;
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the X direction.
  rsKxDisplayName = rsKx;
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Y direction.
  rsKyDisplayName = rsKy;
  // @name is the name of the @link(TDataArray) that specifies
  // the hydraulic conductivity in the Z direction.
  rsKzDisplayName = rsKz;
  // @name is the name of the @link(TDataArray) that specifies
  // the porosity.
  rsPorosityDisplayName = rsPorosity;
  // @name is the name of the @link(TDataArray) that specifies
  // the specific storage.
  rsSpecific_StorageDisplayName = rsSpecific_Storage;
  // @name is the name of the @link(TDataArray) that specifies
  // the longitudinal dispersivity.
  rsLong_DispersivityDisplayName = rsLong_Dispersivity;
  // @name is the name of the @link(TDataArray) that specifies
  // the horizontal transverse dispersivity.
  rsHorizontal_Transv_DispersivityDisplayName = rsHorizontal_Transv_Dispersivity;
  // @name is the name of the @link(TDataArray) that specifies
  // the vertical transverse dispersivity.
  rsVertical_Transv_DispersivityDisplayName = rsVertical_Transv_Dispersivity;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial head.
  rsInitial_HeadDisplayName = rsInitial_Head;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial water table.
  rsInitial_Water_TableDisplayName = rsInitial_Water_Table;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solution.
  rsChemistry_Initial_SolutionDisplayName = rsChemistry_Initial_Solution;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial equilibrium phases.
  rsChemistry_Initial_Equilibrium_PhasesDisplayName =
    rsChemistry_Initial_Equilibrium_Phases;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial surface properties.
  rsChemistry_Initial_SurfaceDisplayName = rsChemistry_Initial_Surface;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial exchange properties.
  rsChemistry_Initial_ExchangeDisplayName = rsChemistry_Initial_Exchange;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial gas phase properties.
  rsChemistry_Initial_Gas_PhaseDisplayName = rsChemistry_Initial_Gas_Phase;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial solid-solution properties.
  rsChemistry_Initial_Solid_SolutionsDisplayName = rsChemistry_Initial_Solid_Solutions;
  // @name is the name of the @link(TDataArray) that specifies
  // the initial kinetic properties.
  rsChemistry_Initial_KineticsDisplayName = rsChemistry_Initial_Kinetics;
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print Chemistry" distribution.
  rsPrint_ChemistryDisplayName = rsPrint_Chemistry;
  // @name is the name of the @link(TDataArray) that specifies
  // the "Print XYZ Chemistry" distribution.
  rsPrint_XYZ_ChemistryDisplayName = rsPrint_XYZ_Chemistry;

  // @name indicates the layer number for a MODFLOW reservoir.
  rsResLayerDisplayName = rsResLayer;
  // @name is the elevation of a MODFLOW reservoir.
  rsResBottomDisplayName = rsResBottom;
  // @name is the hydraulic conductivity of a MODFLOW reservoir.
  rsResKvDisplayName = rsResKv;
  // @name is the bed thickness of a MODFLOW reservoir.
  rsResBedThicknessDisplayName = rsResBedThickness;

  // @name is the lake number of a MODFLOW lake.

  rsLakeIDDisplayName = rsLakeID;

  // @name is the leakance of a MODFLOW lake.
  rsLakeLeakanceDisplayName = rsLakeLeakance;

  rsModflowSpecifiedHeadDisplayName = rsModflowSpecifiedHead;

  // @name is the land surface in UZF. It is used to set IUZFBND.
  StrUzfLandSurfaceDisplayName = StrUzfLandSurface;
  // @name set IUZFBND.
  StrUzfLayerDisplayName = StrUzfLayer;
  StrUzfDischargeRoutingDisplayName = StrUzfDischargeRouting;
  StrUzfVerticalKDisplayName = StrUzfVerticalK;
  StrUzfBrooksCoreyEpsilonDisplayName = StrUzfBrooksCoreyEpsilon;
  StrUzfSaturatedWaterContentDisplayName = StrUzfSaturatedWaterContent;
  StrUzfInitialUnsaturatedWaterContentDisplayName = StrUzfInitialUnsaturatedWaterContent;
  StrUzfReisidualWaterContentDisplayName = StrUzfReisidualWaterContent;
  StrUzfSurfaceKDisplayName = StrUzfSurfaceK;
  StrUzfGage_1_and_2DisplayName = StrUzfGage_1_and_2;
  StrUzfGage3DisplayName = StrUzfGage3;

  StrUzfMf6BrooksCoreyEpsilonDisplayName = StrUzfMf6BrooksCoreyEpsilon;
  StrUzfMf6InitialUnsaturatedWaterContentDisplayName = StrUzfMf6InitialUnsaturatedWaterContent;
  StrUzfMf6ReisidualWaterContentDisplayName = StrUzfMf6ReisidualWaterContent;
  StrUzfMf6SaturatedWaterContentDisplayName = StrUzfMf6SaturatedWaterContent;
  StrUzfMf6SurfaceDepressionDepthDisplayName = StrUzfMf6SurfaceDepressionDepth;
  StrUzfMf6VerticalSaturatedKDisplayName = StrUzfMf6VerticalSaturatedK;

  rsModflow_Initial_HeadDisplayName = rsModflow_Initial_Head;
  rsModflow_CBKzDisplayName = rsModflow_CBKz;
  rsVerticalAnisotropyDisplayName = rsVerticalAnisotropy;
  rsHorizontalAnisotropyDisplayName = rsHorizontalAnisotropy;
  rsSpecificYieldDisplayName = rsSpecificYield;
  rsWetDryThresholdDisplayName = rsWetDryThreshold;
  rsWetDryFlagDisplayName = rsWetDryFlag;
  rsWetDryDisplayName = rsWetDry;

  StrModpathZoneDisplayName = StrModpathZone;
  StrHufReferenceSurfaceDisplayName = StrHufReferenceSurface;
  StrTransmissivityDisplayName = StrTransmissivity;
  StrConfinedStorageCoeDisplayName = StrConfinedStorageCoe;

  StrHUFKxNameDisplayName = StrHUFKxName;
  StrHUFKyNameDisplayName = StrHUFKyName;
  StrHUFInterlayerKzDisplayName = StrHUFInterlayerKz;
  StrHUFSSNameDisplayName = StrHUFSSName;
  StrHUFAverageSYNameDisplayName = StrHUFAverageSYName;
  StrHUFSYNameDisplayName = StrHUFSYName;
  StrHUFSYTPNameDisplayName = StrHUFSYTPName;

  StrZonesDisplayName = StrZones;
  StrGeostaticStressDisplayName = StrGeostaticStress;
  StrSpecificGravityUnsDisplayName = StrSpecificGravityUns;
  StrSpecificGravitySatDisplayName = StrSpecificGravitySat;
  StrInitialPreOffsetsDisplayName = StrInitialPreOffsets;
  StrInitialPreconsolidaDisplayName = StrInitialPreconsolida;

  STR_MT3DMS_Observation_LocationsDisplayName = STR_MT3DMS_Observation_Locations;
  StrMT3DMSActiveDisplayName = StrMT3DMSActive;
  rsBulkDensityDisplayName = rsBulkDensity;
  rsImmobPorosityDisplayName = rsImmobPorosity;
  rsMT3DMS_Layer_ThicknessDisplayName = rsMT3DMS_Layer_Thickness;

  KModpathBudgetDisplayName = KModpathBudget;
  KModpathRetardationDisplayName = KModpathRetardation;

  StrNodalPorosityDisplayName = KNodalPorosity;
  StrNodalThicknessDisplayName = KNodalThickness;
  StrUnsatRegionNodesDisplayName = KUnsatRegionNodes;
  StrUnsatRegionElementsDisplayName = KUnsatRegionElements;

  StrMaximumPermeability = KMaximumPermeability;
  StrMiddlePermeability = KMiddlePermeability;
  StrMinimumPermeability = KMinimumPermeability;
  StrMaximumK = KMaximumK;
  StrMiddleK = KMiddleK;
  StrMinimumK = KMinimumK;
  StrHorizontalAngle = KHorizontalAngle;
  StrVerticalAngle = KVerticalAngle;
  StrRotationalAngle = KRotationalAngle;
  StrMaxLongitudinalDisp = KMaxLongitudinalDisp;
  StrMidLongitudinalDisp = KMidLongitudinalDisp;
  StrMinLongitudinalDisp = KMinLongitudinalDisp;
  StrMaxTransverseDisp = KMaxTransverseDisp;
  StrMidTransverseDisp = KMidTransverseDisp;
  StrMinTransverseDisp = KMinTransverseDisp;
  StrInitialPressure = KInitialPressure;
  StrInitialConcentration = KInitialConcentration;
  StrInitialTemperature = KInitialTemperature;
  StrSpecifiedPressure = 'SUTRA Specified Pressure';
  StrSutraSpecifiedHead = 'SUTRA Specified Head';
  StrAssocPresConc = 'SP Associated Conc.';
  StrAssocHeadConc = 'SH Associated Conc.';
  StrAssocPresTemp = 'SP Associated Temp.';
  StrFluxAssocPresConc = 'Flux Associated Conc.';
  StrFluxAssocPresTemp = 'Flux Associated Temp.';
  StrLowerPressureValue = 'Lower Pressure Value';
  StrLowerHeadValue = 'Lower Head Value';
  StrHigherPressureValue = 'Higher Pressure Value';
  StrHigherHeadValue = 'Higher Head Value';
  StrLowerRateP = 'Flow Rate at Lower Pressure Value';
  StrHigherRateP = 'Flow Rate at Higher Pressure Value';
  StrLowerRateH = 'Flow Rate at Lower Head Value';
  StrHigherRateH = 'Flow Rate at Higher Head Value';
  StrLowerConcentrationP = 'Concentration In';
  StrHigherConcentrationP = 'Concentration Out';
  StrLowerConcentrationH = 'Concentration In';
  StrHigherConcentrationH = 'Concentration Out';
  StrLowerTemperature = 'Temperature In';
  StrHigherTemperature = 'Temperature Out';
  StrLowerConcentrationValue = 'Lower Concentration Value';
  StrHigherConcentrationValue = 'Higher Concentration Value';
  StrLowerTemperatureValue = 'Lower Temperature Value';
  StrHigherTemperatureValue = 'Higher Temperature Value';

  StrMassFlowAtLowerConcentration = 'Mass Flow at Lower Concentration';
  StrMassFlowAtHigherConcentration = 'Mass Flow at Higher Concentration';
  StrEnergyFlowAtLowerTemperature = 'Energy Flow at Lower Temperature';
  StrEnergyFlowAtHigherTemperature = 'Energy Flow at Higher Temperature';
  StrSoilID = KSoilID;
  StrSUTRAInitialLakeS = KSUTRAInitialLakeS;
  StrSUTRAInitialLakeU = KSUTRAInitialLakeU;
  StrSUTRALakeRecharge = KSUTRALakeRecharge;
  StrSUTRALakeDischarge = KSUTRALakeDischarge;
  StrLakeBottom = KLake_Bottom;

  StrXT3DAngle1 = KXT3DAngle1;
  StrXT3DAngle2 = KXT3DAngle2;
  StrXT3DAngle3 = KXT3DAngle3;

  StrReachLengthSFR = KReachLengthSFR;
  StrReachWidthSFR6 = KReachWidthSFR6;
  StrGradientSFR6 = KGradientSFR6;
  StrStreambedTopSFR6 = KStreambedTopSFR6;
  StrStreambedThicknessSFR6 = KStreambedThicknessSFR6;
  StrHydraulicConductivitySFR6 = KHydraulicConductivitySFR6;
  StrMAWRadius = KMAWRadius;
  StrMAWBottom = KMAWBottom;
  StrMAWInitialHead = KMAWInitialHead;
  StrMAWScreenTop = KMAWScreenTop;
  StrMAWScreenBottom = KMAWScreenBottom;
  StrMAWSkinK = KMAWSkinK;
  StrMAWSkinRadius = KMAWSkinRadius;
//  StrLakeMf6 = KLakeMf6;
  StrMf6LakeConnectionTypes = KMf6LakeConnectionTypes;
  StrLake_Bed_Leakance = KLake_Bed_Leakance;
  StrLake_Bed_Thickness = KLake_Bed_Thickness;
  StrLake_Bottom_Elevation = KLake_Bottom_Elevation;
  StrLake_Top_Elevation = KLake_Top_Elevation;
  StrLake_Connection_Length = KLake_Connection_Length;
  StrLake_Connection_Width = KLake_Connection_Width;

  KSutraDefaultPath = 'C:\SutraSuite\SUTRA_2_2\bin\sutra_2_2.exe';
  KSutra30DefaultPath = 'C:\SutraSuite\SUTRA_3_0\bin\sutra_3_0.exe';
//  KSutra40DefaultPath = 'C:\SutraSuite\SUTRA_3_0\bin\sutra_4_0.exe';
  KSutra40DefaultPath = 'C:\WRDAPP\SUTRA_4-develop\msvs\bin_PROVISIONAL\sutra_4_0_0_PROVISIONAL_2023-02-28_8b569f6.exe';
  StrMpathDefaultPath = 'C:\WRDAPP\Mpath.5_0\setup\Mpathr5_0.exe';
  StrMpathDefaultPathVersion6 = 'C:\WRDAPP\modpath.6_0\bin\mp6.exe';
  StrMpathDefaultPathVersion7 = 'C:\WRDAPP\modpath_7_2_001\bin\mpath7.exe';
  // See also Mf2005Date on frmGoPhastUnit.
  StrModflowDefaultPath = 'C:\WRDAPP\MF2005.1_12\bin\mf2005.exe';
  StrPhastDefaultPath = 'C:\Program Files\USGS\phast-1.5.1\bin\phast.bat';
  StrPhastDefaultPath64 = 'C:\Program Files (x86)\USGS\phast-1.5.1\bin\phast.bat';
  StrZoneBudgetDefaultPath = 'C:\WRDAPP\Zonbud.3_01\Bin\zonbud.exe';
  StrModelMateDefaultPath = 'C:\WRDAPP\ModelMate_1_0_3\Bin\ModelMate.exe';
  strModflowLgrDefaultPath = 'C:\WRDAPP\mflgr.1_2\bin\mflgr.exe';
  strModflowLgr2DefaultPath = 'C:\WRDAPP\mflgr.2_0\bin\mflgr.exe';
  strModflowNwtDefaultPath = 'C:\WRDAPP\MODFLOW-NWT_1.2.0\bin\MODFLOW-NWT.exe';
  strMt3dmsDefaultPath = 'C:\mt3dms5\bin\mt3dms5b.exe';
  strMt3dUsgsDefaultPath = 'C:\WRDAPP\mt3dusgs1.0.1\bin\mt3d-usgs_1.0.1_32.exe';
  strModflowCfpDefaultPath = 'C:\WRDAPP\CFP\mf2005cfp.exe';
  StrModelMonitorDefaultPath = 'ModelMonitor.exe';
  strModflowOwhmDefaultPath = 'C:\WRDAPP\MF_OWHM_v1_0\bin\MF_OWHM_Win32.exe';
  StrMFOwhmDefaultPath64 = 'C:\WRDAPP\MF_OWHM_v1_0\bin\MF_OWHM.exe';
  StrPestDefaultDir = 'C:\Pest17\';

  // See also GMshDate in frmMeshGenerationControlVariablesUnit.pas
  StrDefaultGmshPath = 'C:\gmsh-4.12.2-Windows64\gmsh.exe';

  StrDefaultGeompackPath = 'C:\GeompackPlusPlus\zgp1408.exe';
  StrDefaultFootprintPath = 'C:\WRDAPP\WellFootprint.1_0_1\bin\WellFootprint.exe';

  // Be sure to update zonebudget path when updating mf6 path.
  StrDefaultModflow6Path =      'C:\WRDAPP\mf6.4.4_win64\bin\mf6.exe';
  StrZoneBudgetMf6DefaultPath = 'C:\WRDAPP\mf6.4.4_win64\bin\zbud6.exe';

  StrDefaultOwhmV2Path = 'C:\WRDAPP\mf-owhm-2.3.0\bin\mf-owhm.exe';

  StrProgramLocations = 'Program Locations';
  StrMODFLOW2005 = 'MODFLOW-2005';
  StrTextEditor = 'Text Editor';
  StrMODPATH = 'MODPATH';
  StrMODPATHVersion6 = 'MODPATH Version 6';
  StrModelMonitor = 'ModelMonitor';
  StrPHAST = 'PHAST';
  StrModelMate = 'ModelMate';
  StrAnyTimesAfterThe = 'Any times after the end of the last defined stress ' +
  'period will be ignored.';
  StrAnyTimesBeforeThe = 'Any times before the beginning of the first define' +
  'd stress period will be ignored.';
  strModflowLgr = 'MODFLOW-LGR';
  strModflowLgr2 = 'MODFLOW-LGR-V2';
  strModflowNWT = 'MODFLOW-NWT';
  strModflowOWHM = 'MODFLOW-OWHM';
  strModflowCFP = 'MODFLOW-CFP';
  StrModflow6 = 'MODFLOW 6';
  StrPestDir = 'PEST';
  strModflowOWHM_V2 = 'MODFLOW-OWHM_V2';
  StrAtLeastOneConvert = 'At least one layer must be convertible.';
  StrAtLeastOneUnconfConvert = 'At least one layer must be unconfined or ful' +
  'ly convertible.';
  StrSorryNoFilesToA = 'Sorry, no files to archive.';
  StrDOfTheFilesThat = '%d of the files that were to be archived can not be ' +
  'found.';
  StrTheRemainingFiles = 'The remaining files will be archived.';
//  StrErrorSavingArchive = 'Error saving archive.  The archive may be too big' +
//  '.';
//  StrErrorSavingArchive2 = 'Error saving archive. The disk may not have suffi' +
//  'cient space for the archive.';
  StrYourModelMateFile = 'Your ModelMate file contains the following unused ' +
  'parameters. Do you want to delete them?';
  StrYourModelMateFile2 = 'Your ModelMate file contains %d unused parameters.' +
  ' Do you want to delete them?';
  StrYourModelMateFile3 = 'Your ModelMate file contains the following unused ' +
  '%s. Do you want to delete them?';
  StrYourModelMateFile4 = 'Your ModelMate file contains %0:d unused %1:s. Do' +
  ' you want to delete them?';
  StrOneOrMore0sFro = 'One or more %0:s from the ModelMate file are not pres' +
  'ent in the ModelMuse file and have been ignored. If an %1:s has been rena' +
  'med, change the %2:s name so that it matches in ModelMuse and ModelMate a' +
  'nd then try importing again.';
  StrObservation = 'observation';
  StrObservations = 'observations';
  StrPrediction = 'prediction';
  StrPredictions = 'predictions';
  StrOneOrMoreParamete = 'One or more parameters from the ModelMate file are' +
  ' not present in the ModelMuse file and have been ignored. If a parameter ' +
  'has been renamed, change the parameter name so that it matches in ModelMu' +
  'se and ModelMate and then try importing again.';
  StrYourModelHasSTi = 'Your model has %d time steps. Do you want to continu' +
  'e?';
  StrMODPATHRequiresTha = 'MODPATH requires that the heads and flows be save' +
  'd for every time step of the model. That isn''t the case for this model. ' +
  'Do you want to export the MODPATH input anyway? You can change the times ' +
  'at which data are saved in the "Model|MODFLOW Output Control..." dialog box.';
//  StrMT3DMS5 = 'MT3DMS-5';
  StrTheBeginningOfThe = 'The beginning of the first stress period is %0:g. ' +
  'The first defined time is  %1:g. The following objects have defined times' +
  ' before the beginning of the first stress period.';
  StrTheBeginningOfTheCrop = 'The beginning of the first stress period is %0:g. ' +
  'The first defined time is  %1:g. The following crops have defined times' +
  ' before the beginning of the first stress period.';
  StrTheEndOfTheLast = 'The end of the last stress period is %0:g. The last ' +
  'defined time is %1:g. The following objects have defined times after the' +
  ' end of the last stress period.';
  StrTheEndOfTheLastCrop = 'The end of the last stress period is %0:g. The last ' +
  'defined time last %1:g. The following crops have defined times after the' +
  ' end of the first stress period.';
  StrNoObjects = 'The mesh was not created because no objects define the ele' +
  'ment size on the top view of the model.';
  StrNoPolygons = 'The mesh was not created because no polygons define the e' +
  'lement size and constrain the mesh on the top view of the model.';
  StrInvalidTimesForMT = 'Invalid times for MT3DMS';
  StrTheStressPeriodsD = 'The stress periods defined for MT3DMS are not with' +
  'in the stress periods defined for MODFLOW.';
  StrAndNegatedAtCons = ' and negated at constant head cell';
  StrAndMadePositiveA = ' and made positive at constant head cell';
  StrValueOfZeroConver = 'Value of zero converted to 1 at active cell.';
  StrNonzeroValueOfZe = 'Non-zero value converted to 0 at inactive cell.';
  StrAddedTimes = 'The following objects define times at which stresses ' +
    'change that do not coincide with the beginning or ending of stress ' +
    'periods. The stress periods will be adjusted to use these times.';
  StrCropAddedTimes = 'The following crops define times at which stresses ' +
    'change that do not coincide with the beginning or ending of stress ' +
    'periods. The stress periods will be adjusted to use these times.';
  StrTimesDoNotMatch = 'Times do not match';
  StrTheFinalTimeSpeci = 'The final time specified in the MT3DMS simulation ' +
  '(%0:g) is earlier than the final time specified in the MODFLOW simulation' +
  ' (%1:g). Do you want to stop the MT3DMS simulation when it reaches the ti' +
  'me you specified for the end of the MT3DMS simulation? ("Cancel" aborts ' +
  'the export of the MT3DMS files.)';
  StrMODPATHVersion6Re = 'MODPATH version 6 requires that if heads are saved, ' +
  'they must be saved in a binary format. That isn''t the case for this ' +
  'model. Do you want to export the MODPATH input anyway? You can change the ' +
  'file format in the "Model|MODFLOW Output Control..." dialog box.';
  StrYouNeedToDrawQua = 'You need to draw quadrilaterals that will be used t' +
  'o define the mesh and assign the number of nodes along two edges of each ' +
  'quadrilateral.';
  StrOneOrMoreInvalid = 'One or more invalid elements was created. ' +
    'Adjusting the mesh generation parameters or inserting additional ' +
    'control points may help.';
  StrPipeDiameter = 'PipeDiameter';
  StrTortuosity = 'Tortuosity';
  StrRoughnessHeight = 'RoughnessHeight';
  StrLowerCriticalR = 'LowerCriticalR';
  StrUpperCriticalR = 'UpperCriticalR';
  StrPipeConductanceOrPer = 'PipeConductanceOrPermeabilty';
  StrDrainableStorageWidth = KDrainableStorageWidth;
  StrCfpNodeElevation = 'CfpNodeElevation';
  StrCfpFixedHeads = KCfpFixedHeads;
  StrCfpBoundaryType = KCfpBoundaryType;
  StrCfpLimitedFlowValue = KCfpLimitedFlowValue;
  StrCfpWellFlow = KCfpWellFlow;
//  StrCfpWellConductance = KCfpWellConductance;
  StrCfpCauchyHead = KCfpCauchyHead;
  StrCfpCauchyConductivity = KCfpCauchyConductivity;
  StrCfpCauchyLimitedInflow = KCfpCauchyLimitedInflow;
  StrCfpLimitedHead = KCfpLimitedHead;


  StrEffectivePorosity = 'EffectivePorosity';
  StrSourceFluidDensityZo = 'SourceFluidDensityZone';
  StrSWIObservationName = 'SWI_Observation_Name';
  StrVersion = 'Version';
  StrModelMuseVersion = 'ModelMuse Version';
  StrSwrReach = 'SWR_Reach_Number';
  StrSwrReachGroup = 'SWR_Reach_Group_Number';
  StrSwrRoutingType = 'SWR_Routing_Type';
  StrSwrReachLength = 'SWR_Reach_Length';
  // See also GMshDate in frmMeshGenerationControlVariablesUnit
  StrGmsh = 'Gmsh';
  StrGeompack = 'Geompack';
  StrFootprint = 'WellFootprint';
  StrInvalidStressPerio = 'Invalid stress period length';
  StrInModelMuseAllStr = 'In ModelMuse all stress periods must have a length' +
  ' greater than or equal to zero.';
  StrThereAreFarTooMa = 'There are far too many time steps in this model. Pl' +
  'ease check the MODFLOW Time dialog box to make sure that the maximum leng' +
  'th of the first time step of each stress period is greater than zero.';
  StrFarmTimesEarly = 'The specified times for farms include times before th' +
  'e beginning of the first stress period.';
  StrFarmTmesLate = 'The specified times for farms include times after the e' +
  'nd of the last stress period.';
  StrThereWasAnErrorG = 'There was an error generating the mesh. Please chec' +
  'k the objects used to define the mesh element size.';
  StrDepthRateIndex = 'DepthRateIndex';
//  StrDistributedWithdrawals = 'Distributed_Withdrawals';
  StrWithdrawals = 'Withdrawals';
//  StrFootprint_Code = 'Footprint_Code';
  StrNoPumpage = 'No pumpage';
  StrInvalidWithdrawalR = 'Invalid Withdrawal Rate';
  StrInSTheWithdrawa = 'In %s, the withdrawal rate is less than or equal to ' +
  'zero.';
  StrAssignedByObject = 'Assigned by object %0:s with formula %1:s.';
  StrIllegalSWRReachNu = 'Illegal SWR reach numbering';
  StrThereIsAnErrorIn = 'There is an error in the SWR reach numbering. Try c' +
  'oloring the grid with the reach number for more detailed information.';
  StrIDOMAIN = 'IDOMAIN';
  StrCell_Type = 'Cell_Type';
  StrConvertible = 'Convertible';
  StrInTheFollowingCel = 'In the following cells, the Depth-Rate Index is less' +
  ' than or equal to zero. These cells will be treated as inactive.';
  StrRowCol0d1 = 'Row, Col = %0:d, %1:d';
  StrErrorSavingArchive = 'Error saving archive. Error message was %s.';
  StrThereWasAnErrorS = 'There was an error saving the archive list. The err' +
  'or message was "%s".';
//  StrInitialWaterContent = KInitialWaterContent;
//  StrSaturatedThickness = KSaturatedThickness;
  StrSUTRALake = 'SUTRA_Lake';
  StrUzt = 'UZT';



implementation

end.
