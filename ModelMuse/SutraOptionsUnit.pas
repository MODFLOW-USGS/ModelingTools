unit SutraOptionsUnit;

interface

uses
  GoPhastTypes, Classes, SysUtils;

type
  TTransportChoice = (tcSolute, tcSoluteHead, tcEnergy);
  TSaturationChoice = (scSaturated, scUnsaturated);
  TSimulationType = (stSteadyFlowSteadyTransport,
    stSteadyFlowTransientTransport, stTransientFlowTransientTransport);
  TStartType = (stCold, stWarm);
  TPressureSolutionMethod = (psmDirect, pcmCG, psmGMRES, psmOthomin);
  TUSolutionMethod = (usmDirect, usmGMRES, usmOthomin);
  TSorptionModel = (smNone, smLinear, smFreundlich, smLangmuir);
  TReadStart = (rsNone, rsPressure, rsU, rsBoth);
  TLakeBoundaryInteraction = (lbiActivate, lbiNoChange, lbiInactivate);
  TGeneralizedFlowInteractionType = (gfitFluidSource, gfitSpecifiedPressure);
  TGeneralizedTransportInteractionType = (gtitSoluteSource, gtitSpecifiedConcentration);

  TSutraLakeOptions = class(TGoPhastPersistent)
  private
    FStoredSubmergedOutput: TRealStorage;
    FStoredRechargeFraction: TRealStorage;
    FStoredDischargeFraction: TRealStorage;
    FLakeOutputCycle: Integer;
    FMaxLakeIterations: Integer;
    FStoredMinLakeVolume: TRealStorage;
//    FGeneralizedFlowLakeAbsent: TLakeBoundaryInteraction;
    FGeneralizedFlowInteractionType: TGeneralizedFlowInteractionType;
    FFluidSourceSinkLakePresent: TLakeBoundaryInteraction;
//    FSpecifiedULakeAbsent: TLakeBoundaryInteraction;
    FSpecifiedPressureLakePresent: TLakeBoundaryInteraction;
    FUSourceSinkLakePresent: TLakeBoundaryInteraction;
    FGeneralizedTransportLakePresent: TLakeBoundaryInteraction;
    FGeneralizedFlowLakePresent: TLakeBoundaryInteraction;
    FSpecifiedULakePresent: TLakeBoundaryInteraction;
//    FFluidSourceSinkLakeAbsent: TLakeBoundaryInteraction;
//    FSpecifiedPressureLakeAbsent: TLakeBoundaryInteraction;
//    FUSourceSinkLakeAbsent: TLakeBoundaryInteraction;
//    FGeneralizedTransportLakeAbsent: TLakeBoundaryInteraction;
    FGeneralizedTransportInteractionType: TGeneralizedTransportInteractionType;
    FUseLakes: boolean;
    FAllNodesLakes: Boolean;
    procedure SetStoredDischargeFraction(const Value: TRealStorage);
    procedure SetLakeOutputCycle(const Value: Integer);
    procedure SetMaxLakeIterations(const Value: Integer);
    procedure SetStoredMinLakeVolume(const Value: TRealStorage);
    procedure SetStoredRechargeFraction(const Value: TRealStorage);
    procedure SetStoredSubmergedOutput(const Value: TRealStorage);
    function GetDischargeFraction: double;
//    function GetMinLakeVolume: double;
    function GetRechargeFraction: double;
    function GetSubmergedOutput: double;
    procedure SetDischargeFraction(const Value: double);
//    procedure SetMinLakeVolume(const Value: double);
    procedure SetRechargeFraction(const Value: double);
    procedure SetSubmergedOutput(const Value: double);
//    procedure SetFluidSourceSinkLakeAbsent(
//      const Value: TLakeBoundaryInteraction);
    procedure SetFluidSourceSinkLakePresent(
      const Value: TLakeBoundaryInteraction);
    procedure SetGeneralizedFlowInteractionType(
      const Value: TGeneralizedFlowInteractionType);
//    procedure SetGeneralizedFlowLakeAbsent(
//      const Value: TLakeBoundaryInteraction);
    procedure SetGeneralizedFlowLakePresent(
      const Value: TLakeBoundaryInteraction);
    procedure SetGeneralizedTransportInteractionType(
      const Value: TGeneralizedTransportInteractionType);
//    procedure SetGeneralizedTransportLakeAbsent(
//      const Value: TLakeBoundaryInteraction);
    procedure SetGeneralizedTransportLakePresent(
      const Value: TLakeBoundaryInteraction);
//    procedure SetSpecifiedPressureLakeAbsent(
//      const Value: TLakeBoundaryInteraction);
    procedure SetSpecifiedPressureLakePresent(
      const Value: TLakeBoundaryInteraction);
//    procedure SetSpecifiedULakeAbsent(const Value: TLakeBoundaryInteraction);
    procedure SetSpecifiedULakePresent(const Value: TLakeBoundaryInteraction);
//    procedure SetUSourceSinkLakeAbsent(const Value: TLakeBoundaryInteraction);
    procedure SetUSourceSinkLakePresent(const Value: TLakeBoundaryInteraction);
    procedure SetUseLakes(const Value: boolean);
    procedure SetAllNodesLakes(const Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Initialize;
    // FRROD
    property RechargeFraction: double read GetRechargeFraction
      write SetRechargeFraction;
    // FDROD
    property DischargeFraction: double read GetDischargeFraction
      write SetDischargeFraction;
    // VLIM
//    property MinLakeVolume: double read GetMinLakeVolume write SetMinLakeVolume;
    // RNOLK
    property SubmergedOutput: double read GetSubmergedOutput
      write SetSubmergedOutput;
  published
    // ITLMAX
    // @name is retained for backwards compatibility.
    property MaxLakeIterations: Integer read FMaxLakeIterations
      write SetMaxLakeIterations stored False;
    // NLAKPR
    property LakeOutputCycle: Integer read FLakeOutputCycle
      write SetLakeOutputCycle;
    property StoredRechargeFraction: TRealStorage read FStoredRechargeFraction
      write SetStoredRechargeFraction;
    property StoredDischargeFraction: TRealStorage read FStoredDischargeFraction
      write SetStoredDischargeFraction;
    // @name is no longer included in the SUTRA 3.0 input.
    property StoredMinLakeVolume: TRealStorage read FStoredMinLakeVolume
      write SetStoredMinLakeVolume stored False;
    property StoredSubmergedOutput: TRealStorage read FStoredSubmergedOutput
      write SetStoredSubmergedOutput;
    // ILKF
    property FluidSourceSinkLakePresent: TLakeBoundaryInteraction
      read FFluidSourceSinkLakePresent write SetFluidSourceSinkLakePresent
      stored True;
    // INLKF
//    property FluidSourceSinkLakeAbsent: TLakeBoundaryInteraction
//      read FFluidSourceSinkLakeAbsent write SetFluidSourceSinkLakeAbsent
//      stored True;
    // ILKS
    property USourceSinkLakePresent: TLakeBoundaryInteraction
      read FUSourceSinkLakePresent write SetUSourceSinkLakePresent stored True;
    // INLKS
//    property USourceSinkLakeAbsent: TLakeBoundaryInteraction
//      read FUSourceSinkLakeAbsent write SetUSourceSinkLakeAbsent stored True;
    // ILKP
    property SpecifiedPressureLakePresent: TLakeBoundaryInteraction
      read FSpecifiedPressureLakePresent write SetSpecifiedPressureLakePresent
      stored True;
    // INLKP
//    property SpecifiedPressureLakeAbsent: TLakeBoundaryInteraction
//      read FSpecifiedPressureLakeAbsent write SetSpecifiedPressureLakeAbsent
//      stored True;
    // ILKU
    property SpecifiedULakePresent: TLakeBoundaryInteraction
      read FSpecifiedULakePresent write SetSpecifiedULakePresent stored True;
    // INLKU
//    property SpecifiedULakeAbsent: TLakeBoundaryInteraction
//      read FSpecifiedULakeAbsent write SetSpecifiedULakeAbsent stored True;
    // ILKPG
    property GeneralizedFlowLakePresent: TLakeBoundaryInteraction
      read FGeneralizedFlowLakePresent write SetGeneralizedFlowLakePresent
      stored True;
    // INLKPG
//    property GeneralizedFlowLakeAbsent: TLakeBoundaryInteraction
//      read FGeneralizedFlowLakeAbsent write SetGeneralizedFlowLakeAbsent
//      stored True;
    // ILKUG
    property GeneralizedTransportLakePresent: TLakeBoundaryInteraction
      read FGeneralizedTransportLakePresent
      write SetGeneralizedTransportLakePresent stored True;
    // INLKUG
//    property GeneralizedTransportLakeAbsent: TLakeBoundaryInteraction
//      read FGeneralizedTransportLakeAbsent
//      write SetGeneralizedTransportLakeAbsent stored True;
    // CLKPG
    property GeneralizedFlowInteractionType: TGeneralizedFlowInteractionType
      read FGeneralizedFlowInteractionType
      write SetGeneralizedFlowInteractionType stored True;
    // CLKUG
    property GeneralizedTransportInteractionType: TGeneralizedTransportInteractionType
      read FGeneralizedTransportInteractionType
      write SetGeneralizedTransportInteractionType stored True;
    property UseLakes: boolean read FUseLakes write SetUseLakes stored True;
    property AllNodesLakes: Boolean read FAllNodesLakes write SetAllNodesLakes
      stored True;
  end;

  TSutraOptions = class(TGoPhastPersistent)
  strict private
    FModel: TBaseModel;
  private
    FTransportChoice: TTransportChoice;
    FSaturationChoice: TSaturationChoice;
    FTitleLines: AnsiString;
    FSimulationType: TSimulationType;
    FStartType: TStartType;
    FFullRestartFileName: string;
    FRestartFrequency: Integer;
    FMaxIterations: Integer;
    FUSolutionMethod: TUSolutionMethod;
    FMaxTransportIterations: Integer;
    FMaxPressureIterations: Integer;
    FPresSolutionMethod: TPressureSolutionMethod;
    FStoredTransportCriterion: TRealStorage;
    FStoredPressureCriterion: TRealStorage;
    FStoredFractionalUpstreamWeight: TRealStorage;
    FStoredPressureFactor: TRealStorage;
    FStoredUCriterion: TRealStorage;
    FStoredNonLinPressureCriterion: TRealStorage;
    FStoredUFactor: TRealStorage;
    FStoredBaseConcentration: TRealStorage;
    FStoredBaseFluidDensity: TRealStorage;
    FStoredViscosity: TRealStorage;
    FStoredFluidCompressibility: TRealStorage;
    FStoredFluidDiffusivity: TRealStorage;
    FStoredFluidDensityCoefficientConcentration: TRealStorage;
    FStoredFluidSpecificHeat: TRealStorage;
    FStoredSolidGrainSpecificHeat: TRealStorage;
    FStoredMatrixCompressibility: TRealStorage;
    FStoredSolidGrainDensity: TRealStorage;
    FStoredFirstDistributionCoefficient: TRealStorage;
    FStoredSolidGrainDiffusivity: TRealStorage;
    FSorptionModel: TSorptionModel;
    FStoredSecondDistributionCoefficient: TRealStorage;
    FStoredZeroFluidProduction: TRealStorage;
    FStoredFirstFluidProduction: TRealStorage;
    FStoredGravityZ: TRealStorage;
    FStoredGravityX: TRealStorage;
    FStoredGravityY: TRealStorage;
    FStoredZeroImmobileProduction: TRealStorage;
    FStoredFirstImmobileProduction: TRealStorage;
    FStoredScaleFactor: TRealStorage;
    FStoredFluidThermalConductivity: TRealStorage;
    FStoredFluidDensityCoefficientTemperature: TRealStorage;
    FStoredBaseTemperature: TRealStorage;
    FFullReadStartRestartFileName: string;
    FReadStart: TReadStart;
    FLakeOptions: TSutraLakeOptions;
    procedure SetTransportChoice(const Value: TTransportChoice);
    procedure SetSaturationChoice(const Value: TSaturationChoice);
    procedure SetTitleLines(const Value: AnsiString);
    procedure SetSimulationType(const Value: TSimulationType);
    procedure SetRestartFileName(const Value: string);
    procedure SetStartType(const Value: TStartType);
    procedure SetRestartFrequency(const Value: Integer);
    procedure SetFractionalUpstreamWeight(const Value: double);
    procedure SetPressureFactor(const Value: double);
    procedure SetUFactor(const Value: double);
    procedure SetMaxIterations(const Value: Integer);
    procedure SetPressureCriterion(const Value: double);
    procedure SetUCriterion(const Value: double);
    procedure SetMaxPressureIterations(const Value: Integer);
    procedure SetMaxTransportIterations(const Value: Integer);
    procedure SetPresSolutionMethod(const Value: TPressureSolutionMethod);
    procedure SetTransportCriterion(const Value: double);
    procedure SetUSolutionMethod(const Value: TUSolutionMethod);
    procedure SetNonLinPressureCriterion(const Value: double);
    procedure ValueChanged(Sender: TObject);
    function GetFractionalUpstreamWeight: double;
    function GetNonLinPressureCriterion: double;
    function GetPressureCriterion: double;
    function GetPressureFactor: double;
    function GetTransportCriterion: double;
    function GetUCriterion: double;
    function GetUFactor: double;
    procedure SetStoredFractionalUpstreamWeight(const Value: TRealStorage);
    procedure SetStoredNonLinPressureCriterion(const Value: TRealStorage);
    procedure SetStoredPressureCriterion(const Value: TRealStorage);
    procedure SetStoredPressureFactor(const Value: TRealStorage);
    procedure SetStoredTransportCriterion(const Value: TRealStorage);
    procedure SetStoredUCriterion(const Value: TRealStorage);
    procedure SetStoredUFactor(const Value: TRealStorage);
    procedure SetStoredBaseFluidDensity(const Value: TRealStorage);
    procedure SetStoredBaseConcentration(const Value: TRealStorage);
    procedure SetStoredFluidCompressibility(const Value: TRealStorage);
    procedure SetStoredFluidDensityCoefficientConcentration(const Value: TRealStorage);
    procedure SetStoredFluidDiffusivity(const Value: TRealStorage);
    procedure SetStoredFluidSpecificHeat(const Value: TRealStorage);
    procedure SetStoredViscosity(const Value: TRealStorage);
    function GetBaseFluidDensity: double;
    function GetBaseConcentration: double;
    function GetFluidCompressibility: double;
    function GetFluidDensityCoefficientConcentration: double;
    function GetFluidDiffusivity: double;
    function GetFluidSpecificHeat: double;
    function GetViscosity: double;
    procedure SetBaseFluidDensity(const Value: double);
    procedure SetBaseConcentration(const Value: double);
    procedure SetFluidCompressibility(const Value: double);
    procedure SetFluidDensityCoefficientConcentration(const Value: double);
    procedure SetFluidDiffusivity(const Value: double);
    procedure SetFluidSpecificHeat(const Value: double);
    procedure SetViscosity(const Value: double);
    function GetFirstDistributionCoefficient: double;
    function GetMatrixCompressibility: double;
    function GetSecondDistributionCoefficient: double;
    function GetSolidGrainDensity: double;
    function GetSolidGrainDiffusivity: double;
    function GetSolidGrainSpecificHeat: double;
    procedure SetFirstDistributionCoefficient(const Value: double);
    procedure SetMatrixCompressibility(const Value: double);
    procedure SetSecondDistributionCoefficient(const Value: double);
    procedure SetSolidGrainDensity(const Value: double);
    procedure SetSolidGrainDiffusivity(const Value: double);
    procedure SetSolidGrainSpecificHeat(const Value: double);
    procedure SetSorptionModel(const Value: TSorptionModel);
    procedure SetStoredFirstDistributionCoefficient(const Value: TRealStorage);
    procedure SetStoredMatrixCompressibility(const Value: TRealStorage);
    procedure SetStoredSecondDistributionCoefficient(const Value: TRealStorage);
    procedure SetStoredSolidGrainDensity(const Value: TRealStorage);
    procedure SetStoredSolidGrainDiffusivity(const Value: TRealStorage);
    procedure SetStoredSolidGrainSpecificHeat(const Value: TRealStorage);
    function GetFirstFluidProduction: double;
    function GetFirstImmobileProduction: double;
    function GetGravityX: double;
    function GetGravityY: double;
    function GetGravityZ: double;
    function GetZeroFluidProduction: double;
    function GetZeroImmobileProduction: double;
    procedure SetFirstFluidProduction(const Value: double);
    procedure SetFirstImmobileProduction(const Value: double);
    procedure SetGravityX(const Value: double);
    procedure SetGravityY(const Value: double);
    procedure SetGravityZ(const Value: double);
    procedure SetStoredFirstFluidProduction(const Value: TRealStorage);
    procedure SetStoredFirstImmobileProduction(const Value: TRealStorage);
    procedure SetStoredGravityX(const Value: TRealStorage);
    procedure SetStoredGravityY(const Value: TRealStorage);
    procedure SetStoredGravityZ(const Value: TRealStorage);
    procedure SetStoredZeroFluidProduction(const Value: TRealStorage);
    procedure SetStoredZeroImmobileProduction(const Value: TRealStorage);
    procedure SetZeroFluidProduction(const Value: double);
    procedure SetZeroImmobileProduction(const Value: double);
    procedure SetStoredScaleFactor(const Value: TRealStorage);
    function GetScaleFactor: double;
    procedure SetScaleFactor(const Value: double);
    procedure SetStoredFluidThermalConductivity(const Value: TRealStorage);
    function GetFluidThermalConductivity: Double;
    procedure SetFluidThermalConductivity(const Value: Double);
    function GetFluidDensityCoefficientTemperature: double;
    procedure SetFluidDensityCoefficientTemperature(const Value: double);
    procedure SetStoredFluidDensityCoefficientTemperature(
      const Value: TRealStorage);
    procedure SetStoredBaseTemperature(const Value: TRealStorage);
    function GetBaseTemperature: double;
    procedure SetBaseTemperature(const Value: double);
    procedure SetReadStart(const Value: TReadStart);
    procedure SetFullReadStartRestartFileName(Value: string);
    function GetRestartFileName: string;
    procedure SetFullRestartFileName(Value: string);
    function GetReadStartRestartFileName: string;
    procedure SetReadStartRestartFileName(const Value: string);
    procedure SetLakeOptions(const Value: TSutraLakeOptions);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property Model: TBaseModel read FModel;
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    Constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Initialize;
    // Data Set 5: UP
    property FractionalUpstreamWeight: double read GetFractionalUpstreamWeight
      write SetFractionalUpstreamWeight;
    // Data Set 5: GNUP, Pressure boundary condition factor
    property PressureFactor: double read GetPressureFactor
      write SetPressureFactor;
    // Data Set 5: GNUU, Concentration/temperature boundary condition factor.
    property UFactor: double read GetUFactor write SetUFactor;
    // Data Set 7a: RPMAX
    property NonLinPressureCriterion: double read GetNonLinPressureCriterion
      write SetNonLinPressureCriterion;
    // Data Set 7a: RUMAX
    property UCriterion: double read GetUCriterion write SetUCriterion;
    // Data Set 7b: TOLP
    property PressureCriterion: double read GetPressureCriterion
      write SetPressureCriterion;
    // Data Set 7c: TOLU
    property TransportCriterion: double read GetTransportCriterion
      write SetTransportCriterion;

    // Data Set 9: COMPFL
    property FluidCompressibility: double read GetFluidCompressibility
      write SetFluidCompressibility;
    // Data Set 9: CW
    property FluidSpecificHeat: double read GetFluidSpecificHeat
      write SetFluidSpecificHeat;
    // Data Set 9: SIGMAW for solute transport.
    // @seealso(FluidThermalConductivity).
    property FluidDiffusivity: double read GetFluidDiffusivity
      write SetFluidDiffusivity;
    // Data Set 9: SIGMAW for energy transport
    // @seealso(FluidDiffusivity).
    property FluidThermalConductivity: Double read GetFluidThermalConductivity
      write SetFluidThermalConductivity;

    // Data Set 9: RHOW0
    property BaseFluidDensity: double read GetBaseFluidDensity
      write SetBaseFluidDensity;
    // Data Set 9: URHOW0 for solute transport.
    property BaseConcentration: double read GetBaseConcentration write SetBaseConcentration;
    // Data Set 9: URHOW0 for energy transport.
    property BaseTemperature: double read GetBaseTemperature write SetBaseTemperature;
    // Data Set 9: DRWDU for solute transport.
    property FluidDensityCoefficientConcentration: double
      read GetFluidDensityCoefficientConcentration
      write SetFluidDensityCoefficientConcentration;
    // Data Set 9: DRWDU for energy transport.
    property FluidDensityCoefficientTemperature: double
      read GetFluidDensityCoefficientTemperature
      write SetFluidDensityCoefficientTemperature;
    // Data Set 9: VISC0 for solute transport. @seealso(ScaleFactor).
    property Viscosity: double read GetViscosity
      write SetViscosity;
    // Data Set 9: VISC0 for energy transport. @seealso(Viscosity).
    property ScaleFactor: double read GetScaleFactor write SetScaleFactor;

    // Data Set 10: COMPMA
    property MatrixCompressibility: double read GetMatrixCompressibility
      write SetMatrixCompressibility;
    // Data Set 10: CS
    property SolidGrainSpecificHeat: double read GetSolidGrainSpecificHeat
      write SetSolidGrainSpecificHeat;
    // Data Set 10: SIGMAS
    property SolidGrainDiffusivity: double read GetSolidGrainDiffusivity
      write SetSolidGrainDiffusivity;
    // Data Set 10: RHOS
    property SolidGrainDensity: double read GetSolidGrainDensity
      write SetSolidGrainDensity;
    // Data Set 11: CHI1
    property FirstDistributionCoefficient: double
      read GetFirstDistributionCoefficient
      write SetFirstDistributionCoefficient;
    // Data Set 11: CHI2
    property SecondDistributionCoefficient: double
      read GetSecondDistributionCoefficient
      write SetSecondDistributionCoefficient;
    // Data Set 12: PRODFØ
    property ZeroFluidProduction: double read GetZeroFluidProduction
      write SetZeroFluidProduction;
    // Data Set 12: PRODSØ
    property ZeroImmobileProduction: double read GetZeroImmobileProduction
      write SetZeroImmobileProduction;
    // Data Set 12: PRODF1
    property FirstFluidProduction: double read GetFirstFluidProduction
      write SetFirstFluidProduction;
    // Data Set 12: PRODS1
    property FirstImmobileProduction: double read GetFirstImmobileProduction
      write SetFirstImmobileProduction;
    // Data Set 13: GRAVX
    property GravityX: double read GetGravityX write SetGravityX;
    // Data Set 13: GRAVY
    property GravityY: double read GetGravityY write SetGravityY;
    // Data Set 13: GRAVZ
    property GravityZ: double read GetGravityZ write SetGravityZ;
    property FullRestartFileName: string read FFullRestartFileName
      write SetFullRestartFileName;
    // ICS Data sets 2 and 3
    property FullReadStartRestartFileName: string read FFullReadStartRestartFileName
      write SetFullReadStartRestartFileName;
  published
    // Data Set 1: TITLE1 and TITLE2 plus some comments
    property TitleLines: AnsiString read FTitleLines write SetTitleLines;
    // Data Set 2A: SIMULA
    property TransportChoice: TTransportChoice read FTransportChoice
      write SetTransportChoice stored true;
    // Data Set 4: CUNSAT
    property SaturationChoice: TSaturationChoice read FSaturationChoice
      write SetSaturationChoice stored true;
    // Data Set 4: CSSFLO and CSSTRA
    property SimulationType: TSimulationType read FSimulationType
      write SetSimulationType stored true;
    // Data Set 4: CREAD
    property StartType: TStartType read FStartType write SetStartType stored true;
    // @name is a relative file name.
    property RestartFileName: string read GetRestartFileName
      write SetRestartFileName;
    // Data Set 4: ISTORE
    property RestartFrequency: Integer read FRestartFrequency
      write SetRestartFrequency stored true;
    // Data Set 5: UP
    property StoredFractionalUpstreamWeight: TRealStorage
      read FStoredFractionalUpstreamWeight
      write SetStoredFractionalUpstreamWeight;
    // Data Set 5: GNUP, Pressure boundary condition factor
    property StoredPressureFactor: TRealStorage read FStoredPressureFactor
      write SetStoredPressureFactor;
    // Data Set 5: GNUU, Concentration/temperature boundary condition factor.
    property StoredUFactor: TRealStorage read FStoredUFactor
      write SetStoredUFactor;
    // Data Set 7a: ITRMAX
    property MaxIterations: Integer read FMaxIterations write SetMaxIterations stored true;
    // Data Set 7a: RPMAX
    property StoredNonLinPressureCriterion: TRealStorage
      read FStoredNonLinPressureCriterion
      write SetStoredNonLinPressureCriterion;
    // Data Set 7a: RUMAX
    property StoredUCriterion: TRealStorage read FStoredUCriterion
      write SetStoredUCriterion;
    // Data Set 7b: CSOLVP
    property PresSolutionMethod: TPressureSolutionMethod
      read FPresSolutionMethod write SetPresSolutionMethod stored true;
    // Data Set 7b: ITRMXP
    property MaxPressureIterations: Integer read FMaxPressureIterations
      write SetMaxPressureIterations stored true;
    // Data Set 7b: TOLP
    property StoredPressureCriterion: TRealStorage read FStoredPressureCriterion
      write SetStoredPressureCriterion;
    // Data Set 7c: CSOLVU
    property USolutionMethod: TUSolutionMethod read FUSolutionMethod
      write SetUSolutionMethod stored true;
    // Data Set 7c: ITRMXU
    property MaxTransportIterations: Integer read FMaxTransportIterations
      write SetMaxTransportIterations stored true;
    // Data Set 7c: TOLU
    property StoredTransportCriterion: TRealStorage
      read FStoredTransportCriterion write SetStoredTransportCriterion;

    // Data Set 9: COMPFL
    property StoredFluidCompressibility: TRealStorage
      read FStoredFluidCompressibility write SetStoredFluidCompressibility;
    // Data Set 9: CW
    property StoredFluidSpecificHeat: TRealStorage read FStoredFluidSpecificHeat
      write SetStoredFluidSpecificHeat;

    // Data Set 9: SIGMAW for solute transport.
    // @seealso(StoredFluidThermalConductivity).
    property StoredFluidDiffusivity: TRealStorage read FStoredFluidDiffusivity
      write SetStoredFluidDiffusivity;
    // Data Set 9: SIGMAW for energy transport.
    // @seealso(StoredFluidDiffusivity).
    property StoredFluidThermalConductivity: TRealStorage
      read FStoredFluidThermalConductivity
      write SetStoredFluidThermalConductivity;

    // Data Set 9: RHOW0
    property StoredBaseFluidDensity: TRealStorage read FStoredBaseFluidDensity
      write SetStoredBaseFluidDensity;
    // Data Set 9: URHOW0 for solute transport.
    property StoredBaseConcentration: TRealStorage read FStoredBaseConcentration
      write SetStoredBaseConcentration;
    // Data Set 9: URHOW0 for energy transport.
    property StoredBaseTemperature: TRealStorage read FStoredBaseTemperature
      write SetStoredBaseTemperature;
    // Data Set 9: DRWDU for solute transport
    property StoredFluidDensityCoefficientConcentration: TRealStorage
      read FStoredFluidDensityCoefficientConcentration
      write SetStoredFluidDensityCoefficientConcentration;
    // Data Set 9: DRWDU for energy transport
    property StoredFluidDensityCoefficientTemperature: TRealStorage
      read FStoredFluidDensityCoefficientTemperature
      write SetStoredFluidDensityCoefficientTemperature;
    // Data Set 9: VISC0 for solute transport. @seealso(StoredScaleFactor).
    property StoredViscosity: TRealStorage
      read FStoredViscosity write SetStoredViscosity;
    // Data Set 9: VISC0 for energy transport. @seealso(StoredViscosity).
    property StoredScaleFactor: TRealStorage
      read FStoredScaleFactor write SetStoredScaleFactor;

    // Data Set 10: COMPMA
    property StoredMatrixCompressibility: TRealStorage
      read FStoredMatrixCompressibility write SetStoredMatrixCompressibility;
    // Data Set 10: CS
    property StoredSolidGrainSpecificHeat: TRealStorage
      read FStoredSolidGrainSpecificHeat write SetStoredSolidGrainSpecificHeat;
    // Data Set 10: SIGMAS
    property StoredSolidGrainDiffusivity: TRealStorage
      read FStoredSolidGrainDiffusivity write SetStoredSolidGrainDiffusivity;
    // Data Set 10: RHOS
    property StoredSolidGrainDensity: TRealStorage read FStoredSolidGrainDensity
      write SetStoredSolidGrainDensity;
    // Data Set 11: ADSMOD
    property SorptionModel: TSorptionModel read FSorptionModel
      write SetSorptionModel stored true;
    // Data Set 11: CHI1
    property StoredFirstDistributionCoefficient: TRealStorage
      read FStoredFirstDistributionCoefficient
      write SetStoredFirstDistributionCoefficient;
    // Data Set 11: CHI2
    property StoredSecondDistributionCoefficient: TRealStorage
      read FStoredSecondDistributionCoefficient
      write SetStoredSecondDistributionCoefficient;

    // Data Set 12: PRODFØ
    property StoredZeroFluidProduction: TRealStorage
      read FStoredZeroFluidProduction write SetStoredZeroFluidProduction;
    // Data Set 12: PRODSØ
    property StoredZeroImmobileProduction: TRealStorage
      read FStoredZeroImmobileProduction write SetStoredZeroImmobileProduction;
    // Data Set 12: PRODF1
    property StoredFirstFluidProduction: TRealStorage
      read FStoredFirstFluidProduction write SetStoredFirstFluidProduction;
    // Data Set 12: PRODS1
    property StoredFirstImmobileProduction: TRealStorage
      read FStoredFirstImmobileProduction
      write SetStoredFirstImmobileProduction;
    // Data Set 13: GRAVX
    property StoredGravityX: TRealStorage read FStoredGravityX
      write SetStoredGravityX;
    // Data Set 13: GRAVY
    property StoredGravityY: TRealStorage read FStoredGravityY
      write SetStoredGravityY;
    // Data Set 13: GRAVZ
    property StoredGravityZ: TRealStorage read FStoredGravityZ
      write SetStoredGravityZ;
    // ICS Data sets 2 and 3
    property ReadStart: TReadStart read FReadStart write SetReadStart;
    // ICS Data sets 2 and 3
    // @name is a relative file name.
    property ReadStartRestartFileName: string read GetReadStartRestartFileName
      write SetReadStartRestartFileName;
    property LakeOptions: TSutraLakeOptions read FLakeOptions
      write SetLakeOptions;
  end;


implementation

uses
  PhastModelUnit, VectorDisplayUnit;

{ TSutraOptions }

procedure TSutraOptions.Assign(Source: TPersistent);
var
  SourceOptions: TSutraOptions;
begin
  if Source is TSutraOptions then
  begin
    SourceOptions := TSutraOptions(Source);
    TransportChoice := SourceOptions.TransportChoice;
    SaturationChoice := SourceOptions.SaturationChoice;
    TitleLines := SourceOptions.TitleLines;
    SimulationType := SourceOptions.SimulationType;
    StartType := SourceOptions.StartType;
    FullRestartFileName := SourceOptions.FullRestartFileName;
    RestartFrequency := SourceOptions.RestartFrequency;
    FractionalUpstreamWeight := SourceOptions.FractionalUpstreamWeight;
    PressureFactor := SourceOptions.PressureFactor;
    UFactor := SourceOptions.UFactor;
    MaxIterations := SourceOptions.MaxIterations;
    PressureCriterion := SourceOptions.PressureCriterion;
    NonLinPressureCriterion := SourceOptions.NonLinPressureCriterion;
    UCriterion := SourceOptions.UCriterion;
    PresSolutionMethod := SourceOptions.PresSolutionMethod;
    MaxPressureIterations := SourceOptions.MaxPressureIterations;
    PressureCriterion := SourceOptions.PressureCriterion;
    USolutionMethod := SourceOptions.USolutionMethod;
    MaxTransportIterations := SourceOptions.MaxTransportIterations;
    TransportCriterion := SourceOptions.TransportCriterion;

    FluidCompressibility := SourceOptions.FluidCompressibility;
    FluidSpecificHeat := SourceOptions.FluidSpecificHeat;
    FluidDiffusivity := SourceOptions.FluidDiffusivity;
    FluidThermalConductivity := SourceOptions.FluidThermalConductivity;
    BaseFluidDensity := SourceOptions.BaseFluidDensity;
    BaseConcentration := SourceOptions.BaseConcentration;
    BaseTemperature := SourceOptions.BaseTemperature;
    FluidDensityCoefficientConcentration :=
      SourceOptions.FluidDensityCoefficientConcentration;
    StoredFluidDensityCoefficientTemperature  :=
      SourceOptions.StoredFluidDensityCoefficientTemperature;
    Viscosity := SourceOptions.Viscosity;
    ScaleFactor := SourceOptions.ScaleFactor;

    MatrixCompressibility := SourceOptions.MatrixCompressibility;
    SolidGrainSpecificHeat := SourceOptions.SolidGrainSpecificHeat;
    SolidGrainDiffusivity := SourceOptions.SolidGrainDiffusivity;
    SolidGrainDensity := SourceOptions.SolidGrainDensity;
    SorptionModel := SourceOptions.SorptionModel;
    FirstDistributionCoefficient := SourceOptions.FirstDistributionCoefficient;
    SecondDistributionCoefficient :=
      SourceOptions.SecondDistributionCoefficient;

    ZeroFluidProduction := SourceOptions.ZeroFluidProduction;
    ZeroImmobileProduction := SourceOptions.ZeroImmobileProduction;
    FirstFluidProduction := SourceOptions.FirstFluidProduction;
    FirstImmobileProduction := SourceOptions.FirstImmobileProduction;
    GravityX := SourceOptions.GravityX;
    GravityY := SourceOptions.GravityY;
    GravityZ := SourceOptions.GravityZ;

    ReadStart := SourceOptions.ReadStart;
    FullReadStartRestartFileName := SourceOptions.FullReadStartRestartFileName;

    LakeOptions := SourceOptions.LakeOptions;
    // SimulationType := SourceOptions.SimulationType;
    // SimulationType := SourceOptions.SimulationType;
    // SimulationType := SourceOptions.SimulationType;
    // SimulationType := SourceOptions.SimulationType;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraOptions.Create(Model: TBaseModel);
begin
  if Model = nil then
  begin
    inherited Create(nil);
    FLakeOptions := TSutraLakeOptions.Create(nil);
  end
  else
  begin
    inherited Create(Model.Invalidate);
    FLakeOptions := TSutraLakeOptions.Create(Model.Invalidate);
  end;
  Assert((Model = nil) or (Model is TCustomModel));
  FModel := Model;
  FRestartFrequency := 10000;

  FStoredTransportCriterion := TRealStorage.Create;
  FStoredPressureCriterion := TRealStorage.Create;
  FStoredFractionalUpstreamWeight := TRealStorage.Create;
  FStoredPressureFactor := TRealStorage.Create;
  FStoredUCriterion := TRealStorage.Create;
  FStoredNonLinPressureCriterion := TRealStorage.Create;
  FStoredUFactor := TRealStorage.Create;

  FStoredBaseConcentration := TRealStorage.Create;
  FStoredBaseTemperature := TRealStorage.Create;
  FStoredBaseFluidDensity := TRealStorage.Create;
  FStoredViscosity := TRealStorage.Create;
  FStoredScaleFactor := TRealStorage.Create;
  FStoredFluidCompressibility := TRealStorage.Create;
  FStoredFluidDiffusivity := TRealStorage.Create;
  FStoredFluidThermalConductivity := TRealStorage.Create;
  FStoredFluidDensityCoefficientConcentration := TRealStorage.Create;
  FStoredFluidDensityCoefficientTemperature := TRealStorage.Create;
  FStoredFluidSpecificHeat := TRealStorage.Create;

  FStoredSolidGrainSpecificHeat := TRealStorage.Create;
  FStoredMatrixCompressibility := TRealStorage.Create;
  FStoredSolidGrainDensity := TRealStorage.Create;
  FStoredFirstDistributionCoefficient := TRealStorage.Create;
  FStoredSolidGrainDiffusivity := TRealStorage.Create;
  FStoredSecondDistributionCoefficient := TRealStorage.Create;

  FStoredZeroFluidProduction := TRealStorage.Create;
  FStoredFirstFluidProduction := TRealStorage.Create;
  FStoredGravityZ := TRealStorage.Create;
  FStoredGravityX := TRealStorage.Create;
  FStoredGravityY := TRealStorage.Create;
  FStoredZeroImmobileProduction := TRealStorage.Create;
  FStoredFirstImmobileProduction := TRealStorage.Create;

  Initialize;

  FStoredTransportCriterion.OnChange := ValueChanged;
  FStoredPressureCriterion.OnChange := ValueChanged;
  FStoredFractionalUpstreamWeight.OnChange := ValueChanged;
  FStoredPressureFactor.OnChange := ValueChanged;
  FStoredUCriterion.OnChange := ValueChanged;
  FStoredNonLinPressureCriterion.OnChange := ValueChanged;
  FStoredUFactor.OnChange := ValueChanged;

  FStoredBaseConcentration.OnChange := ValueChanged;
  FStoredBaseTemperature.OnChange := ValueChanged;
  FStoredBaseFluidDensity.OnChange := ValueChanged;
  FStoredViscosity.OnChange := ValueChanged;
  FStoredScaleFactor.OnChange := ValueChanged;
  FStoredFluidCompressibility.OnChange := ValueChanged;
  FStoredFluidDiffusivity.OnChange := ValueChanged;
  FStoredFluidThermalConductivity.OnChange := ValueChanged;
  FStoredFluidDensityCoefficientConcentration.OnChange := ValueChanged;
  FStoredFluidDensityCoefficientTemperature.OnChange := ValueChanged;
  FStoredFluidSpecificHeat.OnChange := ValueChanged;

  FStoredSolidGrainSpecificHeat.OnChange := ValueChanged;
  FStoredMatrixCompressibility.OnChange := ValueChanged;
  FStoredSolidGrainDensity.OnChange := ValueChanged;
  FStoredFirstDistributionCoefficient.OnChange := ValueChanged;
  FStoredSolidGrainDiffusivity.OnChange := ValueChanged;
  FStoredSecondDistributionCoefficient.OnChange := ValueChanged;

  FStoredZeroFluidProduction.OnChange := ValueChanged;
  FStoredFirstFluidProduction.OnChange := ValueChanged;
  FStoredGravityZ.OnChange := ValueChanged;
  FStoredGravityX.OnChange := ValueChanged;
  FStoredGravityY.OnChange := ValueChanged;
  FStoredZeroImmobileProduction.OnChange := ValueChanged;
  FStoredFirstImmobileProduction.OnChange := ValueChanged;

end;

destructor TSutraOptions.Destroy;
begin
  FLakeOptions.Free;

  FStoredTransportCriterion.Free;
  FStoredPressureCriterion.Free;
  FStoredFractionalUpstreamWeight.Free;
  FStoredPressureFactor.Free;
  FStoredUCriterion.Free;
  FStoredNonLinPressureCriterion.Free;
  FStoredUFactor.Free;

  FStoredBaseConcentration.Free;
  FStoredBaseTemperature.Free;
  FStoredBaseFluidDensity.Free;
  FStoredViscosity.Free;
  FStoredScaleFactor.Free;
  FStoredFluidCompressibility.Free;
  FStoredFluidDiffusivity.Free;
  FStoredFluidThermalConductivity.Free;
  FStoredFluidDensityCoefficientConcentration.Free;
  FStoredFluidDensityCoefficientTemperature.Free;
  FStoredFluidSpecificHeat.Free;

  FStoredSolidGrainSpecificHeat.Free;
  FStoredMatrixCompressibility.Free;
  FStoredSolidGrainDensity.Free;
  FStoredFirstDistributionCoefficient.Free;
  FStoredSolidGrainDiffusivity.Free;
  FStoredSecondDistributionCoefficient.Free;

  FStoredZeroFluidProduction.Free;
  FStoredFirstFluidProduction.Free;
  FStoredGravityZ.Free;
  FStoredGravityX.Free;
  FStoredGravityY.Free;
  FStoredZeroImmobileProduction.Free;
  FStoredFirstImmobileProduction.Free;

  inherited;
end;

function TSutraOptions.GetBaseFluidDensity: double;
begin
  result := StoredBaseFluidDensity.Value;
end;

function TSutraOptions.GetBaseTemperature: double;
begin
  result := StoredBaseTemperature.Value;
end;

function TSutraOptions.GetBaseConcentration: double;
begin
  result := StoredBaseConcentration.Value;
end;

function TSutraOptions.GetFirstDistributionCoefficient: double;
begin
  result := StoredFirstDistributionCoefficient.Value;
end;

function TSutraOptions.GetFirstFluidProduction: double;
begin
  result := StoredFirstFluidProduction.Value;
end;

function TSutraOptions.GetFirstImmobileProduction: double;
begin
  result := StoredFirstImmobileProduction.Value;
end;

function TSutraOptions.GetFluidCompressibility: double;
begin
  result := StoredFluidCompressibility.Value;
end;

function TSutraOptions.GetFluidDensityCoefficientConcentration: double;
begin
  result := StoredFluidDensityCoefficientConcentration.Value;
end;

function TSutraOptions.GetFluidDensityCoefficientTemperature: double;
begin
  result := StoredFluidDensityCoefficientTemperature.Value;
end;

function TSutraOptions.GetFluidDiffusivity: double;
begin
  result := StoredFluidDiffusivity.Value;
end;

function TSutraOptions.GetFluidSpecificHeat: double;
begin
  result := StoredFluidSpecificHeat.Value;
end;

function TSutraOptions.GetFluidThermalConductivity: Double;
begin
  result := StoredFluidThermalConductivity.Value
end;

function TSutraOptions.GetFractionalUpstreamWeight: double;
begin
  result := StoredFractionalUpstreamWeight.Value;
end;

function TSutraOptions.GetGravityX: double;
begin
  result := StoredGravityX.Value;
end;

function TSutraOptions.GetGravityY: double;
begin
  result := StoredGravityY.Value;
end;

function TSutraOptions.GetGravityZ: double;
begin
  result := StoredGravityZ.Value;
end;

function TSutraOptions.GetMatrixCompressibility: double;
begin
  result := StoredMatrixCompressibility.Value;
end;

function TSutraOptions.GetNonLinPressureCriterion: double;
begin
  result := StoredNonLinPressureCriterion.Value;
end;

function TSutraOptions.GetPressureCriterion: double;
begin
  result := StoredPressureCriterion.Value;
end;

function TSutraOptions.GetPressureFactor: double;
begin
  result := StoredPressureFactor.Value;
end;

function TSutraOptions.GetReadStartRestartFileName: string;
//var
//  BaseDir: string;
begin
  result := FFullReadStartRestartFileName;
  if (Model <> nil) and (Result <> '') then
  begin
    result := (Model as TCustomModel).RelativeFileName(result);
  end;
end;

function TSutraOptions.GetRestartFileName: string;
//var
//  BaseDir: string;
begin
  result := FFullRestartFileName;
  if (Model <> nil) and (Result <> '') then
  begin
    result := (Model as TCustomModel).RelativeFileName(result);
  end;
end;

function TSutraOptions.GetScaleFactor: double;
begin
  result := StoredScaleFactor.Value;
end;

function TSutraOptions.GetSecondDistributionCoefficient: double;
begin
  result := StoredSecondDistributionCoefficient.Value;
end;

function TSutraOptions.GetSolidGrainDensity: double;
begin
  result := StoredSolidGrainDensity.Value;
end;

function TSutraOptions.GetSolidGrainDiffusivity: double;
begin
  result := StoredSolidGrainDiffusivity.Value;
end;

function TSutraOptions.GetSolidGrainSpecificHeat: double;
begin
  result := StoredSolidGrainSpecificHeat.Value;
end;

function TSutraOptions.GetTransportCriterion: double;
begin
  result := StoredTransportCriterion.Value;
end;

function TSutraOptions.GetUCriterion: double;
begin
  result := StoredUCriterion.Value;
end;

function TSutraOptions.GetUFactor: double;
begin
  result := StoredUFactor.Value;
end;

function TSutraOptions.GetViscosity: double;
begin
  result := StoredViscosity.Value;
end;

function TSutraOptions.GetZeroFluidProduction: double;
begin
  result := StoredZeroFluidProduction.Value;
end;

function TSutraOptions.GetZeroImmobileProduction: double;
begin
  result := StoredZeroImmobileProduction.Value;
end;

procedure TSutraOptions.Initialize;
begin
  TitleLines := '';
  TransportChoice := tcSolute;
  SaturationChoice := scSaturated;
  SimulationType := stSteadyFlowSteadyTransport;
  StartType := stCold;
  FullRestartFileName := '';
  RestartFrequency := 9999;
  StoredFractionalUpstreamWeight.Value := 0;
  StoredPressureFactor.Value := 0.1;
  StoredUFactor.Value := 1;
  MaxIterations := 1;
  StoredNonLinPressureCriterion.Value := 0;
  StoredUCriterion.Value := 0;
  PresSolutionMethod := psmDirect;
  MaxPressureIterations := 300;
  StoredPressureCriterion.Value := 1e-8;
  USolutionMethod := usmDirect;
  MaxTransportIterations := 300;
  StoredTransportCriterion.Value := 1e-8;
  StoredFluidCompressibility.Value := 4.47e-10;
  StoredFluidSpecificHeat.Value := 4182;
  StoredFluidDiffusivity.Value := 1.0e-9;
  StoredFluidThermalConductivity.Value := 0.6;
  StoredBaseFluidDensity.Value := 1000;
  StoredBaseConcentration.Value := 0;
  StoredBaseTemperature.Value := 20;
  StoredFluidDensityCoefficientConcentration.Value := 700;
  StoredFluidDensityCoefficientTemperature.Value := -0.375;
  StoredViscosity.Value := 0.001;
  StoredScaleFactor.Value := 1;
  StoredMatrixCompressibility.Value := 1e-8;
  StoredSolidGrainSpecificHeat.Value := 840;
  StoredSolidGrainDiffusivity.Value := 3.5;
  StoredSolidGrainDensity.Value := 2600;
  SorptionModel := smNone;
  StoredFirstDistributionCoefficient.Value := 0;
  StoredSecondDistributionCoefficient.Value := 0;
  StoredZeroFluidProduction.Value := 0;
  StoredZeroImmobileProduction.Value := 0;
  StoredFirstFluidProduction.Value := 0;
  StoredFirstImmobileProduction.Value := 0;
  StoredGravityX.Value := 0;
  StoredGravityY.Value := 0;
  StoredGravityZ.Value := -9.81;
  ReadStart := rsNone;
  FullReadStartRestartFileName := '';

  FLakeOptions.Initialize;
end;

procedure TSutraOptions.SetBaseFluidDensity(const Value: double);
begin
  StoredBaseFluidDensity.Value := Value;
end;

procedure TSutraOptions.SetBaseTemperature(const Value: double);
begin
  StoredBaseTemperature.Value := Value;
end;

procedure TSutraOptions.SetBaseConcentration(const Value: double);
begin
  StoredBaseConcentration.Value := Value;
end;

procedure TSutraOptions.SetFirstDistributionCoefficient(const Value: double);
begin
  StoredFirstDistributionCoefficient.Value := Value;
end;

procedure TSutraOptions.SetFirstFluidProduction(const Value: double);
begin
  StoredFirstFluidProduction.Value := Value;
end;

procedure TSutraOptions.SetFirstImmobileProduction(const Value: double);
begin
  StoredFirstImmobileProduction.Value := Value;
end;

procedure TSutraOptions.SetFluidCompressibility(const Value: double);
begin
  StoredFluidCompressibility.Value := Value;
end;

procedure TSutraOptions.SetFluidDensityCoefficientConcentration(const Value: double);
begin
  StoredFluidDensityCoefficientConcentration.Value := Value;
end;

procedure TSutraOptions.SetFluidDensityCoefficientTemperature(
  const Value: double);
begin
  StoredFluidDensityCoefficientTemperature.Value := Value;
end;

procedure TSutraOptions.SetFluidDiffusivity(const Value: double);
begin
  StoredFluidDiffusivity.Value := Value;
end;

procedure TSutraOptions.SetFluidSpecificHeat(const Value: double);
begin
  StoredFluidSpecificHeat.Value := Value;
end;

procedure TSutraOptions.SetFluidThermalConductivity(const Value: Double);
begin
  StoredFluidThermalConductivity.Value := Value;
end;

procedure TSutraOptions.SetFractionalUpstreamWeight(const Value: double);
begin
  StoredFractionalUpstreamWeight.Value := Value;
end;

procedure TSutraOptions.SetFullRestartFileName(Value: string);
begin
  Value := ExpandFileName(Value);
  SetStringProperty(FFullRestartFileName, Value);
end;

procedure TSutraOptions.SetGravityX(const Value: double);
begin
  StoredGravityX.Value := Value;
end;

procedure TSutraOptions.SetGravityY(const Value: double);
begin
  StoredGravityY.Value := Value;
end;

procedure TSutraOptions.SetGravityZ(const Value: double);
begin
  StoredGravityZ.Value := Value;
end;

procedure TSutraOptions.SetLakeOptions(const Value: TSutraLakeOptions);
begin
  FLakeOptions.Assign(Value);
end;


procedure TSutraOptions.SetMatrixCompressibility(const Value: double);
begin
  StoredMatrixCompressibility.Value := Value;
end;

procedure TSutraOptions.SetMaxIterations(const Value: Integer);
begin
  SetIntegerProperty(FMaxIterations, Value);
end;

procedure TSutraOptions.SetMaxPressureIterations(const Value: Integer);
begin
  SetIntegerProperty(FMaxPressureIterations, Value);
end;

procedure TSutraOptions.SetMaxTransportIterations(const Value: Integer);
begin
  SetIntegerProperty(FMaxTransportIterations, Value);
end;

procedure TSutraOptions.SetNonLinPressureCriterion(const Value: double);
begin
  StoredNonLinPressureCriterion.Value := Value;
end;

procedure TSutraOptions.SetPresSolutionMethod(const Value
  : TPressureSolutionMethod);
begin
  if FPresSolutionMethod <> Value then
  begin
    FPresSolutionMethod := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetPressureCriterion(const Value: double);
begin
  StoredPressureCriterion.Value := Value;
end;

procedure TSutraOptions.SetPressureFactor(const Value: double);
begin
  StoredPressureFactor.Value := Value;
end;

procedure TSutraOptions.SetReadStart(const Value: TReadStart);
begin
  if FReadStart <> Value then
  begin
    FReadStart := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetReadStartRestartFileName(const Value: string);
begin
  FullReadStartRestartFileName := Value;
end;

procedure TSutraOptions.SetFullReadStartRestartFileName(Value: string);
begin
  Value := ExpandFileName(Value);
  SetStringProperty(FFullReadStartRestartFileName, Value);
end;

procedure TSutraOptions.SetRestartFileName(const Value: string);
begin
  FullRestartFileName := Value;
end;

procedure TSutraOptions.SetRestartFrequency(const Value: Integer);
begin
  SetIntegerProperty(FRestartFrequency, Value);
end;

procedure TSutraOptions.SetSaturationChoice(const Value: TSaturationChoice);
begin
  if FSaturationChoice <> Value then
  begin
    FSaturationChoice := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetScaleFactor(const Value: double);
begin
  StoredScaleFactor.Value := Value;
end;

procedure TSutraOptions.SetSecondDistributionCoefficient(const Value: double);
begin
  StoredSecondDistributionCoefficient.Value := Value;
end;

procedure TSutraOptions.SetSimulationType(const Value: TSimulationType);
begin
  if FSimulationType <> Value then
  begin
    FSimulationType := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetSolidGrainDensity(const Value: double);
begin
  StoredSolidGrainDensity.Value := Value;
end;

procedure TSutraOptions.SetSolidGrainDiffusivity(const Value: double);
begin
  StoredSolidGrainDiffusivity.Value := Value;
end;

procedure TSutraOptions.SetSolidGrainSpecificHeat(const Value: double);
begin
  StoredSolidGrainSpecificHeat.Value := Value;
end;

procedure TSutraOptions.SetSorptionModel(const Value: TSorptionModel);
begin
  if FSorptionModel <> Value then
  begin
    FSorptionModel := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetStartType(const Value: TStartType);
begin
  if FStartType <> Value then
  begin
    FStartType := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetStoredBaseFluidDensity(const Value: TRealStorage);
begin
  FStoredBaseFluidDensity.Assign(Value);
end;

procedure TSutraOptions.SetStoredBaseTemperature(const Value: TRealStorage);
begin
  FStoredBaseTemperature.Assign(Value);
end;

procedure TSutraOptions.SetStoredBaseConcentration(const Value: TRealStorage);
begin
  FStoredBaseConcentration.Assign(Value);
end;

procedure TSutraOptions.SetStoredFirstDistributionCoefficient
  (const Value: TRealStorage);
begin
  FStoredFirstDistributionCoefficient.Assign(Value);
end;

procedure TSutraOptions.SetStoredFirstFluidProduction
  (const Value: TRealStorage);
begin
  FStoredFirstFluidProduction.Assign(Value);
end;

procedure TSutraOptions.SetStoredFirstImmobileProduction
  (const Value: TRealStorage);
begin
  FStoredFirstImmobileProduction.Assign(Value);
end;

procedure TSutraOptions.SetStoredFluidCompressibility
  (const Value: TRealStorage);
begin
  FStoredFluidCompressibility.Assign(Value)
end;

procedure TSutraOptions.SetStoredFluidDensityCoefficientConcentration
  (const Value: TRealStorage);
begin
  FStoredFluidDensityCoefficientConcentration.Assign(Value)
end;

procedure TSutraOptions.SetStoredFluidDensityCoefficientTemperature(
  const Value: TRealStorage);
begin
  FStoredFluidDensityCoefficientTemperature.Assign(Value);
end;

procedure TSutraOptions.SetStoredFluidDiffusivity(const Value: TRealStorage);
begin
  FStoredFluidDiffusivity.Assign(Value);
end;

procedure TSutraOptions.SetStoredFluidSpecificHeat(const Value: TRealStorage);
begin
  FStoredFluidSpecificHeat.Assign(Value);
end;

procedure TSutraOptions.SetStoredFluidThermalConductivity(
  const Value: TRealStorage);
begin
  FStoredFluidThermalConductivity.Assign(Value);
end;

procedure TSutraOptions.SetStoredFractionalUpstreamWeight
  (const Value: TRealStorage);
begin
  FStoredFractionalUpstreamWeight.Assign(Value);
end;

procedure TSutraOptions.SetStoredGravityX(const Value: TRealStorage);
begin
  FStoredGravityX.Assign(Value);
end;

procedure TSutraOptions.SetStoredGravityY(const Value: TRealStorage);
begin
  FStoredGravityY.Assign(Value);
end;

procedure TSutraOptions.SetStoredGravityZ(const Value: TRealStorage);
begin
  FStoredGravityZ.Assign(Value);
end;

procedure TSutraOptions.SetStoredMatrixCompressibility
  (const Value: TRealStorage);
begin
  FStoredMatrixCompressibility.Assign(Value)
end;

procedure TSutraOptions.SetStoredNonLinPressureCriterion
  (const Value: TRealStorage);
begin
  FStoredNonLinPressureCriterion.Assign(Value);
end;

procedure TSutraOptions.SetStoredPressureCriterion(const Value: TRealStorage);
begin
  FStoredPressureCriterion.Assign(Value);
end;

procedure TSutraOptions.SetStoredPressureFactor(const Value: TRealStorage);
begin
  FStoredPressureFactor.Assign(Value);
end;

procedure TSutraOptions.SetStoredScaleFactor(const Value: TRealStorage);
begin
  FStoredScaleFactor.Assign(Value)
end;

procedure TSutraOptions.SetStoredSecondDistributionCoefficient
  (const Value: TRealStorage);
begin
  FStoredSecondDistributionCoefficient.Assign(Value)
end;

procedure TSutraOptions.SetStoredSolidGrainDensity(const Value: TRealStorage);
begin
  FStoredSolidGrainDensity.Assign(Value)
end;

procedure TSutraOptions.SetStoredSolidGrainDiffusivity
  (const Value: TRealStorage);
begin
  FStoredSolidGrainDiffusivity.Assign(Value)
end;

procedure TSutraOptions.SetStoredSolidGrainSpecificHeat
  (const Value: TRealStorage);
begin
  FStoredSolidGrainSpecificHeat.Assign(Value)
end;

procedure TSutraOptions.SetStoredTransportCriterion(const Value: TRealStorage);
begin
  FStoredTransportCriterion.Assign(Value);
end;

procedure TSutraOptions.SetStoredUCriterion(const Value: TRealStorage);
begin
  FStoredUCriterion.Assign(Value);
end;

procedure TSutraOptions.SetStoredUFactor(const Value: TRealStorage);
begin
  FStoredUFactor.Assign(Value);
end;

procedure TSutraOptions.SetStoredViscosity
  (const Value: TRealStorage);
begin
  FStoredViscosity.Assign(Value);
end;

procedure TSutraOptions.SetStoredZeroFluidProduction(const Value: TRealStorage);
begin
  FStoredZeroFluidProduction.Assign(Value);
end;

procedure TSutraOptions.SetStoredZeroImmobileProduction
  (const Value: TRealStorage);
begin
  FStoredZeroImmobileProduction.Assign(Value);
end;

procedure TSutraOptions.SetTitleLines(const Value: AnsiString);
begin
  SetAnsiStringProperty(FTitleLines, Value);
end;

procedure TSutraOptions.SetTransportChoice(const Value: TTransportChoice);
var
  PriorHeadUsed: Boolean;
  PostHeadUsed: Boolean;
  LocalModel: TPhastModel;
begin
  if FTransportChoice <> Value then
  begin
    PriorHeadUsed := FTransportChoice = tcSoluteHead;
    FTransportChoice := Value;
    PostHeadUsed := FTransportChoice = tcSoluteHead;
    if (PriorHeadUsed <> PostHeadUsed) and (Model <> nil) then
    begin
      if Model is TChildModel then
      begin
        LocalModel := TChildModel(Model).ParentModel as TPhastModel;
      end
      else
      begin
        LocalModel := Model as TPhastModel;
      end;

      if PostHeadUsed then
      begin
        if LocalModel.MaxVectors.VectorType = pvtPermeability then
        begin
          LocalModel.MaxVectors.VectorType := pvtConductivity;
          LocalModel.MidVectors.VectorType := pvtConductivity;
          LocalModel.MinVectors.VectorType := pvtConductivity;
        end;
      end
      else
      begin
        if LocalModel.MaxVectors.VectorType = pvtConductivity then
        begin
          LocalModel.MaxVectors.VectorType := pvtPermeability;
          LocalModel.MidVectors.VectorType := pvtPermeability;
          LocalModel.MinVectors.VectorType := pvtPermeability;
        end;
      end;
    end;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetTransportCriterion(const Value: double);
begin
  StoredTransportCriterion.Value := Value;
end;

procedure TSutraOptions.SetUCriterion(const Value: double);
begin
  StoredUCriterion.Value := Value;
end;

procedure TSutraOptions.SetUFactor(const Value: double);
begin
  StoredUFactor.Value := Value;
end;

procedure TSutraOptions.SetUSolutionMethod(const Value: TUSolutionMethod);
begin
  if FUSolutionMethod <> Value then
  begin
    FUSolutionMethod := Value;
    InvalidateModel;
  end;
end;

procedure TSutraOptions.SetViscosity(const Value: double);
begin
  StoredViscosity.Value := Value;
end;

procedure TSutraOptions.SetZeroFluidProduction(const Value: double);
begin
  StoredZeroFluidProduction.Value := Value;
end;

procedure TSutraOptions.SetZeroImmobileProduction(const Value: double);
begin
  StoredZeroImmobileProduction.Value := Value;
end;

procedure TSutraOptions.ValueChanged(Sender: TObject);
begin
  InvalidateModel;
end;

{ TSutraLakeOptions }

//procedure TSutraLakeOptions.SetSpecifiedPressureLakeAbsent(
//  const Value: TLakeBoundaryInteraction);
//begin
//  if FSpecifiedPressureLakeAbsent <> Value then
//  begin
//    FSpecifiedPressureLakeAbsent := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSutraLakeOptions.SetSpecifiedPressureLakePresent(
  const Value: TLakeBoundaryInteraction);
begin
  if FSpecifiedPressureLakePresent <> Value then
  begin
    FSpecifiedPressureLakePresent := Value;
    InvalidateModel;
  end;
end;

//procedure TSutraLakeOptions.SetSpecifiedULakeAbsent(
//  const Value: TLakeBoundaryInteraction);
//begin
//  if FSpecifiedULakeAbsent <> Value then
//  begin
//    FSpecifiedULakeAbsent := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSutraLakeOptions.SetSpecifiedULakePresent(
  const Value: TLakeBoundaryInteraction);
begin
  if FSpecifiedULakePresent <> Value then
  begin
    FSpecifiedULakePresent := Value;
    InvalidateModel;
  end;
end;

procedure TSutraLakeOptions.SetStoredDischargeFraction(const Value: TRealStorage);
begin
  FStoredDischargeFraction.Assign(Value);
end;

procedure TSutraLakeOptions.Assign(Source: TPersistent);
var
  SourceLake: TSutraLakeOptions;
begin
  if Source is TSutraLakeOptions then
  begin
    SourceLake := TSutraLakeOptions(Source);
//    MaxLakeIterations := SourceLake.MaxLakeIterations;
    LakeOutputCycle := SourceLake.LakeOutputCycle;
    RechargeFraction := SourceLake.RechargeFraction;
    DischargeFraction := SourceLake.DischargeFraction;
//    MinLakeVolume := SourceLake.MinLakeVolume;
    SubmergedOutput := SourceLake.SubmergedOutput;

    FluidSourceSinkLakePresent := SourceLake.FluidSourceSinkLakePresent;
    USourceSinkLakePresent := SourceLake.USourceSinkLakePresent;
    SpecifiedPressureLakePresent := SourceLake.SpecifiedPressureLakePresent;
    SpecifiedULakePresent := SourceLake.SpecifiedULakePresent;
    GeneralizedFlowLakePresent := SourceLake.GeneralizedFlowLakePresent;
    GeneralizedTransportLakePresent := SourceLake.GeneralizedTransportLakePresent;
    GeneralizedFlowInteractionType := SourceLake.GeneralizedFlowInteractionType;
    GeneralizedTransportInteractionType := SourceLake.GeneralizedTransportInteractionType;
    UseLakes := SourceLake.UseLakes;
    AllNodesLakes := SourceLake.AllNodesLakes;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSutraLakeOptions.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredRechargeFraction := TRealStorage.Create;
  FStoredDischargeFraction := TRealStorage.Create;
  FStoredMinLakeVolume := TRealStorage.Create;
  FStoredSubmergedOutput := TRealStorage.Create;

  FStoredRechargeFraction.OnChange := OnInvalidateModel;
  FStoredDischargeFraction.OnChange := OnInvalidateModel;
//  FStoredMinLakeVolume.OnChange := OnInvalidateModel;
  FStoredSubmergedOutput.OnChange := OnInvalidateModel;

  Initialize;
end;

destructor TSutraLakeOptions.Destroy;
begin
  FStoredRechargeFraction.Free;
  FStoredDischargeFraction.Free;
  FStoredMinLakeVolume.Free;
  FStoredSubmergedOutput.Free;
  inherited;
end;

function TSutraLakeOptions.GetDischargeFraction: double;
begin
  result := StoredDischargeFraction.Value;
end;

//function TSutraLakeOptions.GetMinLakeVolume: double;
//begin
//  result := StoredMinLakeVolume.Value;
//end;
//
function TSutraLakeOptions.GetRechargeFraction: double;
begin
  result := StoredRechargeFraction.Value;
end;

function TSutraLakeOptions.GetSubmergedOutput: double;
begin
  result := StoredSubmergedOutput.Value;
end;

procedure TSutraLakeOptions.Initialize;
begin
//  MaxLakeIterations := 1;
  LakeOutputCycle := 1;
  RechargeFraction := 0;
  DischargeFraction := 0;
//  MinLakeVolume := 0;
  SubmergedOutput := 0;
  FluidSourceSinkLakePresent := lbiNoChange;
  USourceSinkLakePresent := lbiNoChange;
  SpecifiedPressureLakePresent := lbiNoChange;
  SpecifiedULakePresent := lbiNoChange;
  GeneralizedFlowLakePresent := lbiNoChange;
  GeneralizedTransportLakePresent := lbiNoChange;
  GeneralizedFlowInteractionType := gfitFluidSource;
  GeneralizedTransportInteractionType := gtitSpecifiedConcentration;
  UseLakes := False;
  AllNodesLakes := False;
end;

procedure TSutraLakeOptions.SetAllNodesLakes(const Value: Boolean);
begin
  SetBooleanProperty(FAllNodesLakes, Value);
end;

procedure TSutraLakeOptions.SetDischargeFraction(const Value: double);
begin
  StoredDischargeFraction.Value := Value;
end;

//procedure TSutraLakeOptions.SetFluidSourceSinkLakeAbsent(
//  const Value: TLakeBoundaryInteraction);
//begin
//  if FFluidSourceSinkLakeAbsent <> Value then
//  begin
//    FFluidSourceSinkLakeAbsent := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSutraLakeOptions.SetFluidSourceSinkLakePresent(
  const Value: TLakeBoundaryInteraction);
begin
  if FFluidSourceSinkLakePresent <> Value then
  begin
    FFluidSourceSinkLakePresent := Value;
    InvalidateModel;
  end;
end;

procedure TSutraLakeOptions.SetGeneralizedFlowInteractionType(
  const Value: TGeneralizedFlowInteractionType);
begin
  if FGeneralizedFlowInteractionType <> Value then
  begin
    FGeneralizedFlowInteractionType := Value;
    InvalidateModel;
  end;
end;

//procedure TSutraLakeOptions.SetGeneralizedFlowLakeAbsent(
//  const Value: TLakeBoundaryInteraction);
//begin
//  if FGeneralizedFlowLakeAbsent <> Value then
//  begin
//    FGeneralizedFlowLakeAbsent := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSutraLakeOptions.SetGeneralizedFlowLakePresent(
  const Value: TLakeBoundaryInteraction);
begin
  if FGeneralizedFlowLakePresent <> Value then
  begin
    FGeneralizedFlowLakePresent := Value;
    InvalidateModel;
  end;
end;

procedure TSutraLakeOptions.SetGeneralizedTransportInteractionType(
  const Value: TGeneralizedTransportInteractionType);
begin
  if FGeneralizedTransportInteractionType <> Value then
  begin
    FGeneralizedTransportInteractionType := Value;
    InvalidateModel;
  end;
end;

//procedure TSutraLakeOptions.SetGeneralizedTransportLakeAbsent(
//  const Value: TLakeBoundaryInteraction);
//begin
//  if FGeneralizedTransportLakeAbsent <> Value then
//  begin
//    FGeneralizedTransportLakeAbsent := Value;
//    InvalidateModel;
//  end;
//end;

procedure TSutraLakeOptions.SetGeneralizedTransportLakePresent(
  const Value: TLakeBoundaryInteraction);
begin
  if FGeneralizedTransportLakePresent <> Value then
  begin
    FGeneralizedTransportLakePresent := Value;
    InvalidateModel;
  end;
end;

procedure TSutraLakeOptions.SetLakeOutputCycle(const Value: Integer);
begin
  SetIntegerProperty(FLakeOutputCycle, Value);
end;

procedure TSutraLakeOptions.SetMaxLakeIterations(const Value: Integer);
begin
  FMaxLakeIterations := Value;
//  SetIntegerProperty(FMaxLakeIterations, Value);
end;

//procedure TSutraLakeOptions.SetMinLakeVolume(const Value: double);
//begin
//  StoredMinLakeVolume.Value := Value;
//end;

procedure TSutraLakeOptions.SetRechargeFraction(const Value: double);
begin
  StoredRechargeFraction.Value := Value;
end;

procedure TSutraLakeOptions.SetStoredMinLakeVolume(const Value: TRealStorage);
begin
  FStoredMinLakeVolume.Assign(Value);
end;
//
procedure TSutraLakeOptions.SetStoredRechargeFraction(const Value: TRealStorage);
begin
  FStoredRechargeFraction.Assign(Value);
end;

procedure TSutraLakeOptions.SetStoredSubmergedOutput(const Value: TRealStorage);
begin
  FStoredSubmergedOutput.Assign(Value);
end;

procedure TSutraLakeOptions.SetSubmergedOutput(const Value: double);
begin
  StoredSubmergedOutput.Value := Value;
end;

procedure TSutraLakeOptions.SetUseLakes(const Value: boolean);
begin
  SetBooleanProperty(FUseLakes, Value);
end;

procedure TSutraLakeOptions.SetUSourceSinkLakePresent(
  const Value: TLakeBoundaryInteraction);
begin
  if FUSourceSinkLakePresent <> Value then
  begin
    FUSourceSinkLakePresent := Value;
    InvalidateModel;
  end;
end;

end.
