{@abstract(The main purpose of @name is to define @link(TPrintFrequencyItem)
  and @link(TPrintFrequencyCollection) which are used to store and retrieve
  the data in the PRINT_FREQUENCY data block in PHAST.)}
unit PrintFrequency;

interface

uses Classes, GoPhastTypes;

type

  {@abstract(@name defines a single PRINT_FREQUENCY data block.)}
  TPrintFrequencyItem = class(TCollectionItem)
  private
    // @name: double;
    // See @link(BC_FlowRates).
    FBC_FlowRates: double;
    // @name: @link(TFrequencyUnits);
    // See @link(BC_FlowRatesUnits).
    FBC_FlowRatesUnits: TFrequencyUnits;
    // @name: boolean;
    // See @link(BoundaryConditions).
    FBoundaryConditions: boolean;
    // @name: double;
    // See @link(Components).
    FComponents: double;
    // @name: @link(TFrequencyUnits);
    // See @link(ComponentsUnits).
    FComponentsUnits: TFrequencyUnits;
    // @name: double;
    // See @link(Conductances).
    FConductances: double;
    // @name: @link(TFrequencyUnits);
    // See @link(ConductancesUnits).
    FConductancesUnits: TFrequencyUnits;
    // @name: double;
    // See @link(FlowBalance).
    FFlowBalance: double;
    // @name: @link(TFrequencyUnits);
    // See @link(FlowBalanceUnits).
    FFlowBalanceUnits: TFrequencyUnits;
    // @name: double;
    // See @link(ForceChemistryPrint).
    FForceChemistryPrint: double;
    // @name: @link(TFrequencyUnits);
    // See @link(ForceChemistryPrintUnits).
    FForceChemistryPrintUnits: TFrequencyUnits;
    // @name: double;
    // See @link(HDF_Chemistry).
    FHDF_Chemistry: double;
    // @name: @link(TFrequencyUnits);
    // See @link(HDF_ChemistryUnits).
    FHDF_ChemistryUnits: TFrequencyUnits;
    // @name: double;
    // See @link(HDF_Heads).
    FHDF_Heads: double;
    // @name: @link(TFrequencyUnits);
    // See @link(HDF_HeadsUnits).
    FHDF_HeadsUnits: TFrequencyUnits;
    // @name: double;
    // See @link(HDF_Velocities).
    FHDF_Velocities: double;
    // @name: @link(TFrequencyUnits);
    // See @link(HDF_VelocitiesUnits).
    FHDF_VelocitiesUnits: TFrequencyUnits;
    // @name: double;
    // See @link(Heads).
    FHeads: double;
    // @name: @link(TFrequencyUnits);
    // See @link(HeadsUnits).
    FHeadsUnits: TFrequencyUnits;
    // @name: double;
    // See @link(ProgressStatistics).
    FProgressStatistics: double;
    // @name: @link(TFrequencyUnits);
    // See @link(ProgressStatisticsUnits).
    FProgressStatisticsUnits: TFrequencyUnits;
    // @name: boolean;
    // See @link(SaveFinalHeads).
    FSaveFinalHeads: boolean;
    // @name: double;
    // See @link(Time).
    FTime: double;
    // @name: double;
    // See @link(Velocities).
    FVelocities: double;
    // @name: @link(TFrequencyUnits);
    // See @link(VelocitiesUnits).
    FVelocitiesUnits: TFrequencyUnits;
    // @name: double;
    // See @link(Wells).
    FWells: double;
    // @name: @link(TFrequencyUnits);
    // See @link(WellsUnits).
    FWellsUnits: TFrequencyUnits;
    // @name: double;
    // See @link(XYZ_Chemistry).
    FXYZ_Chemistry: double;
    // @name: @link(TFrequencyUnits);
    // See @link(XYZ_ChemistryUnits).
    FXYZ_ChemistryUnits: TFrequencyUnits;
    // @name: double;
    // See @link(XYZ_Components).
    FXYZ_Components: double;
    // @name: @link(TFrequencyUnits);
    // See @link(XYZ_ComponentsUnits).
    FXYZ_ComponentsUnits: TFrequencyUnits;
    // @name: double;
    // See @link(XYZ_Heads).
    FXYZ_Heads: double;
    // @name: @link(TFrequencyUnits);
    // See @link(XYZ_HeadsUnits).
    FXYZ_HeadsUnits: TFrequencyUnits;
    // @name: double;
    // See @link(XYZ_Velocities).
    FXYZ_Velocities: double;
    // @name: @link(TFrequencyUnits);
    // See @link(XYZ_VelocitiesUnits).
    FXYZ_VelocitiesUnits: TFrequencyUnits;
    // @name: double;
    // See @link(XYZ_Wells).
    FXYZ_Wells: double;
    // @name: @link(TFrequencyUnits);
    // See @link(XYZ_WellsUnits).
    FXYZ_WellsUnits: TFrequencyUnits;
    FRestartFrequency: TRealStorage;
    FRestartFrequencyUnits: TFrequencyUnits;
    FEndOfPeriodDefault: boolean;
    // See @link(Rates).
    function GetRates(const Index: TPrintFrequencyRow): double;
    // See @link(Units).
    function GetUnits(const Index: TPrintFrequencyRow): TFrequencyUnits;
    // See @link(BC_FlowRates).
    procedure SetBC_FlowRates(const Value: double);
    // See @link(BC_FlowRatesUnits).
    procedure SetBC_FlowRatesUnits(const Value: TFrequencyUnits);
    // See @link(BoundaryConditions).
    procedure SetBoundaryConditions(const Value: boolean);
    // See @link(Components).
    procedure SetComponents(const Value: double);
    // See @link(ComponentsUnits).
    procedure SetComponentsUnits(const Value: TFrequencyUnits);
    // See @link(Conductances).
    procedure SetConductances(const Value: double);
    // See @link(ConductancesUnits).
    procedure SetConductancesUnits(const Value: TFrequencyUnits);
    // See @link(FlowBalance).
    procedure SetFlowBalance(const Value: double);
    // See @link(FlowBalanceUnits).
    procedure SetFlowBalanceUnits(const Value: TFrequencyUnits);
    // See @link(ForceChemistryPrint).
    procedure SetForceChemistryPrint(const Value: double);
    // See @link(ForceChemistryPrintUnits).
    procedure SetForceChemistryPrintUnits(const Value: TFrequencyUnits);
    // See @link(HDF_Chemistry).
    procedure SetHDF_Chemistry(const Value: double);
    // See @link(HDF_ChemistryUnits).
    procedure SetHDF_ChemistryUnits(const Value: TFrequencyUnits);
    // See @link(HDF_Heads).
    procedure SetHDF_Heads(const Value: double);
    // See @link(HDF_HeadsUnits).
    procedure SetHDF_HeadsUnits(const Value: TFrequencyUnits);
    // See @link(HDF_Velocities).
    procedure SetHDF_Velocities(const Value: double);
    // See @link(HDF_VelocitiesUnits).
    procedure SetHDF_VelocitiesUnits(const Value: TFrequencyUnits);
    // See @link(Heads).
    procedure SetHeads(const Value: double);
    // See @link(HeadsUnits).
    procedure SetHeadsUnits(const Value: TFrequencyUnits);
    // See @link(ProgressStatistics).
    procedure SetProgressStatistics(const Value: double);
    // See @link(ProgressStatisticsUnits).
    procedure SetProgressStatisticsUnits(const Value: TFrequencyUnits);
    // See @link(Rates).
    procedure SetRates(const Index: TPrintFrequencyRow;
      const Value: double);
    // See @link(SaveFinalHeads).
    procedure SetSaveFinalHeads(const Value: boolean);
    // See @link(Time).
    procedure SetTime(const Value: double);
    // See @link(Units).
    procedure SetUnits(const Index: TPrintFrequencyRow;
      const Value: TFrequencyUnits);
    // See @link(Velocities).
    procedure SetVelocities(const Value: double);
    // See @link(VelocitiesUnits).
    procedure SetVelocitiesUnits(const Value: TFrequencyUnits);
    // See @link(Wells).
    procedure SetWells(const Value: double);
    // See @link(WellsUnits).
    procedure SetWellsUnits(const Value: TFrequencyUnits);
    // See @link(XYZ_Chemistry).
    procedure SetXYZ_Chemistry(const Value: double);
    // See @link(XYZ_ChemistryUnits).
    procedure SetXYZ_ChemistryUnits(const Value: TFrequencyUnits);
    // See @link(XYZ_Components).
    procedure SetXYZ_Components(const Value: double);
    // See @link(XYZ_ComponentsUnits).
    procedure SetXYZ_ComponentsUnits(const Value: TFrequencyUnits);
    // See @link(XYZ_Heads).
    procedure SetXYZ_Heads(const Value: double);
    // See @link(XYZ_HeadsUnits).
    procedure SetXYZ_HeadsUnits(const Value: TFrequencyUnits);
    // See @link(XYZ_Velocities).
    procedure SetXYZ_Velocities(const Value: double);
    // See @link(XYZ_VelocitiesUnits).
    procedure SetXYZ_VelocitiesUnits(const Value: TFrequencyUnits);
    // See @link(XYZ_Wells).
    procedure SetXYZ_Wells(const Value: double);
    // See @link(XYZ_WellsUnits).
    procedure SetXYZ_WellsUnits(const Value: TFrequencyUnits);
    procedure SetRestartFrequency(const Value: TRealStorage);
    procedure SetRestartFrequencyUnits(const Value: TFrequencyUnits);
    procedure SetEndOfPeriodDefault(const Value: boolean);
  public
    // If Source is a @Classname, @name copies the properties of Source
    // to the @classname that called @name.
    procedure Assign(Source: TPersistent); override;
    // @name creates and initializes an instance of @classname.
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // @name retrieves a rate based on Index.  See @link(TPrintFrequencyRows).
    property Rates[const Index: TPrintFrequencyRow]: double read GetRates
      write SetRates;
    // @name retrieves the units for a rate based on Index.
    // See @link(TPrintFrequencyRows).
    property Units[const Index: TPrintFrequencyRow]: TFrequencyUnits
      read GetUnits write SetUnits;
  published
    // @name is the frequency with which boundary-condition flow rates
    // are printed by PHAST.
    property BC_FlowRates: double read FBC_FlowRates write SetBC_FlowRates;
    // @name is the units for @link(BC_FlowRates).
    // See @link(TFrequencyUnits).
    property BC_FlowRatesUnits: TFrequencyUnits read FBC_FlowRatesUnits write
      SetBC_FlowRatesUnits;
    // @name specifies whether boundary conditions should be printed by PHAST.
    property BoundaryConditions: boolean read FBoundaryConditions write
      SetBoundaryConditions;
    // @name is the frequency with which components
    // are printed by PHAST.
    property Components: double read FComponents write SetComponents;
    // @name is the units for @link(Components).
    // See @link(TFrequencyUnits).
    property ComponentsUnits: TFrequencyUnits read FComponentsUnits write
      SetComponentsUnits;
    // @name is the frequency with which conductances
    // are printed by PHAST.
    property Conductances: double read FConductances write SetConductances;
    // @name is the units for @link(Conductances).
    // See @link(TFrequencyUnits).
    property ConductancesUnits: TFrequencyUnits read FConductancesUnits write
      SetConductancesUnits;
    // @name is the frequency with which the flow balance
    // is printed by PHAST.
    property FlowBalance: double read FFlowBalance write SetFlowBalance;
    // @name is the units for @link(FlowBalance).
    // See @link(TFrequencyUnits).
    property FlowBalanceUnits: TFrequencyUnits read FFlowBalanceUnits write
      SetFlowBalanceUnits;
    // @name is the frequency with which the chemistry
    // is printed by PHAST.
    property ForceChemistryPrint: double read FForceChemistryPrint write
      SetForceChemistryPrint;
    // @name is the units for @link(ForceChemistryPrint).
    // See @link(TFrequencyUnits).
    property ForceChemistryPrintUnits: TFrequencyUnits read
      FForceChemistryPrintUnits write SetForceChemistryPrintUnits;
    // @name is the frequency with which the chemistry
    // is printed to the HDF file by PHAST.
    property HDF_Chemistry: double read FHDF_Chemistry write SetHDF_Chemistry;
    // @name is the units for @link(HDF_Chemistry).
    // See @link(TFrequencyUnits).
    property HDF_ChemistryUnits: TFrequencyUnits read FHDF_ChemistryUnits write
      SetHDF_ChemistryUnits;
    // @name is the frequency with which the heads
    // are printed to the HDF file by PHAST.
    property HDF_Heads: double read FHDF_Heads write SetHDF_Heads;
    // @name is the units for @link(HDF_Heads).
    // See @link(TFrequencyUnits).
    property HDF_HeadsUnits: TFrequencyUnits read FHDF_HeadsUnits write
      SetHDF_HeadsUnits;
    // @name is the frequency with which the velocities
    // are printed to the HDF file by PHAST.
    property HDF_Velocities: double read FHDF_Velocities write
      SetHDF_Velocities;
    // @name is the units for @link(HDF_Velocities).
    // See @link(TFrequencyUnits).
    property HDF_VelocitiesUnits: TFrequencyUnits read FHDF_VelocitiesUnits write
      SetHDF_VelocitiesUnits;
    // @name is the frequency with which the heads
    // are printed by PHAST.
    property Heads: double read FHeads write SetHeads;
    // @name is the units for @link(Heads).
    // See @link(TFrequencyUnits).
    property HeadsUnits: TFrequencyUnits read FHeadsUnits write SetHeadsUnits;
    // @name is the frequency with which the progress statistics
    // are printed by PHAST.
    property ProgressStatistics: double read FProgressStatistics write
      SetProgressStatistics;
    // @name is the units for @link(ProgressStatistics).
    // See @link(TFrequencyUnits).
    property ProgressStatisticsUnits: TFrequencyUnits read
      FProgressStatisticsUnits write SetProgressStatisticsUnits;
    property RestartFrequency: TRealStorage read FRestartFrequency write SetRestartFrequency;
    property RestartFrequencyUnits: TFrequencyUnits read FRestartFrequencyUnits write SetRestartFrequencyUnits;
    // @name is no longer used.  It is present only for backwards compatibility.
    property SaveFinalHeads: boolean read FSaveFinalHeads write
      SetSaveFinalHeads stored False;
    // @name is the time at which this item takes effect.
    property Time: double read FTime write SetTime;
    // @name is the frequency with which the velocities
    // are printed by PHAST.
    property Velocities: double read FVelocities write SetVelocities;
    // @name is the units for @link(Velocities).
    // See @link(TFrequencyUnits).
    property VelocitiesUnits: TFrequencyUnits read FVelocitiesUnits write
      SetVelocitiesUnits;
    // @name is the frequency with which the wells
    // are printed by PHAST.
    property Wells: double read FWells write SetWells;
    // @name is the units for @link(Wells).
    // See @link(TFrequencyUnits).
    property WellsUnits: TFrequencyUnits read FWellsUnits write SetWellsUnits;
    // @name is the frequency with which the chemistry at each X,Y,Z location
    // is printed by PHAST.
    property XYZ_Chemistry: double read FXYZ_Chemistry write SetXYZ_Chemistry;
    // @name is the units for @link(XYZ_Chemistry).
    // See @link(TFrequencyUnits).
    property XYZ_ChemistryUnits: TFrequencyUnits read FXYZ_ChemistryUnits write
      SetXYZ_ChemistryUnits;
    // @name is the frequency with which the components at each X,Y,Z location
    // are printed by PHAST.
    property XYZ_Components: double read FXYZ_Components write
      SetXYZ_Components;
    // @name is the units for @link(XYZ_Components).
    // See @link(TFrequencyUnits).
    property XYZ_ComponentsUnits: TFrequencyUnits read FXYZ_ComponentsUnits write
      SetXYZ_ComponentsUnits;
    // @name is the frequency with which the heads at each X,Y,Z location
    // are printed by PHAST.
    property XYZ_Heads: double read FXYZ_Heads write SetXYZ_Heads;
    // @name is the units for @link(XYZ_Heads).
    // See @link(TFrequencyUnits).
    property XYZ_HeadsUnits: TFrequencyUnits read FXYZ_HeadsUnits write
      SetXYZ_HeadsUnits;
    // @name is the frequency with which the velocities at each X,Y,Z location
    // are printed by PHAST.
    property XYZ_Velocities: double read FXYZ_Velocities write
      SetXYZ_Velocities;
    // @name is the units for @link(XYZ_Velocities).
    // See @link(TFrequencyUnits).
    property XYZ_VelocitiesUnits: TFrequencyUnits read FXYZ_VelocitiesUnits
      write SetXYZ_VelocitiesUnits;
    // @name is the frequency with which the wells at each X,Y,Z location
    // are printed by PHAST.
    property XYZ_Wells: double read FXYZ_Wells write SetXYZ_Wells;
    // @name is the units for @link(XYZ_Wells).
    // See @link(TFrequencyUnits).
    property XYZ_WellsUnits: TFrequencyUnits read FXYZ_WellsUnits write
      SetXYZ_WellsUnits;
    property EndOfPeriodDefault: boolean read FEndOfPeriodDefault
      write SetEndOfPeriodDefault;
  end;

  {@abstract(@name represents a group of @link(TPrintFrequencyItem)s.)}
  TPrintFrequencyCollection = class(TPhastCollection)
  private
    // @name: boolean;
    // See @link(SaveFinalHeads).
    FSaveFinalHeads: boolean;
    // See @link(SaveFinalHeads).
    procedure SetSaveFinalHeads(const Value: boolean);
  public
    // If source is a @classname, @name copies Source to the item that
    // called name.
    procedure Assign(Source: TPersistent); override;
    // @name returns the index of the first @link(TPrintFrequencyItem)
    // whose @link(TPrintFrequencyItem.Time) is greater than or equal to Time.
    function PriorTimeIndex(const Time: double): integer;
    // @name creates an instance of @classname.
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    // @name returns the first @link(TPrintFrequencyItem)
    // whose @link(TPrintFrequencyItem.Time) is greater than or equal to Time.
    function RetrieveByTime(const Time: double): TPrintFrequencyItem;
    function BcfPrinted: boolean;
    function OCompPrinted: boolean;
    function KdPrinted: boolean;
    function OBalPrinted: boolean;
    function OChemPrinted: boolean;
    function H5Printed: boolean;
    function OHeadPrinted: boolean;
    function LogPrinted: boolean;
    function RestartPrinted: boolean;
    function OVelPrinted: boolean;
    function OWelPrinted: boolean;
    function XyzChemPrinted: boolean;
    function XyzCompsPrinted: boolean;
    function XyzHeadPrinted: boolean;
    function XyzVelPrinted: boolean;
    function XyzWelPrinted: boolean;
    function ProbDefPrinted: boolean;
    function ItemPrinted(Rows: TPrintFrequencyRows): Boolean;
  published
    // @name specifies whether or not the final heads in the simulation
    // should be saved by PHAST.
    property SaveFinalHeads: boolean read FSaveFinalHeads write
      SetSaveFinalHeads;
  end;

implementation

uses frmGoPhastUnit;

{ TPrintFrequencyItem }

procedure TPrintFrequencyItem.Assign(Source: TPersistent);
var
  SourceItem: TPrintFrequencyItem;
begin
  if Source is TPrintFrequencyItem then
  begin
    SourceItem := TPrintFrequencyItem(Source);

    BC_FlowRates := SourceItem.BC_FlowRates;
    BC_FlowRatesUnits := SourceItem.BC_FlowRatesUnits;
    BoundaryConditions := SourceItem.BoundaryConditions;
    Components := SourceItem.Components;
    ComponentsUnits := SourceItem.ComponentsUnits;
    Conductances := SourceItem.Conductances;
    ConductancesUnits := SourceItem.ConductancesUnits;
    FlowBalance := SourceItem.FlowBalance;
    FlowBalanceUnits := SourceItem.FlowBalanceUnits;
    ForceChemistryPrint := SourceItem.ForceChemistryPrint;
    ForceChemistryPrintUnits := SourceItem.ForceChemistryPrintUnits;
    HDF_Chemistry := SourceItem.HDF_Chemistry;
    HDF_ChemistryUnits := SourceItem.HDF_ChemistryUnits;
    HDF_Heads := SourceItem.HDF_Heads;
    HDF_HeadsUnits := SourceItem.HDF_HeadsUnits;
    HDF_Velocities := SourceItem.HDF_Velocities;
    HDF_VelocitiesUnits := SourceItem.HDF_VelocitiesUnits;
    Heads := SourceItem.Heads;
    HeadsUnits := SourceItem.HeadsUnits;
    ProgressStatistics := SourceItem.ProgressStatistics;
    ProgressStatisticsUnits := SourceItem.ProgressStatisticsUnits;
    SaveFinalHeads := SourceItem.SaveFinalHeads;
    Time := SourceItem.Time;
    Velocities := SourceItem.Velocities;
    VelocitiesUnits := SourceItem.VelocitiesUnits;
    Wells := SourceItem.Wells;
    WellsUnits := SourceItem.WellsUnits;
    XYZ_Chemistry := SourceItem.XYZ_Chemistry;
    XYZ_ChemistryUnits := SourceItem.XYZ_ChemistryUnits;
    XYZ_Components := SourceItem.XYZ_Components;
    XYZ_ComponentsUnits := SourceItem.XYZ_ComponentsUnits;
    XYZ_Heads := SourceItem.XYZ_Heads;
    XYZ_HeadsUnits := SourceItem.XYZ_HeadsUnits;
    XYZ_Velocities := SourceItem.XYZ_Velocities;
    XYZ_VelocitiesUnits := SourceItem.XYZ_VelocitiesUnits;
    XYZ_Wells := SourceItem.XYZ_Wells;
    XYZ_WellsUnits := SourceItem.XYZ_WellsUnits;
    RestartFrequency := SourceItem.RestartFrequency;
    RestartFrequencyUnits := SourceItem.RestartFrequencyUnits;
    EndOfPeriodDefault := SourceItem.EndOfPeriodDefault;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPrintFrequencyItem.Create(Collection: TCollection);
begin
  inherited;
  FFlowBalanceUnits := fuEnd;
  FHDF_ChemistryUnits := fuEnd;
  FHDF_HeadsUnits := fuEnd;
  FHDF_VelocitiesUnits := fuEnd;
  FHeadsUnits := fuEnd;
  FProgressStatisticsUnits := fuEnd;
  FWellsUnits := fuEnd;
  FRestartFrequency := TRealStorage.Create;
end;

destructor TPrintFrequencyItem.Destroy;
begin
  FRestartFrequency.Free;
  inherited;
end;

function TPrintFrequencyItem.GetRates(
  const Index: TPrintFrequencyRow): double;
begin
  result := 0;
  case Index of
    pfrFlowRate:
      begin
        result := BC_FlowRates;
      end;
    pfrComponents:
      begin
        result := Components;
      end;
    pfrConductance:
      begin
        result := Conductances;
      end;
    pfrFlowBalance:
      begin
        result := FlowBalance;
      end;
    pfrChemPrint:
      begin
        result := ForceChemistryPrint;
      end;
    pfrHDFChem:
      begin
        result := HDF_Chemistry;
      end;
    pfrHDFHeads:
      begin
        result := HDF_Heads;
      end;
    pfrHDFVelocity:
      begin
        result := HDF_Velocities;
      end;
    pfrHeads:
      begin
        result := Heads;
      end;
    pfrProgress:
      begin
        result := ProgressStatistics;
      end;
    pfrRestart:
      begin
        result := RestartFrequency.Value;
      end;
    pfrVelocities:
      begin
        result := Velocities;
      end;
    pfrWells:
      begin
        result := Wells;
      end;
    pfrXYZChem:
      begin
        result := XYZ_Chemistry;
      end;
    pfrXYZComponents:
      begin
        result := XYZ_Components;
      end;
    pfrXYZHeads:
      begin
        result := XYZ_Heads;
      end;
    pfrXYZVelocities:
      begin
        result := XYZ_Velocities;
      end;
    pfrXYZWells:
      begin
        result := XYZ_Wells;
      end;
  else
    Assert(False);
  end;
end;

function TPrintFrequencyItem.GetUnits(
  const Index: TPrintFrequencyRow): TFrequencyUnits;
begin
  result := fuDefault;
  case Index of
    pfrFlowRate:
      begin
        result := BC_FlowRatesUnits;
      end;
    pfrComponents:
      begin
        result := ComponentsUnits;
      end;
    pfrConductance:
      begin
        result := ConductancesUnits;
      end;
    pfrFlowBalance:
      begin
        result := FlowBalanceUnits;
      end;
    pfrChemPrint:
      begin
        result := ForceChemistryPrintUnits;
      end;
    pfrHDFChem:
      begin
        result := HDF_ChemistryUnits;
      end;
    pfrHDFHeads:
      begin
        result := HDF_HeadsUnits;
      end;
    pfrHDFVelocity:
      begin
        result := HDF_VelocitiesUnits;
      end;
    pfrHeads:
      begin
        result := HeadsUnits;
      end;
    pfrProgress:
      begin
        result := ProgressStatisticsUnits;
      end;
    pfrRestart:
      begin
        result := RestartFrequencyUnits;
      end;
    pfrVelocities:
      begin
        result := VelocitiesUnits;
      end;
    pfrWells:
      begin
        result := WellsUnits;
      end;
    pfrXYZChem:
      begin
        result := XYZ_ChemistryUnits;
      end;
    pfrXYZComponents:
      begin
        result := XYZ_ComponentsUnits;
      end;
    pfrXYZHeads:
      begin
        result := XYZ_HeadsUnits;
      end;
    pfrXYZVelocities:
      begin
        result := XYZ_VelocitiesUnits;
      end;
    pfrXYZWells:
      begin
        result := XYZ_WellsUnits;
      end;
  else
    Assert(False);
  end;
end;

procedure TPrintFrequencyItem.SetBC_FlowRates(const Value: double);
begin
  if FBC_FlowRates <> Value then
  begin
    FBC_FlowRates := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetBC_FlowRatesUnits(
  const Value: TFrequencyUnits);
begin
  if FBC_FlowRatesUnits <> Value then
  begin
    FBC_FlowRatesUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetBoundaryConditions(const Value: boolean);
begin
  if FBoundaryConditions <> Value then
  begin
    FBoundaryConditions := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetComponents(const Value: double);
begin
  if FComponents <> Value then
  begin
    FComponents := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetComponentsUnits(
  const Value: TFrequencyUnits);
begin
  if FComponentsUnits <> Value then
  begin
    FComponentsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetConductances(const Value: double);
begin
  if FConductances <> Value then
  begin
    FConductances := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetConductancesUnits(
  const Value: TFrequencyUnits);
begin
  if FConductancesUnits <> Value then
  begin
    FConductancesUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetEndOfPeriodDefault(const Value: boolean);
begin
  if FEndOfPeriodDefault <> Value then
  begin
    FEndOfPeriodDefault := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetFlowBalance(const Value: double);
begin
  if FFlowBalance <> Value then
  begin
    FFlowBalance := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetFlowBalanceUnits(
  const Value: TFrequencyUnits);
begin
  if FFlowBalanceUnits <> Value then
  begin
    FFlowBalanceUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetForceChemistryPrint(const Value: double);
begin
  if FForceChemistryPrint <> Value then
  begin
    FForceChemistryPrint := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetForceChemistryPrintUnits(
  const Value: TFrequencyUnits);
begin
  if FForceChemistryPrintUnits <> Value then
  begin
    FForceChemistryPrintUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHDF_Chemistry(const Value: double);
begin
  if FHDF_Chemistry <> Value then
  begin
    FHDF_Chemistry := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHDF_ChemistryUnits(
  const Value: TFrequencyUnits);
begin
  if FHDF_ChemistryUnits <> Value then
  begin
    FHDF_ChemistryUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHDF_Heads(const Value: double);
begin
  if FHDF_Heads <> Value then
  begin
    FHDF_Heads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHDF_HeadsUnits(
  const Value: TFrequencyUnits);
begin
  if FHDF_HeadsUnits <> Value then
  begin
    FHDF_HeadsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHDF_Velocities(const Value: double);
begin
  if FHDF_Velocities <> Value then
  begin
    FHDF_Velocities := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHDF_VelocitiesUnits(
  const Value: TFrequencyUnits);
begin
  if FHDF_VelocitiesUnits <> Value then
  begin
    FHDF_VelocitiesUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHeads(const Value: double);
begin
  if FHeads <> Value then
  begin
    FHeads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetHeadsUnits(const Value: TFrequencyUnits);
begin
  if FHeadsUnits <> Value then
  begin
    FHeadsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetProgressStatistics(const Value: double);
begin
  if FProgressStatistics <> Value then
  begin
    FProgressStatistics := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetProgressStatisticsUnits(
  const Value: TFrequencyUnits);
begin
  if FProgressStatisticsUnits <> Value then
  begin
    FProgressStatisticsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetRates(const Index: TPrintFrequencyRow;
  const Value: double);
begin
  case Index of
    pfrFlowRate:
      begin
        BC_FlowRates := Value
      end;
    pfrComponents:
      begin
        Components := Value;
      end;
    pfrConductance:
      begin
        Conductances := Value
      end;
    pfrFlowBalance:
      begin
        FlowBalance := Value
      end;
    pfrChemPrint:
      begin
        ForceChemistryPrint := Value
      end;
    pfrHDFChem:
      begin
        HDF_Chemistry := Value
      end;
    pfrHDFHeads:
      begin
        HDF_Heads := Value
      end;
    pfrHDFVelocity:
      begin
        HDF_Velocities := Value
      end;
    pfrHeads:
      begin
        Heads := Value
      end;
    pfrProgress:
      begin
        ProgressStatistics := Value
      end;
    pfrRestart:
      begin
        RestartFrequency.Value := Value;
      end;
    pfrVelocities:
      begin
        Velocities := Value
      end;
    pfrWells:
      begin
        Wells := Value
      end;
    pfrXYZChem:
      begin
        XYZ_Chemistry := Value
      end;
    pfrXYZComponents:
      begin
        XYZ_Components := Value
      end;
    pfrXYZHeads:
      begin
        XYZ_Heads := Value
      end;
    pfrXYZVelocities:
      begin
        XYZ_Velocities := Value
      end;
    pfrXYZWells:
      begin
        XYZ_Wells := Value
      end;
  else
    Assert(False);
  end;
end;

procedure TPrintFrequencyItem.SetRestartFrequency(const Value: TRealStorage);
begin
  if FRestartFrequency.Value <> Value.Value then
  begin
    FRestartFrequency.Assign(Value);
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetRestartFrequencyUnits(
  const Value: TFrequencyUnits);
begin
  if FRestartFrequencyUnits <> Value then
  begin
    FRestartFrequencyUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetSaveFinalHeads(const Value: boolean);
begin
  if FSaveFinalHeads <> Value then
  begin
    FSaveFinalHeads := Value;
    //frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetTime(const Value: double);
begin
  if FTime <> Value then
  begin
    FTime := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetUnits(const Index: TPrintFrequencyRow;
  const Value: TFrequencyUnits);
begin
  case Index of
    pfrFlowRate:
      begin
        BC_FlowRatesUnits := Value
      end;
    pfrComponents:
      begin
        ComponentsUnits := Value;
      end;
    pfrConductance:
      begin
        ConductancesUnits := Value
      end;
    pfrFlowBalance:
      begin
        FlowBalanceUnits := Value
      end;
    pfrChemPrint:
      begin
        ForceChemistryPrintUnits := Value
      end;
    pfrHDFChem:
      begin
        HDF_ChemistryUnits := Value
      end;
    pfrHDFHeads:
      begin
        HDF_HeadsUnits := Value
      end;
    pfrHDFVelocity:
      begin
        HDF_VelocitiesUnits := Value
      end;
    pfrHeads:
      begin
        HeadsUnits := Value
      end;
    pfrProgress:
      begin
        ProgressStatisticsUnits := Value
      end;
    pfrRestart:
      begin
        RestartFrequencyUnits := Value;
      end;
    pfrVelocities:
      begin
        VelocitiesUnits := Value
      end;
    pfrWells:
      begin
        WellsUnits := Value
      end;
    pfrXYZChem:
      begin
        XYZ_ChemistryUnits := Value
      end;
    pfrXYZComponents:
      begin
        XYZ_ComponentsUnits := Value
      end;
    pfrXYZHeads:
      begin
        XYZ_HeadsUnits := Value
      end;
    pfrXYZVelocities:
      begin
        XYZ_VelocitiesUnits := Value
      end;
    pfrXYZWells:
      begin
        XYZ_WellsUnits := Value
      end;
  else
    Assert(False);
  end;
end;

procedure TPrintFrequencyItem.SetVelocities(const Value: double);
begin
  if FVelocities <> Value then
  begin
    FVelocities := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetVelocitiesUnits(
  const Value: TFrequencyUnits);
begin
  if FVelocitiesUnits <> Value then
  begin
    FVelocitiesUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetWells(const Value: double);
begin
  if FWells <> Value then
  begin
    FWells := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetWellsUnits(const Value: TFrequencyUnits);
begin
  if FWellsUnits <> Value then
  begin
    FWellsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_Chemistry(const Value: double);
begin
  if FXYZ_Chemistry <> Value then
  begin
    FXYZ_Chemistry := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_ChemistryUnits(
  const Value: TFrequencyUnits);
begin
  if FXYZ_ChemistryUnits <> Value then
  begin
    FXYZ_ChemistryUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_Components(const Value: double);
begin
  if FXYZ_Components <> Value then
  begin
    FXYZ_Components := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_ComponentsUnits(
  const Value: TFrequencyUnits);
begin
  if FXYZ_ComponentsUnits <> Value then
  begin
    FXYZ_ComponentsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_Heads(const Value: double);
begin
  if FXYZ_Heads <> Value then
  begin
    FXYZ_Heads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_HeadsUnits(
  const Value: TFrequencyUnits);
begin
  if FXYZ_HeadsUnits <> Value then
  begin
    FXYZ_HeadsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_Velocities(const Value: double);
begin
  if FXYZ_Velocities <> Value then
  begin
    FXYZ_Velocities := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_VelocitiesUnits(
  const Value: TFrequencyUnits);
begin
  if FXYZ_VelocitiesUnits <> Value then
  begin
    FXYZ_VelocitiesUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_Wells(const Value: double);
begin
  if FXYZ_Wells <> Value then
  begin
    FXYZ_Wells := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

procedure TPrintFrequencyItem.SetXYZ_WellsUnits(
  const Value: TFrequencyUnits);
begin
  if FXYZ_WellsUnits <> Value then
  begin
    FXYZ_WellsUnits := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

{ TPrintFrequencyCollection }

procedure TPrintFrequencyCollection.Assign(Source: TPersistent);
begin
  if Source is TPrintFrequencyCollection then
  begin
    SaveFinalHeads := TPrintFrequencyCollection(Source).SaveFinalHeads;
  end;
  inherited;
end;

function TPrintFrequencyCollection.BcfPrinted: boolean;
begin
  result := ItemPrinted([pfrFlowRate]);
end;

constructor TPrintFrequencyCollection.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TPrintFrequencyItem, InvalidateModelEvent);
end;

function TPrintFrequencyCollection.H5Printed: boolean;
begin
  result := ItemPrinted([pfrHDFChem, pfrHDFHeads, pfrHDFVelocity]);
end;

function TPrintFrequencyCollection.ItemPrinted(
  Rows: TPrintFrequencyRows): Boolean;
var
  ItemIndex: integer;
  PrintItem: TPrintFrequencyItem;
  ARow: TPrintFrequencyRow;
begin
  result := false;
  for ItemIndex := 0 to Count - 1 do
  begin
    PrintItem := Items[ItemIndex] as TPrintFrequencyItem;
    for ARow in Rows do
    begin
      if ARow = pfrBoundaryConditions then
      begin
        result := PrintItem.BoundaryConditions;
      end
      else if PrintItem.Rates[ARow] > 0 then
      begin
        result := True;
        Exit;
      end;
    end;
//    PrintItem.Rates[pfrFlowRate]
  end;
end;

function TPrintFrequencyCollection.KdPrinted: boolean;
begin
  result := ItemPrinted([pfrConductance]);
end;

function TPrintFrequencyCollection.LogPrinted: boolean;
begin
  result := ItemPrinted([pfrProgress]);
end;

function TPrintFrequencyCollection.OBalPrinted: boolean;
begin
  result := ItemPrinted([pfrFlowBalance]);
end;

function TPrintFrequencyCollection.OChemPrinted: boolean;
begin
  result := ItemPrinted([pfrChemPrint]);
end;

function TPrintFrequencyCollection.OCompPrinted: boolean;
begin
  result := ItemPrinted([pfrComponents]);
end;

function TPrintFrequencyCollection.OHeadPrinted: boolean;
begin
  result := ItemPrinted([pfrHeads]);
end;

function TPrintFrequencyCollection.OVelPrinted: boolean;
begin
  result := ItemPrinted([pfrVelocities]);
end;

function TPrintFrequencyCollection.OWelPrinted: boolean;
begin
  result := ItemPrinted([pfrWells]);
end;

function TPrintFrequencyCollection.PriorTimeIndex(
  const Time: double): integer;
var
  Index: integer;
  AnItem: TPrintFrequencyItem;
begin
  result := -1;
  Assert(Time >= 0);
  for Index := 0 to Count - 1 do
  begin
    AnItem := Items[Index] as TPrintFrequencyItem;
    if Time >= AnItem.Time then
    begin
      result := Index;
    end
    else
    begin
      Exit;
    end;
  end;
end;

function TPrintFrequencyCollection.ProbDefPrinted: boolean;
begin
  result := ItemPrinted([pfrBoundaryConditions]);
end;

function TPrintFrequencyCollection.RestartPrinted: boolean;
begin
  result := ItemPrinted([pfrRestart]);
end;

function TPrintFrequencyCollection.RetrieveByTime(
  const Time: double): TPrintFrequencyItem;
var
  Index: integer;
begin
  result := nil;
  Index := PriorTimeIndex(Time);
  if Index >= 0 then
  begin
    result := Items[Index] as TPrintFrequencyItem;
  end;
end;

procedure TPrintFrequencyCollection.SetSaveFinalHeads(
  const Value: boolean);
begin
  if FSaveFinalHeads <> Value then
  begin
    FSaveFinalHeads := Value;
    frmGoPhast.InvalidateModel;
  end;
end;

function TPrintFrequencyCollection.XyzChemPrinted: boolean;
begin
  result := ItemPrinted([pfrXYZChem]);
end;

function TPrintFrequencyCollection.XyzCompsPrinted: boolean;
begin
  result := ItemPrinted([pfrXYZComponents]);
end;

function TPrintFrequencyCollection.XyzHeadPrinted: boolean;
begin
  result := ItemPrinted([pfrXYZHeads]);
end;

function TPrintFrequencyCollection.XyzVelPrinted: boolean;
begin
  result := ItemPrinted([pfrXYZVelocities]);
end;

function TPrintFrequencyCollection.XyzWelPrinted: boolean;
begin
  result := ItemPrinted([pfrXYZWells]);
end;

end.

