{@abstract(@name is used to create classes to define zones of the
initial chemistry.  These classes are
@link(TInitialChemistryDataSets), @link(TInitialChemistryZone), and
@link(TInitialChemistryZoneGroup).)}
unit InitialChemistryZone;

interface

uses System.Types, SysUtils, Classes, GoPhastTypes, DataSetUnit, PhastDataSets,
  ZoneUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for the initial chemistry.)}
  TInitialChemistryDataSets = class(TCustomDataSets)
  private
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial equilibrium.
    // It is set in @link(Create).
    FChemistry_Initial_Equilibrium_Phases: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial exchange.
    // It is set in @link(Create).
    FChemistry_Initial_Exchange: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial gas phase.
    // It is set in @link(Create).
    FChemistry_Initial_Gas_Phase: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial kinetics.
    // It is set in @link(Create).
    FChemistry_Initial_Kinetics: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial solid solution.
    // It is set in @link(Create).
    FChemistry_Initial_Solid_Solutions: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial solution.
    // It is set in @link(Create).
    FChemistry_Initial_Solution: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial surface.
    // It is set in @link(Create).
    FChemistry_Initial_Surface: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private fields and initializes them.
    constructor Create;
  end;

  {@abstract(@name represents an individual initial chemistry zone.)
   It provides @link(TIProperty)s for each type of initial chemistry.}
  TInitialChemistryZone = class(TCustomPhastZone)
  protected
    // @name calls the inherited @name and then
    // assigns values to the @link(TIProperty)s
    // based on the values at (LayerIndex, RowIndex, ColIndex)
    // in DataSets.
    // Some of the @link(TIProperty)s may be removed from
    // @link(TCustomPhastZone.FPropertyList)
    // in @name if they aren't used.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name: @link(TIProperty);
    // @name specifies the initial equilibrium phase of the zone.
    FChemistry_Initial_Equilibrium_Phases: TIProperty;
    // @name: @link(TIProperty);
    // @name specifies the initial exchange of the zone.
    FChemistry_Initial_Exchange: TIProperty;
    // @name: @link(TIProperty);
    // @name specifies the initial gas phase of the zone.
    FChemistry_Initial_Gas_Phase: TIProperty;
    // @name: @link(TIProperty);
    // @name specifies the initial kinetics of the zone.
    FChemistry_Initial_Kinetics: TIProperty;
    // @name: @link(TIProperty);
    // @name specifies the initial solid solution of the zone.
    FChemistry_Initial_Solid_Solutions: TIProperty;
    // @name: @link(TIProperty);
    // @name specifies the initial solution of the zone.
    FChemistry_Initial_Solution: TIProperty;
    // @name: @link(TIProperty);
    // @name specifies the initial surface of the zone.
    FChemistry_Initial_Surface: TIProperty;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  the initial chemistry.)}
  TInitialChemistryZoneGroup = class(TCustomZoneGroup)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TInitialChemistryZone;
  public
    // @name creates an instance of @classname and creates the
    // @link(TInitialChemistryZone)s for it.
    constructor Create;
    // @name allows access to the @link(TInitialChemistryZone)s
    // for this @classname.
    // See @inherited @link(ZoneUnit.TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TInitialChemistryZone read GetZone;
  end;

implementation

uses Contnrs, frmGoPhastUnit, PhastModelUnit;

{ TInitialChemistryDataSets }

constructor TInitialChemistryDataSets.Create;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  FEvalAt := eaNodes;
  FZoneClass := TInitialChemistryZone;
  FChemistry_Initial_Solution := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Solution);
  FChemistry_Initial_Solution.Initialize;
  if frmGoPhast.PhastModel.ChemistryOptions.UseEquilibriumPhases then
  begin
    FChemistry_Initial_Equilibrium_Phases :=
      DataArrayManager.GetDataSetByName(rsChemistry_Initial_Equilibrium_Phases);
    FChemistry_Initial_Equilibrium_Phases.Initialize;
  end
  else
  begin
    FChemistry_Initial_Equilibrium_Phases := nil;
  end;

  if frmGoPhast.PhastModel.ChemistryOptions.UseSurfaceAssemblages then
  begin
    FChemistry_Initial_Surface := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Surface);
    FChemistry_Initial_Surface.Initialize;
  end
  else
  begin
    FChemistry_Initial_Surface := nil;
  end;

  if frmGoPhast.PhastModel.ChemistryOptions.UseExchange then
  begin
    FChemistry_Initial_Exchange := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Exchange);
    FChemistry_Initial_Exchange.Initialize;
  end
  else
  begin
    FChemistry_Initial_Exchange := nil;
  end;

  if frmGoPhast.PhastModel.ChemistryOptions.UseGasPhases then
  begin
    FChemistry_Initial_Gas_Phase := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Gas_Phase);
    FChemistry_Initial_Gas_Phase.Initialize;
  end
  else
  begin
    FChemistry_Initial_Gas_Phase := nil;
  end;

  if frmGoPhast.PhastModel.ChemistryOptions.UseSolidSolution then
  begin
    FChemistry_Initial_Solid_Solutions :=
      DataArrayManager.GetDataSetByName(rsChemistry_Initial_Solid_Solutions);
    FChemistry_Initial_Solid_Solutions.Initialize;
  end
  else
  begin
    FChemistry_Initial_Solid_Solutions := nil;
  end;

  if frmGoPhast.PhastModel.ChemistryOptions.UseKineticReactants then
  begin
    FChemistry_Initial_Kinetics := DataArrayManager.GetDataSetByName(rsChemistry_Initial_Kinetics);
    FChemistry_Initial_Kinetics.Initialize;
  end
  else
  begin
    FChemistry_Initial_Kinetics := nil;
  end;
end;

{ TInitialChemistryZone }

procedure TInitialChemistryZone.AssignProperties(const LayerIndex,
  RowIndex, ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  InitialChemistryDataSets: TInitialChemistryDataSets;
begin
  inherited;
  InitialChemistryDataSets := DataSets as TInitialChemistryDataSets;
  FChemistry_Initial_Solution.Assign(InitialChemistryDataSets.
    FChemistry_Initial_Solution, LayerIndex, RowIndex, ColIndex);

  if InitialChemistryDataSets.FChemistry_Initial_Equilibrium_Phases = nil then
  begin
    FPropertyList.Remove(FChemistry_Initial_Equilibrium_Phases);
  end
  else
  begin
    FChemistry_Initial_Equilibrium_Phases.Assign(InitialChemistryDataSets.
      FChemistry_Initial_Equilibrium_Phases, LayerIndex, RowIndex, ColIndex);
  end;

  if InitialChemistryDataSets.FChemistry_Initial_Surface = nil then
  begin
    FPropertyList.Remove(FChemistry_Initial_Surface);
  end
  else
  begin
    FChemistry_Initial_Surface.Assign(InitialChemistryDataSets.
      FChemistry_Initial_Surface, LayerIndex, RowIndex, ColIndex);
  end;

  if InitialChemistryDataSets.FChemistry_Initial_Exchange = nil then
  begin
    FPropertyList.Remove(FChemistry_Initial_Exchange);
  end
  else
  begin
    FChemistry_Initial_Exchange.Assign(InitialChemistryDataSets.
      FChemistry_Initial_Exchange, LayerIndex, RowIndex, ColIndex);
  end;

  if InitialChemistryDataSets.FChemistry_Initial_Gas_Phase = nil then
  begin
    FPropertyList.Remove(FChemistry_Initial_Gas_Phase);
  end
  else
  begin
    FChemistry_Initial_Gas_Phase.Assign(InitialChemistryDataSets.
      FChemistry_Initial_Gas_Phase, LayerIndex, RowIndex, ColIndex);
  end;

  if InitialChemistryDataSets.FChemistry_Initial_Solid_Solutions = nil then
  begin
    FPropertyList.Remove(FChemistry_Initial_Solid_Solutions);
  end
  else
  begin
    FChemistry_Initial_Solid_Solutions.Assign(InitialChemistryDataSets.
      FChemistry_Initial_Solid_Solutions, LayerIndex, RowIndex, ColIndex);
  end;

  if InitialChemistryDataSets.FChemistry_Initial_Kinetics = nil then
  begin
    FPropertyList.Remove(FChemistry_Initial_Kinetics);
  end
  else
  begin
    FChemistry_Initial_Kinetics.Assign(InitialChemistryDataSets.
      FChemistry_Initial_Kinetics, LayerIndex, RowIndex, ColIndex);
  end;
end;

constructor TInitialChemistryZone.Create;
begin
  inherited;
  FChemistry_Initial_Solution := TIProperty.Create;
  FChemistry_Initial_Equilibrium_Phases := TIProperty.Create;
  FChemistry_Initial_Surface := TIProperty.Create;
  FChemistry_Initial_Exchange := TIProperty.Create;
  FChemistry_Initial_Gas_Phase := TIProperty.Create;
  FChemistry_Initial_Solid_Solutions := TIProperty.Create;
  FChemistry_Initial_Kinetics := TIProperty.Create;

  FPropertyList.Add(FChemistry_Initial_Solution);
  FPropertyList.Add(FChemistry_Initial_Equilibrium_Phases);
  FPropertyList.Add(FChemistry_Initial_Surface);
  FPropertyList.Add(FChemistry_Initial_Exchange);
  FPropertyList.Add(FChemistry_Initial_Gas_Phase);
  FPropertyList.Add(FChemistry_Initial_Solid_Solutions);
  FPropertyList.Add(FChemistry_Initial_Kinetics);
end;

destructor TInitialChemistryZone.Destroy;
begin
  FChemistry_Initial_Solution.Free;
  FChemistry_Initial_Equilibrium_Phases.Free;
  FChemistry_Initial_Surface.Free;
  FChemistry_Initial_Exchange.Free;
  FChemistry_Initial_Gas_Phase.Free;
  FChemistry_Initial_Solid_Solutions.Free;
  FChemistry_Initial_Kinetics.Free;
  inherited;
end;

{ TInitialChemistryZoneGroup }

constructor TInitialChemistryZoneGroup.Create;
var
  DataSets: TInitialChemistryDataSets;
begin
  DataSets := TInitialChemistryDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TInitialChemistryZoneGroup.GetZone(
  Index: integer): TInitialChemistryZone;
begin
  result := inherited Zones[index] as TInitialChemistryZone;
end;

end.

