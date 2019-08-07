{ @abstract(@name is used to define abstract
   ancestors for leaky boundary condition zones.  These classes are
   @link(TCustomLeakyDataSets), @link(TCustomLeakyZone),
   and @link(TCustomLeakyZoneGroup).)}
unit CustomLeakyZone;

interface

uses GoPhastTypes, DataSetUnit, ZoneUnit, CustomBoundaryZone;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones
   for leaky boundary conditions.)
   @name adds @link(TDataArray)s for hydraulic conductivity and thickness
   to those provided by @inherited.}
  TCustomLeakyDataSets = class(TCustomBoundaryDataSets)
  protected
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for hydraulic conductivity.
    // It is set by @link(HydraulicConductivityDataSet) in @link(Create).
    FHydraulicConductivity: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for thickness.
    // It is set by @link(ThicknessDataSet) in @link(Create).
    FThickness: TDataArray;
    // @name is used to set @link(FHydraulicConductivity) in @link(Create).
    function HydraulicConductivityDataSet: TDataArray; virtual; abstract;
    // @name is used to set @link(FThickness) in @link(Create).
    function ThicknessDataSet: TDataArray; virtual; abstract;
  public
    // @name sets @link(FHydraulicConductivity) and @link(FThickness)
    // and initializes them. It also sets @link(TCustomDataSets.FEvalAt) and
    // @link(TCustomDataSets.FZoneClass).
    constructor Create; override;
  end;

  {@abstract(@name represents an individual leaky boundary zone.)
   It adds @link(TRProperty)s for hydraulic conductivity and thickness
   to those provided by @inherited.}
  TCustomLeakyZone = class(TBoundaryZone)
  protected
    // @name calls the inherited @name and then
    // assigns values to @link(FHydraulicConductivity) and @link(FThickness)
    // based on the values at (LayerIndex, RowIndex, ColIndex).
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name: @link(TRProperty);
    // @name is the hydraulic conductivity of the zone.
    FHydraulicConductivity: TRProperty;
    // @name: @link(TRProperty);
    // @name is the thickness of the zone.
    FThickness: TRProperty;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  leaky boundary conditions.)  The main change from @inherited
  is @link(TCustomLeakyZoneGroup.EliminateExtraZones) is overridden.}
  TCustomLeakyZoneGroup = class(TCustomBoundaryGroup)
  private
    // See @link(Zones);
    function GetZone(Index: integer): TCustomLeakyZone;
  protected
   // @name returns ord(btLeaky).
    function GetBoundaryType: integer; override;
    // @name deletes zones for which the hydraulic conductivity
    // or thickness are not defined.
    procedure EliminateExtraZones; override;
  public
    // @name allows access to the @link(TCustomLeakyZone)s for this @classname.
    // See @inherited @link(ZoneUnit.TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TCustomLeakyZone read GetZone;
  end;

implementation

{ TCustomLeakyDataSets }

constructor TCustomLeakyDataSets.Create;
begin
  inherited;
  FEvalAt := eaNodes;
  FZoneClass := TCustomLeakyZone;

  FHydraulicConductivity := HydraulicConductivityDataSet;
  FThickness := ThicknessDataSet;
  FHydraulicConductivity.Initialize;
  FThickness.Initialize;

end;

{ TCustomLeakyZone }

procedure TCustomLeakyZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  LeakyDataSets: TCustomLeakyDataSets;
begin
  inherited;
  LeakyDataSets := DataSets as TCustomLeakyDataSets;
  FHydraulicConductivity.Assign(LeakyDataSets.FHydraulicConductivity, LayerIndex,
    RowIndex, ColIndex);
  FThickness.Assign(LeakyDataSets.FThickness, LayerIndex, RowIndex, ColIndex);
end;

constructor TCustomLeakyZone.Create;
begin
  inherited;
  FHydraulicConductivity := TRProperty.Create;
  FThickness := TRProperty.Create;
  FPropertyList.Add(FHydraulicConductivity);
  FPropertyList.Add(FThickness);
end;

destructor TCustomLeakyZone.Destroy;
begin
  FHydraulicConductivity.Free;
  FThickness.Free;
  inherited;
end;

{ TCustomLeakyZoneGroup }

procedure TCustomLeakyZoneGroup.EliminateExtraZones;
var
  Index: integer;
  Zone: TCustomLeakyZone;
begin
  inherited;
  for Index := ZoneCount - 1 downto 0 do
  begin
    Zone := Zones[Index];
    if not Zone.FHydraulicConductivity.IsValue
      or not Zone.FThickness.IsValue then
    begin
      FFinalZones.Delete(Index);
    end;
  end;
end;

function TCustomLeakyZoneGroup.GetBoundaryType: integer;
begin
  result := ord(btLeaky);
end;

function TCustomLeakyZoneGroup.GetZone(Index: integer): TCustomLeakyZone;
begin
  result := inherited Zones[Index] as TCustomLeakyZone;
end;

end.


