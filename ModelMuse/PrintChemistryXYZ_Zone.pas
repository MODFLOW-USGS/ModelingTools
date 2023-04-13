{@abstract(@name is used to create classes to define zones of
"Print Chemistry XYZ".  These classes are
@link(TPrintChemistryXYZ_DataSets), @link(TPrintChemistryXYZ_Zone),
and @link(TPrintChemistryXYZ_ZoneGroup).)}
unit PrintChemistryXYZ_Zone;

interface

uses SysUtils, Classes, GoPhastTypes, DataSetUnit, ZoneUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for "Print Chemistry XYZ".)}
  TPrintChemistryXYZ_DataSets = class(TCustomDataSets)
  private
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for "Print Chemistry XYZ".
    // It is set in @link(Create).
    FPrint_ChemistryXYZ: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private field and initializes it.
    constructor Create;
  end;

  {@abstract(@name represents an individual "Print Chemistry XYZ" zone.)
   It provides a @link(TIProperty) for "Print Chemistry XYZ".}
  TPrintChemistryXYZ_Zone = class(TCustomPhastZone)
  protected
    // @name calls the inherited @name and then
    // assigns a value to @link(FPrint_ChemistryXYZ)
    // based on the values at (LayerIndex, RowIndex, ColIndex)
    // in DataSets.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name: @link(TIProperty);
    // @name specifies "Print Chemistry XYZ" for the zone.
    FPrint_ChemistryXYZ: TIProperty;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  "Print Chemistry XYZ".)}
  TPrintChemistryXYZ_ZoneGroup = class(TCustomZoneGroup)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TPrintChemistryXYZ_Zone;
  public
    // @name creates an instance of @classname and creates the
    // @link(TPrintChemistryXYZ_Zone)s for it.
    constructor Create;
    // @name allows access to the @link(TPrintChemistryXYZ_Zone)s
    // for this @classname.
    // See TCustomZoneGroup.@link(TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TPrintChemistryXYZ_Zone read GetZone;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, DataSetNamesUnit;

{ TPrintChemistryXYZ_DataSets }

constructor TPrintChemistryXYZ_DataSets.Create;
begin
  FEvalAt := eaNodes;
  FZoneClass := TPrintChemistryXYZ_Zone;
  FPrint_ChemistryXYZ := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsPrint_XYZ_Chemistry);
  FPrint_ChemistryXYZ.Initialize;
end;

{ TPrintChemistryXYZ_Zone }

procedure TPrintChemistryXYZ_Zone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  PrintChemistryXYZ_DataSets: TPrintChemistryXYZ_DataSets;
begin
  inherited;
  PrintChemistryXYZ_DataSets := DataSets as TPrintChemistryXYZ_DataSets;
  FPrint_ChemistryXYZ.Assign(PrintChemistryXYZ_DataSets.FPrint_ChemistryXYZ,
    LayerIndex, RowIndex, ColIndex);
end;

constructor TPrintChemistryXYZ_Zone.Create;
begin
  inherited;
  FPrint_ChemistryXYZ := TIProperty.Create;
  FPropertyList.Add(FPrint_ChemistryXYZ);
end;

destructor TPrintChemistryXYZ_Zone.Destroy;
begin
  FPrint_ChemistryXYZ.Free;
  inherited;
end;

{ TPrintChemistryXYZ_ZoneGroup }

constructor TPrintChemistryXYZ_ZoneGroup.Create;
var
  DataSets: TPrintChemistryXYZ_DataSets;
begin
  DataSets := TPrintChemistryXYZ_DataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TPrintChemistryXYZ_ZoneGroup.GetZone(
  Index: integer): TPrintChemistryXYZ_Zone;
begin
  result := inherited Zones[Index] as TPrintChemistryXYZ_Zone;
end;

end.

