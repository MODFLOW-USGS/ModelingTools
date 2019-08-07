{@abstract(@name is used to create classes to define zones of
"Print Chemistry".  These classes are
@link(TPrintChemistryDataSets), @link(TPrintChemistryZone), and
@link(TPrintChemistryZoneGroup).)}
unit PrintChemistryZone;

interface

uses SysUtils, Classes, GoPhastTypes, DataSetUnit, PhastDataSets, ZoneUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for "Print Chemistry".)}
  TPrintChemistryDataSets = class(TCustomDataSets)
  private
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for "Print Chemistry".
    // It is set in @link(Create).
    FPrint_Chemistry: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private field and initializes it.
    constructor Create;
  end;

  {@abstract(@name represents an individual "Print Chemistry" zone.)
   It provides a @link(TIProperty) for "Print Chemistry".}
  TPrintChemistryZone = class(TCustomPhastZone)
  protected
    // @name calls the inherited @name and then
    // assigns a value to @link(FPrint_Chemistry)
    // based on the values at (LayerIndex, RowIndex, ColIndex)
    // in DataSets.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name: @link(TIProperty);
    // @name specifies "Print Chemistry" for the zone.
    FPrint_Chemistry: TIProperty;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  "Print Chemistry".)}
  TPrintChemistryZoneGroup = class(TCustomZoneGroup)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TPrintChemistryZone;
  public
    // @name creates an instance of @classname and creates the
    // @link(TPrintChemistryZone)s for it.
    constructor Create;
    // @name allows access to the @link(TPrintChemistryZone)s
    // for this @classname.
    // See TCustomZoneGroup.@link(TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TPrintChemistryZone read GetZone;
  end;

implementation

uses Contnrs, frmGoPhastUnit, PhastModelUnit;

{ TPrintChemistryDataSets }

constructor TPrintChemistryDataSets.Create;
begin
  FEvalAt := eaNodes;
  FZoneClass := TPrintChemistryZone;
  FPrint_Chemistry := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsPrint_Chemistry);
  FPrint_Chemistry.Initialize;
end;

{ TPrintChemistryZone }

procedure TPrintChemistryZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  PrintChemistryDataSets: TPrintChemistryDataSets;
begin
  inherited;
  PrintChemistryDataSets := DataSets as TPrintChemistryDataSets;
  FPrint_Chemistry.Assign(PrintChemistryDataSets.FPrint_Chemistry, LayerIndex,
    RowIndex, ColIndex);
end;

constructor TPrintChemistryZone.Create;
begin
  inherited;
  FPrint_Chemistry := TIProperty.Create;
  FPropertyList.Add(FPrint_Chemistry);
end;

destructor TPrintChemistryZone.Destroy;
begin
  FPrint_Chemistry.Free;
  inherited;
end;

{ TPrintChemistryZoneGroup }

constructor TPrintChemistryZoneGroup.Create;
var
  DataSets: TPrintChemistryDataSets;
begin
  DataSets := TPrintChemistryDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TPrintChemistryZoneGroup.GetZone(
  Index: integer): TPrintChemistryZone;
begin
  result := inherited Zones[Index] as TPrintChemistryZone;
end;

end.

