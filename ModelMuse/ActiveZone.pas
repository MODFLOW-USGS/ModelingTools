{@abstract(@name is used to define classes to create zones of the
active area of the model.  These classes are
  @link(TActiveZone) and @link(TActiveZoneGroup).)}
unit ActiveZone;

interface

uses SysUtils, Classes, GoPhastTypes, DataSetUnit, ZoneUnit;

type
  {@abstract(@name represents an individual active area zone.)
   It provides a @link(TBProperty) for the active area.}
  TActiveZone = class(TCustomPhastZone)
  protected
    // @name calls the inherited @name and then
    // assigns a value to @link(FActive)
    // based on the values at (LayerIndex, RowIndex, ColIndex)
    // in DataSets.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name: @link(TBProperty);
    // @name specifies whether the zone represents an active (true)
    // on inactive (false) area of the model.
    FActive: TBProperty;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  the active area of the model.)}
  TActiveZoneGroup = class(TCustomZoneGroup)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TActiveZone;
  public
    // @name creates an instance of @classname and creates the
    // @link(TActiveZone)s for it.
    constructor Create;
    // @name allows access to the @link(TActiveZone)s
    // for this @classname.
    // See TCustomZoneGroup.@link(TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TActiveZone read GetZone;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, DataSetNamesUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for the active area.)}
  TActiveDataSets = class(TCustomDataSets)
  private
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the active area of the model.
    // It is set in @link(Create).
    FActive: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private field and initializes it.
    constructor Create;
  end;

{ TActiveDataSets }

constructor TActiveDataSets.Create;
begin
  FEvalAt := eaBlocks;
  FZoneClass := TActiveZone;
  FActive := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsActive);
  Assert(FActive <> nil);
  FActive.Initialize;
end;

{ TActiveZone }

procedure TActiveZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  ActiveDataSets: TActiveDataSets;
begin
  inherited;
  ActiveDataSets := DataSets as TActiveDataSets;
  FActive.Assign(ActiveDataSets.FActive, LayerIndex, RowIndex, ColIndex);
end;

constructor TActiveZone.Create;
begin
  inherited;
  FActive := TBProperty.Create;
  FPropertyList.Add(FActive);
end;

destructor TActiveZone.Destroy;
begin
  FActive.Free;
  inherited;
end;

{ TActiveZoneGroup }

constructor TActiveZoneGroup.Create;
var
  DataSets: TActiveDataSets;
begin
  DataSets := TActiveDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TActiveZoneGroup.GetZone(Index: integer): TActiveZone;
begin
  result := inherited Zones[Index] as TActiveZone;
end;

end.

