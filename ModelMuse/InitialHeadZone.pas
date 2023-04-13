{@abstract(@name is used to create classes to define zones of the
initial head.  These classes are
@link(TInitialHeadDataSets), @link(TInitialHeadZone), and
@link(TInitialHeadZoneGroup).)}
unit InitialHeadZone;

interface

uses SysUtils, Classes, GoPhastTypes, DataSetUnit, ZoneUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for the initial head.)}
  TInitialHeadDataSets = class(TCustomDataSets)
  private
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the initial head.
    // It is set in @link(Create).
    FInitialHead: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private field and initializes it.
    constructor Create;
  end;

  {@abstract(@name represents an individual initial head zone.)
   It provides a @link(TRProperty) for the initial head.}
  TInitialHeadZone = class(TCustomPhastZone)
  protected
    // @name calls the inherited @name and then
    // assigns a value to @link(FInitialHead)
    // based on the values at (LayerIndex, RowIndex, ColIndex)
    // in DataSets.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name: @link(TRProperty);
    // @name specifies the initial head of the zone.
    FInitialHead: TRProperty;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  the initial head.)}
  TInitialHeadZoneGroup = class(TCustomZoneGroup)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TInitialHeadZone;
  public
    // @name creates an instance of @classname and creates the
    // @link(TInitialHeadZone)s for it.
    constructor Create;
    // @name allows access to the @link(TInitialHeadZone)s
    // for this @classname.
    // See TCustomZoneGroup.@link(TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TInitialHeadZone read GetZone;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, DataSetNamesUnit;

{ TInitialHeadDataSets }

constructor TInitialHeadDataSets.Create;
begin
  FEvalAt := eaNodes;
  FZoneClass := TInitialHeadZone;
  FInitialHead := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(rsInitial_Head);
  FInitialHead.Initialize;
end;

{ TInitialHeadZone }

procedure TInitialHeadZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  InitialHeadDataSets: TInitialHeadDataSets;
begin
  inherited;
  InitialHeadDataSets := DataSets as TInitialHeadDataSets;
  FInitialHead.Assign(InitialHeadDataSets.FInitialHead, LayerIndex, RowIndex,
    ColIndex);
end;

constructor TInitialHeadZone.Create;
begin
  inherited;
  FInitialHead := TRProperty.Create;
  FPropertyList.Add(FInitialHead);
end;

destructor TInitialHeadZone.Destroy;
begin
  FInitialHead.Free;
  inherited;
end;

{ TInitialHeadZoneGroup }

constructor TInitialHeadZoneGroup.Create;
var
  DataSets: TInitialHeadDataSets;
begin
  DataSets := TInitialHeadDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TInitialHeadZoneGroup.GetZone(Index: integer): TInitialHeadZone;
begin
  result := inherited Zones[Index] as TInitialHeadZone;
end;

end.

