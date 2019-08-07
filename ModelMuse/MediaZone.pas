{@abstract(@name is used to create classes to define zones of
the media properties such as
hydraulic conductivity.  These classes are
@link(TMediaDataSets), @link(TMediaZone), and @link(TMediaZoneGroup).)}
unit MediaZone;

interface

uses SysUtils, Classes, GoPhastTypes, DataSetUnit, PhastDataSets, ZoneUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for the media properties.)}
  TMediaDataSets = class(TCustomDataSets)
  private
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the horizontal transverse dispersivity.
    // It is set in @link(Create).
    FHorzTransDisp: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the permeability in the X direction.
    // It is set in @link(Create).
    FKx: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the permeability in the Y direction.
    // It is set in @link(Create).
    FKy: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the permeability in the Z direction.
    // It is set in @link(Create).
    FKz: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the longitudinal dispersivity.
    // It is set in @link(Create).
    FLongDisp: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the porosity.
    // It is set in @link(Create).
    FPorosity: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the specific storage.
    // It is set in @link(Create).
    FSpecificStorage: TDataArray;
    // @name: @link(TDataArray);
    // @name is the @link(TDataArray) for the vertical transverse dispersivity.
    // It is set in @link(Create).
    FVertTransDisp: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private fields and initializes them.
    constructor Create;
  end;

  {@abstract(@name represents an individual media properties zone.)
   It provides @link(TRProperty)s for each type of media property.}
  TMediaZone = class(TCustomPhastZone)
  private
    FVertTransDisp: TRProperty;
    FSpecificStorage: TRProperty;
    FPorosity: TRProperty;
    FLongDisp: TRProperty;
    FKz: TRProperty;
    FKy: TRProperty;
    FKx: TRProperty;
    FHorzTransDisp: TRProperty;
  protected
    // @name calls the inherited @name and then
    // assigns values to the @link(TRProperty)s
    // based on the values at (LayerIndex, RowIndex, ColIndex)
    // in DataSets.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name specifies the horizontal transverse dispersivity of the zone.
    property HorzTransDisp: TRProperty read FHorzTransDisp;
    // @name specifies the permeability in the X direction of the zone.
    property Kx: TRProperty read FKx;
    // @name specifies the permeability in the Y direction of the zone.
    property Ky: TRProperty read FKy;
    // @name specifies the permeability in the Z direction of the zone.
    property Kz: TRProperty read FKz;
    // @name specifies the longitudinal dispersivity of the zone.
    property LongDisp: TRProperty read FLongDisp;
    // @name specifies the porosity of the zone.
    property Porosity: TRProperty read FPorosity;
    // @name specifies the specific storage of the zone.
    property SpecificStorage: TRProperty read FSpecificStorage;
    // @name specifies the vertical transverse dispersivity of the zone.
    property VertTransDisp: TRProperty read FVertTransDisp;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  the media properties such as hydraulic conductivity.)}
  TMediaZoneGroup = class(TCustomZoneGroup)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TMediaZone;
  public
    // @name creates an instance of @classname and creates the
    // @link(TMediaZone)s for it.
    constructor Create;
    // @name allows access to the @link(TMediaZone)s
    // for this @classname.
    // See @inherited @link(ZoneUnit.TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TMediaZone read GetZone;
  end;

implementation

uses Contnrs, frmGoPhastUnit, PhastModelUnit;

{ TMediaDataSets }

constructor TMediaDataSets.Create;
var
  DataArrayManager: TDataArrayManager;
begin
  FEvalAt := eaBlocks;
  FZoneClass := TMediaZone;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  FKx := DataArrayManager.GetDataSetByName(rsKx);
  FKy := DataArrayManager.GetDataSetByName(rsKy);
  FKz := DataArrayManager.GetDataSetByName(rsKz);
  FPorosity := DataArrayManager.GetDataSetByName(rsPorosity);
  FSpecificStorage := DataArrayManager.GetDataSetByName(rsSpecific_Storage);
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FLongDisp := DataArrayManager.GetDataSetByName(rsLong_Dispersivity);
    FHorzTransDisp := DataArrayManager.GetDataSetByName(rsHorizontal_Transv_Dispersivity);
    FVertTransDisp := DataArrayManager.GetDataSetByName(rsVertical_Transv_Dispersivity);
  end;

  FKx.Initialize;
  FKy.Initialize;
  FKz.Initialize;
  FPorosity.Initialize;
  FSpecificStorage.Initialize;
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FLongDisp.Initialize;
    FHorzTransDisp.Initialize;
    FVertTransDisp.Initialize;
  end;

end;

{ TMediaZone }

procedure TMediaZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  MediaDataSets: TMediaDataSets;
begin
  inherited;
  MediaDataSets := DataSets as TMediaDataSets;
  Kx.Assign(MediaDataSets.FKx, LayerIndex, RowIndex, ColIndex);
  Ky.Assign(MediaDataSets.FKy, LayerIndex, RowIndex, ColIndex);
  Kz.Assign(MediaDataSets.FKz, LayerIndex, RowIndex, ColIndex);
  Porosity.Assign(MediaDataSets.FPorosity, LayerIndex, RowIndex, ColIndex);
  SpecificStorage.Assign(MediaDataSets.FSpecificStorage, LayerIndex, RowIndex,
    ColIndex);
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    LongDisp.Assign(MediaDataSets.FLongDisp, LayerIndex, RowIndex, ColIndex);
    HorzTransDisp.Assign(MediaDataSets.FHorzTransDisp, LayerIndex, RowIndex,
      ColIndex);
    VertTransDisp.Assign(MediaDataSets.FVertTransDisp, LayerIndex, RowIndex,
      ColIndex);
  end;
end;

constructor TMediaZone.Create;
begin
  inherited;
  FKx := TRProperty.Create;
  FKy := TRProperty.Create;
  FKz := TRProperty.Create;
  FPorosity := TRProperty.Create;
  FSpecificStorage := TRProperty.Create;
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FLongDisp := TRProperty.Create;
    FHorzTransDisp := TRProperty.Create;
    FVertTransDisp := TRProperty.Create;
  end;
  FPropertyList.Add(Kx);
  FPropertyList.Add(Ky);
  FPropertyList.Add(Kz);
  FPropertyList.Add(Porosity);
  FPropertyList.Add(SpecificStorage);
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FPropertyList.Add(LongDisp);
    FPropertyList.Add(HorzTransDisp);
    FPropertyList.Add(VertTransDisp);
  end;
end;

destructor TMediaZone.Destroy;
begin
  FKx.Free;
  FKy.Free;
  FKz.Free;
  FPorosity.Free;
  FSpecificStorage.Free;
  FLongDisp.Free;
  FHorzTransDisp.Free;
  FVertTransDisp.Free;
  inherited;
end;

{ TMediaZoneGroup }

constructor TMediaZoneGroup.Create;
var
  DataSets: TMediaDataSets;
begin
  DataSets := TMediaDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TMediaZoneGroup.GetZone(Index: integer): TMediaZone;
begin
  result := inherited Zones[Index] as TMediaZone;
end;

end.

