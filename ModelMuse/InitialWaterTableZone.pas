unit InitialWaterTableZone;

interface

uses SysUtils, Classes, BasicTypes, DataSetUnit, PhastDataSets, ZoneUnit;

type
  TInitialWaterTableDataSets = class(TDataSets)
  private
    InitialWaterTable: TDataSet;
  public
    constructor Create;
  end;

  TInitialWaterTableZone = class(TPhastZone)
  protected
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    InitialWaterTable: TRProperty;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TInitialWaterTableZoneGroup = class(TZoneGroup)
  private
    function GetZone(Index: integer): TInitialWaterTableZone;
  public
    constructor Create;
    property Zones[Index: integer]: TInitialWaterTableZone read GetZone;
  end;


implementation

uses Contnrs, frmGoPhastUnit, ModelUnit;

{ TInitialWaterTableDataSets }

constructor TInitialWaterTableDataSets.Create;
begin
  EvalAt := eaNodes;
  ZoneClass := TInitialWaterTableZone;
  InitialWaterTable := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(rsInitial_Water_Table)];
  InitialWaterTable.Initialize;
end;

{ TInitialWaterTableZone }

procedure TInitialWaterTableZone.AssignProperties(const LayerIndex,
  RowIndex, ColIndex: integer; const DataSets: TDataSets; const CanMergeX,
  CanMergeY, CanMergeZ: boolean);
var
  InitialWaterTableDataSets: TInitialWaterTableDataSets;
begin
  inherited;
  InitialWaterTableDataSets := DataSets as TInitialWaterTableDataSets;
  InitialWaterTable.Assign(InitialWaterTableDataSets.InitialWaterTable,
    LayerIndex, RowIndex, ColIndex);
end;

constructor TInitialWaterTableZone.Create;
begin
  inherited;
  InitialWaterTable := TRProperty.Create;
  PropertyList.Add(InitialWaterTable);
end;

destructor TInitialWaterTableZone.Destroy;
begin
  InitialWaterTable.Free;
  inherited;
end;

{ TInitialWaterTableZoneGroup }

constructor TInitialWaterTableZoneGroup.Create;
var
  DataSets: TInitialWaterTableDataSets;
begin
  DataSets := TInitialWaterTableDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TInitialWaterTableZoneGroup.GetZone(
  Index: integer): TInitialWaterTableZone;
begin
  result := inherited Zones[Index] as TInitialWaterTableZone;

end;

end.
