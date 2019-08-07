unit SpecifiedSolutionZone;

interface

uses Classes, BasicTypes, PhastDataSets, ZoneUnit, RealListUnit;

type
  TSpecifiedSolutionDataSets = class(TCustomDataSets)
  private
    Solutions: TTimeList;
  public
    constructor Create;
  end;

  TSpecifiedSolutionZone = class(TCustomPhastZone)
  protected
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    Solutions: TList;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TSpecifiedSolutionZoneGroup = class(TCustomZoneGroup)
  private
    SolutionTimeList: TRealList;
    function GetZone(Index: integer): TSpecifiedSolutionZone;
    function GetSolutionTimeCount: integer;
    function GetSolutionTimes(Index: integer): double;
  public
    constructor Create;
    property Zones[Index: integer]: TSpecifiedSolutionZone read GetZone;
    property SolutionTimes[Index: integer]: double read GetSolutionTimes;
    property SolutionTimeCount: integer read GetSolutionTimeCount;
    destructor Destroy; override;
  end;

implementation

uses Contnrs, frmGoPhastUnit, DataSetUnit;

{ TSpecifiedSolutionDataSets }

constructor TSpecifiedSolutionDataSets.Create;
begin
  EvalAt := eaNodes;
  ZoneClass := TSpecifiedSolutionZone;
  Solutions := frmGoPhast.Model.SpecifiedSolution;
  Solutions.Initialize;
end;

{ TSpecifiedSolutionZone }

procedure TSpecifiedSolutionZone.AssignProperties(const LayerIndex,
  RowIndex, ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  SpecifiedSolutionDataSets: TSpecifiedSolutionDataSets;
  Index: integer;
  DataSet: TDataSet;
  IProperty: TIProperty;
begin
  inherited;
  SpecifiedSolutionDataSets := DataSets as TSpecifiedSolutionDataSets;
  for Index := 0 to SpecifiedSolutionDataSets.Solutions.Count - 1 do
  begin
    DataSet := SpecifiedSolutionDataSets.Solutions.Items[Index];
    if Index < Solutions.Count then
    begin
      IProperty := Solutions[Index]
    end
    else
    begin
      IProperty := TIProperty.Create;
      Solutions.Add(IProperty);
      FPropertyList.Add(IProperty);
    end;
    IProperty.Assign(DataSet, LayerIndex, RowIndex, ColIndex);
  end;
end;

constructor TSpecifiedSolutionZone.Create;
begin
  inherited;
  Solutions := TObjectList.Create;
end;

destructor TSpecifiedSolutionZone.Destroy;
begin
  Solutions.Free;
  inherited;
end;

{ TSpecifiedSolutionZoneGroup }

constructor TSpecifiedSolutionZoneGroup.Create;
var
  DataSets: TSpecifiedSolutionDataSets;
  Index: integer;
  Zone: TSpecifiedSolutionZone;
  IProperty: TIProperty;
  DeleteZone: boolean;
begin
  SolutionTimeList := TRealList.Create;
  DataSets := TSpecifiedSolutionDataSets.Create;
  try
    inherited Create(DataSets);
    for Index := 0 to DataSets.Solutions.Count - 1 do
    begin
      SolutionTimeList.Add(DataSets.Solutions.Times[Index])
    end;
  finally
    DataSets.Free;
  end;
  for Index := ZoneCount - 1 downto 0 do
  begin
    Zone := Zones[Index];
    DeleteZone := False;
    if (Zone.Solutions.Count = 0) then
    begin
      DeleteZone := true;
    end
    else
    begin
      IProperty := Zone.Solutions[0];
      if not IProperty.IsValue then
      begin
        DeleteZone := True;
      end
    end;
    if DeleteZone then
    begin
      FinalZones.Delete(Index);
    end;
  end;
end;

destructor TSpecifiedSolutionZoneGroup.Destroy;
begin
  SolutionTimeList.Free;
  inherited;
end;

function TSpecifiedSolutionZoneGroup.GetSolutionTimeCount: integer;
begin
  result := SolutionTimeList.Count;
end;

function TSpecifiedSolutionZoneGroup.GetSolutionTimes(
  Index: integer): double;
begin
  result := SolutionTimeList[Index];
end;

function TSpecifiedSolutionZoneGroup.GetZone(
  Index: integer): TSpecifiedSolutionZone;
begin
  result := inherited Zones[Index] as TSpecifiedSolutionZone;
end;

end.
