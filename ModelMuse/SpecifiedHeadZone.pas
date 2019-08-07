{@abstract(@name is used to create classes to define zones of specified head
boundary conditions on the top, front, and side view of the model.
These classes are
@link(TSpecifiedHeadDataSets), @link(TSpecifiedHeadZone),
and @link(TSpecifiedHeadZoneGroup).)}
unit SpecifiedHeadZone;

interface

uses Classes, GoPhastTypes, PhastDataSets, ZoneUnit, RealListUnit, DataSetUnit;

type
  TSpecifiedHeadDataSets = class(TCustomDataSets)
  private
    // @name: @link(PhastDataSets.TPhastTimeList);
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.SpecifiedHeadAssociatedSolution).
    FAssociatedSolutions: TPhastTimeList;
    // @name: @link(DataSetUnit.TDataArray);
    // @name is the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated
    // for the front view of the model.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.FrontBoundaryType).
    FFrontBoundaryType: TDataArray;
    // @name: @link(DataSetUnit.TDataArray);
    // @name is the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated
    // for the side view of the model.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.SideBoundaryType).
    FSideBoundaryType: TDataArray;
    // @name: @link(DataSetUnit.TDataArray);
    // @name is the @link(DataSetUnit.TDataArray)
    // that identifies whether the solution is a specified solution
    // or an associated solution.
    FSolutionType: TDataArray;
    // @name: @link(PhastDataSets.TPhastTimeList);
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the specified heads.
    // in the boundary condition.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.SpecifiedHeadHead).
    FSpecifiedHeads: TPhastTimeList;
    // @name: @link(DataSetUnit.TDataArray);
    // @name is the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated
    // for the tope view of the model.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.TopBoundaryType).
    FTopBoundaryType: TDataArray;
  public
    // @name creates an instance of @classname, assigns
    // the private fields and initializes them.
    constructor Create;
  end;

  {@abstract(@name represents an individual specified head zone.)}
  TSpecifiedHeadZone = class(TCustomPhastZone)
  private
    // @name indicates the type of boundary condition in the zone
    // in the front view of the model.
    FFrontBoundaryType: TIproperty;
    // @name indicates the type of boundary condition in the zone
    // in the side view of the model.
    FSideBoundaryType: TIproperty;
    // @name indicates the type of boundary condition in the zone
    // in the top view of the model.
    FTopBoundaryType: TIproperty;
  protected
    // @name gets the values of each data set related to the zone and
    // assigns a value to a @link(ZoneUnit.TCustomProperty)
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    // @name is a list of @link(ZoneUnit.TIProperty)s
    // whose values represent the solution in the zone.
    FAssociatedSolutions: TIPropertyList;
    // @name indicates whether the solution is an associated solution
    // or a specified solution.
    FSolutionType: TIProperty;
    // @name is a list of @link(ZoneUnit.TRProperty)s
    // whose values represent the specified head in the zone.
    FSpecifiedHeads: TRPropertyList;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   specified head boundary conditions.)}
  TSpecifiedHeadZoneGroup = class(TCustomZoneGroup)
  private
    // @name: @link(RealListUnit.TRealList);
    // See @link(HeadTimes).
    FHeadTimeList: TRealList;
    // @name: @link(RealListUnit.TRealList);
    // See @link(SolutionTimes).
    FSolutionTimeList: TRealList;
    // See @link(HeadTimeCount).
    function GetHeadTimeCount: integer;
    // See @link(HeadTimes).
    function GetHeadTimes(Index: integer): double;
    // See @link(SolutionTimeCount).
    function GetSolutionTimeCount: integer;
    // See @link(SolutionTimes).
    function GetSolutionTimes(Index: integer): double;
    // See @link(Zones).
    function GetZone(Index: integer): TSpecifiedHeadZone;
  protected
    { TODO : create an ancestor that calls EliminateExtraZones. }
    // @name checks each of the zones that have been created
    // and deletes any that don't have values.  Thus, it
    // eliminates all the zones where the boundary condition is absent.
    procedure EliminateExtraZones; virtual;
  public
    // @name creates an instance of @classname and creates the
    // @link(TSpecifiedHeadZone)s for it.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name gives the number of times
    // associated with the specified head boundary.
    // See @link(HeadTimes).
    property HeadTimeCount: integer read GetHeadTimeCount;
    // @name gives the times
    // associated with the specified head boundary.
    // See @link(HeadTimeCount).
    property HeadTimes[Index: integer]: double read GetHeadTimes;
    // @name gives the number of times
    // associated with the specified head boundary solution.
    // See @link(SolutionTimes).
    property SolutionTimeCount: integer read GetSolutionTimeCount;
    // @name gives the times
    // associated with the specified head boundary solution.
    // See @link(SolutionTimeCount).
    property SolutionTimes[Index: integer]: double read GetSolutionTimes;
    // @name allows access to the @link(TSpecifiedHeadZone)s
    // for this @classname.
    // See @inherited @link(ZoneUnit.TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TSpecifiedHeadZone read GetZone;
  end;

implementation

uses Contnrs, frmGoPhastUnit, PhastModelUnit;

{ TSpecifiedHeadDataSets }

constructor TSpecifiedHeadDataSets.Create;
var
  DataArrayManager: TDataArrayManager;
begin
  FEvalAt := eaNodes;
  FZoneClass := TSpecifiedHeadZone;
  FSpecifiedHeads := frmGoPhast.PhastModel.SpecifiedHeadHead;
  FSpecifiedHeads.Initialize;
  FTopBoundaryType := frmGoPhast.PhastModel.TopBoundaryType;
  FTopBoundaryType.Initialize;
  FFrontBoundaryType := frmGoPhast.PhastModel.FrontBoundaryType;
  FFrontBoundaryType.Initialize;
  FSideBoundaryType := frmGoPhast.PhastModel.SideBoundaryType;
  FSideBoundaryType.Initialize;

  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    FSolutionType := DataArrayManager.BoundaryDataSets[
      DataArrayManager.IndexOfBoundaryDataSet(rsSolutionType)];
    FSolutionType.Initialize;
    FAssociatedSolutions := frmGoPhast.PhastModel.SpecifiedHeadAssociatedSolution;
    FAssociatedSolutions.Initialize;
  end
  else
  begin
    FAssociatedSolutions := nil;
    FSolutionType := nil;
  end;
end;

{ TSpecifiedHeadZone }

procedure TSpecifiedHeadZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
var
  SpecifiedHeadDataSets: TSpecifiedHeadDataSets;
  Index: integer;
  DataSet: TDataArray;
  RProperty: TRProperty;
  IProperty: TIProperty;
begin
  inherited;
  SpecifiedHeadDataSets := DataSets as TSpecifiedHeadDataSets;
  for Index := 0 to SpecifiedHeadDataSets.FSpecifiedHeads.Count - 1 do
  begin
    DataSet := SpecifiedHeadDataSets.FSpecifiedHeads.Items[Index];
    if Index < FSpecifiedHeads.Count then
    begin
      RProperty := FSpecifiedHeads[Index]
    end
    else
    begin
      RProperty := TRProperty.Create;
      FSpecifiedHeads.Add(RProperty);
      FPropertyList.Add(RProperty);
    end;
    RProperty.Assign(DataSet, LayerIndex, RowIndex, ColIndex);
  end;
  FTopBoundaryType.Assign(SpecifiedHeadDataSets.FTopBoundaryType,
    LayerIndex, RowIndex, ColIndex);
  FFrontBoundaryType.Assign(SpecifiedHeadDataSets.FFrontBoundaryType,
    LayerIndex, RowIndex, ColIndex);
  FSideBoundaryType.Assign(SpecifiedHeadDataSets.FSideBoundaryType,
    LayerIndex, RowIndex, ColIndex);
  if SpecifiedHeadDataSets.FSolutionType <> nil then
  begin
    FSolutionType.Assign(SpecifiedHeadDataSets.FSolutionType,
      LayerIndex, RowIndex, ColIndex);
  end;
  if SpecifiedHeadDataSets.FAssociatedSolutions <> nil then
  begin
    for Index := 0 to SpecifiedHeadDataSets.FAssociatedSolutions.Count - 1 do
    begin
      DataSet := SpecifiedHeadDataSets.FAssociatedSolutions.Items[Index];
      if Index < FAssociatedSolutions.Count then
      begin
        IProperty := FAssociatedSolutions[Index]
      end
      else
      begin
        IProperty := TIProperty.Create;
        FAssociatedSolutions.Add(IProperty);
        FPropertyList.Add(IProperty);
      end;
      IProperty.Assign(DataSet, LayerIndex, RowIndex, ColIndex);
    end;
  end;
end;

constructor TSpecifiedHeadZone.Create;
begin
  inherited;
  FTopBoundaryType := TIproperty.Create;
  FPropertyList.Add(FTopBoundaryType);
  FFrontBoundaryType := TIproperty.Create;
  FPropertyList.Add(FFrontBoundaryType);
  FSideBoundaryType := TIproperty.Create;
  FPropertyList.Add(FSideBoundaryType);
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FSolutionType := TIProperty.Create;
    FPropertyList.Add(FSolutionType);
  end;
  FSpecifiedHeads := TRPropertyList.Create;
  FAssociatedSolutions := TIPropertyList.Create;
end;

destructor TSpecifiedHeadZone.Destroy;
begin
  FSolutionType.Free;
  FTopBoundaryType.Free;
  FFrontBoundaryType.Free;
  FSideBoundaryType.Free;
  FSpecifiedHeads.Free;
  FAssociatedSolutions.Free;
  inherited;
end;

{ TSpecifiedHeadZoneGroup }

constructor TSpecifiedHeadZoneGroup.Create;
var
  DataSets: TSpecifiedHeadDataSets;
  Index: integer;
begin
  FHeadTimeList := TRealList.Create;
  if not frmGoPhast.PhastModel.SoluteTransport then
  begin
    FSolutionTimeList := nil
  end
  else
  begin
    FSolutionTimeList := TRealList.Create;
  end;

  DataSets := TSpecifiedHeadDataSets.Create;
  try
    inherited Create(DataSets);
    for Index := 0 to DataSets.FSpecifiedHeads.Count - 1 do
    begin
      FHeadTimeList.Add(DataSets.FSpecifiedHeads.Times[Index])
    end;
    if FSolutionTimeList <> nil then
    begin
      for Index := 0 to DataSets.FAssociatedSolutions.Count - 1 do
      begin
        FSolutionTimeList.Add(DataSets.FAssociatedSolutions.Times[Index])
      end;
    end;
  finally
    DataSets.Free;
  end;
  EliminateExtraZones;
end;

destructor TSpecifiedHeadZoneGroup.Destroy;
begin
  FHeadTimeList.Free;
  FSolutionTimeList.Free;
  inherited;
end;

procedure TSpecifiedHeadZoneGroup.EliminateExtraZones;
var
  Index: integer;
  Zone: TSpecifiedHeadZone;
  RProperty: TRProperty;
  IProperty: TIProperty;
  DeleteZone: boolean;
begin
  for Index := ZoneCount - 1 downto 0 do
  begin
    Zone := Zones[Index];
    DeleteZone := (Zone.FTopBoundaryType.IValue <> Ord(btSpecifiedHead))
      or (Zone.FFrontBoundaryType.IValue <> Ord(btSpecifiedHead))
      or (Zone.FSideBoundaryType.IValue <> Ord(btSpecifiedHead));
    if (Zone.FSpecifiedHeads.Count = 0)
      or ((FSolutionTimeList <> nil)
      and (Zone.FAssociatedSolutions.Count = 0)) then
    begin
      DeleteZone := true;
    end
    else
    begin
      RProperty := Zone.FSpecifiedHeads[0];
      if not RProperty.IsValue then
      begin
        DeleteZone := True;
      end
      else if FSolutionTimeList <> nil then
      begin
        IProperty := Zone.FAssociatedSolutions[0];
        if not IProperty.IsValue then
        begin
          DeleteZone := True;
        end
      end;
    end;
    if DeleteZone then
    begin
      FFinalZones.Delete(Index);
    end;
  end;
end;

function TSpecifiedHeadZoneGroup.GetHeadTimeCount: integer;
begin
  result := FHeadTimeList.Count;
end;

function TSpecifiedHeadZoneGroup.GetHeadTimes(Index: integer): double;
begin
  result := FHeadTimeList[Index];
end;

function TSpecifiedHeadZoneGroup.GetSolutionTimeCount: integer;
begin
  result := FSolutionTimeList.Count;
end;

function TSpecifiedHeadZoneGroup.GetSolutionTimes(Index: integer): double;
begin
  result := FSolutionTimeList[Index];
end;

function TSpecifiedHeadZoneGroup.GetZone(
  Index: integer): TSpecifiedHeadZone;
begin
  result := inherited Zones[Index] as TSpecifiedHeadZone;
end;

end.
