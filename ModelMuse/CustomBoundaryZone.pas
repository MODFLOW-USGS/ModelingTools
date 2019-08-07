{ @abstract(@name is used to define abstract
   ancestors for flux and leaky boundary condition zones. These classes are
   @link(TCustomBoundaryDataSets), @link(TBoundaryZone),
   and @link(TCustomBoundaryGroup).)}
unit CustomBoundaryZone;

interface

uses Classes, GoPhastTypes, PhastDataSets, ZoneUnit, RealListUnit, DataSetUnit;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones
   for flux and leaky boundary conditions.)}
  TCustomBoundaryDataSets = class(TCustomDataSets)
  private
    // @name: @link(PhastDataSets.TPhastTimeList);
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // In @link(Create) it is set to @link(SolutionTimes).
    FAssociatedSolutionTimes: TPhastTimeList;
    // @name: @link(DataSetUnit.TDataArray);
    // @name is the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // In @link(Create) it is set to @link(BoundaryDataType).
    FBoundaryDataType: TDataArray;
    // @name: @link(PhastDataSets.TPhastTimeList);
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate or leaky head.
    // in the boundary condition.
    // In @link(Create) it is set to @link(BoundaryTimes).
    FBoundaryTimes: TPhastTimeList;
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    function BoundaryDataType: TDataArray; virtual; abstract;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate or leaky head.
    // in the boundary condition.
    function BoundaryTimes: TPhastTimeList; virtual; abstract;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    function SolutionTimes: TPhastTimeList; virtual; abstract;
  public
    // @name creates an instance of @classname, assigns
    // the private fields and initializes them.
    constructor Create; virtual;
  end;

  // @abstract(@name is used to create descendants of
  // @link(TCustomBoundaryDataSets)
  // using class references.)
  TBoundaryDataSetsType = class of TCustomBoundaryDataSets;

  {@abstract(@name represents an individual leaky or flux boundary zone.)}
  TBoundaryZone = class(TCustomPhastZone)
  private
    // @name indicates the type of boundary condition in the zone.
    FBoundaryType: TIproperty;
  protected
    // @name gets the values of each data set related to the zone and
    // assigns a value to a @link(ZoneUnit.TCustomProperty)
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); override;
  public
    { TODO : Replace these TLists with properties that restrict
      access to what is really needed. }
    // @name: TList;
    // @name is a list of @link(ZoneUnit.TIProperty)s
    // whose values represent the solution in the zone.
    FAssociatedSolutions: TList;
    // @name: TList;
    // @name is a list of @link(ZoneUnit.TRProperty)s
    // whose values represent the flux or leaky head in the zone.
    FBoundaries: TList;
    // @name creates an instance of @classname.
    constructor Create; override;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
  end;

  {@abstract(@name is used to determine a set of zones for
  leaky and flux boundary conditions.)}
  TCustomBoundaryGroup = class(TCustomZoneGroup)
  private
    // @name: @link(RealListUnit.TRealList);
    // See @link(BoundaryTimes).
    FBoundaryTimeList: TRealList;
    // @name: @link(RealListUnit.TRealList);
    // See @link(SolutionTimes).
    FSolutionTimeList: TRealList;
    // See @link(BoundaryTimeCount).
    function GetBoundaryTimeCount: integer;
    // See @link(BoundaryTimes).
    function GetBoundaryTimes(Index: integer): double;
    // See @link(SolutionTimeCount).
    function GetSolutionTimeCount: integer;
    // See @link(SolutionTimes).
    function GetSolutionTimes(Index: integer): double;
    // See @link(Zones).
    function GetZone(Index: integer): TBoundaryZone;
  protected
    // @name checks each of the zones that have been created
    // and deletes any that don't have values.  Thus, it
    // eliminates all the zones where the boundary condition is absent.
    procedure EliminateExtraZones; virtual;
    // @name identifies the type of boundary represented by this zone.
    function GetBoundaryType: integer; virtual; abstract;
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(Create).
    function GetDataSets: TBoundaryDataSetsType; virtual; abstract;
    // @name reads the times in the model for the flux boundary or leaky
    // boundary and stores them in @link(FBoundaryTimeList) and
    // @link(FSolutionTimeList).  See @link(BoundaryTimes)
    // and @link(SolutionTimes).
    procedure StoreTimes(const DataSets: TCustomBoundaryDataSets);
  public
    // @name gives the number of times
    // associated with the leaky or flux boundary.
    property BoundaryTimeCount: integer read GetBoundaryTimeCount;
    // @name gives the times
    // associated with the leaky or flux boundary.
    property BoundaryTimes[Index: integer]: double read GetBoundaryTimes;
    // @name creates an instance of @classname and creates the
    // @link(TBoundaryZone)s for it.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name gives the number of times
    // associated with the leaky or flux boundary associated solution.
    property SolutionTimeCount: integer read GetSolutionTimeCount;
    // @name gives the times
    // associated with the leaky or flux boundary associated solution.
    property SolutionTimes[Index: integer]: double read GetSolutionTimes;
    // @name allows access to the @link(TBoundaryZone)s for this @classname.
    // See @inherited @link(ZoneUnit.TCustomZoneGroup.ZoneCount).
    property Zones[Index: integer]: TBoundaryZone read GetZone;
  end;

implementation

uses Contnrs, frmGoPhastUnit;

{ TCustomBoundaryDataSets }

constructor TCustomBoundaryDataSets.Create;
begin
  inherited;
  FBoundaryDataType := BoundaryDataType;
  FBoundaryDataType.Initialize;
  FEvalAt := eaNodes;
  FZoneClass := TBoundaryZone;
  FBoundaryTimes := BoundaryTimes;
  FBoundaryTimes.Initialize;
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FAssociatedSolutionTimes := SolutionTimes;
    FAssociatedSolutionTimes.Initialize;
  end;
end;

{ TBoundaryZone }

procedure TBoundaryZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
  CanMergeZ: boolean);
var
  BoundaryDataSets: TCustomBoundaryDataSets;
  Index: integer;
  DataSet: TDataArray;
  RProperty: TRProperty;
  IProperty: TIProperty;
begin
  inherited;
  BoundaryDataSets := DataSets as TCustomBoundaryDataSets;
  for Index := 0 to BoundaryDataSets.FBoundaryTimes.Count - 1 do
  begin
    DataSet := BoundaryDataSets.FBoundaryTimes.Items[Index];
    if Index < FBoundaries.Count then
    begin
      RProperty := FBoundaries[Index];
    end
    else
    begin
      RProperty := TRProperty.Create;
      FBoundaries.Add(RProperty);
      FPropertyList.Add(RProperty);
    end;
    RProperty.Assign(DataSet, LayerIndex, RowIndex, ColIndex);
  end;
  FBoundaryType.Assign(BoundaryDataSets.FBoundaryDataType,
    LayerIndex, RowIndex, ColIndex);
  if BoundaryDataSets.FAssociatedSolutionTimes <> nil then
  begin
    for Index := 0 to BoundaryDataSets.FAssociatedSolutionTimes.Count - 1 do
    begin
      DataSet := BoundaryDataSets.FAssociatedSolutionTimes.Items[Index];
      if Index < FAssociatedSolutions.Count then
      begin
        IProperty := FAssociatedSolutions[Index];
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

constructor TBoundaryZone.Create;
begin
  inherited;
  FBoundaryType := TIproperty.Create;
  FPropertyList.Add(FBoundaryType);
  FBoundaries := TObjectList.Create;
  FAssociatedSolutions := TObjectList.Create;
end;

destructor TBoundaryZone.Destroy;
begin
  FBoundaryType.Free;
  FBoundaries.Free;
  FAssociatedSolutions.Free;
  inherited;
end;

{ TCustomBoundaryGroup }

constructor TCustomBoundaryGroup.Create;
var
  DataSets: TCustomBoundaryDataSets;
begin
  FBoundaryTimeList := TRealList.Create;
  FSolutionTimeList := TRealList.Create;
  DataSets := GetDataSets.Create;
  try
    inherited Create(DataSets);
    StoreTimes(DataSets);
  finally
    DataSets.Free;
  end;
  EliminateExtraZones;
end;

destructor TCustomBoundaryGroup.Destroy;
begin
  FBoundaryTimeList.Free;
  FSolutionTimeList.Free;
  inherited;
end;

procedure TCustomBoundaryGroup.EliminateExtraZones;
var
  Index: integer;
  Zone: TBoundaryZone;
  RProperty: TRProperty;
  IProperty: TIProperty;
  DeleteZone: boolean;
begin
  for Index := ZoneCount - 1 downto 0 do
  begin
    Zone := Zones[Index];
    DeleteZone := Zone.FBoundaryType.IValue <> GetBoundaryType;
    if (Zone.FBoundaries.Count = 0) then
    begin
      DeleteZone := true;
    end
    else if frmGoPhast.PhastModel.SoluteTransport
      and (Zone.FAssociatedSolutions.Count = 0) then
    begin
      DeleteZone := true;
    end
    else
    begin
      RProperty := Zone.FBoundaries[0];
      if not RProperty.IsValue then
      begin
        DeleteZone := True;
      end
      else if frmGoPhast.PhastModel.SoluteTransport then
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

function TCustomBoundaryGroup.GetBoundaryTimeCount: integer;
begin
  result := FBoundaryTimeList.Count;
end;

function TCustomBoundaryGroup.GetBoundaryTimes(Index: integer): double;
begin
  result := FBoundaryTimeList[Index];
end;

function TCustomBoundaryGroup.GetSolutionTimeCount: integer;
begin
  result := FSolutionTimeList.Count;
end;

function TCustomBoundaryGroup.GetSolutionTimes(
  Index: integer): double;
begin
  result := FSolutionTimeList[Index];
end;

function TCustomBoundaryGroup.GetZone(
  Index: integer): TBoundaryZone;
begin
  result := inherited Zones[Index] as TBoundaryZone;
end;

procedure TCustomBoundaryGroup.StoreTimes(
  const DataSets: TCustomBoundaryDataSets);
var
  Index: integer;
begin
  for Index := 0 to DataSets.FBoundaryTimes.Count - 1 do
  begin
    FBoundaryTimeList.Add(DataSets.FBoundaryTimes.Times[Index])
  end;
  if DataSets.FAssociatedSolutionTimes <> nil then
  begin
    for Index := 0 to DataSets.FAssociatedSolutionTimes.Count - 1 do
    begin
      FSolutionTimeList.Add(DataSets.FAssociatedSolutionTimes.Times[Index])
    end;
  end;
end;

end.
