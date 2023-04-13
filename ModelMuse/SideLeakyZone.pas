{@abstract(@name is used to create classes to define zones of Leaky boundary
conditions on the side view of the model.  These classes are
@link(TSideLeakyDataSets) and @link(TSideLeakyZoneGroup).)}
unit SideLeakyZone;

interface

uses CustomLeakyZone, DataSetUnit, PhastDataSets, CustomBoundaryZone;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for side leaky boundary conditions.)
   Most of the work is done by @inherited and @link(TCustomBoundaryDataSets)
   so @classname only needs to override a few functions.}
  TSideLeakyDataSets = class(TCustomLeakyDataSets)
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // (frmGoPhast.Model.@link(TPhastModel.SideBoundaryType));
    function BoundaryDataType: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.SideLeakyHead));
    function BoundaryTimes: TPhastTimeList; override;
    // @name is used to set @link(TCustomLeakyDataSets.FHydraulicConductivity)
    // in @link(TCustomLeakyDataSets.Create).
    function HydraulicConductivityDataSet: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.SideLeakyAssociatedSolution));
    function SolutionTimes: TPhastTimeList; override;
    // @name is used to set @link(TCustomLeakyDataSets.FThickness)
    // in @link(TCustomLeakyDataSets.Create).
    // (frmGoPhast.Model.BoundaryDataSets[frmGoPhast.Model.
    // IndexOfBoundaryDataSet(rsSideLeakyThickness)]);
    function ThicknessDataSet: TDataArray; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   side leaky boundary conditions.)
   Most of the work is done by @inherited and @link(TCustomBoundaryDataSets)
   so @classname only needs to override a few functions.}
  TSideLeakyZoneGroup = class(TCustomLeakyZoneGroup)
  protected
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(TCustomBoundaryGroup.Create)
    // (@link(TSideLeakyDataSets)).
    function GetDataSets: TBoundaryDataSetsType; override;
    // @name returns @false.
    function GetMergeInXDirection: boolean; override;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, DataArrayManagerUnit, DataSetNamesUnit;

{ TSideLeakyDataSets }

function TSideLeakyDataSets.BoundaryDataType: TDataArray;
begin
  result := frmGoPhast.PhastModel.SideBoundaryType;
end;

function TSideLeakyDataSets.BoundaryTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.SideLeakyHead;
end;

function TSideLeakyDataSets.HydraulicConductivityDataSet: TDataArray;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  result := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsSideLeakyHydraulicConductivity)];
end;

function TSideLeakyDataSets.SolutionTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.SideLeakyAssociatedSolution;
end;

function TSideLeakyDataSets.ThicknessDataSet: TDataArray;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  result := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsSideLeakyThickness)];
end;

{ TSideLeakyZoneGroup }

function TSideLeakyZoneGroup.GetDataSets: TBoundaryDataSetsType;
begin
  result := TSideLeakyDataSets;
end;

function TSideLeakyZoneGroup.GetMergeInXDirection: boolean;
begin
  result := False;
end;

end.
