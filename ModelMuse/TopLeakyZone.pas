{@abstract(@name is used to create classes that define zones of Leaky boundary
conditions on the top view of the model. These classes are
@link(TTopLeakyDataSets) and @link(TTopLeakyZoneGroup).)}
unit TopLeakyZone;

interface

uses CustomLeakyZone, DataSetUnit, PhastDataSets, CustomBoundaryZone;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for top leaky boundary conditions.)
   Most of the work is done by @inherited and @link(TCustomBoundaryDataSets)
   so @classname only needs to override a few functions.}
  TTopLeakyDataSets = class(TCustomLeakyDataSets)
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // (frmGoPhast.Model.@link(TPhastModel.TopBoundaryType));
    function BoundaryDataType: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.TopLeakyHead));
    function BoundaryTimes: TPhastTimeList; override;
    // @name is used to set @link(TCustomLeakyDataSets.FHydraulicConductivity)
    // in @link(TCustomLeakyDataSets.Create).
    function HydraulicConductivityDataSet: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.TopLeakyAssociatedSolution));
    function SolutionTimes: TPhastTimeList; override;
    // @name is used to set @link(TCustomLeakyDataSets.FThickness)
    // in @link(TCustomLeakyDataSets.Create).
    // (frmGoPhast.Model.BoundaryDataSets[frmGoPhast.Model.
    // IndexOfBoundaryDataSet(rsTopLeakyThickness)]);
    function ThicknessDataSet: TDataArray; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   top leaky boundary conditions.)
   Most of the work is done by @inherited and @link(TCustomBoundaryDataSets)
   so @classname only needs to override a few functions.}
  TTopLeakyZoneGroup = class(TCustomLeakyZoneGroup)
  protected
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(TCustomBoundaryGroup.Create)
    // (@link(TTopLeakyDataSets)).
    function GetDataSets: TBoundaryDataSetsType; override;
    // @name returns @false.
    function GetMergeInZDirection: boolean; override;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, DataArrayManagerUnit, DataSetNamesUnit;

{ TTopLeakyDataSets }

function TTopLeakyDataSets.BoundaryDataType: TDataArray;
begin
  result := frmGoPhast.PhastModel.TopBoundaryType;
end;

function TTopLeakyDataSets.BoundaryTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.TopLeakyHead;
end;

function TTopLeakyDataSets.HydraulicConductivityDataSet: TDataArray;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  result := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsTopLeakyHydraulicConductivity)];
end;

function TTopLeakyDataSets.SolutionTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.TopLeakyAssociatedSolution;
end;

function TTopLeakyDataSets.ThicknessDataSet: TDataArray;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  result := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsTopLeakyThickness)];
end;

{ TTopLeakyZoneGroup }

function TTopLeakyZoneGroup.GetDataSets: TBoundaryDataSetsType;
begin
  result := TTopLeakyDataSets;
end;

function TTopLeakyZoneGroup.GetMergeInZDirection: boolean;
begin
  result := False;
end;

end.
