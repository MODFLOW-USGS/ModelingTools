{@abstract(@name is used to create classes to define zones of Leaky boundary
conditions on the front view of the model.  These classes are
@link(TFrontLeakyDataSets) and @link(TFrontLeakyZoneGroup). )}
unit FrontLeakyZone;

interface

uses CustomLeakyZone, DataSetUnit, PhastDataSets, CustomBoundaryZone;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for front leaky boundary conditions.)
   Most of the work is done by @inherited and @link(TCustomBoundaryDataSets)
   so @classname only needs to override a few functions.}
  TFrontLeakyDataSets = class(TCustomLeakyDataSets)
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // (frmGoPhast.Model.@link(TPhastModel.FrontBoundaryType));
    function BoundaryDataType: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.FrontLeakyHead));
    function BoundaryTimes: TPhastTimeList; override;
    // @name is used to set @link(TCustomLeakyDataSets.FHydraulicConductivity)
    // in @link(TCustomLeakyDataSets.Create).
    function HydraulicConductivityDataSet: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.FrontLeakyAssociatedSolution));
    function SolutionTimes: TPhastTimeList; override;
    // @name is used to set @link(TCustomLeakyDataSets.FThickness)
    // in @link(TCustomLeakyDataSets.Create).
    // (frmGoPhast.Model.BoundaryDataSets[frmGoPhast.Model.
    // IndexOfBoundaryDataSet(rsFrontLeakyThickness)]);
    function ThicknessDataSet: TDataArray; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   front leaky boundary conditions.)
   Most of the work is done by @inherited and @link(TCustomBoundaryDataSets)
   so @classname only needs to override a few functions.}
  TFrontLeakyZoneGroup = class(TCustomLeakyZoneGroup)
  protected
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(TCustomBoundaryGroup.Create)
    // (@link(TFrontLeakyDataSets)).
    function GetDataSets: TBoundaryDataSetsType; override;
    // @name returns @false.
    function GetMergeInYDirection: boolean; override;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, DataArrayManagerUnit, DataSetNamesUnit;

{ TFrontLeakyDataSets }

function TFrontLeakyDataSets.BoundaryDataType: TDataArray;
begin
  result := frmGoPhast.PhastModel.FrontBoundaryType;
end;

function TFrontLeakyDataSets.BoundaryTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.FrontLeakyHead;
end;

function TFrontLeakyDataSets.HydraulicConductivityDataSet: TDataArray;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  result := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsFrontLeakyHydraulicConductivity)];
end;

function TFrontLeakyDataSets.SolutionTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.FrontLeakyAssociatedSolution;
end;

function TFrontLeakyDataSets.ThicknessDataSet: TDataArray;
var
  DataArrayManager: TDataArrayManager;
begin
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  result := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsFrontLeakyThickness)];
end;

{ TFrontLeakyZoneGroup }

function TFrontLeakyZoneGroup.GetDataSets: TBoundaryDataSetsType;
begin
  result := TFrontLeakyDataSets;
end;

function TFrontLeakyZoneGroup.GetMergeInYDirection: boolean;
begin
  result := False;
end;

end.
