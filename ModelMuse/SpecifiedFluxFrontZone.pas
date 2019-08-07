{@abstract(@name is used to create classes to defined zones of
specified flux boundary
conditions on the front view of the model.  These classes are
@link(TSpecifiedFluxFrontDataSets) and @link(TSpecifiedFluxFrontGroup).)}

unit SpecifiedFluxFrontZone;

interface

uses ZoneUnit, CustomBoundaryZone, DataSetUnit, PhastDataSets;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for front flux boundary conditions.)
   Most of the work is done by @inherited so @classname only needs to override
   a few functions.}
  TSpecifiedFluxFrontDataSets = class(TCustomBoundaryDataSets)
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // (frmGoPhast.Model.@link(TPhastModel.FrontBoundaryType));
    function BoundaryDataType: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.FrontFluxBoundaryFlux));
    function BoundaryTimes: TPhastTimeList; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.FrontFluxBoundaryChemistry));
    function SolutionTimes: TPhastTimeList; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   front flux boundary conditions.)
   Most of the work is done by @inherited so @classname only needs to override
   a few functions.}
  TSpecifiedFluxFrontGroup = class(TCustomBoundaryGroup)
  protected
    // @name identifies the type of boundary represented by this zone.
    // (Ord(btFlux))
    function GetBoundaryType: integer; override;
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(TCustomBoundaryGroup.Create)
    // (@link(TSpecifiedFluxFrontDataSets)).
    function GetDataSets: TBoundaryDataSetsType; override;
    // @name returns @false.
    function GetMergeInYDirection: boolean; override;
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes;

{ TSpecifiedFluxFrontDataSets }

function TSpecifiedFluxFrontDataSets.BoundaryDataType: TDataArray;
begin
  result := frmGoPhast.PhastModel.FrontBoundaryType;
end;

function TSpecifiedFluxFrontDataSets.BoundaryTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.FrontFluxBoundaryFlux;
end;

function TSpecifiedFluxFrontDataSets.SolutionTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.FrontFluxBoundaryChemistry;
end;

{ TSpecifiedFluxFrontGroup }

function TSpecifiedFluxFrontGroup.GetBoundaryType: integer;
begin
  result := Ord(btFlux);
end;

function TSpecifiedFluxFrontGroup.GetDataSets: TBoundaryDataSetsType;
begin
  result := TSpecifiedFluxFrontDataSets;
end;

function TSpecifiedFluxFrontGroup.GetMergeInYDirection: boolean;
begin
  result := False;
end;

end.
