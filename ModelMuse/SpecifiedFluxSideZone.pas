{@abstract(@name is used to create classes to define zones of
specified flux boundary
conditions on the side view of the model.  These classes are
@link(TSpecifiedFluxSideDataSets) and @link(TSpecifiedFluxSideGroup).)}

unit SpecifiedFluxSideZone;

interface

uses ZoneUnit, CustomBoundaryZone, DataSetUnit, PhastDataSets;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for side flux boundary conditions.)
   Most of the work is done by @inherited so @classname only needs to override
   a few functions.}
  TSpecifiedFluxSideDataSets = class(TCustomBoundaryDataSets)
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // (frmGoPhast.Model.@link(TPhastModel.SideBoundaryType));
    function BoundaryDataType: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.SideFluxBoundaryFlux));
    function BoundaryTimes: TPhastTimeList; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.SideFluxBoundaryChemistry));
    function SolutionTimes: TPhastTimeList; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   side flux boundary conditions.)
   Most of the work is done by @inherited so @classname only needs to override
   a few functions.}
  TSpecifiedFluxSideGroup = class(TCustomBoundaryGroup)
  protected
    // @name identifies the type of boundary represented by this zone.
    // (Ord(btFlux))
    function GetBoundaryType: integer; override;
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(TCustomBoundaryGroup.Create)
    // (@link(TSpecifiedFluxSideDataSets)).
    function GetDataSets: TBoundaryDataSetsType; override;
    // @name returns @false.
    function GetMergeInXDirection: boolean; override;
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes;

{ TSpecifiedFluxSideDataSets }

function TSpecifiedFluxSideDataSets.BoundaryDataType: TDataArray;
begin
  result := frmGoPhast.PhastModel.SideBoundaryType;
end;

function TSpecifiedFluxSideDataSets.BoundaryTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.SideFluxBoundaryFlux;
end;

function TSpecifiedFluxSideDataSets.SolutionTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.SideFluxBoundaryChemistry;
end;

{ TSpecifiedFluxSideGroup }

function TSpecifiedFluxSideGroup.GetBoundaryType: integer;
begin
  result := Ord(btFlux);
end;

function TSpecifiedFluxSideGroup.GetDataSets: TBoundaryDataSetsType;
begin
  result := TSpecifiedFluxSideDataSets;
end;

function TSpecifiedFluxSideGroup.GetMergeInXDirection: boolean;
begin
  result := False;
end;

end.
