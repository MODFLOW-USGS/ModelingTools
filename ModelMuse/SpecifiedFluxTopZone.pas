{@abstract(@name is used to create classes to define zones of
specified flux boundary
conditions on the top view of the model.  These classes are
@link(TSpecifiedFluxTopDataSets) and @link(TSpecifiedFluxTopGroup).)}

unit SpecifiedFluxTopZone;

interface

uses ZoneUnit, CustomBoundaryZone, DataSetUnit, PhastDataSets;

type
  {@abstract(@name stores and initializes the @link(DataSetUnit.TDataArray)s
   used to create zones for top flux boundary conditions.)
   Most of the work is done by @inherited so @classname only needs to override
   a few functions.}
  TSpecifiedFluxTopDataSets = class(TCustomBoundaryDataSets)
  protected
    // @name returns the @link(DataSetUnit.TDataArray)
    // that identifies the type of boundary being evaluated.
    // (frmGoPhast.Model.@link(TPhastModel.TopBoundaryType));
    function BoundaryDataType: TDataArray; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the flux rate
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.TopFluxBoundaryFlux));
    function BoundaryTimes: TPhastTimeList; override;
    // @name returns the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the boundary condition.
    // (frmGoPhast.Model.@link(TPhastModel.TopFluxBoundaryChemistry));
    function SolutionTimes: TPhastTimeList; override;
  end;

  {@abstract(@name is used to determine a set of zones for
   top flux boundary conditions.)
   Most of the work is done by @inherited so @classname only needs to override
   a few functions.}
  TSpecifiedFluxTopGroup = class(TCustomBoundaryGroup)
  protected
    // @name identifies the type of boundary represented by this zone.
    // (Ord(btFlux))
    function GetBoundaryType: integer; override;
    // @name returns a TBoundaryDataSetsType which is used in
    // @link(TCustomBoundaryGroup.Create)
    // (@link(TSpecifiedFluxTopDataSets)).
    function GetDataSets: TBoundaryDataSetsType; override;
    // @name returns @false.
    function GetMergeInZDirection: boolean; override;
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes;

{ TSpecifiedFluxTopDataSets }

function TSpecifiedFluxTopDataSets.BoundaryDataType: TDataArray;
begin
  result := frmGoPhast.PhastModel.TopBoundaryType;
end;

function TSpecifiedFluxTopDataSets.BoundaryTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.TopFluxBoundaryFlux;
end;

function TSpecifiedFluxTopDataSets.SolutionTimes: TPhastTimeList;
begin
  result := frmGoPhast.PhastModel.TopFluxBoundaryChemistry;
end;

{ TSpecifiedFluxTopGroup }

function TSpecifiedFluxTopGroup.GetBoundaryType: integer;
begin
  result := Ord(btFlux);
end;

function TSpecifiedFluxTopGroup.GetDataSets: TBoundaryDataSetsType;
begin
  result := TSpecifiedFluxTopDataSets;
end;

function TSpecifiedFluxTopGroup.GetMergeInZDirection: boolean;
begin
  result := False;
end;

end.
