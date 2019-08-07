{@abstract(@name defines @link(TWellWriter) which is used to help
  write data for Wells in PHAST.)}
unit WriteWellUnit;

interface

uses Classes, DataSetUnit, PhastDataSets;

type
  {@abstract(@name is used to help
  write data for Wells in PHAST.)}
  TWellWriter = class(TObject)
  private
    // See @link(PumpingRate).
    FPumpingRate: TPhastTimeList;
    // See @link(Solution).
    FSolution: TPhastTimeList;
    FScreenObjectList: TList;
  public
    // @name is a list of @link(TScreenObject)s that
    // are valid well boundaries.
    property ScreenObjectList: TList read FScreenObjectList;
    // @name creates an instance of @classname, sets and initializes
    // the private variables and adds the@link(TScreenObject)s that
    // are valid well boundaries to @link(FScreenObjectList).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the pumping rate.
    // in the well.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.WellInjectionOrPumpingRate).
    property PumpingRate: TPhastTimeList read FPumpingRate;
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the solution.
    // in the well.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.WellSolution).
    property Solution: TPhastTimeList read FSolution;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, GoPhastTypes, ScreenObjectUnit;

{ TWellWriter }

constructor TWellWriter.Create;
var
  Index: integer;
  AScreenObject: TScreenObject;
begin
  inherited;

  FPumpingRate := frmGoPhast.PhastModel.WellInjectionOrPumpingRate;
  PumpingRate.Initialize;
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FSolution := frmGoPhast.PhastModel.WellSolution;
    Solution.Initialize;
  end;
  frmGoPhast.PhastModel.Top2DBoundaryType.Initialize;
  FScreenObjectList := TList.Create;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if (AScreenObject.ViewDirection = vdTop) and (AScreenObject.Count = 1)
      and (AScreenObject.Segments[frmGoPhast.PhastModel].Count > 0)
      and AScreenObject.WellBoundary.IsBoundary then
    begin
      ScreenObjectList.Add(AScreenObject);
    end;
  end;
end;

destructor TWellWriter.Destroy;
begin
  FScreenObjectList.Free;
  inherited;
end;

end.

