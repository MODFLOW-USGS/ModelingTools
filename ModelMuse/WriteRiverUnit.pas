{@abstract(@name defines @link(TRiverWriter) which is used to help
  write data for Rivers in PHAST.)}
unit WriteRiverUnit;

interface

uses Classes, DataSetUnit, PhastDataSets;

type
  {@abstract(@name is used to help
  write data for Rivers in PHAST.)}
  TRiverWriter = class(TObject)
  private
    // See @link(AssociatedSolution).
    FAssociatedSolution: TPhastTimeList;
    // See @link(BedHydraulicConductivity).
    FBedHydraulicConductivity: TDataArray;
    // See @link(BedThickness).
    FBedThickness: TDataArray;
    // See @link(Depth).
    FDepth: TDataArray;
    // See @link(Head).
    FHead: TPhastTimeList;
    // See @link(Width).
    FWidth: TDataArray;
    FScreenObjectList: TList;
  public
    // @name is a list of @link(TScreenObject)s that
    // are valid river boundaries.
    property ScreenObjectList: TList read FScreenObjectList;
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the associated solution.
    // in the river.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.RiverAssociatedSolution).
    property AssociatedSolution: TPhastTimeList read FAssociatedSolution;
    // @name is the @link(DataSetUnit.TDataArray) that specifies the
    // bed hydraulic conductivity in the river.
    // It is set in @link(Create).
    property BedHydraulicConductivity: TDataArray read FBedHydraulicConductivity;
    // @name is the @link(DataSetUnit.TDataArray) that specifies the
    // bed thickness in the river.
    // It is set in @link(Create).
    property BedThickness: TDataArray read FBedThickness;
    // @name creates an instance of @classname, sets and initializes
    // the private variables and adds the@link(TScreenObject)s that
    // are valid river boundaries to @link(FScreenObjectList).
    constructor Create;
    // @name is the @link(DataSetUnit.TDataArray) that specifies the
    // depth of the river.
    // It is set in @link(Create).
    property Depth: TDataArray read FDepth;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name is the @link(PhastDataSets.TPhastTimeList) that has the
    // @link(DataSetUnit.TDataArray)s for the head.
    // in the river.
    // In @link(Create) it is set to
    // frmGoPhast.Model.@link(TPhastModel.RiverHead).
    property Head: TPhastTimeList read FHead;
    // @name is the @link(DataSetUnit.TDataArray) that specifies the
    // width of the river.
    // It is set in @link(Create).
    property Width: TDataArray read FWidth;
  end;

implementation

uses frmGoPhastUnit, PhastModelUnit, GoPhastTypes, ScreenObjectUnit,
  DataArrayManagerUnit, DataSetNamesUnit;

{ TRiverWriter }

constructor TRiverWriter.Create;
var
  Index: integer;
  AScreenObject: TScreenObject;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  FWidth := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsRiverWidth)];
  Width.Initialize;

  FDepth := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsRiverDepth)];
  Depth.Initialize;

  FBedHydraulicConductivity :=
    DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsRiverHydraulicConductivity)];
  BedHydraulicConductivity.Initialize;

  FBedThickness := DataArrayManager.BoundaryDataSets[
    DataArrayManager.IndexOfBoundaryDataSet(rsRiverBedThickness)];
  BedThickness.Initialize;

  FHead := frmGoPhast.PhastModel.RiverHead;
  Head.Initialize;
  if frmGoPhast.PhastModel.SoluteTransport then
  begin
    FAssociatedSolution := frmGoPhast.PhastModel.RiverAssociatedSolution;
    AssociatedSolution.Initialize;
  end;
  frmGoPhast.PhastModel.Top2DBoundaryType.Initialize;
  FScreenObjectList := TList.Create;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if (AScreenObject.ViewDirection = vdTop) and (AScreenObject.Count > 1)
      and (AScreenObject.Segments[frmGoPhast.PhastModel].Count > 0) and not AScreenObject.Closed
      and AScreenObject.RiverBoundary.IsBoundary then
    begin
      ScreenObjectList.Add(AScreenObject);
    end;
  end;
end;

destructor TRiverWriter.Destroy;
begin
  FScreenObjectList.Free;
  inherited;
end;


end.

