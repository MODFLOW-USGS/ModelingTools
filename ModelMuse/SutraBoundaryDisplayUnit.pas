unit SutraBoundaryDisplayUnit;

interface

uses
  DataSetUnit;

type
  TSutraBoundaryDisplayDataArray = class(TCustomBoundaryRealSparseDataSet)
  public
    procedure InitializeDisplayArray(DefaultValue: Double); override;
  end;


implementation

uses
  PhastModelUnit, GoPhastTypes, SutraMeshUnit;

{ TSutraBoundaryDisplayDataArray }

procedure TSutraBoundaryDisplayDataArray.InitializeDisplayArray(
  DefaultValue: Double);
begin
  // do nothing
end;

end.
