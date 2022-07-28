unit ModflowCncWriterUnit;

interface

uses SysUtils, Classes, Contnrs, CustomModflowWriterUnit, ModflowDrnUnit,
  PhastModelUnit, ScreenObjectUnit, ModflowBoundaryUnit, ModflowCellUnit,
  ModflowPackageSelectionUnit, OrderedCollectionUnit, FluxObservationUnit,
  GoPhastTypes, Modflow6ObsUnit;

type
  TModflowCncWriter = class(TCustomTransientWriter)
  private
    procedure PrintOptions;
    procedure PrintDimensions;
    procedure PrintStressPeriods;
  public
    procedure WriteFile(const AFileName: string);
  end;

implementation

end.
