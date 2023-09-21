unit ModflowTvkWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowBoundaryDisplayUnit,
  ModflowPackageSelectionUnit;

type
  TModflowTvk_Writer = class(TCustomPackageWriter)
  protected
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);

  end;

implementation

{ TModflowTvk_Writer }

class function TModflowTvk_Writer.Extension: string;
begin
  result := '.tvk';
end;

function TModflowTvk_Writer.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.TvkPackage;
end;

procedure TModflowTvk_Writer.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
begin

end;

end.
