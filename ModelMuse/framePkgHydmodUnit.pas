unit framePkgHydmodUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, StdCtrls, ArgusDataEntry, RbwController,
  ModflowPackageSelectionUnit;

type
  TframePkgHydmod = class(TframePackage)
    lblHYDNOH: TLabel;
    rdeHYDNOH: TRbwDataEntry;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePkgHydmod: TframePkgHydmod;

implementation

{$R *.dfm}

{ TframePkgHydmod }

procedure TframePkgHydmod.GetData(Package: TModflowPackageSelection);
var
  HydPackage: THydPackageSelection;
begin
  inherited;
  HydPackage := Package as THydPackageSelection;
  rdeHYDNOH.Text := FloatToStr(HydPackage.HYDNOH);
end;

procedure TframePkgHydmod.SetData(Package: TModflowPackageSelection);
var
  HydPackage: THydPackageSelection;
begin
  inherited;
  HydPackage := Package as THydPackageSelection;
  HydPackage.HYDNOH := StrToFloat(rdeHYDNOH.Text);
end;

end.
