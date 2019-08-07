unit frameMt3dmsDispersionPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls,
  ModflowPackageSelectionUnit;

type
  TframeMt3dmsDispersionPkg = class(TframePackage)
    cbMultiDiffusion: TCheckBox;
    cbCrossTermsUsed: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dmsDispersionPkg: TframeMt3dmsDispersionPkg;

implementation

{$R *.dfm}

{ TframeMt3dmsDispersionPkg }

procedure TframeMt3dmsDispersionPkg.GetData(Package: TModflowPackageSelection);
var
  Disp: TMt3dmsDispersion;
begin
  inherited;
  Disp := Package as TMt3dmsDispersion;
  cbMultiDiffusion.Checked := Disp.MultiDifussion;
  cbCrossTermsUsed.Checked := Disp.CrossTermsUsed;
end;

procedure TframeMt3dmsDispersionPkg.SetData(Package: TModflowPackageSelection);
var
  Disp: TMt3dmsDispersion;
begin
  inherited;
  Disp := Package as TMt3dmsDispersion;
  Disp.MultiDifussion := cbMultiDiffusion.Checked;
  Disp.CrossTermsUsed := cbCrossTermsUsed.Checked;
end;

end.
