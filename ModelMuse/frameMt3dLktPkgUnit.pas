unit frameMt3dLktPkgUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframeMt3dLktPkg = class(TframePackage)
    cbSoluteEvap: TCheckBox;
    cbPrintLakeBudget: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dLktPkg: TframeMt3dLktPkg;

implementation

{$R *.dfm}

{ TframeMt3dLktPkg }

procedure TframeMt3dLktPkg.GetData(Package: TModflowPackageSelection);
var
  LktPackage: TMt3dLktPackage;
begin
  inherited;
  LktPackage := (Package as TMt3dLktPackage);
  cbSoluteEvap.Checked := LktPackage.EvaporateMass;
  cbPrintLakeBudget.Checked := LktPackage.PrintLakeTransportBudget;
end;

procedure TframeMt3dLktPkg.SetData(Package: TModflowPackageSelection);
var
  LktPackage: TMt3dLktPackage;
begin
  inherited;
  LktPackage := (Package as TMt3dLktPackage);
  LktPackage.EvaporateMass := cbSoluteEvap.Checked;
  LktPackage.PrintLakeTransportBudget := cbPrintLakeBudget.Checked;
end;

end.
