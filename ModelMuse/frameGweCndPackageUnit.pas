unit frameGweCndPackageUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, Vcl.StdCtrls,
  RbwController, Vcl.ExtCtrls, ModflowPackageSelectionUnit;

type
  TframeGweCndPackage = class(TframePackage)
    cbUseXT3D: TCheckBox;
    cbXT3D_RHS: TCheckBox;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure cbUseXT3DClick(Sender: TObject);
  private
    procedure Enable_XT3D_RHS;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameGweCndPackage: TframeGweCndPackage;

implementation

{$R *.dfm}

procedure TframeGweCndPackage.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  Enable_XT3D_RHS;
end;

procedure TframeGweCndPackage.SetData(Package: TModflowPackageSelection);
var
  Cnd: TGweConductionAndDispersionPackage;
begin
  inherited;
  Cnd := Package as TGweConductionAndDispersionPackage;

  Cnd.UseXt3d := cbUseXT3D.Checked;
  Cnd.Xt3dRightHandSide := cbXT3D_RHS.Checked;
end;

procedure TframeGweCndPackage.cbUseXT3DClick(Sender: TObject);
begin
  inherited;
  Enable_XT3D_RHS;
end;

procedure TframeGweCndPackage.Enable_XT3D_RHS;
begin
  cbXT3D_RHS.Enabled := rcSelectionController.Enabled and cbUseXT3D.Checked;
end;

procedure TframeGweCndPackage.GetData(Package: TModflowPackageSelection);
var
  Cnd: TGweConductionAndDispersionPackage;
begin
  inherited;
  Cnd := Package as TGweConductionAndDispersionPackage;

  cbUseXT3D.Checked := Cnd.UseXt3d;
  cbXT3D_RHS.Checked := Cnd.Xt3dRightHandSide;
end;

end.
