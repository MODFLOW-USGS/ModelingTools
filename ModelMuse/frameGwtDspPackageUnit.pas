unit frameGwtDspPackageUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, Vcl.StdCtrls,
  RbwController, Vcl.ExtCtrls, ModflowPackageSelectionUnit;

type
  TframeGwtDspPackage = class(TframePackage)
    cbUseXT3D: TCheckBox;
    cbXT3D_RHS: TCheckBox;
    rgLongDisp: TRadioGroup;
    rgTransDisp: TRadioGroup;
    cbVertFlowTransDisp: TCheckBox;
    cbSeparateDataSets: TCheckBox;
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
  frameGwtDspPackage: TframeGwtDspPackage;

implementation

{$R *.dfm}

procedure TframeGwtDspPackage.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  Enable_XT3D_RHS;
end;

procedure TframeGwtDspPackage.SetData(Package: TModflowPackageSelection);
var
  Disp: TGwtDispersionPackage;
begin
  inherited;
  Disp := Package as TGwtDispersionPackage;

  Disp.UseXt3d := cbUseXT3D.Checked;
  Disp.Xt3dRightHandSide := cbXT3D_RHS.Checked;
  Disp.LongitudinalDispTreatement := TDispersivityTreatment(rgLongDisp.ItemIndex);
  Disp.TransverseDispTreatement := TDispersivityTreatment(rgTransDisp.ItemIndex);
  Disp.UseTransverseDispForVertFlow := cbVertFlowTransDisp.Checked;
  Disp.SeparateDataSetsForEachSpecies := TDispersivityTreatment(cbSeparateDataSets.Checked);
end;

procedure TframeGwtDspPackage.cbUseXT3DClick(Sender: TObject);
begin
  inherited;
  Enable_XT3D_RHS;
end;

procedure TframeGwtDspPackage.Enable_XT3D_RHS;
begin
  cbXT3D_RHS.Enabled := rcSelectionController.Enabled and cbUseXT3D.Checked;
end;

procedure TframeGwtDspPackage.GetData(Package: TModflowPackageSelection);
var
  Disp: TGwtDispersionPackage;
begin
  inherited;
  Disp := Package as TGwtDispersionPackage;

  cbUseXT3D.Checked := Disp.UseXt3d;
  cbXT3D_RHS.Checked := Disp.Xt3dRightHandSide;
  rgLongDisp.ItemIndex := Ord(Disp.LongitudinalDispTreatement);
  rgTransDisp.ItemIndex := Ord(Disp.TransverseDispTreatement);
  cbVertFlowTransDisp.Checked := Disp.UseTransverseDispForVertFlow;
  cbSeparateDataSets.Checked := Disp.SeparateDataSetsForEachSpecies = dtSeparate;
end;

end.
