unit framePackageWellUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUseMultiplierUnit, RbwController,
  StdCtrls, ArgusDataEntry, ModflowPackageSelectionUnit;

type
  TframePackageWell = class(TframePackageUseMultiplier)
    rdePhiRamp: TRbwDataEntry;
    lblPhiRamp: TLabel;
    cbTabfiles: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageWell: TframePackageWell;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePackageWell }

procedure TframePackageWell.GetData(Package: TModflowPackageSelection);
var
  WellPackage: TWellPackage;
begin
  WellPackage := Package as TWellPackage;
  rdePhiRamp.Text := FloatToStr(WellPackage.PhiRamp);
//  cbSeepageLoss.Checked := WellPackage.LossFactorOption;
//  rdeLossAdjustmentFactor.RealValue := WellPackage.LossFactor;
  cbTabfiles.Checked := WellPackage.UseTabFiles;
  inherited;
end;

procedure TframePackageWell.SetData(Package: TModflowPackageSelection);
var
  WellPackage: TWellPackage;
begin
  WellPackage := Package as TWellPackage;
  WellPackage.PhiRamp := FortranStrToFloat(rdePhiRamp.Text);
//  WellPackage.LossFactorOption := cbSeepageLoss.Checked;
//  WellPackage.LossFactor := rdeLossAdjustmentFactor.RealValue;
  WellPackage.UseTabFiles := cbTabfiles.Checked;
  inherited;
end;

end.
