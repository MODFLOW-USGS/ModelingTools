unit framePackagePcgnUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ArgusDataEntry, Mask,
  JvExMask, JvSpin, ComCtrls, JvExStdCtrls, JvCombobox, JvListComb,
  ModflowPackageSelectionUnit, GrayTabs;

type
  TframePackagePcgn = class(TframePackage)
    pcControls: TPageControl;
    tabBasic: TTabSheet;
    seIter_mo: TJvSpinEdit;
    lblIter_mo: TLabel;
    lblIter_mi: TLabel;
    seIter_mi: TJvSpinEdit;
    rdeCLOSE_R: TRbwDataEntry;
    lblCLOSE_R: TLabel;
    rdeClose_H: TRbwDataEntry;
    lblClose_H: TLabel;
    rdeRelax: TRbwDataEntry;
    lblRelax: TLabel;
    seIfill: TJvSpinEdit;
    lblIfill: TLabel;
    cbUnit_pc: TCheckBox;
    cbUnit_ts: TCheckBox;
    tabNonLinear: TTabSheet;
    comboDampingMode: TJvImageComboBox;
    lblDampingMode: TLabel;
    rdeDamp: TRbwDataEntry;
    lblDamp: TLabel;
    rdeDamp_Lb: TRbwDataEntry;
    lblDamp_Lb: TLabel;
    rdeRate_D: TRbwDataEntry;
    lblDamp_D: TLabel;
    rdeChglimit: TRbwDataEntry;
    lblChglimit: TLabel;
    comboAcnvg: TJvImageComboBox;
    lblAcnvg: TLabel;
    rdeCnvg_Lb: TRbwDataEntry;
    lblChvg_Lb: TLabel;
    seMcnvg: TJvSpinEdit;
    lblMcnvg: TLabel;
    rdeRate_C: TRbwDataEntry;
    lblRate_C: TLabel;
    comboIpunit: TJvImageComboBox;
    lblIpunit: TLabel;
    procedure comboAcnvgChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure Loaded; override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackagePcgn: TframePackagePcgn;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

procedure TframePackagePcgn.comboAcnvgChange(Sender: TObject);
begin
  inherited;
  rdeCnvg_Lb.Enabled := rcSelectionController.Enabled
    and (comboAcnvg.ItemIndex = 1);
  seMcnvg.Enabled := rcSelectionController.Enabled
    and (comboAcnvg.ItemIndex = 2);
  rdeRate_C.Enabled := rcSelectionController.Enabled
    and (comboAcnvg.ItemIndex = 2);
end;

procedure TframePackagePcgn.GetData(Package: TModflowPackageSelection);
var
  PcgnPkg: TPcgnSelection;
begin
  inherited;
  PcgnPkg := Package as TPcgnSelection;
  seIter_mo.AsInteger := PcgnPkg.ITER_MO;
  seIter_mi.AsInteger := PcgnPkg.ITER_MI;
  rdeCLOSE_R.Text := FloatToStr(PcgnPkg.CLOSE_R.Value);
  rdeClose_H.Text := FloatToStr(PcgnPkg.CLOSE_H.Value);
  rdeRelax.Text := FloatToStr(PcgnPkg.RELAX.Value);
  seIfill.AsInteger := PcgnPkg.IFILL;
  cbUnit_pc.Checked := PcgnPkg.UNIT_PC;
  cbUnit_ts.Checked := PcgnPkg.UNIT_TS;

  comboDampingMode.ItemIndex := Ord(PcgnPkg.ADAMP);
  rdeDamp.Text := FloatToStr(PcgnPkg.DAMP.Value);
  rdeDamp_Lb.Text := FloatToStr(PcgnPkg.DAMP_LB.Value);
  rdeRate_D.Text := FloatToStr(PcgnPkg.RATE_D.Value);
  rdeChglimit.Text := FloatToStr(PcgnPkg.CHGLIMIT.Value);
  comboAcnvg.ItemIndex := Ord(PcgnPkg.ACNVG);
  rdeCnvg_Lb.Text := FloatToStr(PcgnPkg.CNVG_LB.Value);
  seMcnvg.AsInteger := PcgnPkg.MCNVG;
  rdeRate_C.Text := FloatToStr(PcgnPkg.RATE_C.Value);
  comboIpunit.ItemIndex := Ord(PcgnPkg.IPUNIT);

  lblPackage.Width := ClientWidth - lblPackage.Left - 8;
end;

procedure TframePackagePcgn.Loaded;
begin
  inherited;
  pcControls.ActivePageIndex := 0;
end;

procedure TframePackagePcgn.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  comboAcnvgChange(Sender);
end;

procedure TframePackagePcgn.SetData(Package: TModflowPackageSelection);
var
  PcgnPkg: TPcgnSelection;
begin
  inherited;
  PcgnPkg := Package as TPcgnSelection;
  PcgnPkg.ITER_MO := seIter_mo.AsInteger;
  PcgnPkg.ITER_MI := seIter_mi.AsInteger;
  PcgnPkg.CLOSE_R.Value := FortranStrToFloat(rdeCLOSE_R.Text);
  PcgnPkg.CLOSE_H.Value := FortranStrToFloat(rdeClose_H.Text);
  PcgnPkg.RELAX.Value := FortranStrToFloat(rdeRelax.Text);
  PcgnPkg.IFILL := seIfill.AsInteger;
  PcgnPkg.UNIT_PC := cbUnit_pc.Checked;
  PcgnPkg.UNIT_TS := cbUnit_ts.Checked;

  PcgnPkg.ADAMP := TDamping(comboDampingMode.ItemIndex);
  PcgnPkg.DAMP.Value := FortranStrToFloat(rdeDamp.Text);
  PcgnPkg.DAMP_LB.Value := FortranStrToFloat(rdeDamp_Lb.Text);
  PcgnPkg.RATE_D.Value := FortranStrToFloat(rdeRate_D.Text);
  PcgnPkg.CHGLIMIT.Value := FortranStrToFloat(rdeChglimit.Text);
  PcgnPkg.ACNVG := TConvergenceMode(comboAcnvg.ItemIndex);
  PcgnPkg.CNVG_LB.Value := FortranStrToFloat(rdeCnvg_Lb.Text);
  PcgnPkg.MCNVG := seMcnvg.AsInteger;
  PcgnPkg.RATE_C.Value := FortranStrToFloat(rdeRate_C.Text);
  PcgnPkg.IPUNIT := TProgressReporting(comboIpunit.ItemIndex);
end;

end.
