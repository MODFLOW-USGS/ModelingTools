unit framePackageNwtUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ArgusDataEntry, Mask,
  JvExMask, JvSpin, JvExStdCtrls, JvCombobox, JvListComb, ComCtrls,
  ModflowPackageSelectionUnit, GrayTabs;

type
  TframePackageNwt = class(TframePackage)
    pcNWT: TPageControl;
    tabBasic: TTabSheet;
    rdeHeadTolerance: TRbwDataEntry;
    rdeFluxTolerance: TRbwDataEntry;
    spinMaxOuterIt: TJvSpinEdit;
    rdeThicknessFactor: TRbwDataEntry;
    comboSolverMethod: TJvImageComboBox;
    lblSolverMethod: TLabel;
    lblThicknessFactor: TLabel;
    lblMaxOuterIt: TLabel;
    lblFluxTolerance: TLabel;
    lblHeadTolerance: TLabel;
    cbPrintFlag: TCheckBox;
    cbCorrectForCellBottom: TCheckBox;
    comboOptions: TJvImageComboBox;
    lblOptions: TLabel;
    tabAdditional: TTabSheet;
    rdeDbdTheta: TRbwDataEntry;
    lblDbdTheta: TLabel;
    lblDbdKappa: TLabel;
    rdeDbdKappa: TRbwDataEntry;
    rdeDbdGamma: TRbwDataEntry;
    lblDbdGamma: TLabel;
    rdeMomentumCoefficient: TRbwDataEntry;
    lblMomentumCoefficient: TLabel;
    cbUseResidualControl: TCheckBox;
    seMaxReductions: TJvSpinEdit;
    Label4: TLabel;
    rdeBackTol: TRbwDataEntry;
    lblBackTol: TLabel;
    rdeReductionFactor: TRbwDataEntry;
    lblReductionFactor: TLabel;
    tabGmresVariables: TTabSheet;
    seMaxIterationsGmres: TJvSpinEdit;
    lblMaxIterationsGmres: TLabel;
    comboIluMethod: TJvImageComboBox;
    lblIluMethod: TLabel;
    lblFillLimit1: TLabel;
    seFillLimit1: TJvSpinEdit;
    seFillLimit2: TJvSpinEdit;
    lblFillLimit2: TLabel;
    rdeTolerance: TRbwDataEntry;
    lblTolerance: TLabel;
    seRestarts: TJvSpinEdit;
    lblRestarts: TLabel;
    TabChi_MD_Variables: TTabSheet;
    comboAccelMethod: TJvImageComboBox;
    lblAccelMethod: TLabel;
    comboOrderingScheme: TJvImageComboBox;
    lblOrderingScheme: TLabel;
    seFillLevel: TJvSpinEdit;
    lblFillLevel: TLabel;
    lblNumOrtho: TLabel;
    seNumOrtho: TJvSpinEdit;
    cbApplyReducedPreconditioning: TCheckBox;
    rdeResRedCrit: TRbwDataEntry;
    lblResRedCrit: TLabel;
    cbUseDropTolerance: TCheckBox;
    rdeDropTolerance: TRbwDataEntry;
    lblDropTolerance: TLabel;
    rdeHeadClosure: TRbwDataEntry;
    lblHeadClosure: TLabel;
    seMaxIterChimd: TJvSpinEdit;
    lblMaxIterChimd: TLabel;
    cbContinue: TCheckBox;
    procedure comboOptionsChange(Sender: TObject);
    procedure comboSolverMethodChange(Sender: TObject);
    procedure comboIluMethodChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    procedure EnableTabs;
    procedure EnableFillLevelControls;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageNwt: TframePackageNwt;

implementation

uses
  ModelMuseUtilities, frmGoPhastUnit, ModflowNWT_WriterUnit;

{$R *.dfm}

{ TframePackageNwt }

procedure TframePackageNwt.comboIluMethodChange(Sender: TObject);
begin
  inherited;
  EnableFillLevelControls;
end;

procedure TframePackageNwt.comboOptionsChange(Sender: TObject);
begin
  inherited;
  EnableTabs;
end;

procedure TframePackageNwt.comboSolverMethodChange(Sender: TObject);
begin
  inherited;
  EnableTabs;
  if frmGoPhast.PhastModel.LgrUsed and (comboSolverMethod.ItemIndex = 1) then
  begin
    Beep;
    MessageDlg(StrTheΧMDInTheNWTS, mtError, [mbOK], 0);
  end;
end;

procedure TframePackageNwt.EnableFillLevelControls;
begin
  seFillLimit1.Enabled := rcSelectionController.Enabled and
    (TNewtonIluMethod(comboIluMethod.ItemIndex) = nimDropTol);
  seFillLimit2.Enabled := rcSelectionController.Enabled and
    (TNewtonIluMethod(comboIluMethod.ItemIndex) = nimKOrder);
end;

procedure TframePackageNwt.EnableTabs;
begin
  tabAdditional.TabVisible := TNewtonOption(comboOptions.ItemIndex)= noSpecified;
  tabGmresVariables.TabVisible := tabAdditional.TabVisible
    and (TNewtonSolverMethod(comboSolverMethod.ItemIndex) = nsmGmres);
  TabChi_MD_Variables.TabVisible := tabAdditional.TabVisible
    and (TNewtonSolverMethod(comboSolverMethod.ItemIndex) = nsmChiMD);
end;

procedure TframePackageNwt.GetData(Package: TModflowPackageSelection);
var
  NwtPackage: TNwtPackageSelection;
begin
  inherited;
  pcNWT.ActivePageIndex := 0;
  NwtPackage := Package as TNwtPackageSelection;
  rdeHeadTolerance.Text := FloatToStr(NwtPackage.HeadTolerance.Value);
  rdeFluxTolerance.Text := FloatToStr(NwtPackage.FluxTolerance.Value);
  spinMaxOuterIt.AsInteger := NwtPackage.MaxOuterIterations;
  rdeThicknessFactor.Text := FloatToStr(NwtPackage.ThicknessFactor.Value);
  comboSolverMethod.ItemIndex := Ord(NwtPackage.SolverMethod);
  cbPrintFlag.Checked := (NwtPackage.PrintFlag <> 0);
  cbCorrectForCellBottom.Checked := (NwtPackage.CorrectForCellBottom <> 0);
  comboOptions.ItemIndex := Ord(NwtPackage.Option);

  rdeDbdTheta.Text := FloatToStr(NwtPackage.DBDTheta.Value);
  rdeDbdKappa.Text := FloatToStr(NwtPackage.DBDKappa.Value);
  rdeDbdGamma.Text := FloatToStr(NwtPackage.DBDGamma.Value);
  rdeMomentumCoefficient.Text := FloatToStr(NwtPackage.MomementumCoefficient.Value);
  cbUseResidualControl.Checked := (NwtPackage.BackFlag <> 0);
  seMaxReductions.AsInteger := NwtPackage.MaxBackIterations;
  rdeBackTol.Text := FloatToStr(NwtPackage.BackTol.Value);
  rdeReductionFactor.Text := FloatToStr(NwtPackage.BackReduce.Value);

  seMaxIterationsGmres.AsInteger := NwtPackage.MaxIterInner;
  comboIluMethod.ItemIndex := Ord(NwtPackage.IluMethod);
  seFillLimit1.AsInteger := NwtPackage.FillLimit;
  seFillLimit2.AsInteger := NwtPackage.FillLevel;
  rdeTolerance.Text := FloatToStr(NwtPackage.StopTolerance.Value);
  seRestarts.AsInteger := NwtPackage.MaxGmresRestarts;

  comboAccelMethod.ItemIndex := Ord(NwtPackage.AccelMethod);
  comboOrderingScheme.ItemIndex := Ord(NwtPackage.OrderingMethod);
  seFillLevel.AsInteger := NwtPackage.Level;
  seNumOrtho.AsInteger := NwtPackage.NumberOfOrthogonalizations;
  cbApplyReducedPreconditioning.Checked := (NwtPackage.ApplyReducedPrecondition = narpApply);
  rdeResRedCrit.Text := FloatToStr(NwtPackage.ResidReducConv.Value);
  cbUseDropTolerance.Checked := (NwtPackage.UseDropTolerance = nudtUse);
  rdeDropTolerance.Text := FloatToStr(NwtPackage.DropTolerancePreconditioning.Value);
  rdeHeadClosure.Text := FloatToStr(NwtPackage.InnerHeadClosureCriterion.Value);
  seMaxIterChimd.AsInteger := NwtPackage.MaxInnerIterations;
  cbContinue.Checked := NwtPackage.ContinueNWT;

  EnableTabs;
  EnableFillLevelControls;
end;

procedure TframePackageNwt.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableFillLevelControls;
end;

procedure TframePackageNwt.SetData(Package: TModflowPackageSelection);
var
  NwtPackage: TNwtPackageSelection;
begin
  inherited;
  if frmGoPhast.PhastModel.LgrUsed and (comboSolverMethod.ItemIndex = 1) then
  begin
    Beep;
    MessageDlg(StrTheΧMDInTheNWTS, mtError, [mbOK], 0);
  end;
  NwtPackage := Package as TNwtPackageSelection;
  NwtPackage.HeadTolerance.Value := FortranStrToFloat(rdeHeadTolerance.Text);
  NwtPackage.FluxTolerance.Value := FortranStrToFloat(rdeFluxTolerance.Text);
  NwtPackage.MaxOuterIterations := spinMaxOuterIt.AsInteger;
  NwtPackage.ThicknessFactor.Value := FortranStrToFloat(rdeThicknessFactor.Text);
  NwtPackage.SolverMethod := TNewtonSolverMethod(comboSolverMethod.ItemIndex);
  NwtPackage.PrintFlag := Ord(cbPrintFlag.Checked);
  NwtPackage.CorrectForCellBottom := Ord(cbCorrectForCellBottom.Checked);
  NwtPackage.Option := TNewtonOption(comboOptions.ItemIndex);

  NwtPackage.DBDTheta.Value := FortranStrToFloat(rdeDbdTheta.Text);
  NwtPackage.DBDKappa.Value := FortranStrToFloat(rdeDbdKappa.Text);
  NwtPackage.DBDGamma.Value := FortranStrToFloat(rdeDbdGamma.Text);
  NwtPackage.MomementumCoefficient.Value := FortranStrToFloat(rdeMomentumCoefficient.Text);
  NwtPackage.BackFlag := Ord(cbUseResidualControl.Checked);
  NwtPackage.MaxBackIterations := seMaxReductions.AsInteger;
  NwtPackage.BackTol.Value := FortranStrToFloat(rdeBackTol.Text);
  NwtPackage.BackReduce.Value := FortranStrToFloat(rdeReductionFactor.Text);

  NwtPackage.MaxIterInner := seMaxIterationsGmres.AsInteger;
  NwtPackage.IluMethod := TNewtonIluMethod(comboIluMethod.ItemIndex);
  NwtPackage.FillLimit := seFillLimit1.AsInteger;
  NwtPackage.FillLevel := seFillLimit2.AsInteger;
  NwtPackage.StopTolerance.Value := FortranStrToFloat(rdeTolerance.Text);
  NwtPackage.MaxGmresRestarts := seRestarts.AsInteger;

  NwtPackage.AccelMethod := TNewtonAccelMethod(comboAccelMethod.ItemIndex);
  NwtPackage.OrderingMethod := TNewtonOrderingMethod(comboOrderingScheme.ItemIndex);
  NwtPackage.Level := seFillLevel.AsInteger;
  NwtPackage.NumberOfOrthogonalizations := seNumOrtho.AsInteger;
  NwtPackage.ApplyReducedPrecondition := TNewtonApplyReducedPrecondition(cbApplyReducedPreconditioning.Checked);
  NwtPackage.ResidReducConv.Value := FortranStrToFloat(rdeResRedCrit.Text);
  NwtPackage.UseDropTolerance := TNewtonUseDropTolerance(cbUseDropTolerance.Checked);
  NwtPackage.DropTolerancePreconditioning.Value := FortranStrToFloat(rdeDropTolerance.Text);
  NwtPackage.InnerHeadClosureCriterion.Value := FortranStrToFloat(rdeHeadClosure.Text);
  NwtPackage.MaxInnerIterations := seMaxIterChimd.AsInteger;
  NwtPackage.ContinueNWT := cbContinue.Checked;
end;

end.
