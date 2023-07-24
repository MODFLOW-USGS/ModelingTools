unit framePackageCFPUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, RbwController,
  StdCtrls, ArgusDataEntry, JvExStdCtrls, JvCombobox, JvListComb, Mask,
  JvExMask, JvSpin, ModflowPackageSelectionUnit, ComCtrls, GrayTabs;

type
  TframePackageCFP = class(TframePackage)
    pgcConduits: TPageControl;
    tabCFP: TTabSheet;
    tabCRCH_COC: TTabSheet;
    cbPipes: TCheckBox;
    rdeLayerTemperature: TRbwDataEntry;
    cbPrintIterations: TCheckBox;
    rdeRelaxationParameter: TRbwDataEntry;
    seMaxIterations: TJvSpinEdit;
    rdeEpsilon: TRbwDataEntry;
    comboPipeExchange: TJvImageComboBox;
    rdePipeElevationOffset: TRbwDataEntry;
    comboElevationChoice: TJvImageComboBox;
    rdeConduitTemperature: TRbwDataEntry;
    cbLayers: TCheckBox;
    lblLayerTemperature: TLabel;
    lblRelaxationParameter: TLabel;
    lblMaxIterations: TLabel;
    lblEpsilon: TLabel;
    lblPipeExchange: TLabel;
    lblPipeElevationOffset: TLabel;
    lblElevationChoice: TLabel;
    lblConduitTemperature: TLabel;
    cbConduitRecharge: TCheckBox;
    seOutputInterval: TJvSpinEdit;
    lblOutputInterval: TLabel;
    cpCADS: TCheckBox;
    procedure cbPipesClick(Sender: TObject);
    procedure cbLayersClick(Sender: TObject);
    procedure comboElevationChoiceChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    procedure EmphasizeModeCheckBoxes;
    procedure EnablePipeElevationOffset;
    procedure EnablePipeControls;
    procedure EnableLayerTemperature;
    procedure EnableCads(PipesUsed: Boolean);
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageCFP: TframePackageCFP;

implementation

uses
  frmCustomGoPhastUnit, frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

{ TframePackageCFP }

procedure TframePackageCFP.cbLayersClick(Sender: TObject);
begin
  inherited;
  EmphasizeModeCheckBoxes;
  EnableLayerTemperature;
end;

procedure TframePackageCFP.cbPipesClick(Sender: TObject);
begin
  inherited;
  EmphasizeModeCheckBoxes;
  EnablePipeControls;
end;

procedure TframePackageCFP.comboElevationChoiceChange(Sender: TObject);
begin
  inherited;
  EnablePipeElevationOffset;
end;

procedure TframePackageCFP.EmphasizeModeCheckBoxes;
begin
  if Selected then
  begin
    EmphasizeCheckBoxes([cbPipes, cbLayers]);
  end
  else
  begin
    cbPipes.ParentFont := True;
    cbLayers.ParentFont := True;
  end;
end;

procedure TframePackageCFP.EnablePipeElevationOffset;
begin
  rdePipeElevationOffset.Enabled := rcSelectionController.Enabled
    and cbPipes.Checked and (comboElevationChoice.ItemIndex = 1);
end;

procedure TframePackageCFP.GetData(Package: TModflowPackageSelection);
var
  CfpPackage: TConduitFlowProcess;
  PipesUsed: Boolean;
begin
  inherited;
  pgcConduits.ActivePageIndex := 0;

  CfpPackage := Package as TConduitFlowProcess;
  cbPipes.Checked := CfpPackage.PipesUsed;
  cbLayers.Checked := CfpPackage.ConduitLayersUsed;
  rdeConduitTemperature.RealValue := CfpPackage.ConduitTemperature;
  comboElevationChoice.ItemIndex := Ord(CfpPackage.CfpElevationChoice);
  rdePipeElevationOffset.RealValue := CfpPackage.ElevationOffset;
  comboPipeExchange.ItemIndex := Ord(CfpPackage.CfpExchange);
  rdeEpsilon.RealValue := CfpPackage.Epsilon;
  seMaxIterations.AsInteger := CfpPackage.MaxIterations;
  rdeRelaxationParameter.RealValue := CfpPackage.Relax;
  cbPrintIterations.Checked := CfpPackage.CfpPrintIterations = cpiPrint;
  rdeLayerTemperature.RealValue := CfpPackage.LayerTemperature;
  cbConduitRecharge.Checked := CfpPackage.ConduitRechargeUsed;
  seOutputInterval.AsInteger := CfpPackage.OutputInterval;
  cpCADS.Checked := CfpPackage.UseCads;

  PipesUsed := cbPipes.Checked and rcSelectionController.Enabled;
  EnableCads(PipesUsed);
end;

procedure TframePackageCFP.EnablePipeControls;
var
  PipesUsed: Boolean;
begin
  PipesUsed := cbPipes.Checked and rcSelectionController.Enabled;
  rdeConduitTemperature.Enabled := PipesUsed;
  comboElevationChoice.Enabled := PipesUsed;
  comboPipeExchange.Enabled := PipesUsed;
  rdeEpsilon.Enabled := PipesUsed;
  seMaxIterations.Enabled := PipesUsed;
  rdeRelaxationParameter.Enabled := PipesUsed;
  cbPrintIterations.Enabled := PipesUsed;
  seOutputInterval.Enabled := PipesUsed;
  EnableCads(PipesUsed);

  EnablePipeElevationOffset;
end;

procedure TframePackageCFP.EnableLayerTemperature;
begin
  rdeLayerTemperature.Enabled := rcSelectionController.Enabled and cbLayers.Checked;
end;

procedure TframePackageCFP.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EmphasizeModeCheckBoxes;
  EnablePipeControls;
  EnableLayerTemperature;
end;

procedure TframePackageCFP.SetData(Package: TModflowPackageSelection);
var
  CfpPackage: TConduitFlowProcess;
begin
  inherited;
  CfpPackage := Package as TConduitFlowProcess;
  CfpPackage.PipesUsed := cbPipes.Checked;
  CfpPackage.ConduitLayersUsed := cbLayers.Checked;
  CfpPackage.ConduitTemperature := rdeConduitTemperature.RealValue;
  CfpPackage.CfpElevationChoice := TCfpElevationChoice(comboElevationChoice.ItemIndex);
  CfpPackage.ElevationOffset := rdePipeElevationOffset.RealValue;
  CfpPackage.CfpExchange := TCfpExchange(comboPipeExchange.ItemIndex);
  CfpPackage.Epsilon := rdeEpsilon.RealValue;
  CfpPackage.MaxIterations := seMaxIterations.AsInteger;
  CfpPackage.Relax := rdeRelaxationParameter.RealValue;
  CfpPackage.CfpPrintIterations := TCfpPrintIterations(cbPrintIterations.Checked);
  CfpPackage.LayerTemperature := rdeLayerTemperature.RealValue;
  CfpPackage.ConduitRechargeUsed := cbConduitRecharge.Checked;
  CfpPackage.OutputInterval := seOutputInterval.AsInteger;
  CfpPackage.UseCads := cpCADS.Checked;
end;

procedure TframePackageCFP.EnableCads(PipesUsed: Boolean);
begin
{$IFDEF OWHMV2}
  if frmGoPhast.ModelSelection = msModflowOwhm2 then
  begin
    cpCADS.Enabled := PipesUsed;
  end
  else
  begin
    cpCADS.Enabled := False;
  end;
{$ELSE}
  cpCADS.Enabled := False;
{$ENDIF}
end;

end.
