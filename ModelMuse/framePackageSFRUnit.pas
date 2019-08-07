unit framePackageSFRUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, addbtn95, ArgusDataEntry,
  ExtCtrls, ModflowPackageSelectionUnit;

type
  TframePackageSFR = class(TframePackage)   
    cbSfrUnsatflow: TCheckBox95;
    cbSfrLpfHydraulicCond: TCheckBox95;
    lblSfrTrailingWaveIncrements: TLabel;
    lblSfrMaxTrailingWaves: TLabel;
    lblSfrMaxUnsatCells: TLabel;
    lblStreamTolerance: TLabel;
    rgSfr2ISFROPT: TRadioGroup;
    rdeDLEAK: TRbwDataEntry;
    rdeNstrail: TRbwDataEntry;
    rdeNsfrsets: TRbwDataEntry;
    rdeIsuzn: TRbwDataEntry;
    cbIRTFLG: TCheckBox;
    rdeNUMTIM: TRbwDataEntry;
    lblNUMTIM: TLabel;
    rdeFLWTOL: TRbwDataEntry;
    lblFLWTOL: TLabel;
    rdeWeight: TRbwDataEntry;
    lblWeight: TLabel;
    comboPrintStreams: TComboBox;
    lblPrintStreams: TLabel;
    cbGage8: TCheckBox;
    cbUseGsflowFormat: TCheckBox;
    cbSeepageLoss: TCheckBox;
    rdeLossAdjustmentFactor: TRbwDataEntry;
    lblLossAdjustmentFactor: TLabel;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure cbSfrUnsatflowClick(Sender: TObject);
    procedure cbIRTFLGClick(Sender: TObject);
    procedure cbSeepageLossClick(Sender: TObject);
  private
    FLpfUsed: boolean;
    procedure DecodeISFROPT(ISFROPT: integer);
    procedure EnableLpfHydControl;
    procedure SetLpfUsed(const Value: boolean);
    procedure EnableUnsatControls;
    procedure EnableLossAdjustmentOption;
    procedure EnableLossAdjustmentFactor;
    { Private declarations }
  public
    function CalculateISFROPT: integer;
    property LpfUsed: boolean read FLpfUsed write SetLpfUsed;
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

//var
//  framePackageSFR: TframePackageSFR;

implementation

uses
  frmGoPhastUnit, GoPhastTypes;

{$R *.dfm}

{ TframePackageSFR }

procedure TframePackageSFR.cbIRTFLGClick(Sender: TObject);
begin
  inherited;
  rdeNUMTIM.Enabled := cbIRTFLG.Checked and rcSelectionController.Enabled;
  rdeFLWTOL.Enabled := cbIRTFLG.Checked and rcSelectionController.Enabled;
  rdeWeight.Enabled := cbIRTFLG.Checked and rcSelectionController.Enabled;
end;

procedure TframePackageSFR.cbSeepageLossClick(Sender: TObject);
begin
  inherited;
  EnableLossAdjustmentFactor;
end;

procedure TframePackageSFR.cbSfrUnsatflowClick(Sender: TObject);
begin
  inherited;
  EnableUnsatControls;
end;

procedure TframePackageSFR.DecodeISFROPT(ISFROPT: integer);
begin
  case ISFROPT of
    0:
      begin
        cbSfrUnsatflow.Checked := False;
        rgSfr2ISFROPT.ItemIndex := 0;
      end;
    1:
      begin
        cbSfrUnsatflow.Checked := False;
        rgSfr2ISFROPT.ItemIndex := 1;
      end;
    2:
      begin
        cbSfrUnsatflow.Checked := True;
        rgSfr2ISFROPT.ItemIndex := 1;
        cbSfrLpfHydraulicCond.Checked := True;
      end;
    3:
      begin
        cbSfrUnsatflow.Checked := True;
        rgSfr2ISFROPT.ItemIndex := 1;
        cbSfrLpfHydraulicCond.Checked := False;
      end;
    4:
      begin
        cbSfrUnsatflow.Checked := True;
        rgSfr2ISFROPT.ItemIndex := 0;
        cbSfrLpfHydraulicCond.Checked := True;
      end;
    5:
      begin
        cbSfrUnsatflow.Checked := True;
        rgSfr2ISFROPT.ItemIndex := 0;
        cbSfrLpfHydraulicCond.Checked := False;
      end;
    else Assert(False);
  end;
end;

procedure TframePackageSFR.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableLpfHydControl;
  EnableUnsatControls;
  EnableLossAdjustmentOption;
  EnableLossAdjustmentFactor;
  cbIRTFLGClick(Sender);
end;

procedure TframePackageSFR.SetData(Package: TModflowPackageSelection);
var
  SFR: TSfrPackageSelection;

  RealValue: Extended;
  IntValue: integer;
begin
  inherited;
  SFR := Package as TSfrPackageSelection;
  SFR.Isfropt := CalculateISFROPT;
  if TryStrToFloat(rdeDLEAK.Text, RealValue) then
  begin
    SFR.Dleak := RealValue;
  end;
  if TryStrToInt(rdeNstrail.Text, IntValue) then
  begin
    SFR.Nstrail := IntValue;
  end;
  if TryStrToInt(rdeNsfrsets.Text, IntValue) then
  begin
    SFR.Nsfrsets := IntValue;
  end;
  if TryStrToInt(rdeIsuzn.Text, IntValue) then
  begin
    SFR.Isuzn := IntValue;
  end;
//  SFR.PrintStreams := cbPrintStreams.Checked;
  SFR.PrintFlows := TPrintFlows(comboPrintStreams.ItemIndex);
  SFR.KinematicRouting := cbIRTFLG.Checked;
  if TryStrToInt(rdeNUMTIM.Text, IntValue) then
  begin
    SFR.TimeStepsForKinematicRouting := IntValue;
  end;
  if TryStrToFloat(rdeFLWTOL.Text, RealValue) then
  begin
    SFR.KinematicRoutingTolerance := RealValue;
  end;
  if TryStrToFloat(rdeWeight.Text, RealValue) then
  begin
    SFR.KinematicRoutingWeight := RealValue;
  end;
  SFR.UseGsflowFormat := cbUseGsflowFormat.Checked;
  SFR.LossFactorOption := cbSeepageLoss.Checked;
  SFR.LossFactor := rdeLossAdjustmentFactor.RealValue;
end;

procedure TframePackageSFR.EnableUnsatControls;
begin
  rdeNstrail.Enabled := cbSfrUnsatflow.Checked and rcSelectionController.Enabled;
  rdeNsfrsets.Enabled := cbSfrUnsatflow.Checked and rcSelectionController.Enabled;
  rdeIsuzn.Enabled := cbSfrUnsatflow.Checked and rcSelectionController.Enabled;
end;

procedure TframePackageSFR.SetLpfUsed(const Value: boolean);
begin
  FLpfUsed := Value;
  EnableLpfHydControl;
end;

procedure TframePackageSFR.EnableLossAdjustmentFactor;
begin
  rdeLossAdjustmentFactor.Enabled := cbSeepageLoss.Checked and
    cbSeepageLoss.Enabled and rcSelectionController.Enabled;
end;

procedure TframePackageSFR.EnableLossAdjustmentOption;
begin
  cbSeepageLoss.Enabled := rcSelectionController.Enabled
    and (frmGoPhast.PhastModel.NWT_Format = nf1_1);
end;

procedure TframePackageSFR.EnableLpfHydControl;
begin
  cbSfrLpfHydraulicCond.Enabled := rcSelectionController.Enabled
    and LpfUsed and cbSfrUnsatflow.Checked;
  if not cbSfrLpfHydraulicCond.Enabled then
  begin
    cbSfrLpfHydraulicCond.Checked := False;
  end;
end;

procedure TframePackageSFR.GetData(Package: TModflowPackageSelection);
var
  SFR: TSfrPackageSelection;
begin
  inherited;
  SFR := Package as TSfrPackageSelection;
  DecodeISFROPT(SFR.Isfropt);
  rdeDLEAK.Text := FloatToStr(SFR.Dleak);
  rdeNstrail.Text := IntToStr(SFR.Nstrail);
  rdeNsfrsets.Text := IntToStr(SFR.Nsfrsets);
  rdeIsuzn.Text := IntToStr(SFR.Isuzn);
//  cbPrintStreams.Checked := SFR.PrintStreams;
  comboPrintStreams.ItemIndex := Ord(SFR.PrintFlows);
  cbIRTFLG.Checked := SFR.KinematicRouting;
  rdeNUMTIM.Text := IntToStr(SFR.TimeStepsForKinematicRouting);
  rdeFLWTOL.Text := FloatToStr(SFR.KinematicRoutingTolerance);
  rdeWeight.Text := FloatToStr(SFR.KinematicRoutingWeight);
  cbUseGsflowFormat.Checked := SFR.UseGsflowFormat;
  cbSeepageLoss.Checked := SFR.LossFactorOption;
  rdeLossAdjustmentFactor.RealValue := SFR.LossFactor;
end;

function TframePackageSFR.CalculateISFROPT: integer;
begin
  // Calculate the ISFROPT parameter for the SFR2 input.

  if cbSfrUnsatflow.Checked then
  begin
    if rgSfr2ISFROPT.ItemIndex = 0 then
    begin
      if cbSfrLpfHydraulicCond.Checked then
      begin
        result := 4
      end
      else
      begin
        result := 5
      end;
    end
    else
    begin
      if cbSfrLpfHydraulicCond.Checked then
      begin
        result := 2
      end
      else
      begin
        result := 3
      end;
    end;
  end
  else
  begin
    if rgSfr2ISFROPT.ItemIndex = 0 then
    begin
      result := 0
    end
    else
    begin
      result := 1
    end;
  end;
end;

end.
