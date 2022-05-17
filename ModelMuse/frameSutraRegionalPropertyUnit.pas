unit frameSutraRegionalPropertyUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvPageList,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.ExtCtrls, Vcl.ComCtrls, frameGridUnit,
  SutraOptionsUnit, SsButtonEd;

type
  TframeSutraRegionalProperty = class(TFrame)
    grpAdsorption: TGroupBox;
    lblFirstDistributionCoefficient: TLabel;
    lblSecondDistributionCoefficient: TLabel;
    rgSorptionModel: TRadioGroup;
    rgTransportModel: TRadioGroup;
    grpWaterSaturation: TGroupBox;
    rgWatSatFunct: TRadioGroup;
    lblResidWatSat: TLabel;
    lblVgenAlpha: TLabel;
    lblVgenEta: TLabel;
    pgcMain: TPageControl;
    tsAdsorption: TTabSheet;
    tsTotalSaturation: TTabSheet;
    tsPermeability: TTabSheet;
    tsLiquidWater: TTabSheet;
    tsIceProperties: TTabSheet;
    jplTotalSaturation: TJvPageList;
    jvspTotSatControls: TJvStandardPage;
    jvspTotSatUserDefined: TJvStandardPage;
    frameTotSatUserDefined: TframeGrid;
    lblAirEntryPressure: TLabel;
    lblPoreDistIndex: TLabel;
    lblPresAtResid: TLabel;
    grpRelativePerm: TGroupBox;
    rgRelativePermChoice: TRadioGroup;
    jplRelativePerm: TJvPageList;
    jvspRelativePermControls: TJvStandardPage;
    lblMinRelPerm: TLabel;
    lblRelPermEta: TLabel;
    lblRelPermPoreDistIndex: TLabel;
    lblSatAtMinPerm: TLabel;
    jvspRelativePermUserDefined: TJvStandardPage;
    frameRelPermParam: TframeGrid;
    grpLiqWatSat: TGroupBox;
    rgLiqWatSatChoice: TRadioGroup;
    jplLiqWatSat: TJvPageList;
    jvspLiqWatSatControls: TJvStandardPage;
    lblResidLiqWatSat: TLabel;
    lblExpParamW: TLabel;
    lblPowerAlpha: TLabel;
    lblPowerBeta: TLabel;
    lblLiqWatRelTemSatMin: TLabel;
    jvspLiqWatSatParameters: TJvStandardPage;
    frameGrid1: TframeGrid;
    grpFreezeHeat: TGroupBox;
    lblMaxFreezeTemp: TLabel;
    lblLatentHeat: TLabel;
    btnedFirstDistributionCoefficient: TssButtonEdit;
    btnedSecondDistributionCoefficient: TssButtonEdit;
    btnedVgenAlpha: TssButtonEdit;
    btnedVgenEta: TssButtonEdit;
    btnedResidWatSat: TssButtonEdit;
    btnedAirEntryPressure: TssButtonEdit;
    btnedPoreDistIndex: TssButtonEdit;
    btnedPresAtResid: TssButtonEdit;
    btnedMinRelPerm: TssButtonEdit;
    btnedRelPermEta: TssButtonEdit;
    btnedRelPermPoreDistIndex: TssButtonEdit;
    btnedSatAtMinPerm: TssButtonEdit;
    btnedResidLiqWatSat: TssButtonEdit;
    btnedExpParamW: TssButtonEdit;
    btnedPowerAlpha: TssButtonEdit;
    btnedPowerBeta: TssButtonEdit;
    btnedLiqWatRelTemSatMin: TssButtonEdit;
    btnedMaxFreezeTemp: TssButtonEdit;
    btnedLatentHeat: TssButtonEdit;
    procedure pgcMainDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure frameTotSatUserDefinedsbAddClick(Sender: TObject);
    procedure frameTotSatUserDefinedsbInsertClick(Sender: TObject);
    procedure rgWatSatFunctClick(Sender: TObject);
    procedure rgRelativePermChoiceClick(Sender: TObject);
    procedure rgLiqWatSatChoiceClick(Sender: TObject);
    procedure DoFormulaButtonClick(Sender: TObject);
  private
    FOnFormulaButtonClick: TNotifyEvent;
    procedure EnableTabs(TransportChoice: TTransportChoice;
      SaturationChoice: TSaturationChoice);
    { Private declarations }
  public
   property OnFormulaButtonClick: TNotifyEvent read FOnFormulaButtonClick
     write FOnFormulaButtonClick;
   procedure GetData(ARegion: TRegionalProperty;
     TransportChoice: TTransportChoice; SaturationChoice: TSaturationChoice);
   procedure SetData(ARegion: TRegionalProperty);
   { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit;

{$R *.dfm}

resourcestring
  StrAdsorption = 'Adsorption'#13#10'Parameters';
  StrTransportModel = 'Transport'#13#10'Model';
  StrTotalWater = 'Total'#13#10'Water'#13#10'Saturation';
  StrRelativePerm = 'Relative'#13#10'Permeability'#13#10'Parameters';
  StrLiquidWater = 'Liquid'#13#10'Water'#13#10'Saturation';
  StrIceProperties = 'Freezing'#13#10'Temperature'#13#10'and Latent Heat';

procedure TframeSutraRegionalProperty.frameTotSatUserDefinedsbAddClick(
  Sender: TObject);
begin
  if frameTotSatUserDefined.seNumber.AsInteger < 10 then
  begin
    frameTotSatUserDefined.sbAddClick(Sender);
  end;
end;

procedure TframeSutraRegionalProperty.frameTotSatUserDefinedsbInsertClick(
  Sender: TObject);
begin
  if frameTotSatUserDefined.seNumber.AsInteger < 10 then
  begin
    frameTotSatUserDefined.sbInsertClick(Sender);
  end;
end;

procedure TframeSutraRegionalProperty.GetData(ARegion: TRegionalProperty;
     TransportChoice: TTransportChoice; SaturationChoice: TSaturationChoice);
var
  Adsorp: TAdsorptionProperties;
  TotalWaterSat: TWaterSaturationProperties;
  RelPerm: TRelativePermeabilityParameters;
  LiqWat: TLiquidWaterSaturationParameters;
  Freeze: TFreezingTempAndLatentHeat;
begin
  Assert(ARegion <> nil);
  EnableTabs(TransportChoice, SaturationChoice);

  Adsorp := ARegion.AdsorptionProperties;
  rgSorptionModel.ItemIndex := Ord(Adsorp.AdsorptionModel);
  btnedFirstDistributionCoefficient.Text := Adsorp.FirstDistributionCoefficient;
  btnedSecondDistributionCoefficient.Text := Adsorp.SecondDistributionCoefficient;
  rgTransportModel.ItemIndex := Ord(Adsorp.ThermalConductivityModel);

  TotalWaterSat := ARegion.WaterSaturationProperties;
  rgWatSatFunct.ItemIndex := Ord(TotalWaterSat.WaterSaturationChoice);
  btnedResidWatSat.Text := TotalWaterSat.ResidualWaterContent;
  btnedVgenAlpha.Text := TotalWaterSat.VanGenuchtenAlpha;
  btnedVgenEta.Text := TotalWaterSat.VanGenuchtenExponent;
  btnedAirEntryPressure.Text := TotalWaterSat.AirEntryPressure;
  btnedPoreDistIndex.Text := TotalWaterSat.PoreSizeDistributionIndex;
  btnedPresAtResid.Text := TotalWaterSat.PressureForResidualWaterContent;

  RelPerm := ARegion.RelativePermeabilityParameters;
  rgRelativePermChoice.ItemIndex := Ord(RelPerm.RelativePermeabilityChoice);
  btnedMinRelPerm.Text := RelPerm.MinRelativePerm;
  btnedRelPermEta.Text := RelPerm.RelativePermParam;
  btnedRelPermPoreDistIndex.Text := RelPerm.PoreSizeDistributionIndex;
  btnedSatAtMinPerm.Text := RelPerm.WaterSaturationAtMinPermeability;

  LiqWat := ARegion.LiquidWaterSaturationParameters;
  rgLiqWatSatChoice.ItemIndex := Ord(LiqWat.LiquidWaterSaturationChoice);
  btnedResidLiqWatSat.Text := LiqWat.ResidualLiquidWaterSaturation;
  btnedExpParamW.Text := LiqWat.ExponentialParameter;
  btnedPowerAlpha.Text := LiqWat.PowerLawAlpha;
  btnedPowerBeta.Text := LiqWat.PowerLawBeta;
  btnedLiqWatRelTemSatMin.Text := LiqWat.TempAtResidualLiquidWaterSaturation;

  Freeze := ARegion.FreezingTempAndLatentHeat;
  btnedMaxFreezeTemp.Text := Freeze.MaxFreezePoreWaterTemperature;
  btnedLatentHeat.Text := Freeze.LatentHeatOfFusion;
end;

procedure TframeSutraRegionalProperty.pgcMainDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  tabCaption: string;
  SutraOptions: TSutraOptions;
  R: TRect;
begin
  SutraOptions := frmGoPhast.PhastModel.SutraOptions;
  case TabIndex of
    0:
      begin
        if SutraOptions.TransportChoice in [tcSolute, tcSoluteHead] then
        begin
          tabCaption := StrAdsorption;
        end
        else
        begin
          tabCaption := StrTransportModel;
        end;
      end;
    1:
      begin
        tabCaption := StrTotalWater;
      end;
    2:
      begin
        tabCaption := StrRelativePerm;
      end;
    3:
      begin
        tabCaption := StrLiquidWater;
      end;
    4:
      begin
        tabCaption := StrIceProperties;
      end;
    else
      begin
        tabCaption := ''
      end;
  end;
  R := Rect;
  R.Top := R.Top + (pgcMain.Canvas.TextHeight('Hg') div 3);
  DrawText(pgcMain.Canvas.Handle, PChar(tabCaption), Length(tabCaption),R, DT_CENTER);
end;

procedure TframeSutraRegionalProperty.rgLiqWatSatChoiceClick(Sender: TObject);
var
  LiqWatSatChoice: TLiquidWaterSaturationChoice;
begin
  LiqWatSatChoice := TLiquidWaterSaturationChoice(rgLiqWatSatChoice.ItemIndex);
  if LiqWatSatChoice = lwscUserDefined then
  begin
    jplLiqWatSat.ActivePage := jvspLiqWatSatParameters;
  end
  else
  begin
    jplLiqWatSat.ActivePage := jvspLiqWatSatControls;
  end;
  btnedResidLiqWatSat.Enabled := LiqWatSatChoice in
    [lwscExponential, lwscPowerLaw, lwscPiecewiseLinear];
  btnedExpParamW.Enabled := (LiqWatSatChoice = lwscExponential);
  btnedPowerAlpha.Enabled := (LiqWatSatChoice = lwscPowerLaw);
  btnedPowerBeta.Enabled := (LiqWatSatChoice = lwscPowerLaw);
  btnedLiqWatRelTemSatMin.Enabled := (LiqWatSatChoice = lwscPiecewiseLinear);
end;

procedure TframeSutraRegionalProperty.rgRelativePermChoiceClick(Sender: TObject);
var
  PermeabilityChoice: TRelativePermeabilityChoice;
begin
  PermeabilityChoice := TRelativePermeabilityChoice(rgRelativePermChoice.ItemIndex);
  if PermeabilityChoice = rpcUserDefined then
  begin
    jplRelativePerm.ActivePage := jvspRelativePermUserDefined;
  end
  else
  begin
    jplRelativePerm.ActivePage := jvspRelativePermControls;
  end;
  btnedMinRelPerm.Enabled := PermeabilityChoice in
    [rpcVanGenuchten, rpcBrooksCorey, rpcPiecewiseLinear];
  btnedRelPermEta.Enabled := (PermeabilityChoice = rpcVanGenuchten);
  btnedRelPermPoreDistIndex.Enabled := (PermeabilityChoice = rpcBrooksCorey);
  btnedSatAtMinPerm.Enabled := (PermeabilityChoice = rpcPiecewiseLinear);
end;

procedure TframeSutraRegionalProperty.rgWatSatFunctClick(Sender: TObject);
var
  SatChoice: TWaterSaturationChoice;
begin
  if rgWatSatFunct.ItemIndex >= 0 then
  begin
    SatChoice := wscNone;
  end
  else
  begin
    SatChoice := TWaterSaturationChoice(rgWatSatFunct.ItemIndex);
  end;

  if SatChoice = wscUserDefined then
  begin
    jplTotalSaturation.ActivePage := jvspTotSatUserDefined;
  end
  else
  begin
    jplTotalSaturation.ActivePage := jvspTotSatControls;
  end;

  btnedResidWatSat.Enabled := SatChoice in
    [wscVanGenuchten, wscBrooksCorey, wscPiecewiseLinear];
  btnedVgenAlpha.Enabled := (SatChoice = wscVanGenuchten);
  btnedVgenEta.Enabled := (SatChoice = wscVanGenuchten);
  btnedAirEntryPressure.Enabled := SatChoice in
    [wscBrooksCorey, wscPiecewiseLinear];
  btnedPoreDistIndex.Enabled := (SatChoice = wscBrooksCorey);
  btnedPresAtResid.Enabled := (SatChoice = wscPiecewiseLinear);
end;

procedure TframeSutraRegionalProperty.SetData(ARegion: TRegionalProperty);
var
  Adsorp: TAdsorptionProperties;
  TotalWaterSat: TWaterSaturationProperties;
  RelPerm: TRelativePermeabilityParameters;
  LiqWat: TLiquidWaterSaturationParameters;
  Freeze: TFreezingTempAndLatentHeat;
  AValue: double;
begin
  Assert(ARegion <> nil);

  Adsorp := ARegion.AdsorptionProperties;
  Adsorp.AdsorptionModel := TSorptionModel(rgSorptionModel.ItemIndex);
  Adsorp.FirstDistributionCoefficient := btnedFirstDistributionCoefficient.Text;
  Adsorp.SecondDistributionCoefficient := btnedSecondDistributionCoefficient.Text;
  Adsorp.ThermalConductivityModel :=
    TThermalConductivityModel(rgTransportModel.ItemIndex);

  TotalWaterSat := ARegion.WaterSaturationProperties;
  TotalWaterSat.WaterSaturationChoice :=
    TWaterSaturationChoice(rgWatSatFunct.ItemIndex);
  TotalWaterSat.ResidualWaterContent := btnedResidWatSat.Text;
  TotalWaterSat.VanGenuchtenAlpha := btnedVgenAlpha.Text;
  TotalWaterSat.VanGenuchtenExponent := btnedVgenEta.Text;
  TotalWaterSat.AirEntryPressure := btnedAirEntryPressure.Text;
  TotalWaterSat.PoreSizeDistributionIndex := btnedPoreDistIndex.Text;
  TotalWaterSat.PressureForResidualWaterContent := btnedPresAtResid.Text;

  RelPerm := ARegion.RelativePermeabilityParameters;
  RelPerm.RelativePermeabilityChoice :=
    TRelativePermeabilityChoice(rgRelativePermChoice.ItemIndex);
  RelPerm.MinRelativePerm := btnedMinRelPerm.Text;
  RelPerm.RelativePermParam := btnedRelPermEta.Text;
  RelPerm.PoreSizeDistributionIndex := btnedRelPermPoreDistIndex.Text;
  RelPerm.WaterSaturationAtMinPermeability := btnedSatAtMinPerm.Text;

  LiqWat := ARegion.LiquidWaterSaturationParameters;
  LiqWat.LiquidWaterSaturationChoice :=
    TLiquidWaterSaturationChoice(rgLiqWatSatChoice.ItemIndex);
  LiqWat.ResidualLiquidWaterSaturation := btnedResidLiqWatSat.Text;
  LiqWat.ExponentialParameter := btnedExpParamW.Text;
  LiqWat.PowerLawAlpha := btnedPowerAlpha.Text;
  LiqWat.PowerLawBeta := btnedPowerBeta.Text;
  LiqWat.TempAtResidualLiquidWaterSaturation := btnedLiqWatRelTemSatMin.Text;

  Freeze := ARegion.FreezingTempAndLatentHeat;
  Freeze.MaxFreezePoreWaterTemperature := btnedMaxFreezeTemp.Text;
  Freeze.LatentHeatOfFusion := btnedLatentHeat.Text;
end;

procedure TframeSutraRegionalProperty.DoFormulaButtonClick(Sender: TObject);
begin
  if Assigned(OnFormulaButtonClick) then
  begin
    OnFormulaButtonClick(Sender)
  end;
end;

procedure TframeSutraRegionalProperty.EnableTabs(
  TransportChoice: TTransportChoice; SaturationChoice: TSaturationChoice);
begin
  tsTotalSaturation.TabVisible := (TransportChoice = tcFreezing)
    or (SaturationChoice = scUnsaturated);
  tsPermeability.TabVisible := (TransportChoice = tcFreezing)
    or (SaturationChoice = scUnsaturated);
  tsLiquidWater.TabVisible := (TransportChoice = tcFreezing);
  tsIceProperties.TabVisible := (TransportChoice = tcFreezing);
end;

end.
