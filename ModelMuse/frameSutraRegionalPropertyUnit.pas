unit frameSutraRegionalPropertyUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvPageList,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.ExtCtrls, Vcl.ComCtrls, frameGridUnit,
  SutraOptionsUnit;

type
  TframeSutraRegionalProperty = class(TFrame)
    grpAdsorption: TGroupBox;
    lblFirstDistributionCoefficient: TLabel;
    lblSecondDistributionCoefficient: TLabel;
    rgSorptionModel: TRadioGroup;
    rdeFirstDistributionCoefficient: TRbwDataEntry;
    rdeSecondDistributionCoefficient: TRbwDataEntry;
    rgTransportModel: TRadioGroup;
    grpWaterSaturation: TGroupBox;
    rgWatSatFunct: TRadioGroup;
    rdeResidWatSat: TRbwDataEntry;
    lblResidWatSat: TLabel;
    lblVgenAlpha: TLabel;
    rdeVgenAlpha: TRbwDataEntry;
    rdeVgenEta: TRbwDataEntry;
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
    rdePoreDistIndex: TRbwDataEntry;
    rdePresAtResid: TRbwDataEntry;
    lblAirEntryPressure: TLabel;
    lblPoreDistIndex: TLabel;
    lblPoreDistInd: TLabel;
    rdeAirEntryPressure: TRbwDataEntry;
    grpRelativePerm: TGroupBox;
    rgRelativePermChoice: TRadioGroup;
    jplRelativePerm: TJvPageList;
    jvspRelativePermControls: TJvStandardPage;
    lblMinRelPerm: TLabel;
    lblRelPermEta: TLabel;
    lblRelPermPoreDistIndex: TLabel;
    lblSatAtMinPerm: TLabel;
    rdeMinRelPerm: TRbwDataEntry;
    rdeRelPermEta: TRbwDataEntry;
    rdeRelPermPoreDistIndex: TRbwDataEntry;
    rdeSatAtMinPerm: TRbwDataEntry;
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
    rdeResidLiqWatSat: TRbwDataEntry;
    rdeExpParamW: TRbwDataEntry;
    rdePowerAlpha: TRbwDataEntry;
    rdeLiqWatRelTemSatMin: TRbwDataEntry;
    rdePowerBeta: TRbwDataEntry;
    jvspLiqWatSatParameters: TJvStandardPage;
    frameGrid1: TframeGrid;
    grpFreezeHeat: TGroupBox;
    rdeMaxFreezeTemp: TRbwDataEntry;
    rdeLatentHeat: TRbwDataEntry;
    lblMaxFreezeTemp: TLabel;
    lblLatentHeat: TLabel;
    procedure pgcMainDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);
    procedure frameTotSatUserDefinedsbAddClick(Sender: TObject);
    procedure frameTotSatUserDefinedsbInsertClick(Sender: TObject);
    procedure rgWatSatFunctClick(Sender: TObject);
    procedure rgRelativePermChoiceClick(Sender: TObject);
    procedure rgLiqWatSatChoiceClick(Sender: TObject);
  private
    procedure EnableTabs(TransportChoice: TTransportChoice;
      SaturationChoice: TSaturationChoice);
    { Private declarations }
  public
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
  rdeFirstDistributionCoefficient.RealValue := Adsorp.FirstDistributionCoefficient;
  rdeSecondDistributionCoefficient.RealValue := Adsorp.SecondDistributionCoefficient;
  rgTransportModel.ItemIndex := Ord(Adsorp.ThermalConductivityModel);

  TotalWaterSat := ARegion.WaterSaturationProperties;
  rgWatSatFunct.ItemIndex := Ord(TotalWaterSat.WaterSaturationChoice);
  rdeResidWatSat.RealValue := TotalWaterSat.ResidualWaterContent;
  rdeVgenAlpha.RealValue := TotalWaterSat.VanGenuchtenAlpha;
  rdeVgenEta.RealValue := TotalWaterSat.VanGenuchtenExponent;
  rdeAirEntryPressure.RealValue := TotalWaterSat.AirEntryPressure;
  rdePoreDistIndex.RealValue := TotalWaterSat.PoreSizeDistributionIndex;
  rdePresAtResid.RealValue := TotalWaterSat.PressureForResidualWaterContent;

  RelPerm := ARegion.RelativePermeabilityParameters;
  rgRelativePermChoice.ItemIndex := Ord(RelPerm.RelativePermeabilityChoice);
  rdeMinRelPerm.RealValue := RelPerm.MinRelativePerm;
  rdeRelPermEta.RealValue := RelPerm.RelativePermParam;
  rdeRelPermPoreDistIndex.RealValue := RelPerm.PoreSizeDistributionIndex;
  rdeSatAtMinPerm.RealValue := RelPerm.WaterSaturationAtMinPermeability;

  LiqWat := ARegion.LiquidWaterSaturationParameters;
  rgLiqWatSatChoice.ItemIndex := Ord(LiqWat.LiquidWaterSaturationChoice);
  rdeResidLiqWatSat.RealValue := LiqWat.ResidualLiquidWaterSaturation;
  rdeExpParamW.RealValue := LiqWat.ExponentialParameter;
  rdePowerAlpha.RealValue := LiqWat.PowerLawAlpha;
  rdePowerBeta.RealValue := LiqWat.PowerLawBeta;
  rdeLiqWatRelTemSatMin.RealValue := LiqWat.TempAtResidualLiquidWaterSaturation;

  Freeze := ARegion.FreezingTempAndLatentHeat;
  rdeMaxFreezeTemp.RealValue := Freeze.MaxFreezePoreWaterTemperature;
  rdeLatentHeat.RealValue := Freeze.LatentHeatOfFusion;
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
  rdeResidLiqWatSat.Enabled := LiqWatSatChoice in
    [lwscExponential, lwscPowerLaw, lwscPiecewiseLinear];
  rdeExpParamW.Enabled := (LiqWatSatChoice = lwscExponential);
  rdePowerAlpha.Enabled := (LiqWatSatChoice = lwscPowerLaw);
  rdePowerBeta.Enabled := (LiqWatSatChoice = lwscPowerLaw);
  rdeLiqWatRelTemSatMin.Enabled := (LiqWatSatChoice = lwscPiecewiseLinear);
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
  rdeMinRelPerm.Enabled := PermeabilityChoice in
    [rpcVanGenuchten, rpcBrooksCorey, rpcPiecewiseLinear];
  rdeRelPermEta.Enabled := (PermeabilityChoice = rpcVanGenuchten);
  rdeRelPermPoreDistIndex.Enabled := (PermeabilityChoice = rpcBrooksCorey);
  rdeSatAtMinPerm.Enabled := (PermeabilityChoice = rpcPiecewiseLinear);
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

  rdeResidWatSat.Enabled := SatChoice in
    [wscVanGenuchten, wscBrooksCorey, wscPiecewiseLinear];
  rdeVgenAlpha.Enabled := (SatChoice = wscVanGenuchten);
  rdeVgenEta.Enabled := (SatChoice = wscVanGenuchten);
  rdeAirEntryPressure.Enabled := SatChoice in
    [wscBrooksCorey, wscPiecewiseLinear];
  rdePoreDistIndex.Enabled := (SatChoice = wscBrooksCorey);
  rdePresAtResid.Enabled := (SatChoice = wscPiecewiseLinear);
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
  if rdeFirstDistributionCoefficient.TryGetRealValue(AValue) then
  begin
    Adsorp.FirstDistributionCoefficient := AValue;
  end;
  if rdeSecondDistributionCoefficient.TryGetRealValue(AValue) then
  begin
    Adsorp.SecondDistributionCoefficient := AValue;
  end;
  Adsorp.ThermalConductivityModel :=
    TThermalConductivityModel(rgTransportModel.ItemIndex);

  TotalWaterSat := ARegion.WaterSaturationProperties;
  TotalWaterSat.WaterSaturationChoice :=
    TWaterSaturationChoice(rgWatSatFunct.ItemIndex);
  if rdeResidWatSat.TryGetRealValue(AValue) then
  begin
    TotalWaterSat.ResidualWaterContent := AValue;
  end;
  if rdeVgenAlpha.TryGetRealValue(AValue) then
  begin
    TotalWaterSat.VanGenuchtenAlpha := AValue;
  end;
  if rdeVgenEta.TryGetRealValue(AValue) then
  begin
    TotalWaterSat.VanGenuchtenExponent := AValue;
  end;
  if rdeAirEntryPressure.TryGetRealValue(AValue) then
  begin
    TotalWaterSat.AirEntryPressure := AValue;
  end;
  if rdePoreDistIndex.TryGetRealValue(AValue) then
  begin
    TotalWaterSat.PoreSizeDistributionIndex := AValue;
  end;
  if rdePresAtResid.TryGetRealValue(AValue) then
  begin
    TotalWaterSat.PressureForResidualWaterContent := AValue;
  end;

  RelPerm := ARegion.RelativePermeabilityParameters;
  RelPerm.RelativePermeabilityChoice :=
    TRelativePermeabilityChoice(rgRelativePermChoice.ItemIndex);
  if rdeMinRelPerm.TryGetRealValue(AValue) then
  begin
    RelPerm.MinRelativePerm := AValue;
  end;
  if rdeRelPermEta.TryGetRealValue(AValue) then
  begin
    RelPerm.RelativePermParam := AValue;
  end;
  if rdeRelPermPoreDistIndex.TryGetRealValue(AValue) then
  begin
    RelPerm.PoreSizeDistributionIndex := AValue;
  end;
  if rdeSatAtMinPerm.TryGetRealValue(AValue) then
  begin
    RelPerm.WaterSaturationAtMinPermeability := AValue;
  end;

  LiqWat := ARegion.LiquidWaterSaturationParameters;
  LiqWat.LiquidWaterSaturationChoice :=
    TLiquidWaterSaturationChoice(rgLiqWatSatChoice.ItemIndex);
  if rdeResidLiqWatSat.TryGetRealValue(AValue) then
  begin
    LiqWat.ResidualLiquidWaterSaturation := AValue;
  end;
  if rdeExpParamW.TryGetRealValue(AValue) then
  begin
    LiqWat.ExponentialParameter := AValue;
  end;
  if rdePowerAlpha.TryGetRealValue(AValue) then
  begin
    LiqWat.PowerLawAlpha := AValue;
  end;
  if rdePowerBeta.TryGetRealValue(AValue) then
  begin
    LiqWat.PowerLawBeta := AValue;
  end;
  if rdeLiqWatRelTemSatMin.TryGetRealValue(AValue) then
  begin
    LiqWat.TempAtResidualLiquidWaterSaturation := AValue;
  end;

  Freeze := ARegion.FreezingTempAndLatentHeat;
  if rdeMaxFreezeTemp.TryGetRealValue(AValue) then
  begin
    Freeze.MaxFreezePoreWaterTemperature := AValue;
  end;
  if rdeLatentHeat.TryGetRealValue(AValue) then
  begin
    Freeze.LatentHeatOfFusion := AValue;
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
