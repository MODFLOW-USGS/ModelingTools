unit frameSutraRegionalPropertyUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvPageList,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.ExtCtrls, Vcl.ComCtrls, frameGridUnit;

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
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, SutraOptionsUnit;

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

end.
