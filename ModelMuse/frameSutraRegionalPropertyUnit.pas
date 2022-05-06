unit frameSutraRegionalPropertyUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvExControls, JvPageList,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.ExtCtrls;

type
  TframeSutraRegionalProperty = class(TFrame)
    jplMain: TJvPageList;
    jvspAdsorbtion: TJvStandardPage;
    grpAdsorption: TGroupBox;
    lblFirstDistributionCoefficient: TLabel;
    lblSecondDistributionCoefficient: TLabel;
    rgSorptionModel: TRadioGroup;
    rdeFirstDistributionCoefficient: TRbwDataEntry;
    rdeSecondDistributionCoefficient: TRbwDataEntry;
    rgTransportModel: TRadioGroup;
    jvspWaterSaturation: TJvStandardPage;
    grpWaterSaturation: TGroupBox;
    rgWatSatFunct: TRadioGroup;
    rdeResidWatSat: TRbwDataEntry;
    lblResidWatSat: TLabel;
    lblVgenAlpha: TLabel;
    rdeVgenAlpha: TRbwDataEntry;
    rdeVgenEta: TRbwDataEntry;
    lblVgenEta: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
