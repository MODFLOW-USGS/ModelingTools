unit framePackageFmp4ClimateUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframePackageFmp4Climate = class(TframePackage)
    comboPrecipitation: TComboBox;
    comboReferenceET: TComboBox;
    lblPrecipitation: TLabel;
    lblReferenceET: TLabel;
    comboPotET_Bare: TComboBox;
    lblPotET_Bare: TLabel;
    comboDirectRecharge: TComboBox;
    comboPrecipitationPotConsup: TComboBox;
    lblDirectRecharge: TLabel;
    lblPrecipitationPotConsup: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4Climate: TframePackageFmp4Climate;

implementation

{$R *.dfm}

{ TframePackageFmp4Climate }

procedure TframePackageFmp4Climate.GetData(Package: TModflowPackageSelection);
var
  ClimatePkg: TFarmProcess4Climate;
begin
  inherited;
  ClimatePkg := Package as TFarmProcess4Climate;
  comboPrecipitation.ItemIndex := Ord(ClimatePkg.Precipitation);
  comboReferenceET.ItemIndex := Ord(ClimatePkg.ReferenceET);
  comboPotET_Bare.ItemIndex := Ord(ClimatePkg.Potential_Evaporation_Bare);
  comboDirectRecharge.ItemIndex := Ord(ClimatePkg.Direct_Recharge);
  comboPrecipitationPotConsup.ItemIndex := Ord(ClimatePkg.PRECIPITATION_POTENTIAL_CONSUMPTION);

end;

procedure TframePackageFmp4Climate.SetData(Package: TModflowPackageSelection);
var
  ClimatePkg: TFarmProcess4Climate;
begin
  inherited;
  ClimatePkg := Package as TFarmProcess4Climate;
  ClimatePkg.Precipitation := TFarmOption(comboPrecipitation.ItemIndex);
  ClimatePkg.ReferenceET := TFarmOption(comboReferenceET.ItemIndex);
  ClimatePkg.Potential_Evaporation_Bare := TFarmOption(comboPotET_Bare.ItemIndex);
  ClimatePkg.Direct_Recharge := TFarmOption(comboDirectRecharge.ItemIndex);
  ClimatePkg.PRECIPITATION_POTENTIAL_CONSUMPTION := TFarmOption(comboPrecipitationPotConsup.ItemIndex);

end;

end.
