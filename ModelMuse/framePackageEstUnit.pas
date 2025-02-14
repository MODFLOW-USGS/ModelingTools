unit framePackageEstUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, ModflowPackageSelectionUnit;

type
  TframePackageEst = class(TframePackage)
    cbZeroOrderDecayWater: TCheckBox;
    cbZeroOrderDecaySolute: TCheckBox;
    rdeDensityWater: TRbwDataEntry;
    lblDensityWater: TLabel;
    lblHeatCapacityWater: TLabel;
    rdeHeatCapacityWater: TRbwDataEntry;
    rdeLatentHeat: TRbwDataEntry;
    lblLatentHeat: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageEst: TframePackageEst;

implementation

{$R *.dfm}

{ TframePackageEst }

procedure TframePackageEst.GetData(Package: TModflowPackageSelection);
var
  Est: TGweEstPackage;
begin
  inherited;
  Est := Package as TGweEstPackage;
  cbZeroOrderDecayWater.Checked := Est.ZeroOrderDecayWater;
  cbZeroOrderDecaySolute.Checked := Est.ZeroOrderDecaySolid;
  rdeDensityWater.RealValue := Est.DensityWater;
  rdeHeatCapacityWater.RealValue := Est.HeatCapacityWater;
  rdeLatentHeat.RealValue := Est.LatentHeatVaporization;
end;

procedure TframePackageEst.SetData(Package: TModflowPackageSelection);
var
  Est: TGweEstPackage;
begin
  inherited;
  Est := Package as TGweEstPackage;
  Est.ZeroOrderDecayWater := cbZeroOrderDecayWater.Checked;
  Est.ZeroOrderDecaySolid := cbZeroOrderDecaySolute.Checked;
  Est.DensityWater := rdeDensityWater.RealValue;
  Est.HeatCapacityWater := rdeHeatCapacityWater.RealValue;
  Est.LatentHeatVaporization := rdeLatentHeat.RealValue;
end;

end.
