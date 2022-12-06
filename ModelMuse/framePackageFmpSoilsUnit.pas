unit framePackageFmpSoilsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframePackageFmpSoils = class(TframePackage)
    comboCapillaryFringe: TComboBox;
    lblCapillaryFringe: TLabel;
    comboSufaceVK: TComboBox;
    lblSufaceVK: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmpSoils: TframePackageFmpSoils;

implementation

{$R *.dfm}

{ TframePackageFmpSoils }

procedure TframePackageFmpSoils.GetData(Package: TModflowPackageSelection);
var
  SoilPkg: TFarmProcess4Soil;
begin
  inherited;
  SoilPkg := Package as TFarmProcess4Soil;
  comboCapillaryFringe.ItemIndex := Ord(SoilPkg.CapFringeArrayList);
  comboSufaceVK.ItemIndex := Ord(SoilPkg.SurfVertKArrayList);
end;

procedure TframePackageFmpSoils.SetData(Package: TModflowPackageSelection);
var
  SoilPkg: TFarmProcess4Soil;
begin
  inherited;
  SoilPkg := Package as TFarmProcess4Soil;
  SoilPkg.CapFringeArrayList := TArrayList(comboCapillaryFringe.ItemIndex);
  SoilPkg.SurfVertKArrayList := TArrayList(comboSufaceVK.ItemIndex);
end;

end.
