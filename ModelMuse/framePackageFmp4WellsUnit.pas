unit framePackageFmp4WellsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.CheckLst, ModflowPackageSelectionUnit;

type
  TframePackageFmp4Wells = class(TframePackage)
    clbPrint: TCheckListBox;
    comboPumpSpread: TComboBox;
    lblPumpSpread: TLabel;
    comboXY: TComboBox;
    lblXY: TLabel;
    comboWellLayer: TComboBox;
    lblWellLayer: TLabel;
    comboSmoothing: TComboBox;
    lblSmoothing: TLabel;
    comboProrateDemand: TComboBox;
    lblProrateDemand: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmp4Wells: TframePackageFmp4Wells;

implementation

{$R *.dfm}

{ TframePackageFmp4Wells }

procedure TframePackageFmp4Wells.GetData(Package: TModflowPackageSelection);
var
  FrmWellPackage: TFarmProcess4Wells;
  PrintIndex: TFarmWellPrint;
begin
  inherited;
  FrmWellPackage := Package as TFarmProcess4Wells;

  for PrintIndex := Low(TFarmWellPrint) to High(TFarmWellPrint) do
  begin
    clbPrint.Checked[Ord(PrintIndex)] := PrintIndex in FrmWellPackage.FarmWellPrints;
  end;

  comboPumpSpread.ItemIndex := Ord(FrmWellPackage.MnwPumpSpread);
  comboXY.ItemIndex := Ord(FrmWellPackage.WellXY);
  comboWellLayer.ItemIndex := Ord(FrmWellPackage.WellLayerChoice);
  comboSmoothing.ItemIndex := Ord(FrmWellPackage.Smoothing);
  comboProrateDemand.ItemIndex := Ord(FrmWellPackage.ProrateDemand);
end;

procedure TframePackageFmp4Wells.SetData(Package: TModflowPackageSelection);
var
  FrmWellPackage: TFarmProcess4Wells;
  PrintIndex: TFarmWellPrint;
  PrintChoices: TFarmWellPrints;
begin
  inherited;
  FrmWellPackage := Package as TFarmProcess4Wells;

  PrintChoices := [];
  for PrintIndex := Low(TFarmWellPrint) to High(TFarmWellPrint) do
  begin
    if clbPrint.Checked[Ord(PrintIndex)] then
    begin
      Include(PrintChoices, PrintIndex);
    end;
  end;
  FrmWellPackage.FarmWellPrints := PrintChoices;

  FrmWellPackage.MnwPumpSpread := TPumpSpreadChoice(comboPumpSpread.ItemIndex);
  FrmWellPackage.WellXY := TXYChoice(comboXY.ItemIndex);
  FrmWellPackage.WellLayerChoice := TPumpLayerChoice(comboWellLayer.ItemIndex);
  FrmWellPackage.Smoothing := TSmoothing(comboSmoothing.ItemIndex);
  FrmWellPackage.ProrateDemand := TProrateDemand(comboProrateDemand.ItemIndex);

end;

end.
