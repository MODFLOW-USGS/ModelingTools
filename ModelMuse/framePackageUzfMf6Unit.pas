unit framePackageUzfMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, ModflowPackageSelectionUnit, ArgusDataEntry;

type
  TframePackageUzfMf6 = class(TframePackage)
    rgEvapotranspiration: TRadioGroup;
    rgUnsatEt: TRadioGroup;
    cbSeepage: TCheckBox;
    cbSaveBudget: TCheckBox;
    lblNumberOfTrailingWaves: TLabel;
    rdeNumberOfTrailingWaves: TRbwDataEntry;
    lblNumberOfWaveSets: TLabel;
    rdeNumberOfWaveSets: TRbwDataEntry;
    cbPackageConvergence: TCheckBox;
    cbBudgetCsv: TCheckBox;
    cbSaveWaterContent: TCheckBox;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure rgEvapotranspirationClick(Sender: TObject);
  private
    procedure Enable_rgUnsatEt;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

//var
//  framePackageUzfMf6: TframePackageUzfMf6;

implementation

{$R *.dfm}

procedure TframePackageUzfMf6.GetData(Package: TModflowPackageSelection);
var
  UzfPackage: TUzfMf6PackageSelection;
begin
  inherited;
  UzfPackage := Package as TUzfMf6PackageSelection;
  rgEvapotranspiration.ItemIndex := Ord(UzfPackage.GroundwaterET);
  rgUnsatEt.ItemIndex := Ord(UzfPackage.UnsatET);
  cbSeepage.Checked := UzfPackage.SimulateGroundwaterSeepage;
  cbSaveBudget.Checked := UzfPackage.SaveBudgetFile;
  cbBudgetCsv.Checked := UzfPackage.SaveBudgetCsvFile;
  rdeNumberOfTrailingWaves.IntegerValue := UzfPackage.NumberOfTrailingWaves;
  rdeNumberOfWaveSets.IntegerValue := UzfPackage.NumberOfWaveSets;
  cbPackageConvergence.Checked := UzfPackage.WriteConvergenceData;
  cbSaveWaterContent.Checked := UzfPackage.SaveWaterContent;
end;

procedure TframePackageUzfMf6.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  Enable_rgUnsatEt;
end;

procedure TframePackageUzfMf6.rgEvapotranspirationClick(Sender: TObject);
begin
  inherited;
  Enable_rgUnsatEt;
end;

procedure TframePackageUzfMf6.SetData(Package: TModflowPackageSelection);
var
  UzfPackage: TUzfMf6PackageSelection;
begin
  inherited;
  UzfPackage := Package as TUzfMf6PackageSelection;
  UzfPackage.GroundwaterET := TUzfGwEtChoice(rgEvapotranspiration.ItemIndex);
  UzfPackage.UnsatET := TUzfUnsatEtChoice(rgUnsatEt.ItemIndex);
  UzfPackage.SimulateGroundwaterSeepage := cbSeepage.checked;
  UzfPackage.SaveBudgetFile := cbSaveBudget.Checked;
  UzfPackage.SaveBudgetCsvFile := cbBudgetCsv.Checked;
  UzfPackage.NumberOfTrailingWaves := rdeNumberOfTrailingWaves.IntegerValue;
  UzfPackage.NumberOfWaveSets := rdeNumberOfWaveSets.IntegerValue;
  UzfPackage.WriteConvergenceData := cbPackageConvergence.Checked;
  UzfPackage.SaveWaterContent := cbSaveWaterContent.Checked;
end;

procedure TframePackageUzfMf6.Enable_rgUnsatEt;
begin
  rgUnsatEt.Enabled := rcSelectionController.Enabled
    and (TUzfGwEtChoice(rgEvapotranspiration.ItemIndex) <> ugecNoEt);
end;

end.
