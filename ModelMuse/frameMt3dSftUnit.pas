unit frameMt3dSftUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, Vcl.Mask, JvExMask, JvSpin, JvExStdCtrls,
  JvCombobox, JvListComb, ModflowPackageSelectionUnit;

type
  TframeMt3dSftPkg = class(TframePackage)
    cbEvaporateMass: TCheckBox;
    rdeTimeWeightingFactor: TRbwDataEntry;
    rdeSpaceWeightingFactor: TRbwDataEntry;
    rdeClosureCriterion: TRbwDataEntry;
    seMaxIterations: TJvSpinEdit;
    comboPrintChoice: TJvImageComboBox;
    lblTimeWeightingFactor: TLabel;
    lblSpaceWeightingFactor: TLabel;
    lblClosureCriterion: TLabel;
    lblMaxIterations: TLabel;
    lblSolverPrintChoice: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dSftPkg: TframeMt3dSftPkg;

implementation

{$R *.dfm}

{ TframeMt3dSftPkg }

procedure TframeMt3dSftPkg.GetData(Package: TModflowPackageSelection);
var
  SftPackage: TMt3dSftPackageSelection;
begin
  inherited;
  SftPackage := Package as TMt3dSftPackageSelection;
  cbEvaporateMass.Checked := SftPackage.EvaporateMass;
  rdeTimeWeightingFactor.RealValue := SftPackage.TimeWeightingFactor;
  rdeSpaceWeightingFactor.RealValue := SftPackage.SpaceWeightingFactor;
  rdeClosureCriterion.RealValue := SftPackage.ClosureCriterion;
  seMaxIterations.AsInteger := SftPackage.MaxSftIterations;
  comboPrintChoice.ItemIndex := Ord(SftPackage.SolverPrintChoice);
end;

procedure TframeMt3dSftPkg.SetData(Package: TModflowPackageSelection);
var
  SftPackage: TMt3dSftPackageSelection;
begin
  inherited;
  SftPackage := Package as TMt3dSftPackageSelection;
  SftPackage.EvaporateMass := cbEvaporateMass.Checked;
  SftPackage.TimeWeightingFactor := rdeTimeWeightingFactor.RealValue;
  SftPackage.SpaceWeightingFactor := rdeSpaceWeightingFactor.RealValue;
  SftPackage.ClosureCriterion := rdeClosureCriterion.RealValue;
  SftPackage.MaxSftIterations := seMaxIterations.AsInteger;
  SftPackage.SolverPrintChoice := TSftSolverPrintChoice(comboPrintChoice.ItemIndex);
end;

end.
