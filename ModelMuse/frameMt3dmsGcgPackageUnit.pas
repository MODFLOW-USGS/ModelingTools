unit frameMt3dmsGcgPackageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, ArgusDataEntry, StdCtrls, Mask, JvExMask, JvSpin,
  RbwController, ModflowPackageSelectionUnit;

type
  TframeMt3dmsGcgPackage = class(TframePackage)
    spinMaxOuter: TJvSpinEdit;
    lblMaxOuter: TLabel;
    spinMaxInner: TJvSpinEdit;
    lblMaxInner: TLabel;
    comboPreconditioner: TComboBox;
    lblPreconditioner: TLabel;
    comboDispersion: TComboBox;
    lblDispersion: TLabel;
    rdeRelaxationFactor: TRbwDataEntry;
    lblRelaxationFactor: TLabel;
    rdeConvergence: TRbwDataEntry;
    lblConvergence: TLabel;
    spinPrintoutInterval: TJvSpinEdit;
    lblPrintoutInterval: TLabel;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dmsGcgPackage: TframeMt3dmsGcgPackage;

implementation

{$R *.dfm}

procedure TframeMt3dmsGcgPackage.GetData(Package: TModflowPackageSelection);
var
  GcgPkg: TMt3dmsGCGSolverPackage;
begin
  inherited;
  GcgPkg := Package  as TMt3dmsGCGSolverPackage;
  spinMaxOuter.AsInteger := GcgPkg.MaxOuterIterations;
  spinMaxInner.AsInteger := GcgPkg.MaxInnerIterations;
  comboPreconditioner.ItemIndex := Ord(GcgPkg.PreconditionerChoice);
  comboDispersion.ItemIndex := Ord(GcgPkg.DispersionTensorChoice);
  rdeRelaxationFactor.Text := FloatToStr(GcgPkg.RelaxationFactor);
  rdeConvergence.Text := FloatToStr(GcgPkg.ConvergenceCriterion);
  spinPrintoutInterval.AsInteger := GcgPkg.PrintoutInterval;
end;

procedure TframeMt3dmsGcgPackage.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  rdeRelaxationFactor.Enabled := rcSelectionController.Enabled and
    (comboPreconditioner.ItemIndex = 1)
end;

procedure TframeMt3dmsGcgPackage.SetData(Package: TModflowPackageSelection);
var
  GcgPkg: TMt3dmsGCGSolverPackage;
begin
  inherited;
  GcgPkg := Package  as TMt3dmsGCGSolverPackage;
  GcgPkg.MaxOuterIterations := spinMaxOuter.AsInteger;
  GcgPkg.MaxInnerIterations := spinMaxInner.AsInteger;
  GcgPkg.PreconditionerChoice := TGcgPreconditioner(comboPreconditioner.ItemIndex);
  GcgPkg.DispersionTensorChoice := TDispersionTensorTreatment(comboDispersion.ItemIndex);
  GcgPkg.RelaxationFactor := StrToFloat(rdeRelaxationFactor.Text);
  GcgPkg.ConvergenceCriterion := StrToFloat(rdeConvergence.Text);
  GcgPkg.PrintoutInterval := spinPrintoutInterval.AsInteger;
end;

end.
