unit framePackageLakMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, ModflowPackageSelectionUnit, Vcl.Mask, JvExMask,
  JvSpin;

type
  TframePackageLakMf6 = class(TframePackage)
    cbPrintStage: TCheckBox;
    cbSaveBudget: TCheckBox;
    cbSaveStage: TCheckBox;
    rdeSurfaceDepressionDepth: TRbwDataEntry;
    lblSurfaceDepressionDepth: TLabel;
    cbPackageConvergence: TCheckBox;
    cbSaveBudgetCsv: TCheckBox;
    seMaxIterations: TJvSpinEdit;
    lblMaxIterations: TLabel;
    lblConvergence: TLabel;
    rdeConvergence: TRbwDataEntry;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageLakMf6: TframePackageLakMf6;

implementation



{$R *.dfm}

{ TframePackageLakMf6 }

procedure TframePackageLakMf6.GetData(Package: TModflowPackageSelection);
var
  LakMf6: TLakeMf6PackageSelection;
begin
  inherited;
  LakMf6 := Package as TLakeMf6PackageSelection;
  cbPrintStage.Checked := LakMf6.PrintStage;
//  cbPrintFlows.Checked := LakMf6.PrintFlows;
  cbSaveStage.Checked := LakMf6.SaveStage;
  cbSaveBudget.Checked := LakMf6.SaveBudget;
  cbSaveBudgetCsv.Checked := LakMf6.SaveBudgetCsv;
  rdeSurfaceDepressionDepth.RealValue := LakMf6.SurfDepDepth;
  cbPackageConvergence.Checked := LakMf6.WriteConvergenceData;
  seMaxIterations.AsInteger := LakMf6.MaxIterations;
  rdeConvergence.RealValue := LakMf6.MaxStageChange;
end;

procedure TframePackageLakMf6.SetData(Package: TModflowPackageSelection);
var
  LakMf6: TLakeMf6PackageSelection;
begin
  inherited;
  LakMf6 := Package as TLakeMf6PackageSelection;
  LakMf6.PrintStage := cbPrintStage.Checked;
//  LakMf6.PrintFlows := cbPrintFlows.Checked;
  LakMf6.SaveStage := cbSaveStage.Checked;
  LakMf6.SaveBudget := cbSaveBudget.Checked;
  LakMf6.SaveBudgetCsv := cbSaveBudgetCsv.Checked;
  LakMf6.SurfDepDepth := rdeSurfaceDepressionDepth.RealValue;
  LakMf6.WriteConvergenceData := cbPackageConvergence.Checked;
  LakMf6.MaxIterations := seMaxIterations.AsInteger;
  LakMf6.MaxStageChange := rdeConvergence.RealValue;
end;

end.
