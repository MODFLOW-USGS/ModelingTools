unit framePackageSfrMF6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, ArgusDataEntry, Vcl.Mask, JvExMask,
  JvSpin;

type
  TframePackageSfrMF6 = class(TframePackage)
    cbSaveStage: TCheckBox;
    cbSaveBudget: TCheckBox;
    seMaxIterations: TJvSpinEdit;
    rdeMaxDepthChange: TRbwDataEntry;
    lblMaxIterations: TLabel;
    lblMaxDepthChange: TLabel;
    cbPrintStage: TCheckBox;
    cbPrintFlows: TCheckBox;
    cbPackageConvergence: TCheckBox;
    sePicard: TJvSpinEdit;
    lblPicard: TLabel;
    cbSaveBudgetCsv: TCheckBox;
    cbStorage: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageSfrMF6: TframePackageSfrMF6;

implementation

{$R *.dfm}

{ TframePackageSfrMF6 }

procedure TframePackageSfrMF6.GetData(Package: TModflowPackageSelection);
var
  SfrMf6: TSfrModflow6PackageSelection;
begin
  inherited;
{$IFNDEF GWE}
  cbStorage.Visible := False;
{$ENDIF}
  SfrMf6 := Package as TSfrModflow6PackageSelection;
  cbPrintStage.Checked := SfrMf6.PrintStage;
  cbPrintFlows.Checked := SfrMf6.PrintFlows;
  cbSaveStage.Checked := SfrMf6.SaveStageFile;
  cbSaveBudget.Checked := SfrMf6.SaveBudgetFile;
  cbSaveBudgetCsv.Checked := SfrMf6.SaveGwtBudgetCsv;
  sePicard.AsInteger := SfrMf6.MaxPicardIteration;
  seMaxIterations.AsInteger := SfrMf6.MaxIteration;
  rdeMaxDepthChange.RealValue := SfrMf6.MaxDepthChange;
  cbPackageConvergence.Checked := SfrMf6.WriteConvergenceData;
  cbStorage.Checked := SfrMf6.Storage;
end;

procedure TframePackageSfrMF6.SetData(Package: TModflowPackageSelection);
var
  SfrMf6: TSfrModflow6PackageSelection;
begin
  inherited;
  SfrMf6 := Package as TSfrModflow6PackageSelection;
  SfrMf6.PrintStage := cbPrintStage.Checked;
  SfrMf6.PrintFlows := cbPrintFlows.Checked;
  SfrMf6.SaveStageFile := cbSaveStage.Checked;
  SfrMf6.SaveBudgetFile := cbSaveBudget.Checked;
  SfrMf6.SaveGwtBudgetCsv := cbSaveBudgetCsv.Checked;
  SfrMf6.MaxPicardIteration := sePicard.AsInteger;
  SfrMf6.MaxIteration := seMaxIterations.AsInteger;
  SfrMf6.MaxDepthChange := rdeMaxDepthChange.RealValue;
  SfrMf6.WriteConvergenceData := cbPackageConvergence.Checked;
  SfrMf6.Storage := cbStorage.Checked;
end;

end.
