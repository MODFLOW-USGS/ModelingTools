unit framePackageMawUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, ModflowPackageSelectionUnit;

type
  TframePackageMaw = class(TframePackage)
    cbPrintHeads: TCheckBox;
    cbSaveHeads: TCheckBox;
    cbSaveFlows: TCheckBox;
    cbIncludeWellStorage: TCheckBox;
    rdeShutDownTheta: TRbwDataEntry;
    rdeShutDownKappa: TRbwDataEntry;
    lblShutDownTheta: TLabel;
    lblShutDownKappa: TLabel;
    cbBudgetCsv: TCheckBox;
    cbFlowCorrection: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageMaw: TframePackageMaw;

implementation



{$R *.dfm}

{ TframePackageMaw }

procedure TframePackageMaw.GetData(Package: TModflowPackageSelection);
var
  MawPackage: TMawPackage;
begin
  inherited;
  MawPackage := Package as TMawPackage;
  cbPrintHeads.Checked := MawPackage.PrintHead;
  cbSaveHeads.Checked := MawPackage.SaveMawHeads;
  cbSaveFlows.Checked := MawPackage.SaveMawFlows;
  cbBudgetCsv.Checked := MawPackage.SaveBudgetCsv;
  cbFlowCorrection.Checked := MawPackage.FlowCorrection;
  cbIncludeWellStorage.Checked := MawPackage.IncludeWellStorage;
  rdeShutDownTheta.RealValue := MawPackage.ShutDownTheta;
  rdeShutDownKappa.RealValue := MawPackage.ShutDownKappa;
end;

procedure TframePackageMaw.SetData(Package: TModflowPackageSelection);
var
  MawPackage: TMawPackage;
begin
  inherited;
  MawPackage := Package as TMawPackage;
  MawPackage.PrintHead := cbPrintHeads.Checked;
  MawPackage.SaveMawHeads := cbSaveHeads.Checked;
  MawPackage.SaveMawFlows := cbSaveFlows.Checked;
  MawPackage.SaveBudgetCsv := cbBudgetCsv.Checked;
  MawPackage.FlowCorrection := cbFlowCorrection.Checked;
  MawPackage.IncludeWellStorage := cbIncludeWellStorage.Checked;
  MawPackage.ShutDownTheta := rdeShutDownTheta.RealValue;
  MawPackage.ShutDownKappa := rdeShutDownKappa.RealValue;
end;

end.
