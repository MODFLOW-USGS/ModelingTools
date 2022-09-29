unit framePackageFmiUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.ExtCtrls;

type
  TframePackageFmi = class(TframePackage)
    cbFlowImbalance: TCheckBox;
    rgSimulationChoice: TRadioGroup;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageFmi: TframePackageFmi;

implementation

{$R *.dfm}

procedure TframePackageFmi.GetData(Package: TModflowPackageSelection);
var
  GwtProcess: TGwtProcess;
begin
  inherited;
  GwtProcess := Package as TGwtProcess;
//  cbSeparate.Checked := GwtProcess.SeparateGwt;
  cbFlowImbalance.Checked := GwtProcess.FLOW_IMBALANCE_CORRECTION;
  rgSimulationChoice.ItemIndex := Ord(GwtProcess.SeparateGwt);
end;

procedure TframePackageFmi.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  cbFlowImbalance.Enabled := rcSelectionController.Enabled
    and (rgSimulationChoice.ItemIndex > 0);
end;

procedure TframePackageFmi.SetData(Package: TModflowPackageSelection);
var
  GwtProcess: TGwtProcess;
begin
  inherited;
  GwtProcess := Package as TGwtProcess;
//  GwtProcess.SeparateGwt := cbSeparate.Checked;
  GwtProcess.FLOW_IMBALANCE_CORRECTION := cbFlowImbalance.Checked;
  GwtProcess.SeparateGwt := rgSimulationChoice.ItemIndex = 1;
end;

end.
