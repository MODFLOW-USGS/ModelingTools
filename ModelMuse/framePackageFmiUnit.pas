unit framePackageFmiUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframePackageFmi = class(TframePackage)
    cbSeparate: TCheckBox;
    cbFlowImbalance: TCheckBox;
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure cbSeparateClick(Sender: TObject);
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

procedure TframePackageFmi.cbSeparateClick(Sender: TObject);
begin
  inherited;
  rcSelectionControllerEnabledChange(Sender);
end;

procedure TframePackageFmi.GetData(Package: TModflowPackageSelection);
var
  GwtProcess: TGwtProcess;
begin
  inherited;
  GwtProcess := Package as TGwtProcess;
  cbSeparate.Checked := GwtProcess.SeparateGwt;
  cbFlowImbalance.Checked := GwtProcess.FLOW_IMBALANCE_CORRECTION;
end;

procedure TframePackageFmi.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  cbFlowImbalance.Enabled := rcSelectionController.Enabled
    and cbSeparate.Checked;
end;

procedure TframePackageFmi.SetData(Package: TModflowPackageSelection);
var
  GwtProcess: TGwtProcess;
begin
  inherited;
  GwtProcess := Package as TGwtProcess;
  GwtProcess.SeparateGwt := cbSeparate.Checked;
  GwtProcess.FLOW_IMBALANCE_CORRECTION := cbFlowImbalance.Checked;
end;

end.
