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
  cbSaveHeads.Checked := MawPackage.SaveMnwHeads;
  cbSaveFlows.Checked := MawPackage.SaveMnwFlows;
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
  MawPackage.SaveMnwHeads := cbSaveHeads.Checked;
  MawPackage.SaveMnwFlows := cbSaveFlows.Checked;
  MawPackage.IncludeWellStorage := cbIncludeWellStorage.Checked;
  MawPackage.ShutDownTheta := rdeShutDownTheta.RealValue;
  MawPackage.ShutDownKappa := rdeShutDownKappa.RealValue;
end;

end.
