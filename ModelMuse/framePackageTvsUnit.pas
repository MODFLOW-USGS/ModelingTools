unit framePackageTvsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframePackageTvs = class(TframePackage)
    cbEnableStorageChangeIntegration: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageTvs: TframePackageTvs;

implementation

{$R *.dfm}

{ TframePackageTvs }

procedure TframePackageTvs.GetData(Package: TModflowPackageSelection);
begin
  inherited;
  cbEnableStorageChangeIntegration.Checked :=
    (Package as TTvsPackage).Enable_Storage_Change_Integration;
end;

procedure TframePackageTvs.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  (Package as TTvsPackage).Enable_Storage_Change_Integration :=
    cbEnableStorageChangeIntegration.Checked;
end;

end.
