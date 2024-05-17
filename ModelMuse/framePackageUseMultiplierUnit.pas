unit framePackageUseMultiplierUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, Vcl.StdCtrls, ModflowPackageSelectionUnit,
  RbwController;

type
  TframePackageUseMultiplier = class(TframePackage)
    cbUseMultiplierMODFLOW6: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageUseMultiplier: TframePackageUseMultiplier;

implementation

{$R *.dfm}

{ TframePackageUseMultiplier }

procedure TframePackageUseMultiplier.GetData(Package: TModflowPackageSelection);
begin
  inherited;
  cbUseMultiplierMODFLOW6.Checked := (Package as TMultiplierPackage).UseMultiplier;
end;

procedure TframePackageUseMultiplier.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  (Package as TMultiplierPackage).UseMultiplier := cbUseMultiplierMODFLOW6.Checked;
end;

end.
