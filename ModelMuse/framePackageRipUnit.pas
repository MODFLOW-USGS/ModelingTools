unit framePackageRipUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframePackageRip = class(TframePackage)
    cbWritePlantGroupFlows: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageRip: TframePackageRip;

implementation



{$R *.dfm}

{ TframePackage1 }

procedure TframePackageRip.GetData(Package: TModflowPackageSelection);
begin
  inherited;
  cbWritePlantGroupFlows.Checked :=
    (Package as TRipPackage).WritePlantGroupET = wpgWrite;
end;

procedure TframePackageRip.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  (Package as TRipPackage).WritePlantGroupET :=
    TWritePlantGroupET(cbWritePlantGroupFlows.Checked);
end;

end.
