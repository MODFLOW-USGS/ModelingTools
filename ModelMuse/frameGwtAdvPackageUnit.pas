unit frameGwtAdvPackageUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.ExtCtrls;

type
  TframeGwtAdvPackage = class(TframePackage)
    rgScheme: TRadioGroup;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameGwtAdvPackage: TframeGwtAdvPackage;

implementation

{$R *.dfm}

{ TframeGwtAdvPackage }

procedure TframeGwtAdvPackage.GetData(Package: TModflowPackageSelection);
var
  Adv: TGwtAdvectionPackage;
begin
  inherited;
  Adv := Package as TGwtAdvectionPackage;
  rgScheme.ItemIndex := Ord(Adv.Scheme);
end;

procedure TframeGwtAdvPackage.SetData(Package: TModflowPackageSelection);
var
  Adv: TGwtAdvectionPackage;
begin
  inherited;
  Adv := Package as TGwtAdvectionPackage;
  Adv.Scheme := TGwtScheme(rgScheme.ItemIndex);
end;

end.
