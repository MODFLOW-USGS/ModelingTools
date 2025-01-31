unit frameGwtAdvPackageUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.ExtCtrls, ArgusDataEntry;

type
  TframeGwtAdvPackage = class(TframePackage)
    rgScheme: TRadioGroup;
    rdeAdePercel: TRbwDataEntry;
    lblAdePercel: TLabel;
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
  rdeAdePercel.RealValue := Adv.AtsPercel;
end;

procedure TframeGwtAdvPackage.SetData(Package: TModflowPackageSelection);
var
  Adv: TGwtAdvectionPackage;
begin
  inherited;
  Adv := Package as TGwtAdvectionPackage;
  Adv.Scheme := TGwtScheme(rgScheme.ItemIndex);
  Adv.AtsPercel := rdeAdePercel.RealValueDefault(0);
end;

end.
