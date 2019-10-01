unit frameMt3dCtsPkgUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit;

type
  TframeMt3dCtsPkg = class(TframePackage)
    comboForce: TComboBox;
    lblForce: TLabel;
    lblWellPackageChoice: TLabel;
    comboWellPackageChoice: TComboBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dCtsPkg: TframeMt3dCtsPkg;

implementation

{$R *.dfm}

{ TframeMt3dCtsPkg }

procedure TframeMt3dCtsPkg.GetData(Package: TModflowPackageSelection);
var
  CtsPackage: TMt3dCtsPackageSelection;
begin
  inherited;
  CtsPackage := Package as TMt3dCtsPackageSelection;
  comboForce.ItemIndex := Ord(CtsPackage.ForceOption);
  comboWellPackageChoice.ItemIndex := Ord(CtsPackage.WellPackageChoice);

end;

procedure TframeMt3dCtsPkg.SetData(Package: TModflowPackageSelection);
var
  CtsPackage: TMt3dCtsPackageSelection;
begin
  inherited;
  CtsPackage := Package as TMt3dCtsPackageSelection;
  CtsPackage.ForceOption := TCtsForceOption(comboForce.ItemIndex);
  CtsPackage.WellPackageChoice :=
    TCtsWellPackageChoice(comboWellPackageChoice.ItemIndex);
end;

end.
