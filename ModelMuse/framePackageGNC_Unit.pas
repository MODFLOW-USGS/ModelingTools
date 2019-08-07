unit framePackageGNC_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ModflowPackageSelectionUnit, Vcl.ExtCtrls;

type
  TframePackageGNC = class(TframePackage)
    rgFormulation: TRadioGroup;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageGNC: TframePackageGNC;

implementation



{$R *.dfm}

{ TframePackageGNC }

procedure TframePackageGNC.GetData(Package: TModflowPackageSelection);
var
  Gnc: TGncPackage;
begin
  inherited;
  Gnc := Package as TGncPackage;
  rgFormulation.ItemIndex := Ord(Gnc.EquationFormulation);
//  cbExplicit.Checked := Gnc.EquationFormulation = efExplicit;
end;

procedure TframePackageGNC.SetData(Package: TModflowPackageSelection);
var
  Gnc: TGncPackage;
begin
  inherited;
  Gnc := Package as TGncPackage;
  Gnc.EquationFormulation := TEquationFormulation(rgFormulation.ItemIndex);
//  Gnc.EquationFormulation := TEquationFormulation(cbExplicit.Checked);
end;

end.
