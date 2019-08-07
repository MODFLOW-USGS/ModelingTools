unit framePkgStoUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, Vcl.StdCtrls,
  JvExStdCtrls, JvCombobox, JvListComb, RbwController,
  ModflowPackageSelectionUnit;

type
  TframePkgSto = class(TframePackage)
    comboStorageChoice: TJvImageComboBox;
    lblConfinedStorageMethod: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePkgSto: TframePkgSto;

implementation

{$R *.dfm}

{ TframePkgSto }

procedure TframePkgSto.GetData(Package: TModflowPackageSelection);
var
  StoPkg: TStoPackage;
begin
  inherited;
  StoPkg := Package as TStoPackage;
  comboStorageChoice.ItemIndex := Ord(StoPkg.StorageChoice);
//  cbNewton.Checked := StoPkg.NewtonFormulation;
end;

procedure TframePkgSto.SetData(Package: TModflowPackageSelection);
var
  StoPkg: TStoPackage;
begin
  inherited;
  StoPkg := Package as TStoPackage;
  StoPkg.StorageChoice := TStorageChoice(comboStorageChoice.ItemIndex);
//  StoPkg.NewtonFormulation := TNewtonFormulation(cbNewton.Checked);
end;

end.
