unit framePackageMSTUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, System.Generics.Collections,
  ModflowPackageSelectionUnit;

type
  TframePackageMST = class(TframePackage)
    rgPorosity: TRadioGroup;
    rgSorption: TRadioGroup;
    rgDecay: TRadioGroup;
    cbSorbate: TCheckBox;
  private
    { Private declarations }
  protected
    procedure SetSelected(const Value: boolean); override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

  TframePackageMSTObjectList = TObjectList<TframePackageMST>;

var
  framePackageMST: TframePackageMST;

implementation

{$R *.dfm}

{ TframePackageMST }

procedure TframePackageMST.GetData(Package: TModflowPackageSelection);
var
  MstPackage: TGwtMstPackage;
begin
  inherited;
  MstPackage := Package as TGwtMstPackage;
  rgPorosity.ItemIndex := Ord(MstPackage.SeparatePorosity);
  if MstPackage.ZeroOrderDecay then
  begin
    rgDecay.ItemIndex := 1;
  end
  else
  if MstPackage.FirstOrderDecay then
  begin
    rgDecay.ItemIndex := 2;
  end
  else
  begin
    rgDecay.ItemIndex := 0;
  end;
  rgSorption.ItemIndex := Ord(MstPackage.Sorption);
  cbSorbate.Checked := MstPackage.Sorbate;
end;

procedure TframePackageMST.SetData(Package: TModflowPackageSelection);
var
  MstPackage: TGwtMstPackage;
begin
  inherited;
  MstPackage := Package as TGwtMstPackage;
  MstPackage.SeparatePorosity := rgPorosity.ItemIndex = 1;
  MstPackage.FirstOrderDecay := rgDecay.ItemIndex = 2;
  MstPackage.ZeroOrderDecay := rgDecay.ItemIndex = 1;
  MstPackage.Sorption := TGwtSorptionChoice(rgSorption.ItemIndex);
  MstPackage.Sorbate := cbSorbate.Checked;
end;

procedure TframePackageMST.SetSelected(const Value: boolean);
begin
  inherited SetSelected(True);

end;

end.
