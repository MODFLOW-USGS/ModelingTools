unit frmRenumberingMethodUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  Buttons, ExtCtrls;

type
  TfrmRenumberingMethod = class(TfrmCustomGoPhast)
    rgMethod: TRadioGroup;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    rgRenumberingMethod: TRadioGroup;
    procedure FormCreate(Sender: TObject); override;
  private
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRenumberingMethod: TfrmRenumberingMethod;

implementation

{$R *.dfm}

uses frmGoPhastUnit, SutraMeshUnit;

{ TfrmRenumberingMethod }

procedure TfrmRenumberingMethod.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmRenumberingMethod.GetData;
var
  Mesh: TSutraMesh3D;
begin
  Mesh := frmGoPhast.PhastModel.SutraMesh;
  rgRenumberingMethod.ItemIndex :=
    Ord(Mesh.Mesh2D.MeshGenControls.RenumberingAlgorithm);
  rgMethod.Enabled := (Mesh.MeshType = mt3D);
  if rgMethod.Enabled then
  begin
    rgMethod.ItemIndex := 0;
  end;
end;

end.
