unit framePackageMvrUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, Vcl.StdCtrls,
  RbwController, ModflowPackageSelectionUnit;

type
  TframePackageMvr = class(TframePackage)
    cbSaveBudget: TCheckBox;
    chSaveCsv: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageMvr: TframePackageMvr;

implementation

{$R *.dfm}

{ TframePackageMvr }

procedure TframePackageMvr.GetData(Package: TModflowPackageSelection);
var
  MvrPackage: TMvrPackage;
begin
  inherited;
  MvrPackage := Package as TMvrPackage;
  cbSaveBudget.Checked := MvrPackage.SaveBudgetFile;
  chSaveCsv.Checked := MvrPackage.SaveCsvBudgetFile;
end;

procedure TframePackageMvr.SetData(Package: TModflowPackageSelection);
var
  MvrPackage: TMvrPackage;
begin
  inherited;
  MvrPackage := Package as TMvrPackage;
  MvrPackage.SaveBudgetFile := cbSaveBudget.Checked;
  MvrPackage.SaveCsvBudgetFile := chSaveCsv.Checked;
end;

end.
