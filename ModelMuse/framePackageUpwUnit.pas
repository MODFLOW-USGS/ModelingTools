unit framePackageUpwUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, 
  ModflowPackageSelectionUnit;

type
  TframePackageUpw = class(TframePackage)
    cbPrintHDRY: TCheckBox;
    cbNoParCheck: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageUpw: TframePackageUpw;

implementation

uses
  framePackageLpfUnit;

{$R *.dfm}

{ TframePackageUpw }

procedure TframePackageUpw.GetData(Package: TModflowPackageSelection);
var
  UpwPackage: TUpwPackageSelection;
begin
  inherited;
  UpwPackage := Package as TUpwPackageSelection;
  cbPrintHDRY.Checked := UpwPackage.HDryPrintOption = hpoPrintHdry;
  cbNoParCheck.Width := Width - cbNoParCheck.Left - 8;
  cbNoParCheck.Caption := StrSkipCheckingThatA;
  cbNoParCheck.Checked := UpwPackage.NoParCheck;
end;

procedure TframePackageUpw.SetData(Package: TModflowPackageSelection);
var
  UpwPackage: TUpwPackageSelection;
begin
  inherited;
  UpwPackage := (Package as TUpwPackageSelection);
  if cbPrintHDRY.Checked then
  begin
    UpwPackage.HDryPrintOption := hpoPrintHdry;
  end
  else
  begin
    UpwPackage.HDryPrintOption := hpoDontPrintHdry;
  end;
  UpwPackage.NoParCheck := cbNoParCheck.Checked;
end;

end.
