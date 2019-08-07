unit framePackageStrUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, RbwController, StdCtrls,
  ModflowPackageSelectionUnit;

type
  TframePackageStr = class(TframePackage)
    cbCalculateStage: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageStr: TframePackageStr;

implementation

{$R *.dfm}

{ TframePackageStr }

procedure TframePackageStr.GetData(Package: TModflowPackageSelection);
begin
  inherited;
  cbCalculateStage.Checked := (Package as TStrPackageSelection).CalculateStage;
end;

procedure TframePackageStr.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  (Package as TStrPackageSelection).CalculateStage := cbCalculateStage.Checked;
end;

end.
