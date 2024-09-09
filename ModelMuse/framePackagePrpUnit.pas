unit framePackagePrpUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry, frameOptionalValueUnit, ModflowPackageSelectionUnit;

type
  TframePackagePrp = class(TframePackage)
    RdeSolverTolerance: TRbwDataEntry;
    LblSolverTolerance: TLabel;
    EXTEND_TRACKING: TCheckBox;
    ComboBox1: TComboBox;
    frameStopTime: TframeOptionalValue;
    lblPrpTrack: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
  end;

var
  framePackagePrp: TframePackagePrp;

implementation

{$R *.dfm}

{ TframePackagePrp }

procedure TframePackagePrp.GetData(Package: TModflowPackageSelection);
var
  PrpPackage: TPrpPackage;
begin
  inherited;
  PrpPackage := Package as TPrpPackage;
  frameStopTime.cbUsed.Checked := PrpPackage.StopTimeUsed;
  frameStopTime.RdeValue.RealValue := PrpPackage.StopTime;
end;

procedure TframePackagePrp.SetData(Package: TModflowPackageSelection);
var
  PrpPackage: TPrpPackage;
begin
  inherited;
  PrpPackage := Package as TPrpPackage;
  PrpPackage.StopTimeUsed := frameStopTime.cbUsed.Checked;
  PrpPackage.StopTime := frameStopTime.RdeValue.RealValue;

end;

end.
