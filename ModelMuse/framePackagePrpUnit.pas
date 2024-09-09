unit framePackagePrpUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, ArgusDataEntry;

type
  TframePackagePrp = class(TframePackage)
    RdeSolverTolerance: TRbwDataEntry;
    LblSolverTolerance: TLabel;
    EXTEND_TRACKING: TCheckBox;
    ComboBox1: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  framePackagePrp: TframePackagePrp;

implementation

{$R *.dfm}

end.
