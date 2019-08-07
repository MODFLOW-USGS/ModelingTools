unit framePackageHufUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ModflowPackageSelectionUnit,
  ExtCtrls;

type
  TframePackageHuf = class(TframePackage)
    cbSaveHeads: TCheckBox;
    cbSaveFlows: TCheckBox;
    rgElevationSurfaceChoice: TRadioGroup;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageHuf: TframePackageHuf;

implementation

{$R *.dfm}

{ TframePackageHuf }

procedure TframePackageHuf.GetData(Package: TModflowPackageSelection);
var
  HufPkg: THufPackageSelection;
begin
  inherited;
  HufPkg := Package as THufPackageSelection;
  cbSaveHeads.Checked := HufPkg.SaveHeads;
  cbSaveFlows.Checked := HufPkg.SaveFlows;
  rgElevationSurfaceChoice.ItemIndex := Ord(HufPkg.ReferenceChoice);
end;

procedure TframePackageHuf.SetData(Package: TModflowPackageSelection);
var
  HufPkg: THufPackageSelection;
begin
  inherited;
  HufPkg := Package as THufPackageSelection;
  HufPkg.SaveHeads := cbSaveHeads.Checked;
  HufPkg.SaveFlows := cbSaveFlows.Checked;
  HufPkg.ReferenceChoice := THufReferenceChoice(
    rgElevationSurfaceChoice.ItemIndex);
end;

end.
