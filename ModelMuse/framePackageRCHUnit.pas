unit framePackageRCHUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageTransientLayerChoiceUnit, RbwController, StdCtrls,
  ExtCtrls, ModflowPackageSelectionUnit;

type
  TframePackageRCH = class(TframePackageTransientLayerChoice)
    rgAssignmentMethod: TRadioGroup;
    cbUseMultiplierMODFLOW6: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageRCH: TframePackageRCH;

implementation

uses
  GoPhastTypes;

{$R *.dfm}

{ TframePackageRCH }

procedure TframePackageRCH.GetData(Package: TModflowPackageSelection);
var
  RchPackage: TRchPackageSelection;
begin
  inherited;
  RchPackage := Package as TRchPackageSelection;
  rgAssignmentMethod.ItemIndex := Ord(RchPackage.AssignmentMethod);
  cbUseMultiplierMODFLOW6.Checked := RchPackage.UseMultiplier;
end;

procedure TframePackageRCH.SetData(Package: TModflowPackageSelection);
var
  RchPackage: TRchPackageSelection;
begin
  inherited;
  RchPackage := Package as TRchPackageSelection;
  RchPackage.AssignmentMethod := TUpdateMethod(rgAssignmentMethod.ItemIndex);
  RchPackage.UseMultiplier := cbUseMultiplierMODFLOW6.Checked;
end;

end.
