// @name defines a frame used to define the input for the
// Evapotranspiration-Segments package.
unit frameEtsPackageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageTransientLayerChoiceUnit, StdCtrls, ExtCtrls,
  Mask, JvExMask, JvSpin, ModflowPackageSelectionUnit, RbwController;

type
  {@name is a frame used to define the input for the
  Evapotranspiration-Segments package.

  @member(seSegments @name is the number of segments to be defined.)
  
  @member(lblSegments @name is a label for @link(seSegments).)
  }
  TframeEtsPackage = class(TframePackageTransientLayerChoice)
    seSegments: TJvSpinEdit;
    lblSegments: TLabel;
    cbUseMultiplierMODFLOW6: TCheckBox;
  private
    { Private declarations }
  public
    // @name copies the data from Package into the controls of @classname.
    procedure GetData(Package: TModflowPackageSelection); override;
    // @name copies the data from the controls of @classname into Package.
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

//var
//  frameEtsPackage: TframeEtsPackage;

implementation

{$R *.dfm}

{ TframeEtsPackage }

procedure TframeEtsPackage.GetData(Package: TModflowPackageSelection);
var
  EtsPackage: TEtsPackageSelection;
begin
  inherited GetData(Package);
  EtsPackage := Package as TEtsPackageSelection;
  seSegments.AsInteger := EtsPackage.SegmentCount;
  cbUseMultiplierMODFLOW6.Checked := EtsPackage.UseMultiplier;
end;

procedure TframeEtsPackage.SetData(Package: TModflowPackageSelection);
var
  EtsPackage: TEtsPackageSelection;
begin
  inherited SetData(Package);
  EtsPackage := Package as TEtsPackageSelection;
  EtsPackage.SegmentCount := seSegments.AsInteger;
  EtsPackage.UseMultiplier := cbUseMultiplierMODFLOW6.Checked;
end;

end.
