// @name defines a frame used to define the input for the
// Evapotranspiration-Segments package.
unit frameEtsPackageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageTransientLayerChoiceUnit, StdCtrls, ExtCtrls, JvExStdCtrls,
  JvCheckBox, Mask, JvExMask, JvSpin, ModflowPackageSelectionUnit, RbwController;

type
  {@name is a frame used to define the input for the
  Evapotranspiration-Segments package.

  @member(seSegments @name is the number of segments to be defined.)
  
  @member(lblSegments @name is a label for @link(seSegments).)
  }
  TframeEtsPackage = class(TframePackageTransientLayerChoice)
    seSegments: TJvSpinEdit;
    lblSegments: TLabel;
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
begin
  inherited GetData(Package);
  seSegments.AsInteger := (Package as TEtsPackageSelection).SegmentCount;
end;

procedure TframeEtsPackage.SetData(Package: TModflowPackageSelection);
begin
  inherited SetData(Package);
  (Package as TEtsPackageSelection).SegmentCount := seSegments.AsInteger;
end;

end.
