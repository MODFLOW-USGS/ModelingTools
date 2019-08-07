unit framePackageRCHUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageTransientLayerChoiceUnit, RbwController, StdCtrls,
  ExtCtrls, ModflowPackageSelectionUnit;

type
  TframePackageRCH = class(TframePackageTransientLayerChoice)
    rgAssignmentMethod: TRadioGroup;
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
begin
  inherited;
  rgAssignmentMethod.ItemIndex :=
    Ord((Package as TRchPackageSelection).AssignmentMethod);
end;

procedure TframePackageRCH.SetData(Package: TModflowPackageSelection);
begin
  inherited;
  (Package as TRchPackageSelection).AssignmentMethod :=
     TUpdateMethod(rgAssignmentMethod.ItemIndex);
end;

end.
