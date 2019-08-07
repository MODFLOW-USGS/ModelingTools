unit frameMt3dmsChemReactionPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, ModflowPackageSelectionUnit;

type
  TframeMt3dmsChemReactionPkg = class(TframePackage)
    comboSorptionChoice: TJvImageComboBox;
    comboKineticChoice: TJvImageComboBox;
    cbInitialConcChoice: TCheckBox;
    lblSorptionChoice: TLabel;
    lblKineticChoice: TLabel;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dmsChemReactionPkg: TframeMt3dmsChemReactionPkg;

implementation

{$R *.dfm}

{ TframeMt3dmsChemReactionPkg }

procedure TframeMt3dmsChemReactionPkg.GetData(
  Package: TModflowPackageSelection);
var
  RctPkg: TMt3dmsChemReaction;
begin
  inherited;
  RctPkg := Package as TMt3dmsChemReaction;
  comboSorptionChoice.ItemIndex := Ord(RctPkg.SorptionChoice);
  comboKineticChoice.ItemIndex := Ord(RctPkg.KineticChoice);
  cbInitialConcChoice.Checked := RctPkg.OtherInitialConcChoice = oicUse;
end;

procedure TframeMt3dmsChemReactionPkg.SetData(
  Package: TModflowPackageSelection);
var
  RctPkg: TMt3dmsChemReaction;
begin
  inherited;
  RctPkg := Package as TMt3dmsChemReaction;
  RctPkg.SorptionChoice := TSorptionChoice(comboSorptionChoice.ItemIndex);
  RctPkg.KineticChoice := TKineticChoice(comboKineticChoice.ItemIndex);
  if cbInitialConcChoice.Checked then
  begin
    RctPkg.OtherInitialConcChoice := oicUse;
  end
  else
  begin
    RctPkg.OtherInitialConcChoice := oicDontUse;
  end;
end;

end.
