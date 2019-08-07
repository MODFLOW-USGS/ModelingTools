unit framePackageLayerChoiceUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ExtCtrls,
  ModflowPackageSelectionUnit;

type
  TframePackageLayerChoice = class(TframePackage)
    pnLayerOption: TPanel;
    lblLayerOption: TLabel;
    comboLayerOption: TComboBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageLayerChoice: TframePackageLayerChoice;

implementation

{$R *.dfm}

{ TframePackageLayerChoice }

procedure TframePackageLayerChoice.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TCustomLayerPackageSelection;
begin

  inherited GetData(Package);
  SourcePkg := Package as TCustomLayerPackageSelection;
  comboLayerOption.ItemIndex := Ord(SourcePkg.LayerOption);
  if Assigned(comboLayerOption.OnChange) then
  begin
    comboLayerOption.OnChange(comboLayerOption);
  end;
end;

procedure TframePackageLayerChoice.SetData(Package: TModflowPackageSelection);
var
  SourcePkg: TCustomLayerPackageSelection;
begin
  inherited SetData(Package);
  SourcePkg := Package as TCustomLayerPackageSelection;
  SourcePkg.LayerOption := TLayerOption(comboLayerOption.ItemIndex);
end;

end.
