unit framePackageTransientLayerChoiceUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, framePackageLayerChoiceUnit, StdCtrls, JvExStdCtrls, JvCheckBox, ExtCtrls,
  ModflowPackageSelectionUnit, RbwController;

type
  TframePackageTransientLayerChoice = class(TframePackageLayerChoice)
    cbTimeVaryingLayers: TCheckBox;

    procedure comboLayerOptionChange(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetSelected(const Value: boolean); override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

//var
//  framePackageLayerChoice: TframePackageLayerChoice;

implementation

{$R *.dfm}

{ TframePackageLayerChoice }

procedure TframePackageTransientLayerChoice.comboLayerOptionChange(Sender: TObject);
begin
  inherited;
  cbTimeVaryingLayers.Enabled := cbTimeVaryingLayers.Visible
    and Selected
    and (comboLayerOption.ItemIndex = 1);
end;

procedure TframePackageTransientLayerChoice.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TCustomTransientLayerPackageSelection;
begin

  inherited GetData(Package);
  SourcePkg := Package as TCustomTransientLayerPackageSelection;
//  comboLayerOption.ItemIndex := Ord(SourcePkg.LayerOption);
//  if Assigned(comboLayerOption.OnChange) then
//  begin
//   comboLayerOption.OnChange(comboLayerOption);
//  end;
  cbTimeVaryingLayers.Checked := SourcePkg.TimeVaryingLayers;
end;

procedure TframePackageTransientLayerChoice.SetData(Package: TModflowPackageSelection);
var
  SourcePkg: TCustomTransientLayerPackageSelection;
begin
  inherited SetData(Package);
  SourcePkg := Package as TCustomTransientLayerPackageSelection;
//  SourcePkg.LayerOption := TLayerOption(comboLayerOption.ItemIndex);
  SourcePkg.TimeVaryingLayers := cbTimeVaryingLayers.Checked;
end;

procedure TframePackageTransientLayerChoice.SetSelected(const Value: boolean);
begin
  inherited;
  comboLayerOptionChange(nil);
end;

end.
