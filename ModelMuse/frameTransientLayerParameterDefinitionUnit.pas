unit frameTransientLayerParameterDefinitionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameListParameterDefinitionUnit, Grids, RbwDataGrid4, StdCtrls,
  Mask, JvExMask, JvSpin, Buttons, ExtCtrls, ModflowPackageSelectionUnit;

type
  TframeTransientLayerParameterDefinition = class(TframeListParameterDefinition)
    pnLayerOption: TPanel;
    lblLayerOption: TLabel;
    comboLayerOption: TComboBox;
    cbTimeVaryingLayers: TCheckBox;
  private
    { Private declarations }
  protected
    procedure SetEnabled(Value: boolean); override;
  public
//    procedure UpdateChildControlsEnabled(Value: Boolean); override;
    procedure SetLayerChoice(LayerPackageSelection: TLayerPackageSelection);
    procedure GetLayerChoice(LayerPackageSelection: TLayerPackageSelection);
    { Public declarations }
  end;

var
  frameTransientLayerParameterDefinition: TframeTransientLayerParameterDefinition;

implementation

{$R *.dfm}

{ TframeTransientLayerParameterDefinition }

procedure TframeTransientLayerParameterDefinition.GetLayerChoice(
  LayerPackageSelection: TLayerPackageSelection);
begin
  comboLayerOption.ItemIndex := Ord(LayerPackageSelection.LayerOption);
  if Assigned(comboLayerOption.OnChange) then
  begin
   comboLayerOption.OnChange(comboLayerOption);
  end;
  cbTimeVaryingLayers.Checked :=
    LayerPackageSelection.TimeVaryingLayers;
end;


procedure TframeTransientLayerParameterDefinition.SetEnabled(Value: boolean);
begin
  inherited;
  if not (csDestroying in ComponentState) then
  begin
    comboLayerOption.Enabled := Value;
    if Assigned(comboLayerOption.OnChange) then
    begin
      comboLayerOption.OnChange(comboLayerOption);
    end;
  end;
end;

procedure TframeTransientLayerParameterDefinition.SetLayerChoice(
  LayerPackageSelection: TLayerPackageSelection);
begin
  LayerPackageSelection.LayerOption :=
    TLayerOption(comboLayerOption.ItemIndex);
  LayerPackageSelection.TimeVaryingLayers :=
    cbTimeVaryingLayers.Checked;
end;

//procedure TframeTransientLayerParameterDefinition.UpdateChildControlsEnabled(
//  Value: Boolean);
//begin
//  inherited;
//  if not (csDestroying in ComponentState) then
//  begin
//    comboLayerOption.Enabled := Value;
//    if Assigned(comboLayerOption.OnChange) then
//    begin
//      comboLayerOption.OnChange(comboLayerOption);
//    end;
//  end;
//end;

end.
