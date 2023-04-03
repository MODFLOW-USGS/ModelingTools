{@name defines a frame that is used to provide a way to specify parameters
 where the user
 must specify whether or not to use Multiplier arrays and Zone arrays.
 @name is used in @link(TfrmModflowPackages) for the LPF package.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frameArrayParameterDefinitionUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameListParameterDefinitionUnit, StdCtrls, ArgusDataEntry, Grids,
  RbwDataGrid4, Mask, JvExMask, JvSpin, Buttons, ExtCtrls, frmCustomGoPhastUnit;

type
  {@name is used to provide a way to specify parameters where the user
   must specify whether or not to use Multiplier arrays and Zone arrays.
   @name is used in @link(TfrmModflowPackages) for the LPF package.

   @member(pnlTop @name is a panel at the top of the frame that
   holds other controls.)

   @member(lblParamValue @name is a label for @link(rdeParamValue).)

   @member(rdeParamValue is used for editing the values of multiple parameters
   at one time.)

   @member(cbUseZone @name is used to edit the choice to use zone arrays in
   multiple parameters at one time.)

   @member(cbUseMultiplier @name is used to edit the choice to use
   multiplier arrays in multiple parameters at one time.)

   @member(rdeParamValueChange @name changes the values of multiple parameters
   at one time.)

   @member(cbUseZoneClick @name changes the choice to use zone arrays in
   multiple parameters at one time.)

   @member(cbUseMultiplierClick @name changes the choice to use
   multiplier arrays in multiple parameters at one time.)

   @member(dgParametersColSize @name calls @link(ArrangeMultiEditControls).)

   @member(dgParametersHorizontalScroll @name calls
   @link(ArrangeMultiEditControls).)

   @member(dgParametersMouseUp @name is used to enable or disable the control
   that are used to edit properties of multiple parameters at one time.)
  }
  TframeArrayParameterDefinition = class(TframeListParameterDefinition)
    pnlTop: TPanel;
    lblParamValue: TLabel;
    rdeParamValue: TRbwDataEntry;
    cbUseZone: TCheckBox;
    cbUseMultiplier: TCheckBox;
    procedure rdeParamValueChange(Sender: TObject);
    procedure cbUseZoneClick(Sender: TObject);
    procedure cbUseMultiplierClick(Sender: TObject);
    procedure dgParametersColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgParametersHorizontalScroll(Sender: TObject);
    procedure dgParametersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    // @name places the controls used to edit multiple parameters at once
    // at the correct locations.
    procedure ArrangeMultiEditControls;
  protected
    // @name disables the controls used to edit more than
    // a single parameter at once if Value is @false.
    procedure SetEnabled(Value: boolean); override;
    { Private declarations }
  public
    procedure Loaded; override;
    { Public declarations }
  end;

//var
//  frameArrayParameterDefinition: TframeArrayParameterDefinition;

implementation



{$R *.dfm}

procedure TframeArrayParameterDefinition.ArrangeMultiEditControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  LayoutControls(dgParameters, rdeParamValue, lblParamValue, Ord(pcValue));
  LayoutControls(dgParameters, cbUseZone, nil, Ord(pcUseZone), 5);
  LayoutControls(dgParameters, cbUseMultiplier, nil, Ord(pcUseMultiplier), 5);
end;

procedure TframeArrayParameterDefinition.cbUseMultiplierClick(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsStateInColumn(dgParameters, Ord(pcUseMultiplier),
    cbUseMultiplier.State);
end;

procedure TframeArrayParameterDefinition.cbUseZoneClick(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsStateInColumn(dgParameters, Ord(pcUseZone),
    cbUseZone.State);

end;

procedure TframeArrayParameterDefinition.dgParametersColSize(Sender: TObject;
  ACol, PriorWidth: Integer);
begin
  inherited;
  ArrangeMultiEditControls;
end;

procedure TframeArrayParameterDefinition.dgParametersHorizontalScroll(
  Sender: TObject);
begin
  inherited;
  ArrangeMultiEditControls;
end;

procedure TframeArrayParameterDefinition.dgParametersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(dgParameters, rdeParamValue, Ord(pcValue));
  EnableMultiEditControl(dgParameters, cbUseZone, Ord(pcUseZone));
  EnableMultiEditControl(dgParameters, cbUseMultiplier, Ord(pcUseMultiplier));
  lblParamValue.Enabled := rdeParamValue.Enabled;
end;

procedure TframeArrayParameterDefinition.Loaded;
begin
  inherited;
  lblParamValue.Caption := dgParameters.Cells[Ord(pcValue),0];
  cbUseZone.Caption := dgParameters.Cells[Ord(pcUseZone),0];
  cbUseMultiplier.Caption := dgParameters.Cells[Ord(pcUseMultiplier),0];
end;

procedure TframeArrayParameterDefinition.rdeParamValueChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgParameters, Ord(pcValue), rdeParamValue.Text);
end;

procedure TframeArrayParameterDefinition.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  if not Value then
  begin
    rdeParamValue.Enabled := False;
    lblParamValue.Enabled := False;
    cbUseZone.Enabled := False;
    cbUseMultiplier.Enabled := False;
  end;
end;

end.
