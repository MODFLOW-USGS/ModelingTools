unit framePackageMf6ObsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvSpin, ModflowPackageSelectionUnit;

type
  TframePackageMf6Obs = class(TframePackage)
    comboOutputPrecision: TComboBox;
    comboOutputFormat: TComboBox;
    lblOutputFormat: TLabel;
    lblOutputPrecision: TLabel;
    lblNumberOfDigits: TLabel;
    seNumberOfDigits: TJvSpinEdit;
    procedure comboOutputFormatChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
  private
    procedure EnableOutputControls;
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageMf6Obs: TframePackageMf6Obs;

implementation



{$R *.dfm}

procedure TframePackageMf6Obs.comboOutputFormatChange(Sender: TObject);
begin
  inherited;
  EnableOutputControls;
end;

procedure TframePackageMf6Obs.EnableOutputControls;
begin
  comboOutputPrecision.Enabled := rcSelectionController.Enabled
    and (TOutputFormat(comboOutputFormat.ItemIndex) = ofBinary);
  seNumberOfDigits.Enabled := rcSelectionController.Enabled
    and (TOutputFormat(comboOutputFormat.ItemIndex) = ofText);
end;

procedure TframePackageMf6Obs.GetData(Package: TModflowPackageSelection);
var
  Obs: TMf6ObservationUtility;
begin
  inherited;
  Obs := TMf6ObservationUtility(Package);
  comboOutputFormat.ItemIndex := Ord(Obs.OutputFormat);
  comboOutputPrecision.ItemIndex := Ord(Obs.OutputPrecision);
  seNumberOfDigits.AsInteger := Obs.Digits;
end;

procedure TframePackageMf6Obs.rcSelectionControllerEnabledChange(
  Sender: TObject);
begin
  inherited;
  EnableOutputControls;
end;

procedure TframePackageMf6Obs.SetData(Package: TModflowPackageSelection);
var
  Obs: TMf6ObservationUtility;
begin
  inherited;
  Obs := TMf6ObservationUtility(Package);
  Obs.OutputFormat := TOutputFormat(comboOutputFormat.ItemIndex);
  Obs.OutputPrecision := TOutputPrecision(comboOutputPrecision.ItemIndex);
  Obs.Digits := seNumberOfDigits.AsInteger;
end;

end.
