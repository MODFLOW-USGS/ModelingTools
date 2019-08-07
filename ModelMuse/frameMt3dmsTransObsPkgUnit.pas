unit frameMt3dmsTransObsPkgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ArgusDataEntry,
  JvExStdCtrls, JvCombobox, JvListComb, ModflowPackageSelectionUnit;

type
  TframeMt3dmsTransObsPkg = class(TframePackage)
    cbSaveBinary: TCheckBox;
    grpbxConcentrationObservations: TGroupBox;
    rdeConcScaleFactor: TRbwDataEntry;
    lblConcScaleFactor: TLabel;
    lblSaveType: TLabel;
    comboSaveConcType: TJvImageComboBox;
    cbLogTransform: TCheckBox;
    cbInterpolate: TCheckBox;
    grpbxMassFluxObservations: TGroupBox;
    lblMassFluxScaleFactor: TLabel;
    rdeMassFluxScaleFactor: TRbwDataEntry;
    lblSaveMassFluxType: TLabel;
    comboSaveMassFluxType: TJvImageComboBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameMt3dmsTransObsPkg: TframeMt3dmsTransObsPkg;

implementation

{$R *.dfm}

{ TframeMt3dmsTransObsPkg }

procedure TframeMt3dmsTransObsPkg.GetData(Package: TModflowPackageSelection);
var
  Pkg: TMt3dmsTransportObservations;
begin
  inherited;
  Pkg := Package as TMt3dmsTransportObservations;
  rdeConcScaleFactor.Text := FloatToStr(Pkg.ConcScaleFactor);
  rdeMassFluxScaleFactor.Text := FloatToStr(Pkg.FluxScaleFactor);
  case Pkg.SaveBinary of
    sbNoSave: cbSaveBinary.Checked := False;
    sbSave: cbSaveBinary.Checked := True;
    else Assert(False);
  end;
  comboSaveConcType.ItemIndex := Ord(Pkg.ConcObsResult);
  comboSaveMassFluxType.ItemIndex := Ord(Pkg.MassFluxObsResult);
  case Pkg.TransformType of
    ltNoConversion: cbLogTransform.Checked := False;
    ltLogConverion: cbLogTransform.Checked := True;
    else Assert(False);
  end;
  case Pkg.InterpolateObs of
    ioNoInterpolation: cbInterpolate.Checked := False;
    ioBilinear: cbInterpolate.Checked := True;
    else Assert(False);
  end;
end;

procedure TframeMt3dmsTransObsPkg.SetData(Package: TModflowPackageSelection);
var
  Pkg: TMt3dmsTransportObservations;
begin
  inherited;
  Pkg := Package as TMt3dmsTransportObservations;
  Pkg.ConcScaleFactor := StrToFloat(rdeConcScaleFactor.Text);
  Pkg.FluxScaleFactor := StrToFloat(rdeMassFluxScaleFactor.Text);

  if cbSaveBinary.Checked then
  begin
    Pkg.SaveBinary := sbSave;
  end
  else
  begin
    Pkg.SaveBinary := sbNoSave;
  end;

  Pkg.ConcObsResult := TConcObsResult(comboSaveConcType.ItemIndex);
  Pkg.MassFluxObsResult := TMassFluxObsResult(comboSaveMassFluxType.ItemIndex);

  if cbLogTransform.Checked then
  begin
    Pkg.TransformType := ltLogConverion;
  end
  else
  begin
    Pkg.TransformType := ltNoConversion;
  end;

  if cbInterpolate.Checked then
  begin
    Pkg.InterpolateObs := ioBilinear;
  end
  else
  begin
    Pkg.InterpolateObs := ioNoInterpolation;
  end;
end;

end.
