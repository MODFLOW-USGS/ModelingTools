{@name defines a frame that is used
to enter data for the GMG package in MODFLOW.

@author(Richard B. Winston <rbwinst@usgs.gov>)}
unit frameGmgUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ArgusDataEntry,
  JvExStdCtrls, JvCombobox, JvListComb, ModflowPackageSelectionUnit, ComCtrls,
  JvExComCtrls, JvComCtrls, ExtCtrls;

type
  {@name is a frame that is used to enter data for the GMG package in MODFLOW.

  @member(comboGmgIadampChange @name enables controls that are only used
   when an appropriate value of IADAMP is chosen.)

  @member(comboGmgIscChange @name enables controls that are only used
   when an appropriate value of ISC is chosen.)


  }
  TframeGMG = class(TframePackage)
    pcGMG: TJvPageControl;
    tabControlAndPrint: TTabSheet;
    tabDampRelax: TTabSheet;
    rdeGmgDup: TRbwDataEntry;
    rdeGmgRelax: TRbwDataEntry;
    rdeGmgChglimit: TRbwDataEntry;
    rdeGmgDlow: TRbwDataEntry;
    lblGmgDup: TLabel;
    lblGmgDlow: TLabel;
    lblGmgChglimit: TLabel;
    lblGmgRelax: TLabel;
    lblGmgRclose: TLabel;
    rdeGmgRclose: TRbwDataEntry;
    lblGmgIiter: TLabel;
    rdeGmgIiter: TRbwDataEntry;
    lblGmgHclose: TLabel;
    rdeGmgHclose: TRbwDataEntry;
    lblGmgMxiter: TLabel;
    rdeGmgMxiter: TRbwDataEntry;
    lblGmgIoutgmg: TLabel;
    comboGmgIoutgmg: TJvImageComboBox;
    cbGmbIunitmhc: TCheckBox;
    lblGmgIsm: TLabel;
    comboGmgIsm: TJvImageComboBox;
    lblGmgIadamp: TLabel;
    comboGmgIadamp: TJvImageComboBox;
    lblGmgIsc: TLabel;
    comboGmgIsc: TJvImageComboBox;
    lblGmgDamp: TLabel;
    rdeGmgDamp: TRbwDataEntry;
    procedure comboGmgIadampChange(Sender: TObject);
    procedure comboGmgIscChange(Sender: TObject);
  private
  { Private declarations }
  protected
    {@name calls @link(comboGmgIadampChange)
    and @link(comboGmgIscChange).}
    procedure SetSelected(const Value: boolean); override;

    {@name moves controls defined in @link(TframePackage)
    to the first tab of @link(pcGMG).}
    procedure Loaded; override;
  public
    // @name copies the data from Package to the controls in @classname.
    procedure GetData(Package: TModflowPackageSelection); override;
    // @name copies the data from the controls in @classname to Package.
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameGMG: TframeGMG;

implementation

{$R *.dfm}

{ TframeGMG }

procedure TframeGMG.comboGmgIadampChange(Sender: TObject);
var
  ShouldEnable : boolean;
begin
  inherited;
  ShouldEnable := Selected and (comboGmgIadamp.ItemIndex = 2);
  rdeGmgDlow.Enabled := ShouldEnable;
  rdeGmgDup.Enabled := ShouldEnable;
  rdeGmgChglimit.Enabled := ShouldEnable;
end;

procedure TframeGMG.comboGmgIscChange(Sender: TObject);
begin
  inherited;
  rdeGmgRelax.Enabled := Selected and (comboGmgIsc.ItemIndex = 4);
end;

procedure TframeGMG.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TGmgPackageSelection;
begin
  inherited GetData(Package);
  SourcePkg := Package as TGmgPackageSelection;
  rdeGmgRclose.Text := FloatToStr(SourcePkg.RCLOSE.Value);
  rdeGmgIiter.Text := IntToStr(SourcePkg.IITER);
  rdeGmgHclose.Text := FloatToStr(SourcePkg.HCLOSE.Value);
  rdeGmgDamp.Text := FloatToStr(SourcePkg.DAMP.Value);
  comboGmgIadamp.ItemIndex:= SourcePkg.IADAMP;
  comboGmgIoutgmg.ItemIndex:= SourcePkg.IOUTGMG;
  comboGmgIsm.ItemIndex:= SourcePkg.ISM;
  comboGmgIsc.ItemIndex:= SourcePkg.ISC;
  cbGmbIunitmhc.Checked := SourcePkg.IUNITMHC;
  rdeGmgDlow.Text := FloatToStr(SourcePkg.DLOW.Value);
  rdeGmgDup.Text := FloatToStr(SourcePkg.DUP.Value);
  rdeGmgChglimit.Text := FloatToStr(SourcePkg.CHGLIMIT.Value);
  rdeGmgRelax.Text := FloatToStr(SourcePkg.RELAX.Value);
  rdeGmgMxiter.Text := IntToStr(SourcePkg.MXITER);
  comboGmgIadampChange(nil);
  comboGmgIscChange(nil);
end;

procedure TframeGMG.Loaded;
begin
  inherited;

end;

procedure TframeGMG.SetData(Package: TModflowPackageSelection);
var
  SourcePkg: TGmgPackageSelection;
begin
  inherited SetData(Package);
  SourcePkg := Package as TGmgPackageSelection;
  SourcePkg.RCLOSE.Value := StrToFloat(rdeGmgRclose.Text);
  SourcePkg.IITER := StrToInt(rdeGmgIiter.Text);
  SourcePkg.HCLOSE.Value := StrToFloat(rdeGmgHclose.Text);
  SourcePkg.DAMP.Value := StrToFloat(rdeGmgDamp.Text);
  SourcePkg.IADAMP := comboGmgIadamp.ItemIndex;
  SourcePkg.IOUTGMG := comboGmgIoutgmg.ItemIndex;
  SourcePkg.ISM := comboGmgIsm.ItemIndex;
  SourcePkg.ISC := comboGmgIsc.ItemIndex;
  SourcePkg.IUNITMHC := cbGmbIunitmhc.Checked;
  SourcePkg.DLOW.Value := StrToFloat(rdeGmgDlow.Text);
  SourcePkg.DUP.Value := StrToFloat(rdeGmgDup.Text);
  SourcePkg.CHGLIMIT.Value := StrToFloat(rdeGmgChglimit.Text);
  SourcePkg.RELAX.Value := StrToFloat(rdeGmgRelax.Text);
  SourcePkg.MXITER := StrToInt(rdeGmgMxiter.Text);
end;

procedure TframeGMG.SetSelected(const Value: boolean);
begin
  inherited SetSelected(Value);
  comboGmgIadampChange(nil);
  comboGmgIscChange(nil);
end;

end.
