{@name defines a frame that is used to define the input for the DE4 package
 in MODFLOW.}
unit frameDe4Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, StdCtrls, ArgusDataEntry, RbwController,
  JvExStdCtrls, JvCombobox, JvListComb, ModflowPackageSelectionUnit;

type
  {@name is a frame that is used to define the input for the DE4 package
   in MODFLOW.}
  TframeDE4 = class(TframePackage)
    lblDe4Itmx: TLabel;
    rdeDe4Itmx: TRbwDataEntry;
    lblDe4Mxup: TLabel;
    rdeDe4Mxup: TRbwDataEntry;
    rdeDe4Mxlow: TRbwDataEntry;
    lblDe4Mxlow: TLabel;
    lblDe4Mxbw: TLabel;
    rdeDe4Mxbw: TRbwDataEntry;
    comboDe4Ifreq: TJvImageComboBox;
    lblDe4Ifreq: TLabel;
    lblDe4Mutd4: TLabel;
    comboDe4Mutd4: TJvImageComboBox;
    lblDe4Accl: TLabel;
    rdeDe4Accl: TRbwDataEntry;
    lblDe4Hclose: TLabel;
    rdeDe4Hclose: TRbwDataEntry;
    rdeRdeIprd4: TRbwDataEntry;
    lblRdeIprd4: TLabel;
  private
    { Private declarations }
  public
    // @name copies the data from Package into the controls of @classname.
    procedure GetData(Package: TModflowPackageSelection); override;
    // @name copies the data from the controls of @classname into Package.
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameDE4: TframeDE4;

implementation

{$R *.dfm}

{ TframeDE4 }

procedure TframeDE4.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TDE4PackageSelection;
begin
  inherited GetData(Package);
  SourcePkg := Package as TDE4PackageSelection;
  rdeDe4Itmx.Text := IntToStr(SourcePkg.ITMX);
  rdeDe4Mxup.Text := IntToStr(SourcePkg.MXUP);
  rdeDe4Mxlow.Text := IntToStr(SourcePkg.MXLOW);
  // MXBW is actually the maximum bandwidth PLUS ONE.
  // If MXBW = 0, the value is calculated automatically.
  if SourcePkg.MXBW = 0 then
  begin
    rdeDe4Mxbw.Text := '0'
  end
  else
  begin
    rdeDe4Mxbw.Text := IntToStr(SourcePkg.MXBW-1);
  end;
  comboDe4Ifreq.ItemIndex := SourcePkg.IFREQ-1;
  comboDe4Mutd4.ItemIndex := SourcePkg.MUTD4;
  rdeDe4Accl.Text := FloatToStr(SourcePkg.ACCL.Value);
  rdeDe4Hclose.Text := FloatToStr(SourcePkg.HCLOSE.Value);
  rdeRdeIprd4.Text := IntToStr(SourcePkg.IPRD4);
end;

procedure TframeDE4.SetData(Package: TModflowPackageSelection);
var
  SourcePkg: TDE4PackageSelection;
  MXBW: integer;
begin
  inherited SetData(Package);
  SourcePkg := Package as TDE4PackageSelection;
  SourcePkg.ITMX := StrToInt(rdeDe4Itmx.Text);
  SourcePkg.MXUP := StrToInt(rdeDe4Mxup.Text);
  SourcePkg.MXLOW := StrToInt(rdeDe4Mxlow.Text);
  // MXBW is actually the maximum bandwidth PLUS ONE.
  // If MXBW = 0, the value is calculated automatically.
  MXBW := StrToInt(rdeDe4Mxbw.Text);
  if MXBW > 0 then
  begin
    Inc(MXBW);
  end;
  SourcePkg.MXBW := MXBW;
  SourcePkg.IFREQ := comboDe4Ifreq.ItemIndex+1;
  SourcePkg.MUTD4 := comboDe4Mutd4.ItemIndex;
  SourcePkg.ACCL.Value := StrToFloat(rdeDe4Accl.Text);
  SourcePkg.HCLOSE.Value := StrToFloat(rdeDe4Hclose.Text);
  SourcePkg.IPRD4 := StrToInt(rdeRdeIprd4.Text);
end;

end.
