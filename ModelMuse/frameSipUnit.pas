unit frameSipUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, ArgusDataEntry,
  JvExStdCtrls, JvCombobox, JvListComb, ModflowPackageSelectionUnit;

type
  TframeSIP = class(TframePackage)
    lblSipMxiter: TLabel;
    rdeSipMxiter: TRbwDataEntry;
    lblSipNparm: TLabel;
    rdeSipNparm: TRbwDataEntry;
    lblSipAccl: TLabel;
    rdeSipAccl: TRbwDataEntry;
    lblSipHclose: TLabel;
    rdeSipHclose: TRbwDataEntry;
    lblSipIpcalc: TLabel;
    comboSipIpcalc: TJvImageComboBox;
    lblSipWseed: TLabel;
    rdeSipWseed: TRbwDataEntry;
    lblSipIprsip: TLabel;
    rdeSipIprsip: TRbwDataEntry;
    procedure comboSipIpcalcChange(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure SetSelected(const Value: boolean); override;
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  frameSIP: TframeSIP;

implementation

{$R *.dfm}

procedure TframeSIP.comboSipIpcalcChange(Sender: TObject);
begin
  inherited;
  rdeSipWseed.Enabled := Selected and (comboSipIpcalc.ItemIndex = 0);
end;

procedure TframeSIP.GetData(Package: TModflowPackageSelection);
var
  SourcePkg: TSipPackageSelection;
begin
  inherited GetData(Package);
  SourcePkg := Package as TSipPackageSelection;
  rdeSipMxiter.Text := IntToStr(SourcePkg.MXITER);
  rdeSipNparm.Text := IntToStr(SourcePkg.NPARM);
  rdeSipAccl.Text := FloatToStr(SourcePkg.ACCL.Value);
  rdeSipHclose.Text := FloatToStr(SourcePkg.HClose.Value);
  comboSipIpcalc.ItemIndex := SourcePkg.IPCALC;
  rdeSipWseed.Text := FloatToStr(SourcePkg.WSEED.Value);
  rdeSipIprsip.Text := IntToStr(SourcePkg.IPRSIP);
  comboSipIpcalcChange(nil);
end;

procedure TframeSIP.SetData(Package: TModflowPackageSelection);
var
  SourcePkg: TSipPackageSelection;
begin
  inherited SetData(Package);
  SourcePkg := Package as TSipPackageSelection;
  SourcePkg.MXITER := StrToInt(rdeSipMxiter.Text);
  SourcePkg.NPARM := StrToInt(rdeSipNparm.Text);
  SourcePkg.ACCL.Value := StrToFloat(rdeSipAccl.Text);
  SourcePkg.HClose.Value := StrToFloat(rdeSipHclose.Text);
  SourcePkg.IPCALC := comboSipIpcalc.ItemIndex;
  SourcePkg.WSEED.Value := StrToFloat(rdeSipWseed.Text);
  SourcePkg.IPRSIP := StrToInt(rdeSipIprsip.Text);

end;

procedure TframeSIP.SetSelected(const Value: boolean);
begin
  inherited;
  comboSipIpcalcChange(nil);
end;

end.
