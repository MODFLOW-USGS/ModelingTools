unit frameScreenObjectSsmUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frameScreenObjectNoParamUnit, StdCtrls, Grids, RbwDataGrid4,
  ArgusDataEntry, Buttons, Mask, JvExMask, JvSpin, ExtCtrls;

type
  TframeScreenObjectSsm = class(TframeScreenObjectNoParam)
    cbSpecifiedConcentration: TCheckBox;
    cbMassLoading: TCheckBox;
    procedure cbSpecifiedConcentrationClick(Sender: TObject);
    procedure cbMassLoadingClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameScreenObjectSsm: TframeScreenObjectSsm;

implementation

{$R *.dfm}

procedure TframeScreenObjectSsm.cbMassLoadingClick(Sender: TObject);
begin
  inherited;
  if cbMassLoading.AllowGrayed then
  begin
    cbMassLoading.AllowGrayed := False;
  end;
  if cbMassLoading.Checked then
  begin
    cbSpecifiedConcentration.Checked := False;
  end;
end;

procedure TframeScreenObjectSsm.cbSpecifiedConcentrationClick(Sender: TObject);
begin
  inherited;
  if cbSpecifiedConcentration.AllowGrayed then
  begin
    cbSpecifiedConcentration.AllowGrayed := False;
  end;
  if cbSpecifiedConcentration.Checked then
  begin
    cbMassLoading.Checked := False;
  end;
end;

end.
