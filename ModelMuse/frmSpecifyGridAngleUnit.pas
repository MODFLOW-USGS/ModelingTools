unit frmSpecifyGridAngleUnit;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, frmCustomGoPhastUnit, QButtons, clxDataEntry;

type
  TfrmSpecifyGridAngle = class(TfrmCustomGoPhast)
    cbSpecifyGridAngle: TCheckBox;
    adeGridAngle: TArgusDataEntry;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    btnOK: TBitBtn;
    cbSmoothGrid: TCheckBox;
    cbColumns: TCheckBox;
    cbRows: TCheckBox;
    cbLayers: TCheckBox;
    lblCriterion: TLabel;
    adeCriterion: TArgusDataEntry;
    procedure cbSpecifyGridAngleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure cbSmoothGridClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSpecifyGridAngle: TfrmSpecifyGridAngle;

implementation

{$R *.dfm}

uses Math, frmGoPhastUnit, GridGeneration;

procedure TfrmSpecifyGridAngle.cbSpecifyGridAngleClick(Sender: TObject);
begin
  inherited;
  adeGridAngle.Enabled := not cbSpecifyGridAngle.Checked;
end;

procedure TfrmSpecifyGridAngle.FormCreate(Sender: TObject);
begin
  inherited;
  adeGridAngle.Text := FloatToStr(RadToDeg(frmGoPhast.PhastGrid.GridAngle));
end;

procedure TfrmSpecifyGridAngle.cbSmoothGridClick(Sender: TObject);
begin
  inherited;
  cbColumns.Enabled := cbSmoothGrid.Checked;
  cbRows.Enabled := cbSmoothGrid.Checked;
  cbLayers.Enabled := cbSmoothGrid.Checked;
  adeCriterion.Enabled := cbSmoothGrid.Checked;
  lblCriterion.Enabled := cbSmoothGrid.Checked;
end;

procedure TfrmSpecifyGridAngle.btnOKClick(Sender: TObject);
var
  ErrorMessage: string;
begin
  inherited;
  if not GenerateGrid(ErrorMessage, not cbSpecifyGridAngle.Checked,
    DegToRad(strToFloat(adeGridAngle.Text)),
    cbSmoothGrid.Checked and cbColumns.Checked,
    cbSmoothGrid.Checked and cbRows.Checked,
    cbSmoothGrid.Checked and cbLayers.Checked,
    StrToFloat(adeCriterion.Text)) then
  begin
    Beep;
    MessageDlg(ErrorMessage, mtWarning, [mbOK], 0);
  end

end;

end.

