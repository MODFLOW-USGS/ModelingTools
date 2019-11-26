unit frmChartTypeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmChartType = class(TForm)
    rgChartType: TRadioGroup;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure rgChartTypeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChartType: TfrmChartType;

implementation

{$R *.dfm}

procedure TfrmChartType.rgChartTypeClick(Sender: TObject);
begin
  btnOK.Enabled := rgChartType.ItemIndex >= 0;
end;

end.
