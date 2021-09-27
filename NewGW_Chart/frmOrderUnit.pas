unit frmOrderUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfrmOrder = class(TForm)
    rgOrder: TRadioGroup;
    BitBtn1: TBitBtn;
    rgPointOrder: TRadioGroup;
    rgWhatToPlot: TRadioGroup;
    procedure rgWhatToPlotClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOrder: TfrmOrder;

implementation

{$R *.DFM}

procedure TfrmOrder.rgWhatToPlotClick(Sender: TObject);
begin
  rgOrder.Enabled := rgWhatToPlot.ItemIndex = 0;
  rgPointOrder.Enabled := rgOrder.Enabled;
end;

end.
