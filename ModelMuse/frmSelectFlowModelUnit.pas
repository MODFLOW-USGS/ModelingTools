unit frmSelectFlowModelUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons;

type
  TfrmSelectFlowModel = class(TfrmCustomGoPhast)
    rgFlowModels: TRadioGroup;
    Panel1: TPanel;
    Label1: TLabel;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

end.
