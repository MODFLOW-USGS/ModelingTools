unit frmRunMt3dmsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CustomExtendedDialogForm, StdCtrls;

type
  TfrmRunMt3dms = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
    comboMt3dModelSelection: TComboBox;
    lblMt3dModelSelection: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunMt3dms: TfrmRunMt3dms;

implementation

{$R *.dfm}

end.
