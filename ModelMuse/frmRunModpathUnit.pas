unit frmRunModpathUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm;

type
  TfrmRunModpath = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
    cbForceCBF: TCheckBox;
    comboModelSelection: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunModpath: TfrmRunModpath;

implementation

{$R *.dfm}

end.
