unit frmRunPestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm;

type
  TfrmRunPest = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunPest: TfrmRunPest;

implementation

{$R *.dfm}

end.
