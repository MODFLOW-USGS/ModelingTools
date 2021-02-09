unit frmRunPestUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm, Vcl.ExtCtrls;

type
  TfrmRunPest = class(TCustomExtendedDialog)
    rgRun: TRadioGroup;
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
