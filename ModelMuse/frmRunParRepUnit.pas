unit frmRunParRepUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm;

type
  TfrmRunParRep = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunParRep: TfrmRunParRep;

implementation

{$R *.dfm}

end.
