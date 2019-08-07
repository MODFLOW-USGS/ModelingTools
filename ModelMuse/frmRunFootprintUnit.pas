unit frmRunFootprintUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomExtendedDialogForm, Vcl.StdCtrls;

type
  TfrmRunFootprint = class(TCustomExtendedDialog)
    cbRun: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRunFootprint: TfrmRunFootprint;

implementation

{$R *.dfm}

end.
