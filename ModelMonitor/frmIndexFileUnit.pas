unit frmIndexFileUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, CustomExtendedDialogForm, Vcl.StdCtrls;

type
  TfrmIndexFile = class(TCustomExtendedDialog)
    cbIndex: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmIndexFile: TfrmIndexFile;

implementation

{$R *.dfm}

end.
