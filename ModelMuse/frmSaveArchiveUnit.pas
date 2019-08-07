unit frmSaveArchiveUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm;

type
  TfrmSaveArchive = class(TCustomExtendedDialog)
    cbSaveArchive: TCheckBox;
    cbSaveDataSetValues: TCheckBox;
    { Private declarations }
  public
    procedure SetSaveEnabled(enable: boolean);
    { Public declarations }
  end;

var
  frmSaveArchive: TfrmSaveArchive;

implementation

{$R *.dfm}

procedure TfrmSaveArchive.SetSaveEnabled(enable: boolean);
begin
  // set enabled state of Save button.
  enablewindow( GetDlgItem( getparent( FDlg.handle ), IDOK ),
                enable );
end;

end.
