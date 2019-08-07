{ @name defines @link(TCustomExtendedDialog) which
 is an abstract ancestor used to create standard dialog boxes
 that contain extra controls.

@author(Richard B. Winston <rbwinst@usgs.gov>) 
}

unit CustomExtendedDialogForm;

interface

uses Windows, Forms, Dialogs;

type
  // @name is an abstract ancestor used to create standard dialog boxes
  // that contain extra controls.
  TCustomExtendedDialog = class(TForm)
  protected
    // @name is the common dialog box that is
    // the parent of the descendant form.
    FDlg: TCommonDialog;
  public
    // @name creates a new form with aDlg as the parent form.
    constructor CreateForDialog(aDlg: TCommonDialog);
  end;

implementation

{ TCustomExtendedDialog }

constructor TCustomExtendedDialog.CreateForDialog(aDlg: TCommonDialog);
var
  dlgrect, dlgclientrect: TRECT;
  parentwnd: HWND;
Begin
  // store parent reference and get the dialogs window handle
  FDlg := aDlg;
  parentwnd := GetParent( aDlg.handle );

  // create form with the dialog as parent
  CreateParented( parentwnd );

  // get dialog dimensions, position form below existing controls,
  // make dialog larger to fit this form
  GetWindowRect( parentwnd, dlgrect );
  windows.GetClientRect( parentwnd, dlgclientrect );
//  parentrect := clientrect;
  Top := dlgclientrect.bottom;
  Left := 0;
  Show;
//  with dlgrect do
    MoveWindow( GetParent( aDlg.handle ),
                dlgrect.left, dlgrect.top,
                dlgrect.right-dlgrect.left,
                dlgrect.bottom - dlgrect.top + height,
                true );

end;

end.
