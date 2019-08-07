{@abstract(The main purpose of @name is to define @link(TfrmChooseStyle) which
  allows the user to choose the "style" of controls
  to be applied throughout GoPhast.)}
unit frmChooseStyleUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls, Menus, 
  Grids, RbwDataGrid4;

type
  {@abstract(@name allows the user to choose the "style" of controls
    to be applied throughout GoPhast.)}
  TfrmChooseStyle = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name is used to close the dialog box.
    btnClose: TBitBtn;
    // @name: TBitBtn;
    // @name is used to display help for this dialog box.l
    btnHelp: TBitBtn;
    // @name: TButton;
    // @name shows what button looks like for different styles.
    btnPreview: TButton;
    // @name: TCheckBox;
    // @name shows what a checked checkbox looks like for different styles.
    cbChecked: TCheckBox;
    // @name: TCheckBox;
    // @name shows what a disabled checkbox looks like for different styles.
    cbDisabled: TCheckBox;
    // @name: TCheckBox;
    // @name shows what a indeterminate checkbox looks like for different styles.
    cbIndeterminant: TCheckBox;
    // @name: TCheckBox;
    // @name shows what a unchecked checkbox looks like for different styles.
    cbUnchecked: TCheckBox;
    // @name: TComboBox;
    // @name shows what a combobox looks like for different styles.
    comboPreview: TComboBox;
    // @name: TEdit;
    // @name shows what an editbox looks like for different styles.
    edPreview: TEdit;
    // @name: TGroupBox;
    // @name encloses the preview controls.
    gbPreview: TGroupBox;
    // @name: TLabel;
    // @name tells how to use the dialog box.
    lblInstructions: TLabel;
    // @name: TMainMenu;
    // @name holds the preview menu items.
    MainMenu1: TMainMenu;
    // @name: TMemo;
    // @name shows what a memo box looks like for different styles.
    memoPreview: TMemo;
    // @name: TMenuItem;
    // @name shows what a menu item looks like for different styles.
    menuPreview1: TMenuItem;
    // @name: TMenuItem;
    // @name shows what a menu item looks like for different styles.
    menuPreview2: TMenuItem;
    // @name: TPanel;
    // @name holds the buttons on the bottom of the dialog box.
    pnlBottom: TPanel;
    // @name: TRadioGroup;
    // @name is used to select the style.
    // See @link(rgStyleClick).
    rgStyle: TRadioGroup;
    RbwDataGrid41: TRbwDataGrid4;
    // @name shows a message informing the user that the button doesn't
    // do anything.
    procedure btnPreviewClick(Sender: TObject);
    // @name initializes the @classname.
    procedure FormCreate(Sender: TObject); override;
    // @name shows a message informing the user that the menu item doesn't
    // do anything.
    procedure menuPreview2Click(Sender: TObject);
    // @name changes the style of the dialog box.
    procedure rgStyleClick(Sender: TObject);
  private
    // @name gets the current style for GoPhast.
    procedure GetData;
    // @name sets the style for GoPhast to the one selected in @link(rgStyle).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TfrmChooseStyle }

procedure TfrmChooseStyle.GetData;
begin
  rgStyle.ItemIndex := Ord(Application.Style.DefaultStyle);
end;                           

procedure TfrmChooseStyle.SetData;
const
  Delta = 10;
begin
  if ControlState = [] then
  begin
    Application.Style.DefaultStyle := TDefaultStyle(rgStyle.ItemIndex);
    // Resizing helps keep the fonts looking good.
    ChangeBounds(Left, Top, Width + Delta, Height + Delta);
    ChangeBounds(Left, Top, Width - Delta, Height - Delta);
  end;
end;

procedure TfrmChooseStyle.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
  rdgPreview.Cells[2, 0] := 'Combo';
  rdgPreview.Cells[1, 0] := 'Check';
  rdgPreview.Cells[0, 0] := 'Text';
  rdgPreview.Cells[2, 1] := 'Choose Me';
  rdgPreview.Cells[1, 1] := 'Check Me';
  rdgPreview.Cells[0, 1] := 'Edit Me';
  rdgPreview.Cells[2, 2] := 'Choose Me';
  rdgPreview.Cells[1, 2] := 'Check Me';
  rdgPreview.Cells[0, 2] := 'Edit Me';
  rdgPreview.Checked[1, 1] := True;
  memoPreview.SelectAll;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
end;

procedure TfrmChooseStyle.rgStyleClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmChooseStyle.btnPreviewClick(Sender: TObject);
begin
  inherited;
  MessageDlg('This button doesn''t do anything', mtInformation, [mbOK], 0);
  // Well really it does.  It shows the above message! ;-)
end;

procedure TfrmChooseStyle.menuPreview2Click(Sender: TObject);
begin
  inherited;
  ShowMessage('This menu is just to give '
    + 'you a preview of what menus look like.');
end;

end.

