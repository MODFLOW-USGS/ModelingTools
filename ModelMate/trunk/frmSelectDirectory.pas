unit frmSelectDirectory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit, Utilities;

type
  TFormSelectDirectory = class(TForm)
    // Form is designed to work as a dialog
    // It is set up:
    //   (1) To browse, with the initial directory assigned as
    //       the Dir argument of procedure GetDir
    //   (2) To allow the user to select any directory
    //   (3) To display text in a user-specified label above
    //       the edit box (procedure SetLabel)
    //   (4) To allow the user to specify the caption displayed
    //       in the title bar (Caption property)
    //   (5) To display red edit box if directory is invalid.
    DirEdit1: TJvDirectoryEdit;
    lblMain: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure PopulateControls;
    procedure HighlightControls;
    procedure SetData;
  public
    { Public declarations }
    procedure GetDir(var Dir: string);
    procedure SetLabel(const Value: string);
  end;

var
  FormSelectDirectory: TFormSelectDirectory;
  Directory: string;

implementation

{$R *.dfm}

procedure TFormSelectDirectory.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSelectDirectory.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  Close;
end;

procedure TFormSelectDirectory.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

procedure TFormSelectDirectory.FormShow(Sender: TObject);
begin
  PopulateControls;
  HighlightControls;
end;

procedure TFormSelectDirectory.GetDir(var Dir: string);
begin
  Directory := Dir;
  self.ShowModal;
  Dir := Directory;
end;

procedure TFormSelectDirectory.PopulateControls;
begin
  DirEdit1.Text := Directory;
end;

procedure TFormSelectDirectory.HighlightControls;
var
  DirDoesExist, OK: Boolean;
begin
  OK := True;
  DirDoesExist := DirectoryExists(DirEdit1.Text);
  if DirDoesExist then
    begin
      DirEdit1.Color := clWindow;
    end
  else
    begin
      DirEdit1.Color := clRed;
      OK := False;
    end;
  btnOK.Enabled := OK;
end;

procedure TFormSelectDirectory.SetData;
begin
  Directory := DirEdit1.Text;
end;

procedure TFormSelectDirectory.SetLabel(const Value: string);
begin
  lblMain.Caption := Value;
end;

end.
