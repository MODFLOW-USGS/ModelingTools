unit frmReadmeUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TfrmReadme = class(TForm)
    pnl1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    memoReadme: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FFileName: string;
    { Private declarations }
  public
    procedure GetFile(const FileName: string);
    { Public declarations }
  end;

var
  frmReadme: TfrmReadme;

implementation

uses
  frmModelArchiverUnit;



{$R *.fmx}

procedure TfrmReadme.btnHelpClick(Sender: TObject);
begin
    OpenHelpUrl(btnHelp.HelpKeyword);
end;

procedure TfrmReadme.btnOKClick(Sender: TObject);
begin
  memoReadme.Lines.SaveToFile(FFileName);
end;

procedure TfrmReadme.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 112 then
  begin
    // F1 key;
    OpenHelpUrl(btnHelp.HelpKeyword);
  end;
end;

procedure TfrmReadme.GetFile(const FileName: string);
begin
  FFileName := FileName;
  Caption := 'Editing ' + FFileName;
  memoReadme.Lines.LoadFromFile(FFileName);
end;

end.
