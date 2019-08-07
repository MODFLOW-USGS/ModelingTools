unit frmStartUp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, JvExMask, JvToolEdit,
  Buttons, GlobalBasicData, Utilities;

type
  TFormStartUp = class(TForm)
    fedProjFile: TJvFilenameEdit;
    Label1: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure fedProjFileChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormStartUp: TFormStartUp;

implementation

{$R *.dfm}

var
  FNameLocal: string = '';
  ReadyToGo: boolean = False;

procedure GoToMainForm;
begin
  if ReadyToGo then
    begin
      FormStartUp.Visible := False;
      FormStartUp.Close;
      OpenMainForm := True;
    end;
end;

procedure TFormStartUp.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormStartUp.btnOKClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := ExtractFileDir(FNameLocal);
  if DirectoryExists(Dir) then
    begin
      GlobalBasicData.FileToBeOpened := FNameLocal;
      GlobalBasicData.StartNewProject := not FileExists(FNameLocal);
      ReadyToGo := True;
      // Make intro form invisible and open main form
      GoToMainForm;
    end
  else
    begin
      ShowMessage('Directory is invalid!');
      btnOK.Enabled := False;
    end;
end;

procedure TFormStartUp.fedProjFileChange(Sender: TObject);
begin
  FNameLocal := fedProjFile.FileName;
  btnOK.Enabled := True;
  btnOK.SetFocus;
end;

procedure TFormStartUp.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  OpenMainForm := False;
end;

procedure TFormStartUp.FormPaint(Sender: TObject);
begin
  if fedProjFile.Text <> '' then
    begin
      GoToMainForm;
    end;
end;

procedure TFormStartUp.FormShow(Sender: TObject);
begin
  if ParamStr(1) <> '' then
    begin
      FNameLocal := ParamStr(1);
      GlobalBasicData.FileToBeOpened := FNameLocal;
      fedProjFile.Text := FNameLocal;
      GlobalBasicData.StartNewProject := not FileExists(FNameLocal);
      ReadyToGo := True;
    end
  else
    begin
      fedProjFile.Text := '';
      btnOK.Enabled := False;
      ReadyToGo := False;
    end;
end;

end.
