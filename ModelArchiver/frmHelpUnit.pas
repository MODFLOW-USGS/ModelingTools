unit frmHelpUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TfrmHelp = class(TForm)
    tlb1: TToolBar;
    btn1: TButton;
    edt1: TEdit;
    wb1: TWebBrowser;
    procedure edt1Change(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure wb1DidFinishLoad(ASender: TObject);
  private
    FNavigating: Boolean;
    procedure OpenURL;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmHelp: TfrmHelp;

implementation

{$R *.fmx}

{ TfrmHelp }

procedure TfrmHelp.btn1Click(Sender: TObject);
begin
  wb1.GoBack;
end;

procedure TfrmHelp.edt1Change(Sender: TObject);
begin
  if not FNavigating then
  begin
    OpenURL;
  end;
end;

procedure TfrmHelp.OpenURL;
begin
  wb1.Navigate(edt1.Text);
end;

procedure TfrmHelp.wb1DidFinishLoad(ASender: TObject);
begin
  FNavigating := True;
  try
    edt1.Text := wb1.URL;
  finally
    FNavigating := False;
  end;
end;

end.
