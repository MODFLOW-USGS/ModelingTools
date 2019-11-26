unit frmSelectDiscretizationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, framFilePathUnit, Buttons;

type
  TfrmSelectDiscretization = class(TForm)
    Label1: TLabel;
    rgChoose: TRadioGroup;
    framDisFile: TframFilePath;
    btnOK: TBitBtn;
    BitBtn2: TBitBtn;
    Label2: TLabel;
    Label3: TLabel;
    procedure rgChooseClick(Sender: TObject);
    procedure framDisFileedFilePathChange(Sender: TObject);
    procedure framDisFileedFilePathExit(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSelectDiscretization: TfrmSelectDiscretization;

implementation

{$R *.dfm}

procedure TfrmSelectDiscretization.rgChooseClick(Sender: TObject);
begin
  framDisFile.Enabled := rgChoose.ItemIndex = 0;
end;

procedure TfrmSelectDiscretization.framDisFileedFilePathChange(
  Sender: TObject);
var
  FileName: string;
begin
  framDisFile.edFilePathChange(Sender);

  if rgChoose.ItemIndex = 0 then
  begin
    FileName := framDisFile.edFilePath.Text;
    btnOK.Enabled := FileExists(FileName);
  end
  else
  begin
    btnOK.Enabled := False;
  end;

end;

procedure TfrmSelectDiscretization.framDisFileedFilePathExit(
  Sender: TObject);
begin
  framDisFileedFilePathChange(nil);
end;

end.
