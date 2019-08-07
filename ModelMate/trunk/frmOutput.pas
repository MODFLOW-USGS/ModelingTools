unit frmOutput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Utilities;

type
  TFormOutput = class(TForm)
    btnOK: TButton;
    Memo1: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOutput: TFormOutput;

implementation

{$R *.dfm}

procedure TFormOutput.btnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TFormOutput.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

end.
