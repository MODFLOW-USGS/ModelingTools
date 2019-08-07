unit frmHelpUcodeMethodsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Utilities;

type
  TfrmHelpUcodeMethods = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmHelpUcodeMethods: TfrmHelpUcodeMethods;

implementation

{$R *.dfm}

procedure TfrmHelpUcodeMethods.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

end.
