unit frmNamingConvention;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvHtControls, ComCtrls, JvRichEdit, Buttons;

type
  TFormNamingConvention = class(TForm)
    JvRichEdit1: TJvRichEdit;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNamingConvention: TFormNamingConvention;

implementation

{$R *.dfm}

end.
