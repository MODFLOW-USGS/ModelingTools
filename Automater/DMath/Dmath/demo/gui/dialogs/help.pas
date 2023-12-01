unit Help;

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, Buttons,
  StdCtrls, ExtCtrls;

type
  THelpDlg = class(TForm)
    OKBtn: TBitBtn;
    Memo1: TMemo;
  end;

var
  HelpDlg: THelpDlg;

implementation

{$R *.DFM}

end.
