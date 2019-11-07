unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OfficeBalloon, StdCtrls, OfficeControls, ExtCtrls;

type
  TForm2 = class(TForm)
    ksoBalloonForm1: TksoBalloonForm;
    ksoBalloonButton1: TksoBalloonButton;
    Memo1: TMemo;
    Label1: TLabel;
    Bevel1: TBevel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

end.
