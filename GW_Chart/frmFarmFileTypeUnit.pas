unit frmFarmFileTypeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MyFormUnit, SetWindowStateUnit, StdCtrls, ExtCtrls, Buttons;

type
  TfrmFarmFileType = class(TMyForm)
    rgFarmFileType: TRadioGroup;
    pnl1: TPanel;
    btnOK: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFarmFileType: TfrmFarmFileType;

implementation

{$R *.dfm}

end.
