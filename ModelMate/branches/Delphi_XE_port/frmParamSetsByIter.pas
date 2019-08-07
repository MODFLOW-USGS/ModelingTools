unit frmParamSetsByIter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Grids, RbwDataGrid4, Utilities;

type
  TFormParamSetsByIter = class(TForm)
    RbwDataGrid41: TRbwDataGrid4;
    btnReplace: TBitBtn;
    btnGWChart: TBitBtn;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormParamSetsByIter: TFormParamSetsByIter;

implementation

{$R *.dfm}

procedure TFormParamSetsByIter.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

end.
