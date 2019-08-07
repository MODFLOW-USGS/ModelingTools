unit frmParallelJupiter;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DataGrid, Utilities;

type
  TFormParallelJupiter = class(TForm)
    cbUseParallel: TCheckBox;
    edtWait: TEdit;
    lblWait: TLabel;
    lblVerbosity: TLabel;
    cbVerbosity: TComboBox;
    cbAutoStopRunners: TCheckBox;
    edtTimeOutFactor: TEdit;
    lblTimeOutFactor: TLabel;
    DataGrid1: TDataGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormParallelJupiter: TFormParallelJupiter;

implementation

{$R *.dfm}

procedure TFormParallelJupiter.FormCreate(Sender: TObject);
begin
  CenterForm(self);
end;

end.
