unit DBDemoUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DataGrid, Mask, DBCtrls, ExtCtrls, Db, DBTables, DBGrids;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    Table1: TTable;
    DBNavigator1: TDBNavigator;
    DBEdit2: TDBEdit;
    DBEdit1: TDBEdit;
    DBDataGrid1: TDBDataGrid;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DBEdit3: TDBEdit;
    Panel1: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    Info: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
     showmessage(inttostr(dbdatagrid1.columns[1].Internalcol));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     Table1.tablename := extractfilepath(application.exename)+'customer.db';
     Table1.open;
end;

end.
