unit DemoUnit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DataGrid, StdCtrls;

type
  TForm1 = class(TForm)
    DataGrid1: TDataGrid;
    Label1: TLabel;
    procedure DataGrid1CellError(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure DataGrid1EditButtonClick(Sender: TObject);
    procedure DataGrid1SelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DataGrid1CellError(Sender: TObject; ACol, ARow: Integer;
  const Value: String);
begin
     showmessage('Sorry: '+Value+' is not a valid entry');
end;

procedure TForm1.DataGrid1EditButtonClick(Sender: TObject);
begin
     Showmessage('You clicked on the button');
end;

procedure TForm1.DataGrid1SelectCell(Sender: TObject; Col, Row: Integer;
  var CanSelect: Boolean);
begin
     case col of
     1: Label1.caption := 'Enter any string';
     2: Label1.caption := 'Select an item from the popup listbox';
     3: Label1.caption := 'Enter a NUMERIC value';
     4: Label1.caption := 'Enter a date or press "N" or "T" for today';
     5: Label1.caption := 'Enter any string or press button';
     end;
end;

end.
