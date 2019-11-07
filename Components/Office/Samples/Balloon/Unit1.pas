unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, OfficeButtons, OfficeBalloon, basecombo, ComboBox,
  OfficeControls, ColorCombo2000;

type
  TForm1 = class(TForm)
    ksoOfficeButton1: TksoOfficeButton;
    ksoOfficeButton2: TksoOfficeButton;
    ksoOfficeBalloon1: TksoOfficeBalloon;
    ksoOfficeLabel1: TksoOfficeLabel;
    ksoComboBox1: TksoComboBox;
    ksoOfficeLabel2: TksoOfficeLabel;
    ksoColorComboBox20001: TksoColorComboBox2000;
    ksoOfficeLabel3: TksoOfficeLabel;
    ksoColorComboBox20002: TksoColorComboBox2000;
    ksoOfficeButton3: TksoOfficeButton;
    ksoOfficeButton4: TksoOfficeButton;
    ksoOfficeButton5: TksoOfficeButton;
    procedure ksoOfficeButton1Click(Sender: TObject);
    procedure ksoOfficeButton2Click(Sender: TObject);
    procedure ksoOfficeButton3Click(Sender: TObject);
    procedure ksoOfficeButton4Click(Sender: TObject);
    procedure ksoOfficeButton5Click(Sender: TObject);
    procedure ksoColorComboBox20001Change(Sender: TObject);
    procedure ksoColorComboBox20002Change(Sender: TObject);
    procedure ksoComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

procedure TForm1.ksoOfficeButton1Click(Sender: TObject);
begin
  Form2.ShowModal;
end;

procedure TForm1.ksoOfficeButton2Click(Sender: TObject);
begin
  ksoOfficeBalloon1.MessageDlg('message', mtInformation, [mbOk, mbHelp], 0);
end;

procedure TForm1.ksoOfficeButton3Click(Sender: TObject);
begin
  ksoOfficeBalloon1.MessageDlg('This is error message', mtError, [mbOk], 0);
end;

procedure TForm1.ksoOfficeButton4Click(Sender: TObject);
begin
  ksoOfficeBalloon1.MessageDlg('This is confirmation message', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TForm1.ksoOfficeButton5Click(Sender: TObject);
var
  S: string;
begin
  S := 'defult';
  ksoOfficeBalloon1.InputQuery('Input message', 'Type text', S); 
end;

procedure TForm1.ksoColorComboBox20001Change(Sender: TObject);
begin
  ksoOfficeBalloon1.Color := ksoColorComboBox20001.ColorValue;
end;

procedure TForm1.ksoColorComboBox20002Change(Sender: TObject);
begin
  ksoOfficeBalloon1.BorderColor := ksoColorComboBox20002.ColorValue;
end;

procedure TForm1.ksoComboBox1Change(Sender: TObject);
begin
  ksoOfficeBalloon1.Shape := TksoBalloonShape(ksoComboBox1.ItemIndex);
end;

end.
