unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComboBox, fontcombo, basecombo, SpectrumCombo, StdCtrls, ComCtrls,
  ToolWin, OfficeHint;

type
  TForm1 = class(TForm)
    ksoOfficeHint1: TksoOfficeHint;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    Label1: TLabel;
    ksoSpectrumComboBox1: TksoSpectrumComboBox;
    ksoSpectrumComboBox2: TksoSpectrumComboBox;
    Label2: TLabel;
    ksoFontComboBox1: TksoFontComboBox;
    ksoComboBox1: TksoComboBox;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    procedure ksoSpectrumComboBox1Change(Sender: TObject);
    procedure ksoSpectrumComboBox2Change(Sender: TObject);
    procedure ksoFontComboBox1Change(Sender: TObject);
    procedure ksoComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses OfficeHintDsgn; 

{$R *.DFM}

procedure TForm1.ksoSpectrumComboBox1Change(Sender: TObject);
begin
  ksoOfficeHint1.Color := ksoSpectrumComboBox1.ColorValue;
end;

procedure TForm1.ksoSpectrumComboBox2Change(Sender: TObject);
begin
  ksoOfficeHint1.Font.Color := ksoSpectrumComboBox2.ColorValue;
end;

procedure TForm1.ksoFontComboBox1Change(Sender: TObject);
begin
  ksoOfficeHint1.Font.Name := ksoFontComboBox1.FontName;
end;

procedure TForm1.ksoComboBox1Change(Sender: TObject);
begin
  ksoOfficeHint1.Font.Size := StrToInt(ksoComboBox1.Text);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  frmHintShape := TfrmHintShape.Create(Application);
  try
    if frmHintShape.ShowModal = mrOk then
      ksoOfficeHint1.Shape := frmHintShape.FShape;
  finally
    frmHintShape.Free;
  end;
end;

end.
