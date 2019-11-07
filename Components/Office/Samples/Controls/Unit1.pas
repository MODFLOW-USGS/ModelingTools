unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  autocombo, mrucombo, StdCtrls, glyphcombo, treecombo, OfficeTypes,
  TrackCombo, fontcombo, ColorCombo2000, SpectrumCombo, ExtCtrls,
  ColorCombo, basecombo, ComboBox, ComCtrls, ImgList, OfficeEdit,
  OfficeButtons, OfficeControls, OfficePanel, CheckLst;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label6: TLabel;
    BorderStyle: TksoComboBox;
    TabSheet2: TTabSheet;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    escColorComboBox6: TksoColorComboBox;
    Panel1: TPanel;
    escSpectrumComboBox1: TksoSpectrumComboBox;
    escColorComboBox20001: TksoColorComboBox2000;
    escFontComboBox1: TksoFontComboBox;
    escFontComboBox2: TksoFontComboBox;
    escFontComboBox3: TksoFontComboBox;
    TabSheet3: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    escTrackComboBox1: TksoTrackComboBox;
    escTrackComboBox2: TksoTrackComboBox;
    escTrackComboBox3: TksoTrackComboBox;
    escTreeComboBox1: TksoTreeComboBox;
    escTreeComboBox2: TksoTreeComboBox;
    escTreeComboBox3: TksoTreeComboBox;
    escGlyphComboBox1: TksoGlyphComboBox;
    TabSheet4: TTabSheet;
    Label15: TLabel;
    Label16: TLabel;
    escMRUComboBox1: TksoMRUComboBox;
    escAutoFillComboBox1: TksoAutoFillComboBox;
    ImageList1: TImageList;
    TabSheet5: TTabSheet;
    ksoOfficeButton1: TksoOfficeButton;
    ksoOfficeButton2: TksoOfficeButton;
    ksoOfficeButton3: TksoOfficeButton;
    ksoOfficeLabel1: TksoOfficeLabel;
    ksoOfficeLabel2: TksoOfficeLabel;
    ksoOfficeSpeedButton1: TksoOfficeSpeedButton;
    ksoOfficeSpeedButton2: TksoOfficeSpeedButton;
    ksoOfficeSpeedButton3: TksoOfficeSpeedButton;
    Label18: TLabel;
    Label19: TLabel;
    Flat: TksoOfficeCheckBox;
    FocusedStyle: TksoComboBox;
    FlatStyle: TksoComboBox;
    ksoOfficeGroupBox2: TksoOfficeGroupBox;
    ksoComboBox1: TksoComboBox;
    ksoColorComboBox1: TksoColorComboBox;
    ksoColorComboBox20001: TksoColorComboBox2000;
    ksoFontComboBox1: TksoFontComboBox;
    ksoSpectrumComboBox1: TksoSpectrumComboBox;
    ksoOfficePanel2: TksoOfficePanel;
    ksoOfficeMemo2: TksoOfficeMemo;
    ksoOfficeCheckBox3: TksoOfficeCheckBox;
    ksoOfficeRadioButton3: TksoOfficeRadioButton;
    ksoOfficeRadioButton4: TksoOfficeRadioButton;
    ksoOfficeCheckListBox1: TksoOfficeCheckListBox;
    Label2: TLabel;
    BtnBorderStyle: TksoComboBox;
    BtnFlat: TksoOfficeCheckBox;
    BtnFlatStyle: TksoComboBox;
    Label3: TLabel;
    BtnFocusedStyle: TksoComboBox;
    Label4: TLabel;
    BtnDownStyle: TksoComboBox;
    Label5: TLabel;
    ksoEdit1: TksoEdit;
    procedure escColorComboBox6Change(Sender: TObject);
    procedure escSpectrumComboBox1Change(Sender: TObject);
    procedure escColorComboBox20001Change(Sender: TObject);
    procedure FlatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BorderStyleChange(Sender: TObject);
    procedure FlatStyleChange(Sender: TObject);
    procedure FocusedStyleChange(Sender: TObject);
    procedure BtnFlatClick(Sender: TObject);
    procedure BtnBorderStyleChange(Sender: TObject);
    procedure BtnDownStyleChange(Sender: TObject);
    procedure BtnFlatStyleChange(Sender: TObject);
    procedure BtnFocusedStyleChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.escColorComboBox6Change(Sender: TObject);
begin
  Panel1.Color := escColorComboBox6.ColorValue;
end;

procedure TForm1.escSpectrumComboBox1Change(Sender: TObject);
begin
  Panel1.Color := escSpectrumComboBox1.ColorValue;
end;

procedure TForm1.escColorComboBox20001Change(Sender: TObject);
begin
  Panel1.Color := escColorComboBox20001.ColorValue;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BorderStyle.ItemIndex := 6;
  FlatStyle.ItemIndex := 3;
  FocusedStyle.ItemIndex := 11; 

  BtnBorderStyle.ItemIndex := 9;
  BtnDownStyle.ItemIndex := 6;
  BtnFlatStyle.ItemIndex := 0;
  BtnFocusedStyle.ItemIndex := 0;
end;

procedure TForm1.FlatClick(Sender: TObject);
begin
  ksoOfficeGroupBox2.Flat := Flat.Checked;
  ksoComboBox1.Flat := Flat.Checked;
  ksoColorComboBox1.Flat := Flat.Checked;
  ksoColorComboBox20001.Flat := Flat.Checked;
  ksoFontComboBox1.Flat := Flat.Checked;
  ksoSpectrumComboBox1.Flat := Flat.Checked;
  ksoOfficePanel2.Flat := Flat.Checked;
  ksoOfficeMemo2.Flat := Flat.Checked;
  ksoOfficeCheckBox3.Flat := Flat.Checked;
  ksoOfficeRadioButton3.Flat := Flat.Checked;
  ksoOfficeRadioButton4.Flat := Flat.Checked;
  ksoOfficeCheckListBox1.Flat := Flat.Checked;
  ksoEdit1.Flat := Flat.Checked;
end;

procedure TForm1.BorderStyleChange(Sender: TObject);
begin
  ksoComboBox1.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoColorComboBox1.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoColorComboBox20001.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoFontComboBox1.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoSpectrumComboBox1.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoOfficePanel2.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoOfficeMemo2.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoOfficeCheckListBox1.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
  ksoEdit1.BorderStyle := TksoBorderStyle(BorderStyle.ItemIndex);
end;

procedure TForm1.FlatStyleChange(Sender: TObject);
begin
  ksoComboBox1.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoColorComboBox1.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoColorComboBox20001.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoFontComboBox1.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoSpectrumComboBox1.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoOfficePanel2.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoOfficeMemo2.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoOfficeCheckListBox1.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
  ksoEdit1.BorderStyleFlat := TksoBorderStyle(FlatStyle.ItemIndex);
end;

procedure TForm1.FocusedStyleChange(Sender: TObject);
begin
  ksoComboBox1.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoColorComboBox1.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoColorComboBox20001.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoFontComboBox1.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoSpectrumComboBox1.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoOfficePanel2.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoOfficeMemo2.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoOfficeCheckListBox1.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
  ksoEdit1.BorderStyleFocused := TksoBorderStyle(FocusedStyle.ItemIndex);
end;

procedure TForm1.BtnFlatClick(Sender: TObject);
begin
  ksoOfficeSpeedButton1.Flat := BtnFlat.Checked;
  ksoOfficeSpeedButton3.Flat := BtnFlat.Checked;
  ksoOfficeSpeedButton2.Flat := BtnFlat.Checked;
  ksoOfficeButton1.Flat := BtnFlat.Checked;
  ksoOfficeButton2.Flat := BtnFlat.Checked;
  ksoOfficeButton3.Flat := BtnFlat.Checked;
end;

procedure TForm1.BtnBorderStyleChange(Sender: TObject);
begin
  ksoOfficeSpeedButton1.BorderStyle := TksoBorderStyle(BtnBorderStyle.ItemIndex);
  ksoOfficeSpeedButton3.BorderStyle := TksoBorderStyle(BtnBorderStyle.ItemIndex);
  ksoOfficeSpeedButton2.BorderStyle := TksoBorderStyle(BtnBorderStyle.ItemIndex);
  ksoOfficeButton1.BorderStyle := TksoBorderStyle(BtnBorderStyle.ItemIndex);
  ksoOfficeButton2.BorderStyle := TksoBorderStyle(BtnBorderStyle.ItemIndex);
  ksoOfficeButton3.BorderStyle := TksoBorderStyle(BtnBorderStyle.ItemIndex);
end;

procedure TForm1.BtnDownStyleChange(Sender: TObject);
begin
  ksoOfficeSpeedButton1.BorderStyleDown := TksoBorderStyle(BtnDownStyle.ItemIndex);
  ksoOfficeSpeedButton3.BorderStyleDown := TksoBorderStyle(BtnDownStyle.ItemIndex);
  ksoOfficeSpeedButton2.BorderStyleDown := TksoBorderStyle(BtnDownStyle.ItemIndex);
  ksoOfficeButton1.BorderStyleDown := TksoBorderStyle(BtnDownStyle.ItemIndex);
  ksoOfficeButton2.BorderStyleDown := TksoBorderStyle(BtnDownStyle.ItemIndex);
  ksoOfficeButton3.BorderStyleDown := TksoBorderStyle(BtnDownStyle.ItemIndex);
end;

procedure TForm1.BtnFlatStyleChange(Sender: TObject);
begin
  ksoOfficeSpeedButton1.BorderStyleFlat := TksoBorderStyle(BtnFlatStyle.ItemIndex);
  ksoOfficeSpeedButton3.BorderStyleFlat := TksoBorderStyle(BtnFlatStyle.ItemIndex);
  ksoOfficeSpeedButton2.BorderStyleFlat := TksoBorderStyle(BtnFlatStyle.ItemIndex);
  ksoOfficeButton1.BorderStyleFlat := TksoBorderStyle(BtnFlatStyle.ItemIndex);
  ksoOfficeButton2.BorderStyleFlat := TksoBorderStyle(BtnFlatStyle.ItemIndex);
  ksoOfficeButton3.BorderStyleFlat := TksoBorderStyle(BtnFlatStyle.ItemIndex);
end;

procedure TForm1.BtnFocusedStyleChange(Sender: TObject);
begin
  ksoOfficeSpeedButton1.BorderStyleFocused := TksoBorderStyle(BtnFocusedStyle.ItemIndex);
  ksoOfficeSpeedButton3.BorderStyleFocused := TksoBorderStyle(BtnFocusedStyle.ItemIndex);
  ksoOfficeSpeedButton2.BorderStyleFocused := TksoBorderStyle(BtnFocusedStyle.ItemIndex);
  ksoOfficeButton1.BorderStyleFocused := TksoBorderStyle(BtnFocusedStyle.ItemIndex);
  ksoOfficeButton2.BorderStyleFocused := TksoBorderStyle(BtnFocusedStyle.ItemIndex);
  ksoOfficeButton3.BorderStyleFocused := TksoBorderStyle(BtnFocusedStyle.ItemIndex);
end;

end.
