object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 64
  ClientWidth = 113
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 12
  object btn1: TButton
    Left = 6
    Top = 6
    Width = 91
    Height = 46
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Insert google analytics code in web pages.'
    TabOrder = 0
    WordWrap = True
    OnClick = btn1Click
  end
  object jvsd1: TJvSelectDirectory
    Left = 80
    Top = 40
  end
end
