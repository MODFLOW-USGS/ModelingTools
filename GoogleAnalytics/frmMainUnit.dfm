object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 80
  ClientWidth = 141
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object btn1: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 57
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
