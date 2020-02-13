object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 206
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 48
    Top = 24
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object pb1: TProgressBar
    Left = 24
    Top = 104
    Width = 281
    Height = 17
    Step = 1
    TabOrder = 1
  end
  object dlgSave1: TSaveDialog
    DefaultExt = '.txt'
    Left = 136
    Top = 32
  end
end
