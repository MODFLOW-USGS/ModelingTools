object frmChartType: TfrmChartType
  Left = 502
  Top = 257
  Width = 488
  Height = 163
  Caption = 'Pick Chart Type'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 21
  object rgChartType: TRadioGroup
    Left = 8
    Top = 8
    Width = 465
    Height = 73
    Caption = 'Chart Type'
    Items.Strings = (
      'Plot average values for each parameter'
      'Plot a separate series for each observation or prediction')
    TabOrder = 0
    OnClick = rgChartTypeClick
  end
  object btnOK: TBitBtn
    Left = 304
    Top = 88
    Width = 81
    Height = 33
    Enabled = False
    TabOrder = 1
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 392
    Top = 88
    Width = 81
    Height = 33
    TabOrder = 2
    Kind = bkCancel
  end
end
