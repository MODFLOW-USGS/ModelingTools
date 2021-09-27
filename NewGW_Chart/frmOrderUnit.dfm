object frmOrder: TfrmOrder
  Left = 284
  Top = 116
  Width = 201
  Height = 269
  Caption = 'Order of data points and series'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 16
  object rgOrder: TRadioGroup
    Left = 8
    Top = 72
    Width = 177
    Height = 57
    Caption = 'Order of series in legend'
    ItemIndex = 1
    Items.Strings = (
      'Original order'
      'Order from *._sc file')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 110
    Top = 200
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
  object rgPointOrder: TRadioGroup
    Left = 8
    Top = 136
    Width = 177
    Height = 57
    Caption = 'Order of data points in plot'
    ItemIndex = 1
    Items.Strings = (
      'Original order'
      'Largest first')
    TabOrder = 2
  end
  object rgWhatToPlot: TRadioGroup
    Left = 8
    Top = 8
    Width = 177
    Height = 57
    Caption = 'What to plot on X-axis'
    ItemIndex = 0
    Items.Strings = (
      'Observations'
      'Parameters')
    TabOrder = 3
    OnClick = rgWhatToPlotClick
  end
end
