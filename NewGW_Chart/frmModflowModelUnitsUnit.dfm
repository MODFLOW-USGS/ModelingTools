object frmModflowModelUnits: TfrmModflowModelUnits
  Left = 192
  Top = 125
  Width = 385
  Height = 242
  Caption = 'What are the time units of the model?'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object rgTimeUnits: TRadioGroup
    Left = 0
    Top = 0
    Width = 369
    Height = 162
    Align = alClient
    Caption = 'What are the time units of the model?'
    ItemIndex = 0
    Items.Strings = (
      'UNDEFINED'
      'SECONDS'
      'MINUTES'
      'HOURS'
      'DAYS'
      'YEARS')
    TabOrder = 0
  end
  object pnl1: TPanel
    Left = 0
    Top = 162
    Width = 369
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btn1: TBitBtn
      Left = 280
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
  end
end
