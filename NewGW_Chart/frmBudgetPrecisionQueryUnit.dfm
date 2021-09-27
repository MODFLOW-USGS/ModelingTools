object frmBudgetPrecisionQuery: TfrmBudgetPrecisionQuery
  Left = 192
  Top = 208
  Width = 391
  Height = 240
  Caption = 'Budget Precision'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 16
  object rgBudgetPrecision: TRadioGroup
    Left = 0
    Top = 0
    Width = 373
    Height = 154
    Align = alClient
    Caption = 'What is the precision of this file?'
    ItemIndex = 2
    Items.Strings = (
      'Single'
      'Double'
      'I don'#39't know'
      'Huh? What does this mean?')
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 154
    Width = 373
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnClose: TBitBtn
      Left = 288
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkClose
    end
  end
end
