inherited frmBudgetPrecisionQuery: TfrmBudgetPrecisionQuery
  Caption = 'Budget Precision'
  ClientHeight = 195
  ClientWidth = 373
  ExplicitWidth = 391
  PixelsPerInch = 120
  TextHeight = 18
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
    ExplicitHeight = 105
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 154
    Width = 373
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitLeft = 248
    ExplicitTop = 168
    ExplicitWidth = 185
    object btnClose: TBitBtn
      Left = 288
      Top = 6
      Width = 75
      Height = 25
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
    end
  end
end
