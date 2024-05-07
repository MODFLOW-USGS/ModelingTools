inherited frmImportWarnings: TfrmImportWarnings
  Caption = 'MODFLOW 6 Import Warnings'
  ClientWidth = 434
  OnClose = FormClose
  ExplicitWidth = 450
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 161
    Width = 434
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitWidth = 768
    DesignSize = (
      434
      41)
    object btnClose: TBitBtn
      Left = 342
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 545
    end
  end
  object memoWarnings: TMemo
    Left = 0
    Top = 0
    Width = 434
    Height = 161
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 768
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tmr1Timer
    Left = 248
    Top = 48
  end
end
