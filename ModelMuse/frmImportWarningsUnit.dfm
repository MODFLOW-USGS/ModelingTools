inherited frmImportWarnings: TfrmImportWarnings
  Caption = 'MODFLOW 6 Import Warnings'
  ClientHeight = 191
  ClientWidth = 788
  OnClose = FormClose
  ExplicitWidth = 800
  ExplicitHeight = 229
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 150
    Width = 788
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 159
    ExplicitWidth = 430
    DesignSize = (
      788
      41)
    object btnClose: TBitBtn
      Left = 692
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 334
    end
  end
  object memoWarnings: TMemo
    Left = 0
    Top = 0
    Width = 788
    Height = 150
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    ExplicitWidth = 430
    ExplicitHeight = 159
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tmr1Timer
    Left = 248
    Top = 48
  end
end
