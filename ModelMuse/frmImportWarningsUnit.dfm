inherited frmImportWarnings: TfrmImportWarnings
  Caption = 'MODFLOW 6 Import Warnings'
  ExplicitWidth = 416
  ExplicitHeight = 259
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 180
    Width = 404
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 179
    ExplicitWidth = 400
    DesignSize = (
      404
      41)
    object btnClose: TBitBtn
      Left = 320
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 0
      ExplicitLeft = 316
    end
  end
  object memoWarnings: TMemo
    Left = 0
    Top = 0
    Width = 404
    Height = 180
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
