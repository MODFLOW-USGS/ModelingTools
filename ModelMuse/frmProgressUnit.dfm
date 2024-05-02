inherited frmProgressMM: TfrmProgressMM
  Left = 384
  Top = 254
  ActiveControl = memoMessages
  Caption = 'Progress'
  ClientHeight = 235
  ClientWidth = 654
  ExplicitWidth = 670
  ExplicitHeight = 274
  TextHeight = 18
  object memoMessages: TMemo
    Left = 0
    Top = 61
    Width = 654
    Height = 174
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    OnChange = memoMessagesChange
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 654
    Height = 61
    Align = alTop
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      654
      61)
    object lblProgress: TLabel
      Left = 8
      Top = 32
      Width = 79
      Height = 18
      Caption = 'lblProgress'
    end
    object pbProgress: TProgressBar
      Left = 8
      Top = 8
      Width = 613
      Height = 18
      Anchors = [akLeft, akTop, akRight]
      Step = 1
      TabOrder = 0
    end
    object btnAbort: TBitBtn
      Left = 546
      Top = 30
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Abort'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
      TabOrder = 1
      Visible = False
      OnClick = btnAbortClick
    end
  end
end
