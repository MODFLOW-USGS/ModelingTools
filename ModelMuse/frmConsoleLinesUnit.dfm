inherited frmConsoleLines: TfrmConsoleLines
  Caption = 'Console Lines'
  ClientHeight = 363
  ClientWidth = 588
  ExplicitWidth = 604
  ExplicitHeight = 401
  PixelsPerInch = 96
  TextHeight = 18
  object memoConsoleLines: TMemo
    Left = 0
    Top = 89
    Width = 588
    Height = 233
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitTop = 57
    ExplicitHeight = 265
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 322
    Width = 588
    Height = 41
    Align = alBottom
    TabOrder = 2
    object btnClose: TBitBtn
      Left = 509
      Top = 6
      Width = 75
      Height = 25
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 0
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 89
    Align = alTop
    TabOrder = 0
    object lblMessage: TLabel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 580
      Height = 81
      Align = alClient
      Caption = 'lblMessage'
      WordWrap = True
      ExplicitWidth = 80
      ExplicitHeight = 18
    end
  end
end
