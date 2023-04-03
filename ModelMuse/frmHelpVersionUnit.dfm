inherited frmHelpVersion: TfrmHelpVersion
  Caption = 'Help Access Method'
  ClientHeight = 149
  ClientWidth = 383
  OnClose = FormClose
  ExplicitWidth = 395
  ExplicitHeight = 187
  TextHeight = 18
  object rgHelpVersion: TRadioGroup
    Left = 0
    Top = 0
    Width = 383
    Height = 108
    Align = alClient
    Caption = 'How do you want to access the help system?'
    ItemIndex = 0
    Items.Strings = (
      
        'Local (default, faster, no internet connection required, Windows' +
        ' operating system)'
      'Online (any operating system)')
    TabOrder = 0
    WordWrap = True
    ExplicitWidth = 379
    ExplicitHeight = 107
  end
  object pnl1: TPanel
    Left = 0
    Top = 108
    Width = 383
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 107
    ExplicitWidth = 379
    object btnClose: TBitBtn
      Left = 256
      Top = 6
      Width = 75
      Height = 27
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 0
    end
  end
end
