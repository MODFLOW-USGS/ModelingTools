inherited frmHelpVersion: TfrmHelpVersion
  Caption = 'Help Access Method'
  ClientHeight = 149
  ClientWidth = 383
  OnClose = FormClose
  ExplicitWidth = 401
  ExplicitHeight = 194
  PixelsPerInch = 96
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
  end
  object pnl1: TPanel
    Left = 0
    Top = 108
    Width = 383
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 136
    ExplicitTop = 192
    ExplicitWidth = 185
    object btnClose: TBitBtn
      Left = 256
      Top = 6
      Width = 75
      Height = 27
      DoubleBuffered = True
      Kind = bkClose
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
    end
  end
end
