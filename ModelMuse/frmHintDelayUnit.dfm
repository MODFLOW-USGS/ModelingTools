inherited frmHintDelay: TfrmHintDelay
  Left = 554
  Top = 516
  HelpType = htKeyword
  HelpKeyword = 'Hint_Display_Time_Dialog_Box'
  ActiveControl = rdeHintDelay
  Caption = 'Hint Display Time'
  ClientHeight = 88
  ClientWidth = 321
  ExplicitWidth = 333
  ExplicitHeight = 126
  PixelsPerInch = 120
  TextHeight = 18
  object lblHintDisplayTime: TLabel
    Left = 8
    Top = 10
    Width = 136
    Height = 18
    Caption = 'Hint display time (s)'
  end
  object rdeHintDelay: TRbwDataEntry
    Left = 212
    Top = 8
    Width = 101
    Height = 28
    Cursor = crIBeam
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 224
    Top = 48
    Width = 89
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnOK: TBitBtn
    Left = 128
    Top = 48
    Width = 89
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnHelp: TBitBtn
    Left = 32
    Top = 48
    Width = 89
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnHelpClick
  end
end
