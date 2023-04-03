inherited frmGridPosition: TfrmGridPosition
  HelpType = htKeyword
  HelpKeyword = 'Grid_Position_Dialog_Box'
  Caption = 'Grid Position'
  ClientHeight = 130
  ClientWidth = 340
  ExplicitWidth = 352
  ExplicitHeight = 168
  TextHeight = 18
  object lblGridOriginX: TLabel
    Left = 8
    Top = 11
    Width = 88
    Height = 18
    Caption = 'Grid origin X'
  end
  object lblGridOriginY: TLabel
    Left = 8
    Top = 45
    Width = 86
    Height = 18
    Caption = 'Grid origin Y'
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 84
    Width = 89
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 0
  end
  object btnOK: TBitBtn
    Left = 143
    Top = 84
    Width = 91
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnHelp: TBitBtn
    Left = 46
    Top = 84
    Width = 91
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object rdeGridOriginX: TRbwDataEntry
    Left = 120
    Top = 8
    Width = 209
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    TabOrder = 3
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object rdeGridOriginY: TRbwDataEntry
    Left = 120
    Top = 42
    Width = 209
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    TabOrder = 4
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
end
