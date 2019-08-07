inherited frmGridAngle: TfrmGridAngle
  Left = 672
  Top = 486
  HelpType = htKeyword
  HelpKeyword = 'Grid_Angle_Dialog_Box'
  ActiveControl = rdeGridAngle
  Caption = 'Grid Angle'
  ClientHeight = 92
  ClientWidth = 341
  ExplicitWidth = 357
  ExplicitHeight = 130
  PixelsPerInch = 96
  TextHeight = 18
  object lblGridAngle: TLabel
    Left = 8
    Top = 11
    Width = 76
    Height = 18
    Caption = 'Grid angle:'
  end
  object rdeGridAngle: TRbwDataEntry
    Left = 120
    Top = 8
    Width = 209
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    TabOrder = 0
    Text = '0'
    OnChange = rdeGridAngleChange
    DataType = dtReal
    Max = 1.000000000000000000
    ChangeDisabledColor = True
  end
  object btnOK: TBitBtn
    Left = 143
    Top = 48
    Width = 91
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 240
    Top = 48
    Width = 89
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 48
    Top = 48
    Width = 91
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnHelpClick
  end
end
