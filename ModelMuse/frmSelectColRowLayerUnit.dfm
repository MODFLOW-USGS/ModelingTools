inherited frmSelectColRowLayer: TfrmSelectColRowLayer
  HelpType = htKeyword
  HelpKeyword = 'Select_Column_Row_and_Layer'
  ActiveControl = seCol
  Caption = 'Select Column, Row, and Layer'
  ClientHeight = 152
  ClientWidth = 324
  ExplicitWidth = 342
  ExplicitHeight = 197
  PixelsPerInch = 120
  TextHeight = 18
  object lblCol: TLabel
    Left = 8
    Top = 10
    Width = 115
    Height = 18
    Caption = 'Selected column'
  end
  object lblRow: TLabel
    Left = 8
    Top = 42
    Width = 91
    Height = 18
    Caption = 'Selected row'
  end
  object lblLayer: TLabel
    Left = 8
    Top = 74
    Width = 99
    Height = 18
    Caption = 'Selected layer'
  end
  object seCol: TJvSpinEdit
    Left = 152
    Top = 8
    Width = 164
    Height = 26
    ButtonKind = bkClassic
    TabOrder = 0
  end
  object seRow: TJvSpinEdit
    Left = 152
    Top = 40
    Width = 164
    Height = 26
    ButtonKind = bkClassic
    TabOrder = 2
  end
  object seLayer: TJvSpinEdit
    Left = 152
    Top = 72
    Width = 164
    Height = 26
    ButtonKind = bkClassic
    TabOrder = 5
  end
  object btnOK: TBitBtn
    Left = 112
    Top = 108
    Width = 100
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 216
    Top = 108
    Width = 100
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 8
    Top = 108
    Width = 100
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnHelpClick
  end
end
