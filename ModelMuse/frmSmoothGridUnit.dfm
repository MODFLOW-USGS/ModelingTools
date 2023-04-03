inherited frmSmoothGrid: TfrmSmoothGrid
  Left = 672
  Top = 476
  HelpType = htKeyword
  HelpKeyword = 'Smooth_Grid_Dialog_Box'
  ActiveControl = cbColumns
  Caption = 'Smooth Grid'
  ClientHeight = 114
  ClientWidth = 315
  ExplicitWidth = 327
  ExplicitHeight = 152
  PixelsPerInch = 120
  TextHeight = 18
  object lblCriterion: TLabel
    Left = 8
    Top = 44
    Width = 166
    Height = 18
    Caption = 'Grid smoothing criterion'
  end
  object cbColumns: TCheckBox
    Left = 8
    Top = 8
    Width = 100
    Height = 30
    Caption = 'Columns'
    TabOrder = 0
    OnClick = cbClick
  end
  object cbRows: TCheckBox
    Left = 120
    Top = 8
    Width = 81
    Height = 30
    Caption = 'Rows'
    TabOrder = 1
    OnClick = cbClick
  end
  object cbLayers: TCheckBox
    Left = 208
    Top = 8
    Width = 100
    Height = 30
    Caption = 'Layers'
    TabOrder = 2
    OnClick = cbClick
  end
  object rdeCriterion: TRbwDataEntry
    Left = 208
    Top = 40
    Width = 100
    Height = 28
    Cursor = crIBeam
    Color = clWhite
    TabOrder = 4
    Text = '1.2'
    DataType = dtReal
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object btnCancel: TBitBtn
    Left = 217
    Top = 72
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 3
  end
  object btnOK: TBitBtn
    Left = 121
    Top = 72
    Width = 91
    Height = 33
    Enabled = False
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnOKClick
  end
  object btnHelp: TBitBtn
    Left = 25
    Top = 72
    Width = 91
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnHelpClick
  end
end
