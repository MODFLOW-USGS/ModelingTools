inherited frmCustomImportSimpleFile: TfrmCustomImportSimpleFile
  Left = 541
  Top = 541
  HelpType = htKeyword
  ActiveControl = comboDataSets
  Caption = 'frmCustomImportSimpleFile'
  ClientHeight = 269
  ClientWidth = 564
  ExplicitWidth = 582
  ExplicitHeight = 314
  PixelsPerInch = 120
  TextHeight = 18
  object lblDataSet: TLabel
    Left = 8
    Top = 8
    Width = 62
    Height = 18
    Caption = 'Data Set'
  end
  object lblInterpolator: TLabel
    Left = 8
    Top = 64
    Width = 77
    Height = 18
    Caption = 'Interpolator'
  end
  object comboDataSets: TComboBox
    Left = 8
    Top = 32
    Width = 551
    Height = 26
    Style = csDropDownList
    TabOrder = 0
    OnChange = comboDataSetsChange
  end
  object comboInterpolators: TComboBox
    Left = 8
    Top = 85
    Width = 257
    Height = 26
    Style = csDropDownList
    TabOrder = 1
    OnChange = comboInterpolatorsChange
  end
  object cbEnclosedCells: TCheckBox
    Left = 8
    Top = 117
    Width = 395
    Height = 31
    Caption = 'Set values of enclosed elements'
    TabOrder = 2
    OnClick = cbEnclosedCellsClick
  end
  object cbIntersectedCells: TCheckBox
    Left = 8
    Top = 149
    Width = 395
    Height = 31
    Caption = 'Set values of intersected elements'
    TabOrder = 3
    OnClick = cbEnclosedCellsClick
  end
  object cbInterpolation: TCheckBox
    Left = 8
    Top = 181
    Width = 465
    Height = 31
    Caption = 'Set values of elements by interpolation'
    TabOrder = 4
    OnClick = cbEnclosedCellsClick
  end
  object rgEvaluatedAt: TRadioGroup
    Left = 8
    Top = 216
    Width = 257
    Height = 49
    Caption = 'Evaluated at'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Elements'
      'Nodes')
    TabOrder = 5
    OnClick = rgEvaluatedAtClick
  end
  object btnOK: TBitBtn
    Left = 368
    Top = 228
    Width = 91
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 7
  end
  object btnCancel: TBitBtn
    Left = 464
    Top = 228
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 8
  end
  object btnHelp: TBitBtn
    Left = 272
    Top = 228
    Width = 91
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnHelpClick
  end
  object OpenDialogFile: TOpenDialog
    Filter = 'DXF files (*.dxf)|*.dxf;*.DXF'
    FilterIndex = 0
    Title = 'Open a DXF file'
    Left = 64
    Top = 88
  end
end
