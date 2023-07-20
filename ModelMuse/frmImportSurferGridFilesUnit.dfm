inherited frmImportSurferGridFiles: TfrmImportSurferGridFiles
  HelpType = htKeyword
  HelpKeyword = 'Import-Multiple-Surfer-Grid-Fi'
  Caption = 'Import Multiple Surfer Grid Files'
  ClientHeight = 228
  ClientWidth = 582
  ExplicitWidth = 594
  ExplicitHeight = 266
  TextHeight = 18
  object lblInterpolator: TLabel
    Left = 8
    Top = 6
    Width = 77
    Height = 18
    Caption = 'Interpolator'
  end
  object cbIntersectedCells: TCheckBox
    Left = 8
    Top = 59
    Width = 329
    Height = 31
    Caption = 'Set values of intersected elements'
    TabOrder = 0
    OnClick = cbIntersectedCellsClick
  end
  object cbInterpolation: TCheckBox
    Left = 8
    Top = 90
    Width = 321
    Height = 31
    Caption = 'Set values of elements by interpolation'
    TabOrder = 1
    OnClick = cbIntersectedCellsClick
  end
  object rgEvaluatedAt: TRadioGroup
    Left = 8
    Top = 127
    Width = 257
    Height = 49
    Caption = 'Evaluated at'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Elements'
      'Nodes')
    TabOrder = 2
  end
  object rgFilterMethod: TRadioGroup
    Left = 343
    Top = 8
    Width = 227
    Height = 140
    Caption = 'Filter method'
    ItemIndex = 2
    Items.Strings = (
      'Lowest point in cell'
      'Highest point in cell'
      'Average of points in cell'
      'Point closest to cell center'
      'None')
    TabOrder = 3
  end
  object comboInterpolators: TComboBox
    Left = 8
    Top = 30
    Width = 257
    Height = 26
    Style = csDropDownList
    TabOrder = 4
  end
  object btnHelp: TBitBtn
    Left = 285
    Top = 187
    Width = 91
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 382
    Top = 187
    Width = 91
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 6
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 479
    Top = 187
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 7
  end
  object OpenDialogFile: TOpenDialog
    Filter = 'Surfer grid file (*.grd, *.dat)|*.grd;*.dat|All files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing]
    Title = 'Open a Surfer grid file'
    Left = 64
    Top = 65534
  end
end
