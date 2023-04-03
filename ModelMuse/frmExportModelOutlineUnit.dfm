inherited frmExportModelOutline: TfrmExportModelOutline
  HelpType = htKeyword
  HelpKeyword = 'Export_Model_Outline_Dialog_Bo'
  Caption = 'Export Model Outline'
  ClientHeight = 286
  ClientWidth = 346
  ExplicitWidth = 358
  ExplicitHeight = 324
  TextHeight = 18
  object lblModel: TLabel
    Left = 8
    Top = 8
    Width = 107
    Height = 18
    Caption = 'Model to export'
  end
  object lblGridLines: TLabel
    Left = 136
    Top = 211
    Width = 193
    Height = 18
    Caption = 'Grid line frequency to export'
    Enabled = False
  end
  object rgExportChoice: TRadioGroup
    Left = 8
    Top = 64
    Width = 326
    Height = 129
    Anchors = [akLeft, akTop, akRight]
    Caption = 'What to export'
    ItemIndex = 2
    Items.Strings = (
      'Grid outline'
      'Active cells'
      'Active and inactive cells'
      'All grid lines'
      'Active area grid lines')
    TabOrder = 1
    OnClick = rgExportChoiceClick
    ExplicitWidth = 322
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 240
    Width = 346
    Height = 46
    Align = alBottom
    ParentColor = True
    TabOrder = 3
    ExplicitTop = 239
    ExplicitWidth = 342
    object btnCancel: TBitBtn
      Left = 207
      Top = 6
      Width = 91
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 110
      Top = 6
      Width = 91
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 13
      Top = 6
      Width = 91
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
  object comboModelSelection: TComboBox
    Left = 8
    Top = 32
    Width = 330
    Height = 26
    Style = csDropDownList
    TabOrder = 0
  end
  object seGridLines: TJvSpinEdit
    Left = 8
    Top = 208
    Width = 121
    Height = 26
    MaxValue = 10000000.000000000000000000
    MinValue = 1.000000000000000000
    Value = 1.000000000000000000
    Enabled = False
    TabOrder = 2
  end
  object sdShapefile: TSaveDialog
    DefaultExt = '.shp'
    Filter = 'Shapefiles (*.shp)|*.shp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 96
    Top = 48
  end
  object xbsShapeFile: TXBase
    Active = False
    AutoUpDate = True
    DebugErr = False
    Deleted = False
    Left = 136
    Top = 48
  end
end
