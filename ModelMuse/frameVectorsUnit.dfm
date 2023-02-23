object frameVectors: TframeVectors
  Left = 0
  Top = 0
  Width = 380
  Height = 490
  HelpType = htKeyword
  HelpKeyword = 'Vectors_Pane'
  TabOrder = 0
  object lblScale: TLabel
    Left = 3
    Top = 64
    Width = 97
    Height = 15
    Caption = 'Scaling factor (2D)'
  end
  object lblMaxColor: TLabel
    Left = 3
    Top = 197
    Width = 85
    Height = 15
    Caption = 'Maximum color'
  end
  object lblMidColor: TLabel
    Left = 3
    Top = 249
    Width = 67
    Height = 15
    Caption = 'Middle color'
  end
  object lblMinColor: TLabel
    Left = 3
    Top = 301
    Width = 83
    Height = 15
    Caption = 'Minimum color'
  end
  object lblVelocityColor: TLabel
    Left = 3
    Top = 355
    Width = 71
    Height = 15
    Caption = 'Velocity color'
  end
  object lblVectorSource: TLabel
    Left = 3
    Top = 8
    Width = 71
    Height = 15
    Caption = 'Vector source'
  end
  object lblScale3D: TLabel
    Left = 3
    Top = 110
    Width = 97
    Height = 15
    Caption = 'Scaling factor (3D)'
  end
  object lblMinSpacing2D: TLabel
    Left = 154
    Top = 64
    Width = 191
    Height = 15
    Caption = 'Minimum vector spacing 2D (pixels)'
  end
  object lblMinHorizontalSpacing3D: TLabel
    Left = 154
    Top = 110
    Width = 170
    Height = 15
    Caption = 'Minimum horizontal spacing 3D'
  end
  object lblMinVerticalSpacing3D: TLabel
    Left = 154
    Top = 157
    Width = 155
    Height = 15
    Caption = 'Minimum vertical spacing 3D'
  end
  object lblLineThickness: TLabel
    Left = 3
    Top = 425
    Width = 74
    Height = 15
    Caption = 'Line thickness'
  end
  object rdeScale: TRbwDataEntry
    Left = 3
    Top = 83
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboVectorSource: TComboBox
    Left = 3
    Top = 27
    Width = 302
    Height = 23
    Style = csDropDownList
    TabOrder = 0
    OnChange = comboVectorSourceChange
  end
  object clrbxMax: TColorBox
    Left = 3
    Top = 216
    Width = 145
    Height = 22
    Selected = clRed
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
    TabOrder = 7
  end
  object clrbxMid: TColorBox
    Left = 3
    Top = 268
    Width = 145
    Height = 22
    Selected = clGreen
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
    TabOrder = 9
  end
  object clrbxMin: TColorBox
    Left = 3
    Top = 320
    Width = 145
    Height = 22
    Selected = clBlue
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
    TabOrder = 11
  end
  object clrbxVelocity: TColorBox
    Left = 3
    Top = 374
    Width = 145
    Height = 22
    Selected = clFuchsia
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
    TabOrder = 13
  end
  object rdeScale3D: TRbwDataEntry
    Left = 3
    Top = 129
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 4
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object udVectors: TJvUpDown
    Left = 305
    Top = 27
    Width = 20
    Height = 28
    Associate = comboVectorSource
    Min = -1
    TabOrder = 1
    OnChangingEx = udVectorsChangingEx
  end
  object seMinSpacing2D: TJvSpinEdit
    Left = 154
    Top = 83
    Width = 121
    Height = 28
    MaxValue = 2147483647.000000000000000000
    Enabled = False
    TabOrder = 3
  end
  object cbMaxVisible: TCheckBox
    Left = 154
    Top = 220
    Width = 200
    Height = 17
    Caption = 'Show maximum vector'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 8
  end
  object cbMidVisible: TCheckBox
    Left = 154
    Top = 272
    Width = 200
    Height = 17
    Caption = 'Show middle vector'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 10
  end
  object cbMinVisible: TCheckBox
    Left = 154
    Top = 324
    Width = 200
    Height = 17
    Caption = 'Show minimum vector'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 12
  end
  object rdeMinHorizontalSpacing3D: TRbwDataEntry
    Left = 154
    Top = 129
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeMinVerticalSpacing3D: TRbwDataEntry
    Left = 154
    Top = 176
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 6
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbLogTransform: TCheckBox
    Left = 3
    Top = 402
    Width = 190
    Height = 17
    Caption = 'Log transform'
    TabOrder = 14
  end
  object seLineThickness: TJvSpinEdit
    Left = 3
    Top = 447
    Width = 145
    Height = 28
    MaxValue = 100.000000000000000000
    ValueType = vtFloat
    Enabled = False
    TabOrder = 15
  end
end
