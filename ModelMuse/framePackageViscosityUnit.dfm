inherited framePackageViscosity: TframePackageViscosity
  Width = 715
  Height = 524
  ExplicitWidth = 715
  ExplicitHeight = 524
  DesignSize = (
    715
    524)
  object lblRefViscosity: TLabel [2]
    Left = 176
    Top = 208
    Width = 153
    Height = 15
    Caption = 'Reference viscosity'
  end
  object lblThermalSpecies: TLabel [3]
    Left = 176
    Top = 236
    Width = 177
    Height = 15
    Caption = 'Thermal species'
  end
  object lblThermalFormulation: TLabel [4]
    Left = 176
    Top = 267
    Width = 109
    Height = 15
    Caption = 'Thermal formulation'
  end
  object lblThermalA2: TLabel [5]
    Left = 176
    Top = 296
    Width = 109
    Height = 15
    Caption = 'Thermal A2'
  end
  object lblThermalA3: TLabel [6]
    Left = 176
    Top = 324
    Width = 109
    Height = 15
    Caption = 'Thermal A3'
  end
  object lblThermalA4: TLabel [7]
    Left = 176
    Top = 352
    Width = 109
    Height = 15
    Caption = 'Thermal A4'
  end
  inherited memoComments: TMemo
    Width = 684
  end
  object cbSpecifyViscosity: TCheckBox [9]
    Left = 16
    Top = 160
    Width = 185
    Height = 17
    Caption = 'Specify viscosity'
    Enabled = False
    TabOrder = 1
  end
  object rdeRefViscosity: TRbwDataEntry [10]
    Left = 16
    Top = 205
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbWriteViscosity: TCheckBox [11]
    Left = 16
    Top = 183
    Width = 209
    Height = 26
    Caption = 'Write viscosity'
    Enabled = False
    TabOrder = 3
  end
  object comboThermalSpecies: TComboBox [12]
    Left = 16
    Top = 233
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    TabOrder = 4
  end
  object comboThermalFormulation: TComboBox [13]
    Left = 16
    Top = 264
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    TabOrder = 5
    Items.Strings = (
      'LINEAR'
      'NONLINEAR')
  end
  object rdeThermalA2: TRbwDataEntry [14]
    Left = 16
    Top = 293
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
  object rdeThermalA3: TRbwDataEntry [15]
    Left = 16
    Top = 321
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 7
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeThermalA4: TRbwDataEntry [16]
    Left = 16
    Top = 349
    Width = 145
    Height = 22
    Color = clBtnFace
    Enabled = False
    TabOrder = 8
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdgChemViscosity: TRbwDataGrid4 [17]
    Left = 0
    Top = 375
    Width = 715
    Height = 149
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    Enabled = False
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 9
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    Columns = <
      item
        AutoAdjustRowHeights = True
        AutoAdjustCaptionRowHeights = True
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -12
        ButtonFont.Name = 'Segoe UI'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = True
        AutoAdjustCaptionRowHeights = True
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -12
        ButtonFont.Name = 'Segoe UI'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = True
        AutoAdjustCaptionRowHeights = True
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -12
        ButtonFont.Name = 'Segoe UI'
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 20
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitTop = 373
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = cbSpecifyViscosity
      end
      item
        Control = cbWriteViscosity
      end
      item
        Control = rdeRefViscosity
      end
      item
        Control = comboThermalSpecies
      end
      item
        Control = comboThermalFormulation
      end
      item
        Control = rdeThermalA2
      end
      item
        Control = rdeThermalA3
      end
      item
        Control = rdeThermalA4
      end
      item
        Control = rdgChemViscosity
      end>
  end
end
