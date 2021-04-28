inherited frameMt3dmsChemReactionPkg: TframeMt3dmsChemReactionPkg
  Width = 371
  Height = 433
  ExplicitWidth = 371
  ExplicitHeight = 433
  DesignSize = (
    371
    433)
  object lblSorptionChoice: TLabel [2]
    Left = 233
    Top = 160
    Width = 123
    Height = 13
    Caption = 'Sorption choice (ISOTHM)'
  end
  object lblKineticChoice: TLabel [3]
    Left = 233
    Top = 197
    Width = 112
    Height = 13
    Caption = 'Kinetic choice (IREACT)'
  end
  inherited memoComments: TMemo
    Width = 340
    ExplicitWidth = 340
  end
  object comboSorptionChoice: TJvImageComboBox [5]
    Left = 16
    Top = 157
    Width = 211
    Height = 21
    Style = csDropDownList
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 211
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemIndex = -1
    TabOrder = 1
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'None (0)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Linear (1)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Freundlich (2)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Langmuir (3)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'First order kinetic (4)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Dual domain - No sorption (5)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Dual domain - With sorption (6)'
      end>
  end
  object comboKineticChoice: TJvImageComboBox [6]
    Left = 16
    Top = 194
    Width = 211
    Height = 21
    Style = csDropDownList
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 211
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemIndex = -1
    TabOrder = 2
    OnChange = comboKineticChoiceChange
    Items = <
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'None (0)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'First order (1)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Monod (2)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'First-order chain (3)'
      end
      item
        Brush.Style = bsClear
        Indent = 0
        Text = 'Zero order (100)'
      end>
  end
  object cbInitialConcChoice: TCheckBox [7]
    Left = 16
    Top = 231
    Width = 273
    Height = 34
    Caption = 
      'Specify initial concentration of adsorbed and immobile phases of' +
      ' all species (IGETSC)'
    Enabled = False
    TabOrder = 3
    WordWrap = True
  end
  object rdgYieldCoefficient: TRbwDataGrid4 [8]
    Left = 16
    Top = 271
    Width = 340
    Height = 137
    ColCount = 2
    FixedCols = 1
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 4
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = rdgYieldCoefficientButtonClick
    ColorRangeSelection = False
    Columns = <
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustRowHeights = True
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = 'F()'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'Tahoma'
        ButtonFont.Style = []
        ButtonUsed = True
        ButtonWidth = 35
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
      end>
    WordWrapRowCaptions = False
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
        Control = comboSorptionChoice
      end
      item
        Control = comboKineticChoice
      end
      item
        Control = cbInitialConcChoice
      end>
  end
end
