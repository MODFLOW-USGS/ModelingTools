object frameModpathDisplay: TframeModpathDisplay
  Left = 0
  Top = 0
  Width = 465
  Height = 486
  TabOrder = 0
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 486
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 0
    object tabBasic: TTabSheet
      Caption = 'Basic'
      DesignSize = (
        457
        458)
      object lblModpathFile: TLabel
        Left = 8
        Top = 8
        Width = 107
        Height = 13
        Caption = 'MODPATH pathline file'
      end
      object lblColorScheme: TLabel
        Left = 8
        Top = 108
        Width = 64
        Height = 13
        Caption = 'Color scheme'
      end
      object pbColorScheme: TPaintBox
        Left = 8
        Top = 167
        Width = 438
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        OnPaint = pbColorSchemePaint
        ExplicitWidth = 440
      end
      object lblColorAdjustment: TLabel
        Left = 8
        Top = 211
        Width = 82
        Height = 13
        Caption = 'Color adjustment'
      end
      object lblCycles: TLabel
        Left = 345
        Top = 211
        Width = 31
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Cycles'
      end
      object lblMaxTime: TLabel
        Left = 192
        Top = 8
        Width = 52
        Height = 13
        Caption = 'lblMaxTime'
      end
      object lblModelSelection: TLabel
        Left = 8
        Top = 273
        Width = 73
        Height = 13
        Caption = 'Model selection'
      end
      object fedModpathFile: TJvFilenameEdit
        Left = 8
        Top = 29
        Width = 438
        Height = 21
        OnBeforeDialog = fedModpathFileBeforeDialog
        DefaultExt = '.path'
        Filter = 
          'MODPATH Pathline files (*.path;*.path_bin;*.pathline)|*.path;*.p' +
          'ath_bin;*.pathline|All files (*.*)|*.*'
        DialogOptions = [ofHideReadOnly, ofFileMustExist]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
      end
      object cbShowPathlines: TCheckBox
        Left = 8
        Top = 62
        Width = 193
        Height = 17
        Caption = 'Show pathlines'
        TabOrder = 1
      end
      object cbLimitToCurrentIn2D: TCheckBox
        Left = 8
        Top = 85
        Width = 377
        Height = 17
        Caption = 'Limit to current column, row and layer in 2D views'
        TabOrder = 2
      end
      object comboColorScheme: TComboBox
        Left = 8
        Top = 128
        Width = 335
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        ItemIndex = 0
        TabOrder = 3
        Text = 'Rainbow'
        OnChange = comboColorSchemeChange
        Items.Strings = (
          'Rainbow'
          'Green to Magenta'
          'Blue to Red'
          'Blue to Dark Orange'
          'Blue to Green'
          'Brown to Blue'
          'Blue to Gray'
          'Blue to Orange'
          'Blue to Orange-Red'
          'Light Blue to Dark Blue'
          'Modified Spectral Scheme'
          'Stepped Sequential')
      end
      object jsColorExponent: TJvxSlider
        Left = 3
        Top = 227
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 4
        Value = 40
        OnChange = jsColorExponentChange
      end
      object seColorExponent: TJvSpinEdit
        Left = 159
        Top = 241
        Width = 65
        Height = 21
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        TabOrder = 5
        OnChange = seColorExponentChange
      end
      object seCycles: TJvSpinEdit
        Left = 345
        Top = 241
        Width = 101
        Height = 21
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 6
      end
      object comboModelSelection: TComboBox
        Left = 8
        Top = 292
        Width = 241
        Height = 21
        Style = csDropDownList
        TabOrder = 7
        Visible = False
        OnChange = comboModelSelectionChange
      end
      object btnColorSchemes: TButton
        Left = 349
        Top = 108
        Width = 97
        Height = 41
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Edit custom color schemes'
        TabOrder = 8
        WordWrap = True
        OnClick = btnColorSchemesClick
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        457
        458)
      object rgShow2D: TRadioGroup
        Left = 3
        Top = 3
        Width = 449
        Height = 116
        Caption = 'What to show'
        ItemIndex = 0
        Items.Strings = (
          'Show all'
          'Specify columns, rows, layers, and/or times to show'
          'Specify starting columns, rows, layers, and/or times to show'
          'Specify ending columns, rows, layers, and/or times to show')
        TabOrder = 0
        OnClick = rgShow2DClick
      end
      object rgColorBy: TRadioGroup
        Left = 3
        Top = 125
        Width = 137
        Height = 180
        Caption = 'Color by'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Time'
          'log(Time)'
          'X'#39
          'Y'#39
          'Z'
          'Group')
        TabOrder = 1
        OnClick = rgColorByClick
      end
      object rdgLimits: TRbwDataGrid4
        Left = 146
        Top = 125
        Width = 306
        Height = 332
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 3
        FixedCols = 0
        RowCount = 6
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 2
        OnSelectCell = rdgLimitsSelectCell
        OnSetEditText = rdgLimitsSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnStateChange = rdgLimitsStateChange
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = True
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
            Format = rcf4Boolean
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
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
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
            Format = rcf4Integer
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
      end
    end
  end
end
