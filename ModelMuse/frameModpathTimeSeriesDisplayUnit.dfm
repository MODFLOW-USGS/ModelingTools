object frameModpathTimeSeriesDisplay: TframeModpathTimeSeriesDisplay
  Left = 0
  Top = 0
  Width = 465
  Height = 487
  TabOrder = 0
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 487
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 0
    object tabBasic: TTabSheet
      Caption = 'Basic'
      ExplicitLeft = 36
      ExplicitTop = 28
      DesignSize = (
        457
        459)
      object lblModpathFile: TLabel
        Left = 3
        Top = 8
        Width = 120
        Height = 13
        Caption = 'MODPATH time series file'
      end
      object lblTimeToPlot: TLabel
        Left = 3
        Top = 64
        Width = 56
        Height = 13
        Caption = 'Time to plot'
      end
      object lblColorScheme: TLabel
        Left = 3
        Top = 166
        Width = 64
        Height = 13
        Caption = 'Color scheme'
      end
      object pbColorScheme: TPaintBox
        Left = 3
        Top = 225
        Width = 448
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        OnPaint = pbColorSchemePaint
        ExplicitWidth = 449
      end
      object lblColorAdjustment: TLabel
        Left = 3
        Top = 264
        Width = 82
        Height = 13
        Caption = 'Color adjustment'
      end
      object lblCycles: TLabel
        Left = 351
        Top = 264
        Width = 31
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Cycles'
      end
      object lblModelSelection: TLabel
        Left = 3
        Top = 327
        Width = 73
        Height = 13
        Caption = 'Model selection'
      end
      object lbltimeSeriesSize: TLabel
        Left = 287
        Top = 327
        Width = 112
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Time series size (pixels)'
      end
      object fedModpathFile: TJvFilenameEdit
        Left = 2
        Top = 29
        Width = 448
        Height = 21
        OnBeforeDialog = fedModpathFileBeforeDialog
        DefaultExt = '.ts'
        Filter = 
          'MODPATH Time Series files (*.ts, *.ts_bin)|*.ts;*.ts_bin|All fil' +
          'es (*.*)|*.*'
        DialogOptions = [ofHideReadOnly, ofFileMustExist]
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModpathFileChange
      end
      object comboTimeToPlot: TComboBox
        Left = 3
        Top = 85
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 1
        OnChange = comboTimeToPlotChange
      end
      object udTimeToPlot: TJvUpDown
        Left = 148
        Top = 85
        Width = 17
        Height = 26
        TabOrder = 2
        OnChangingEx = udTimeToPlotChangingEx
      end
      object cbShowPathlines: TCheckBox
        Left = 3
        Top = 120
        Width = 193
        Height = 17
        Caption = 'Show time series'
        TabOrder = 3
      end
      object cbLimitToCurrentIn2D: TCheckBox
        Left = 3
        Top = 143
        Width = 377
        Height = 17
        Caption = 'Limit to current column, row and layer in 2D views'
        TabOrder = 4
      end
      object comboColorScheme: TComboBox
        Left = 3
        Top = 186
        Width = 341
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 12
        ItemIndex = 0
        TabOrder = 5
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
        Top = 285
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 6
        Value = 40
        OnChanged = jsColorExponentChanged
      end
      object seColorExponent: TJvSpinEdit
        Left = 156
        Top = 285
        Width = 65
        Height = 21
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        TabOrder = 7
        OnChange = seColorExponentChange
      end
      object seCycles: TJvSpinEdit
        Left = 350
        Top = 285
        Width = 101
        Height = 21
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 8
        OnChange = seCyclesChange
      end
      object comboModelSelection: TComboBox
        Left = 3
        Top = 346
        Width = 241
        Height = 21
        Style = csDropDownList
        TabOrder = 9
        Visible = False
        OnChange = comboModelSelectionChange
      end
      object setimeSeriesSize: TJvSpinEdit
        Left = 350
        Top = 346
        Width = 100
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 10
      end
      object btnColorSchemes: TButton
        Left = 350
        Top = 166
        Width = 97
        Height = 41
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Edit custom color schemes'
        TabOrder = 11
        WordWrap = True
        OnClick = btnColorSchemesClick
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      DesignSize = (
        457
        459)
      object rgShow2D: TRadioGroup
        Left = 2
        Top = 3
        Width = 450
        Height = 101
        Caption = 'What to show'
        ItemIndex = 0
        Items.Strings = (
          'Show all'
          'Specify columns, rows, layers and/or groups to show'
          'Specify starting columns, rows, layers and/or groups to show'
          'Specify ending columns, rows, layers and/or groups to show')
        TabOrder = 0
        OnClick = rgShow2DClick
      end
      object rgColorBy: TRadioGroup
        Left = 3
        Top = 110
        Width = 137
        Height = 251
        Caption = 'Color by'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Particle index'
          'X'#39
          'Y'#39
          'Z'
          'Starting X'#39
          'Starting Y'#39
          'Starting Z'
          'Ending X'#39
          'Ending Y'#39
          'Ending Z'
          'Group')
        TabOrder = 1
        OnClick = rgColorByClick
      end
      object rdgLimits: TRbwDataGrid4
        Left = 155
        Top = 110
        Width = 297
        Height = 343
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
