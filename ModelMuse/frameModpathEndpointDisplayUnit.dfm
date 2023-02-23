object frameModpathEndpointDisplay: TframeModpathEndpointDisplay
  Left = 0
  Top = 0
  Width = 476
  Height = 498
  TabOrder = 0
  object pcMain: TPageControl
    Left = 0
    Top = 0
    Width = 476
    Height = 498
    ActivePage = tabBasic
    Align = alClient
    TabOrder = 0
    object tabBasic: TTabSheet
      Caption = 'Basic'
      DesignSize = (
        468
        468)
      object lblModpathFile: TLabel
        Left = 8
        Top = 8
        Width = 126
        Height = 15
        Caption = 'MODPATH endpoint file'
      end
      object lblColorScheme: TLabel
        Left = 8
        Top = 108
        Width = 73
        Height = 15
        Caption = 'Color scheme'
      end
      object pbColorScheme: TPaintBox
        Left = 8
        Top = 167
        Width = 457
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        OnPaint = pbColorSchemePaint
        ExplicitWidth = 449
      end
      object lblColorAdjustment: TLabel
        Left = 8
        Top = 211
        Width = 92
        Height = 15
        Caption = 'Color adjustment'
      end
      object lblCycles: TLabel
        Left = 364
        Top = 211
        Width = 34
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Cycles'
      end
      object lblModelSelection: TLabel
        Left = 8
        Top = 273
        Width = 84
        Height = 15
        Caption = 'Model selection'
      end
      object lblEndPointSize: TLabel
        Left = 344
        Top = 273
        Width = 111
        Height = 15
        Anchors = [akTop, akRight]
        Caption = 'Endpoint size (pixels)'
      end
      object fedModpathFile: TJvFilenameEdit
        Left = 8
        Top = 29
        Width = 457
        Height = 21
        OnBeforeDialog = fedModpathFileBeforeDialog
        DefaultExt = '.end'
        Filter = 
          'MODPATH Endpoint files (*.end, *.end_bin)|*.end;*.end_bin|All fi' +
          'les (*.*)|*.*'
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
        Caption = 'Show endpoints'
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
        Width = 354
        Height = 23
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
        Left = 8
        Top = 227
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 4
        Value = 40
        OnChanged = jsColorExponentChanged
      end
      object seColorExponent: TJvSpinEdit
        Left = 164
        Top = 232
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
        Left = 365
        Top = 232
        Width = 101
        Height = 21
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 6
        OnChange = seCyclesChange
      end
      object comboModelSelection: TComboBox
        Left = 8
        Top = 292
        Width = 241
        Height = 23
        Style = csDropDownList
        TabOrder = 7
        Visible = False
        OnChange = comboModelSelectionChange
      end
      object seEndPointSize: TJvSpinEdit
        Left = 365
        Top = 292
        Width = 100
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 8
      end
      object btnColorSchemes: TButton
        Left = 368
        Top = 108
        Width = 97
        Height = 41
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Edit custom color schemes'
        TabOrder = 9
        WordWrap = True
        OnClick = btnColorSchemesClick
      end
    end
    object tabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 1
      DesignSize = (
        468
        468)
      object rgShow2D: TRadioGroup
        Left = 9
        Top = 0
        Width = 256
        Height = 61
        Caption = 'What to show'
        ItemIndex = 0
        Items.Strings = (
          'Show all'
          'Limit by location, zone, time, or group')
        TabOrder = 0
        OnClick = rgShow2DClick
      end
      object rgWhereToPlot: TRadioGroup
        Left = 275
        Top = 0
        Width = 185
        Height = 61
        Caption = 'Where to plot'
        ItemIndex = 1
        Items.Strings = (
          'Starting locations'
          'Ending locations')
        TabOrder = 1
      end
      object rgColorBy: TRadioGroup
        Left = 8
        Top = 67
        Width = 177
        Height = 254
        Caption = 'Color by'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Release time'
          'Tracking time'
          'log(Tracking time)'
          'Starting X'#39
          'Starting Y'#39
          'Starting Z'
          'Starting zone'
          'Ending X'#39
          'Ending Y'#39
          'Ending Z'
          'Ending zone'
          'Group')
        TabOrder = 2
        OnClick = rgColorByClick
      end
      object rdgLimits: TRbwDataGrid4
        Left = 216
        Top = 67
        Width = 249
        Height = 394
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 3
        FixedCols = 0
        RowCount = 6
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 3
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
    object tabLegend: TTabSheet
      Caption = 'Legend'
      ImageIndex = 2
      TabVisible = False
      object imLegend: TImage
        Left = 206
        Top = 0
        Width = 262
        Height = 468
        Align = alClient
        ExplicitLeft = 224
        ExplicitTop = -2
        ExplicitWidth = 380
        ExplicitHeight = 335
      end
      object splColor: TSplitter
        Left = 201
        Top = 0
        Width = 5
        Height = 468
        ExplicitLeft = 218
        ExplicitHeight = 400
      end
      object pnlLegend: TPanel
        Left = 0
        Top = 0
        Width = 201
        Height = 468
        Align = alLeft
        BevelInner = bvRaised
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 470
        DesignSize = (
          201
          468)
        object lblMethod: TLabel
          Left = 8
          Top = 6
          Width = 42
          Height = 15
          Caption = 'Method'
        end
        object lblColorLegendRows: TLabel
          Left = 8
          Top = 386
          Width = 86
          Height = 15
          Anchors = [akLeft, akBottom]
          Caption = 'Number of rows'
        end
        object comboMethod: TComboBox
          Left = 8
          Top = 27
          Width = 145
          Height = 23
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'Automatic'
          OnChange = comboMethodChange
          Items.Strings = (
            'Automatic'
            'Manual')
        end
        object seLegendRows: TJvSpinEdit
          Left = 8
          Top = 405
          Width = 121
          Height = 21
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          Enabled = False
          Anchors = [akLeft, akBottom]
          TabOrder = 2
          OnChange = seLegendRowsChange
        end
        object rdgLegend: TRbwDataGrid4
          Left = 8
          Top = 59
          Width = 184
          Height = 321
          Anchors = [akLeft, akTop, akRight, akBottom]
          Color = clBtnFace
          ColCount = 1
          Enabled = False
          FixedCols = 0
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 1
          OnSetEditText = rdgLegendSetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnEndUpdate = rdgLegendEndUpdate
          OnStateChange = rdgLegendStateChange
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
            end>
          WordWrapRowCaptions = False
        end
        object btnFont: TButton
          Left = 8
          Top = 435
          Width = 75
          Height = 25
          Anchors = [akLeft]
          Caption = 'Font'
          TabOrder = 3
          OnClick = btnFontClick
        end
      end
    end
  end
  object dlgFontLegend: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 160
    Top = 304
  end
  object tmrLegend: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrLegendTimer
    Left = 264
    Top = 40
  end
end
