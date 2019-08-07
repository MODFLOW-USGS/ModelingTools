object frameCustomColor: TframeCustomColor
  Left = 0
  Top = 0
  Width = 570
  Height = 428
  TabOrder = 0
  OnResize = FrameResize
  object pcChoices: TPageControl
    Left = 0
    Top = 0
    Width = 570
    Height = 428
    ActivePage = tabSelection
    Align = alClient
    TabOrder = 0
    object tabSelection: TTabSheet
      Caption = 'Selection'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        562
        400)
      object lblDataSet: TLabel
        Left = 8
        Top = 4
        Width = 149
        Height = 13
        Caption = 'Data set or boundary condition'
      end
      object lblColorScheme: TLabel
        Left = 8
        Top = 239
        Width = 64
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Color scheme'
        ExplicitTop = 236
      end
      object lblCycles: TLabel
        Left = 456
        Top = 266
        Width = 31
        Height = 13
        Anchors = [akRight, akBottom]
        Caption = 'Cycles'
        ExplicitTop = 263
      end
      object pbColorScheme: TPaintBox
        Left = 3
        Top = 303
        Width = 397
        Height = 33
        Anchors = [akLeft, akRight, akBottom]
        OnPaint = pbColorSchemePaint
        ExplicitTop = 300
      end
      object lblColorAdjustment: TLabel
        Left = 8
        Top = 342
        Width = 82
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Color adjustment'
        ExplicitTop = 339
      end
      object lblComment: TLabel
        Left = 8
        Top = 61
        Width = 143
        Height = 13
        Caption = 'Data set comment (read only)'
      end
      object comboColorScheme: TComboBox
        Left = 8
        Top = 252
        Width = 442
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akRight, akBottom]
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
      object seCycles: TJvSpinEdit
        Left = 456
        Top = 297
        Width = 101
        Height = 24
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akRight, akBottom]
        TabOrder = 6
        OnChange = seCyclesChange
        OnKeyUp = seCyclesKeyUp
      end
      object jsColorExponent: TJvxSlider
        Left = 4
        Top = 352
        Width = 150
        Height = 40
        Increment = 2
        MaxValue = 200
        TabOrder = 7
        Value = 40
        Anchors = [akLeft, akBottom]
        OnChange = jsColorExponentChange
      end
      object seColorExponent: TJvSpinEdit
        Left = 160
        Top = 363
        Width = 65
        Height = 24
        ButtonKind = bkClassic
        Increment = 0.010000000000000000
        MaxValue = 2.000000000000000000
        ValueType = vtFloat
        Value = 0.400000000000000000
        Anchors = [akLeft, akBottom]
        TabOrder = 8
        OnChange = seColorExponentChange
      end
      object cbLogTransform: TCheckBox
        Left = 247
        Top = 367
        Width = 137
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Log transform'
        TabOrder = 9
      end
      object udDataSets: TJvUpDown
        Left = 479
        Top = 28
        Width = 21
        Height = 25
        Anchors = [akTop, akRight]
        TabOrder = 1
        OnChangingEx = udDataSetsChangingEx
      end
      object rgUpdateLimitChoice: TRadioGroup
        Left = 8
        Top = 153
        Width = 305
        Height = 73
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'When changing data sets:'
        ItemIndex = 0
        Items.Strings = (
          'Update limits and legend (default)'
          'Retain limits and legend (animations)')
        TabOrder = 3
      end
      object virttreecomboDataSets: TRbwStringTreeCombo
        Left = 8
        Top = 25
        Width = 465
        Height = 24
        Tree.Left = 0
        Tree.Top = 0
        Tree.Width = 304
        Tree.Height = 202
        Tree.Align = alClient
        Tree.Header.AutoSizeIndex = 0
        Tree.Header.Font.Charset = DEFAULT_CHARSET
        Tree.Header.Font.Color = clWindowText
        Tree.Header.Font.Height = -11
        Tree.Header.Font.Name = 'Tahoma'
        Tree.Header.Font.Style = []
        Tree.Header.MainColumn = -1
        Tree.TabOrder = 0
        Tree.TreeOptions.SelectionOptions = [toFullRowSelect]
        Tree.OnChange = virttreecomboDataSetsTreeChange
        Tree.OnGetText = virttreecomboDataSetsTreeGetText
        Tree.OnInitNode = virttreecomboDataSetsTreeInitNode
        Tree.ExplicitWidth = 200
        Tree.ExplicitHeight = 100
        Tree.Columns = <>
        DropDownHeight = 207
        Enabled = True
        Glyph.Data = {
          36020000424D3602000000000000360000002800000010000000080000000100
          2000000000000002000000000000000000000000000000000000D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC0000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00000000000000000000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC000000
          000000000000000000000000000000000000D8E9EC00D8E9EC00D8E9EC00C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00000000000000
          00000000000000000000000000000000000000000000D8E9EC00C0C0C000C0C0
          C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
          EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00}
        NumGlyphs = 2
        TabOrder = 0
        OnChange = virttreecomboDataSetsChange
      end
      object reComment: TRichEdit
        Left = 8
        Top = 83
        Width = 549
        Height = 64
        Anchors = [akLeft, akTop, akRight, akBottom]
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
        Zoom = 100
      end
      object btnColorSchemes: TButton
        Left = 360
        Top = 173
        Width = 97
        Height = 41
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Edit custom color schemes'
        TabOrder = 4
        WordWrap = True
        OnClick = btnColorSchemesClick
      end
    end
    object tabFilters: TTabSheet
      Caption = 'Filters'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        562
        400)
      object lblLowerLimit: TLabel
        Left = 8
        Top = 3
        Width = 50
        Height = 13
        Caption = 'Lower limit'
      end
      object lblUpperLimit: TLabel
        Left = 299
        Top = 3
        Width = 50
        Height = 13
        Caption = 'Upper limit'
      end
      object lblValuesToIgnore: TLabel
        Left = 8
        Top = 81
        Width = 77
        Height = 13
        Caption = 'Values to ignore'
      end
      object lblNumberOfValuesToIgnore: TLabel
        Left = 130
        Top = 373
        Width = 130
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Number of values to ignore'
        ExplicitTop = 370
      end
      object lblEpsilon: TLabel
        Left = 203
        Top = 81
        Width = 116
        Height = 13
        Caption = 'Epsilon (margin of error)'
      end
      inline frameCheck3DMax: TframeDisplayLimit
        Left = 299
        Top = 24
        Width = 243
        Height = 35
        TabOrder = 1
        TabStop = True
        ExplicitLeft = 299
        ExplicitTop = 24
        inherited rdeLimit: TRbwDataEntry
          Height = 28
          TabOrder = 0
          ExplicitHeight = 28
        end
        inherited comboBoolLimit: TComboBox
          Left = 89
          TabOrder = 1
          ExplicitLeft = 89
          ExplicitHeight = 21
        end
      end
      inline frameCheck3DMin: TframeDisplayLimit
        Left = 8
        Top = 24
        Width = 243
        Height = 35
        TabOrder = 0
        TabStop = True
        ExplicitLeft = 8
        ExplicitTop = 24
        inherited rdeLimit: TRbwDataEntry
          Height = 28
          ExplicitHeight = 28
        end
        inherited comboBoolLimit: TComboBox
          ExplicitHeight = 21
        end
      end
      object cbActiveOnly: TCheckBox
        Left = 8
        Top = 58
        Width = 97
        Height = 17
        Caption = 'Only active'
        TabOrder = 2
      end
      object rdgValuesToIgnore: TRbwDataGrid4
        Left = 8
        Top = 112
        Width = 177
        Height = 246
        Anchors = [akLeft, akTop, akBottom]
        ColCount = 1
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 3
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
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
        OnEndUpdate = rdgValuesToIgnoreEndUpdate
        WordWrapRowCaptions = False
      end
      object seNumberOfValuesToIgnore: TJvSpinEdit
        Left = 8
        Top = 364
        Width = 121
        Height = 24
        CheckMinValue = True
        ButtonKind = bkClassic
        Anchors = [akLeft, akBottom]
        TabOrder = 5
        OnChange = seNumberOfValuesToIgnoreChange
      end
      object rdeEpsilon: TRbwDataEntry
        Left = 203
        Top = 112
        Width = 145
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabLegend: TTabSheet
      Caption = 'Legend'
      ImageIndex = 2
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object imLegend: TImage
        Left = 206
        Top = 0
        Width = 356
        Height = 400
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
        Height = 400
        ExplicitLeft = 218
      end
      object pnlLegend: TPanel
        Left = 0
        Top = 0
        Width = 201
        Height = 400
        Align = alLeft
        BevelInner = bvRaised
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 397
        DesignSize = (
          201
          400)
        object lblMethod: TLabel
          Left = 8
          Top = 6
          Width = 36
          Height = 13
          Caption = 'Method'
        end
        object lblColorLegendRows: TLabel
          Left = 8
          Top = 303
          Width = 76
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Number of rows'
        end
        object comboMethod: TComboBox
          Left = 8
          Top = 27
          Width = 145
          Height = 21
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
          Top = 322
          Width = 121
          Height = 24
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
          Height = 238
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
          OnStateChange = rdgLegendStateChange
          ColorRangeSelection = False
          Columns = <
            item
              AutoAdjustRowHeights = False
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
          OnEndUpdate = rdgLegendEndUpdate
          WordWrapRowCaptions = False
        end
        object btnFont: TButton
          Left = 8
          Top = 360
          Width = 75
          Height = 25
          Caption = 'Font'
          TabOrder = 3
          OnClick = btnFontClick
        end
      end
    end
  end
  object timerLegend: TTimer
    Interval = 100
    OnTimer = timerLegendTimer
    Left = 264
    Top = 40
  end
  object dlgFontLegend: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 160
    Top = 352
  end
end
