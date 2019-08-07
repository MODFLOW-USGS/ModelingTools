inherited frmSwrStructures: TfrmSwrStructures
  HelpType = htKeyword
  HelpKeyword = 'SWR_Structures_Dialog_Box'
  Caption = 'SWR Structures'
  ClientHeight = 555
  ClientWidth = 782
  OnResize = FormResize
  ExplicitWidth = 798
  ExplicitHeight = 593
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 513
    Width = 782
    Height = 42
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      782
      42)
    object btnHelp: TBitBtn
      Left = 511
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 600
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 689
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object pcStructures: TPageControl
    Left = 0
    Top = 0
    Width = 782
    Height = 513
    ActivePage = tabTiming
    Align = alClient
    TabOrder = 0
    object tabStructureDefinition: TTabSheet
      Caption = 'Structure Definition'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object splttrMain: TJvNetscapeSplitter
        Left = 0
        Top = 272
        Width = 774
        Height = 10
        Cursor = crVSplit
        Align = alBottom
        Maximized = False
        Minimized = False
        ButtonCursor = crDefault
        ExplicitTop = 227
        ExplicitWidth = 782
      end
      object jvplOptions: TJvPageList
        Left = 0
        Top = 282
        Width = 774
        Height = 198
        ActivePage = jvspSmoothing
        PropagateEnable = False
        Align = alBottom
        object jvspCulvert: TJvStandardPage
          Left = 0
          Top = 0
          Width = 774
          Height = 198
          Caption = 'jvspCulvert'
          object rdgCulvert: TRbwDataGrid4
            Left = 0
            Top = 41
            Width = 774
            Height = 157
            Align = alClient
            ColCount = 8
            FixedCols = 1
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
            TabOrder = 1
            OnMouseUp = rdgCulvertMouseUp
            OnSelectCell = rdgCulvertSelectCell
            OnSetEditText = rdgCulvertSetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = False
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnColSize = rdgCulvertColSize
            OnStateChange = rdgCulvertStateChange
            ColorRangeSelection = False
            OnHorizontalScroll = rdgCulvertHorizontalScroll
            Columns = <
              item
                AutoAdjustRowHeights = True
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                PickList.Strings = (
                  'Circular (>0)'
                  'Rectangular (<0)')
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
            ColWidths = (
              64
              64
              127
              64
              64
              64
              64
              64)
          end
          object pnlCulvert: TPanel
            Left = 0
            Top = 0
            Width = 774
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object comboCulvertType: TJvImageComboBox
              Left = 133
              Top = 7
              Width = 145
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              Color = clBtnFace
              DroppedWidth = 145
              Enabled = False
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 0
              OnChange = comboCulvertTypeChange
              Items = <>
            end
            object rdeCulvertValue: TRbwDataEntry
              Left = 48
              Top = 8
              Width = 65
              Height = 22
              Color = clBtnFace
              Enabled = False
              TabOrder = 1
              Text = '0'
              OnChange = rdeCulvertValueChange
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
            object cbCulvert: TCheckBox
              Left = 304
              Top = 16
              Width = 97
              Height = 17
              Enabled = False
              TabOrder = 2
              OnClick = cbCulvertClick
            end
          end
        end
        object jvspStageDischarge: TJvStandardPage
          Left = 0
          Top = 0
          Width = 774
          Height = 198
          Caption = 'jvspStageDischarge'
          inline frameStageDischarge: TframePlotGrid
            Left = 0
            Top = 0
            Width = 774
            Height = 198
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 774
            ExplicitHeight = 198
            inherited pbPlot: TPaintBox
              Left = 283
              Width = 491
              Height = 157
              OnPaint = frameStageDischargepbPlotPaint
              ExplicitLeft = 284
              ExplicitTop = 0
              ExplicitWidth = 491
              ExplicitHeight = 157
            end
            inherited splPlot: TSplitter
              Left = 278
              Height = 157
              ExplicitLeft = 278
              ExplicitHeight = 157
            end
            inherited Panel: TPanel
              Top = 157
              Width = 774
              ExplicitTop = 157
              ExplicitWidth = 774
              inherited lbNumber: TLabel
                Width = 55
                Height = 18
                ExplicitWidth = 55
                ExplicitHeight = 18
              end
              inherited sbAdd: TSpeedButton
                Left = 408
                OnClick = frameStageDischargesbAddClick
                ExplicitLeft = 412
              end
              inherited sbInsert: TSpeedButton
                Left = 483
                OnClick = frameStageDischargesbInsertClick
                ExplicitLeft = 488
              end
              inherited sbDelete: TSpeedButton
                Left = 557
                OnClick = frameStageDischargesbDeleteClick
                ExplicitLeft = 563
              end
              inherited seNumber: TJvSpinEdit
                Height = 26
                OnChange = frameStageDischargeseNumberChange
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 278
              Height = 157
              ColCount = 2
              OnSetEditText = frameStageDischargeGridSetEditText
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
                  ButtonCaption = '...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -13
                  ButtonFont.Name = 'Tahoma'
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
              OnEndUpdate = frameStageDischargeGridEndUpdate
              ExplicitWidth = 278
              ExplicitHeight = 157
            end
          end
        end
        object jvspPumpWeir: TJvStandardPage
          Left = 0
          Top = 0
          Width = 774
          Height = 198
          Caption = 'jvspPumpWeir'
          object rdgPumpWeir: TRbwDataGrid4
            Left = 0
            Top = 41
            Width = 774
            Height = 157
            Align = alClient
            ColCount = 12
            DefaultColWidth = 190
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
            TabOrder = 1
            OnMouseDown = rdgPumpWeirMouseDown
            OnMouseUp = rdgPumpWeirMouseUp
            OnSelectCell = rdgPumpWeirSelectCell
            OnSetEditText = rdgPumpWeirSetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = True
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = False
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            OnColSize = rdgPumpWeirColSize
            ColorRangeSelection = False
            OnHorizontalScroll = rdgPumpWeirHorizontalScroll
            Columns = <
              item
                AutoAdjustRowHeights = True
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                PickList.Strings = (
                  'STAGE'
                  'FLOW')
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                PickList.Strings = (
                  '>= (GE)'
                  '< (LT)')
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                PickList.Strings = (
                  'Value'
                  'Tab file')
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = True
                ButtonCaption = 'Browse...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 80
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
                ButtonCaption = 'Browse...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 80
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ColWidths = (
              99
              105
              104
              93
              130
              190
              190
              190
              190
              190
              190
              190)
            RowHeights = (
              24
              24)
          end
          object pnlPumpWeir: TPanel
            Left = 0
            Top = 0
            Width = 774
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object comboPumpWeirControlType: TJvImageComboBox
              Left = 32
              Top = 6
              Width = 49
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              Color = clBtnFace
              DroppedWidth = 145
              Enabled = False
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 0
              OnChange = comboPumpWeirControlTypeChange
              Items = <>
            end
            object comboPumpWeirControlOperated: TJvImageComboBox
              Left = 87
              Top = 6
              Width = 49
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              Color = clBtnFace
              DroppedWidth = 145
              Enabled = False
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 1
              OnChange = comboPumpWeirControlOperatedChange
              Items = <>
            end
            object comboPumpWeirCriticalMethod: TJvImageComboBox
              Left = 152
              Top = 6
              Width = 49
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              Color = clBtnFace
              DroppedWidth = 145
              Enabled = False
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 2
              OnChange = comboPumpWeirCriticalMethodChange
              Items = <>
            end
            object comboPumpWeirCriticalTabFileName: TJvImageComboBox
              Left = 207
              Top = 6
              Width = 49
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              Color = clBtnFace
              DroppedWidth = 145
              Enabled = False
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 3
              OnChange = comboPumpWeirCriticalTabFileNameChange
              Items = <>
            end
            object comboPumpWeirDischargeTabFile: TJvImageComboBox
              Left = 262
              Top = 6
              Width = 49
              Height = 28
              Style = csOwnerDrawVariable
              ButtonStyle = fsLighter
              Color = clBtnFace
              DroppedWidth = 145
              Enabled = False
              ImageHeight = 0
              ImageWidth = 0
              ItemHeight = 22
              ItemIndex = -1
              TabOrder = 4
              OnChange = comboPumpWeirDischargeTabFileChange
              Items = <>
            end
            object rdePumpWeirValue: TRbwDataEntry
              Left = 336
              Top = 8
              Width = 145
              Height = 22
              Color = clBtnFace
              Enabled = False
              TabOrder = 5
              Text = '0'
              OnChange = rdePumpWeirValueChange
              DataType = dtReal
              Max = 1.000000000000000000
              ChangeDisabledColor = True
            end
          end
        end
        object jvspBlank: TJvStandardPage
          Left = 0
          Top = 0
          Width = 774
          Height = 198
        end
        object jvspSmoothing: TJvStandardPage
          Left = 0
          Top = 0
          Width = 774
          Height = 198
          Caption = 'jvspSmoothing'
          object rdgSmoothing: TRbwDataGrid4
            Left = 0
            Top = 0
            Width = 774
            Height = 198
            Align = alClient
            ColCount = 3
            FixedCols = 1
            RowCount = 2
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
            TabOrder = 0
            OnSelectCell = rdgSmoothingSelectCell
            OnSetEditText = rdgSmoothingSetEditText
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
                AutoAdjustRowHeights = False
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
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
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = True
                Format = rcf4String
                LimitToList = True
                MaxLength = 0
                ParentButtonFont = False
                PickList.Strings = (
                  'Linear smoothing (< 0)'
                  'Sigmoid smoothing (> 0)')
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
                ButtonFont.Height = -13
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = True
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
            ColWidths = (
              64
              193
              64)
          end
        end
      end
      inline frameMain: TframeStructureGrid
        Left = 0
        Top = 0
        Width = 774
        Height = 272
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 774
        ExplicitHeight = 272
        inherited lblType: TLabel
          Width = 48
          Height = 18
          ExplicitWidth = 48
          ExplicitHeight = 18
        end
        inherited lblRestrictions: TLabel
          Width = 97
          Height = 18
          ExplicitWidth = 97
          ExplicitHeight = 18
        end
        inherited lblInitialValueMethod: TLabel
          Width = 141
          Height = 18
          ExplicitWidth = 141
          ExplicitHeight = 18
        end
        inherited lblInitialValueTabFile: TLabel
          Width = 140
          Height = 18
          ExplicitWidth = 140
          ExplicitHeight = 18
        end
        inherited Panel: TPanel
          Top = 231
          Width = 774
          ExplicitTop = 231
          ExplicitWidth = 774
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 255
            OnClick = frameMainsbAddClick
            ExplicitLeft = 255
          end
          inherited sbInsert: TSpeedButton
            Left = 305
            OnClick = frameMainsbInsertClick
            ExplicitLeft = 305
          end
          inherited sbDelete: TSpeedButton
            Left = 355
            OnClick = frameMainsbDeleteClick
            ExplicitLeft = 355
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameMainGridseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 768
          Height = 168
          OnSelectCell = frameMainGridGridSelectCell
          OnSetEditText = frameMainGridGridSetEditText
          OnTopLeftChanged = frameMainGridGridTopLeftChanged
          ExplicitWidth = 768
          ExplicitHeight = 168
        end
        inherited comboType: TJvImageComboBox
          Left = 79
          Height = 28
          ItemHeight = 22
          ExplicitLeft = 79
          ExplicitHeight = 28
        end
        inherited comboRestrictions: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ExplicitHeight = 28
        end
        inherited comboInitialValueMethod: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ExplicitHeight = 28
        end
        inherited comboInitialValueTabFile: TJvImageComboBox
          Height = 28
          ItemHeight = 22
          ExplicitHeight = 28
        end
      end
    end
    object tabTiming: TTabSheet
      Caption = 'Timing'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inline frameTiming: TframeFormulaGrid
        Left = 0
        Top = 0
        Width = 774
        Height = 480
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 774
        ExplicitHeight = 480
        inherited Panel: TPanel
          Top = 439
          Width = 774
          ExplicitTop = 439
          ExplicitWidth = 774
          inherited lbNumber: TLabel
            Width = 55
            Height = 18
            ExplicitWidth = 55
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 408
            ExplicitLeft = 408
          end
          inherited sbInsert: TSpeedButton
            Left = 483
            OnClick = frameTimingsbInsertClick
            ExplicitLeft = 483
          end
          inherited sbDelete: TSpeedButton
            Left = 557
            OnClick = frameTimingsbDeleteClick
            ExplicitLeft = 557
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameTimingseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Top = 25
          Width = 774
          Height = 414
          ColCount = 2
          OnSetEditText = frameTimingGridSetEditText
          OnStateChange = frameTimingGridStateChange
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
              ComboUsed = True
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
              ComboUsed = True
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
          ExplicitTop = 25
          ExplicitWidth = 774
          ExplicitHeight = 414
        end
        inherited pnlTop: TPanel
          Width = 774
          Height = 25
          ExplicitWidth = 774
          ExplicitHeight = 25
          inherited edFormula: TLabeledEdit
            Height = 26
            EditLabel.Width = 57
            EditLabel.Height = 18
            EditLabel.ExplicitTop = 9
            EditLabel.ExplicitWidth = 57
            EditLabel.ExplicitHeight = 18
            TabOrder = 1
            Visible = False
            ExplicitHeight = 26
          end
          inherited cbMultiCheck: TCheckBox
            Top = 0
            Caption = ''
            Enabled = False
            TabOrder = 0
            Visible = True
            ExplicitTop = 0
          end
        end
      end
    end
  end
end
