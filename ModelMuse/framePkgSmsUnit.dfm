inherited framePkgSms: TframePkgSms
  Width = 495
  Height = 517
  ExplicitWidth = 495
  ExplicitHeight = 517
  DesignSize = (
    495
    517)
  inherited lblComments: TLabel
    Enabled = True
  end
  inherited memoComments: TMemo
    Width = 464
    Height = 35
    Enabled = True
    ExplicitWidth = 464
    ExplicitHeight = 35
  end
  object pgcControls: TPageControl [3]
    Left = 0
    Top = 103
    Width = 495
    Height = 414
    ActivePage = tabBasic
    Align = alBottom
    TabOrder = 1
    OnChange = pgcControlsChange
    object tabBasic: TTabSheet
      Caption = 'Basic'
      object lblPrintOption: TLabel
        Left = 16
        Top = 4
        Width = 78
        Height = 20
        Caption = 'Print option'
      end
      object lblComplexity: TLabel
        Left = 152
        Top = 4
        Width = 75
        Height = 20
        Caption = 'Complexity'
      end
      object lblSolutionGroupMaxIter: TLabel
        Left = 264
        Top = 3
        Width = 197
        Height = 20
        Caption = 'Solution group max iterations'
      end
      object lblUsePTC: TLabel
        Left = 16
        Top = 124
        Width = 534
        Height = 20
        Caption = 
          'Use pseudo-transient continuation (PTC)  (Inverse of  NO_PTC and' +
          ' no_ptc_option)'
      end
      object lblMaxErrors: TLabel
        Left = 16
        Top = 175
        Width = 498
        Height = 20
        Caption = 
          'Maximum number of allowed error messages (-1 for no limit) (MAXE' +
          'RROR)'
      end
      object lblMemoryPrint: TLabel
        Left = 16
        Top = 244
        Width = 357
        Height = 20
        Caption = 'Print contents of memory ([MEMORY_PRINT_OPTION) '
      end
      object lblAtsMaxFrac: TLabel
        Left = 144
        Top = 342
        Width = 253
        Height = 40
        Caption = 
          'max outer iterations fraction with ATS (ATS_OUTER_MAXIMUM_FRACTI' +
          'ON)'
        WordWrap = True
      end
      object comboPrintOption: TJvImageComboBox
        Left = 16
        Top = 27
        Width = 113
        Height = 30
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 24
        ItemIndex = -1
        TabOrder = 0
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Summary (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'All (2)'
          end>
      end
      object comboComplexity: TJvImageComboBox
        Left = 152
        Top = 27
        Width = 97
        Height = 30
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 24
        ItemIndex = -1
        TabOrder = 1
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Simple'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Moderate'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Complex'
          end>
      end
      object cbContinue: TCheckBox
        Left = 16
        Top = 56
        Width = 249
        Height = 17
        Caption = 'Continue even if no convergence'
        TabOrder = 2
      end
      object cbCsvOutput: TCheckBox
        Left = 16
        Top = 79
        Width = 464
        Height = 17
        Caption = 'Write outer solver convergence values to CSV file'
        TabOrder = 3
      end
      object seSolutionGroupMaxIter: TJvSpinEdit
        Left = 264
        Top = 29
        Width = 121
        Height = 28
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 4
      end
      object comboUsePTC: TJvImageComboBox
        Left = 16
        Top = 143
        Width = 297
        Height = 30
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 297
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 24
        ItemIndex = 2
        TabOrder = 5
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Use PTC'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Don'#39't use PTC for first stress period'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Don'#39't use PTC for any stress period'
          end>
      end
      object seMaxErrors: TJvSpinEdit
        Left = 16
        Top = 194
        Width = 121
        Height = 28
        MaxValue = 2147483647.000000000000000000
        MinValue = -1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 6
      end
      object cbCheckInput: TCheckBox
        Left = 16
        Top = 221
        Width = 321
        Height = 17
        Caption = 'Check model input (Inverse of NOCHECK)'
        TabOrder = 7
      end
      object comboMemoryPrint: TJvImageComboBox
        Left = 16
        Top = 268
        Width = 121
        Height = 30
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 297
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 24
        ItemIndex = -1
        TabOrder = 8
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Summary'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'All'
          end>
      end
      object cbNewton: TCheckBox
        Left = 16
        Top = 297
        Width = 382
        Height = 17
        Caption = 'Use Newton formulation'
        TabOrder = 9
        OnClick = cbNewtonClick
      end
      object cbUnderRelaxation: TCheckBox
        Left = 16
        Top = 320
        Width = 406
        Height = 18
        Caption = 'Use Under_Relaxation option'
        Enabled = False
        TabOrder = 10
      end
      object cbCsvInnerOutput: TCheckBox
        Left = 16
        Top = 102
        Width = 464
        Height = 17
        Caption = 'Write inner solver convergence values to CSV file'
        TabOrder = 11
      end
      object rdeAtsMaxFrac: TRbwDataEntry
        Left = 17
        Top = 344
        Width = 121
        Height = 22
        TabOrder = 12
        Text = '0'
        DataType = dtReal
        Max = 0.500000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabNonLinear: TTabSheet
      Caption = 'Nonlinear'
      ImageIndex = 1
      object rdgNonlinearOptions: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 487
        Height = 379
        Align = alClient
        ColCount = 3
        FixedCols = 1
        RowCount = 24
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSelectCell = rdgNonlinearOptionsSelectCell
        OnSetEditText = rdgNonlinearOptionsSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnStateChange = rdgNonlinearOptionsStateChange
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
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = True
            Max = 1.000000000000000000
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
          84
          64
          64)
      end
    end
    object tabLinear: TTabSheet
      Caption = 'Linear'
      ImageIndex = 2
      object rdgLinearOptions: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 487
        Height = 379
        Align = alClient
        ColCount = 3
        FixedCols = 1
        RowCount = 23
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSelectCell = rdgLinearOptionsSelectCell
        OnSetEditText = rdgLinearOptionsSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnStateChange = rdgLinearOptionsStateChange
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
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = True
            Max = 1.000000000000000000
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
          84
          64
          64)
      end
    end
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
        Control = comboPrintOption
      end
      item
        Control = comboComplexity
      end
      item
        Control = seSolutionGroupMaxIter
      end
      item
        Control = cbContinue
      end
      item
        Control = cbCsvOutput
      end
      item
        Control = rdgNonlinearOptions
      end
      item
        Control = rdgLinearOptions
      end
      item
        Control = lblUsePTC
      end
      item
        Control = comboUsePTC
      end
      item
        Control = lblMaxErrors
      end
      item
        Control = seMaxErrors
      end
      item
        Control = cbCheckInput
      end
      item
        Control = lblMemoryPrint
      end
      item
        Control = comboMemoryPrint
      end
      item
        Control = cbNewton
      end
      item
        Control = rdeAtsMaxFrac
      end>
    Enabled = True
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
