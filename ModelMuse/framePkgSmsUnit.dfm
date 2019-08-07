inherited framePkgSms: TframePkgSms
  Width = 495
  Height = 410
  ExplicitWidth = 495
  ExplicitHeight = 410
  DesignSize = (
    495
    410)
  inherited memoComments: TMemo
    Width = 464
    ExplicitWidth = 464
  end
  object pgcControls: TPageControl [3]
    Left = 0
    Top = 157
    Width = 495
    Height = 253
    ActivePage = tabLinear
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabBasic: TTabSheet
      Caption = 'Basic'
      object lblPrintOption: TLabel
        Left = 16
        Top = 4
        Width = 55
        Height = 13
        Caption = 'Print option'
      end
      object lblComplexity: TLabel
        Left = 152
        Top = 4
        Width = 53
        Height = 13
        Caption = 'Complexity'
      end
      object lblSolutionGroupMaxIter: TLabel
        Left = 264
        Top = 4
        Width = 140
        Height = 13
        Caption = 'Solution group max iterations'
      end
      object comboPrintOption: TJvImageComboBox
        Left = 16
        Top = 31
        Width = 113
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
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
        Top = 31
        Width = 97
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
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
        Top = 60
        Width = 249
        Height = 17
        Caption = 'Continue even if no convergence'
        Enabled = False
        TabOrder = 2
      end
      object cbCsvOutput: TCheckBox
        Left = 16
        Top = 83
        Width = 464
        Height = 17
        Caption = 'Write solver convergence values to CSV file'
        Enabled = False
        TabOrder = 3
      end
      object seSolutionGroupMaxIter: TJvSpinEdit
        Left = 264
        Top = 33
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 4
      end
    end
    object tabNonLinear: TTabSheet
      Caption = 'Nonlinear'
      ImageIndex = 1
      object rdgNonlinearOptions: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 487
        Height = 225
        Align = alClient
        ColCount = 3
        Enabled = False
        FixedCols = 1
        RowCount = 23
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
        Height = 225
        Align = alClient
        ColCount = 3
        Enabled = False
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
      end>
  end
end
