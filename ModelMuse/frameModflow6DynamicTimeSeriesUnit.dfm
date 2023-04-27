object frameModflow6DynamicTimeSeries: TframeModflow6DynamicTimeSeries
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 377
    Width = 640
    Height = 103
    Align = alBottom
    TabOrder = 0
    object lblTimeSeriesCount: TLabel
      Left = 8
      Top = 8
      Width = 117
      Height = 15
      Caption = 'Number of time series'
    end
    object lblTimeCount: TLabel
      Left = 8
      Top = 54
      Width = 90
      Height = 15
      Caption = 'Number of times'
    end
    object seTimeSeriesCount: TJvSpinEdit
      Left = 8
      Top = 27
      Width = 73
      Height = 23
      CheckMinValue = True
      TabOrder = 0
      OnChange = seTimeSeriesCountChange
    end
    object seTimeCount: TJvSpinEdit
      Left = 8
      Top = 75
      Width = 73
      Height = 23
      MaxValue = 2147483647.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 1
      OnChange = seTimeCountChange
    end
    object btnDeleteTimeSeries: TButton
      Left = 121
      Top = 27
      Width = 152
      Height = 25
      Caption = 'Delete time series'
      Enabled = False
      TabOrder = 2
      OnClick = btnDeleteTimeSeriesClick
    end
    object btnDeleteTime: TButton
      Left = 122
      Top = 72
      Width = 103
      Height = 25
      Caption = 'Delete time'
      Enabled = False
      TabOrder = 3
      OnClick = btnDeleteTimeClick
    end
    object btnInsertTime: TButton
      Left = 231
      Top = 72
      Width = 97
      Height = 25
      Caption = 'Insert Time'
      TabOrder = 4
      OnClick = btnInsertTimeClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 640
    Height = 41
    Align = alTop
    TabOrder = 1
    object edGroupName: TEdit
      Left = 8
      Top = 14
      Width = 121
      Height = 23
      TabOrder = 0
      TextHint = 'Group Name'
    end
  end
  object rrdgTimeSeries: TRbwRowDataGrid
    Left = 0
    Top = 41
    Width = 640
    Height = 336
    Align = alClient
    ColCount = 2
    DefaultColWidth = 50
    FixedCols = 1
    RowCount = 6
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 2
    OnSelectCell = rrdgTimeSeriesSelectCell
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = True
    AutoIncreaseColCount = True
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBlack
    OnBeforeDrawCell = rrdgTimeSeriesBeforeDrawCell
    ColorRangeSelection = False
    ColorSelectedColumn = False
    Columns = <
      item
        AutoAdjustColWidths = True
      end
      item
        AutoAdjustColWidths = False
      end>
    Rows = <
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
        MaxLength = 24
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Multiply'
          'Add')
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        ComboUsed = True
        Format = rcf4String
        LimitToList = True
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Stepwise'
          'Linear'
          'LinearEnd')
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
      end>
  end
end
