object frameSwrObsDisplay: TframeSwrObsDisplay
  Left = 0
  Top = 0
  Width = 574
  Height = 240
  Anchors = [akLeft, akTop, akRight]
  TabOrder = 0
  object pbPlot: TPaintBox
    Left = 126
    Top = 129
    Width = 448
    Height = 111
    Align = alClient
    OnPaint = pbPlotPaint
    ExplicitLeft = 224
    ExplicitTop = 120
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object spl1: TSplitter
    Left = 121
    Top = 129
    Width = 5
    Height = 111
    ExplicitTop = 33
    ExplicitHeight = 207
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 574
    Height = 129
    Align = alTop
    TabOrder = 0
    DesignSize = (
      574
      129)
    object lblAnimationInterval: TLabel
      Left = 280
      Top = 102
      Width = 136
      Height = 16
      Caption = 'Animation interval (sec)'
    end
    object lblObservationFile: TLabel
      Left = 7
      Top = 19
      Width = 119
      Height = 16
      Caption = 'SWR observation file'
    end
    object fedObservationFile: TJvFilenameEdit
      Left = 7
      Top = 38
      Width = 561
      Height = 21
      Filter = 
        'SWR Observation Output files (*.Swr_Obs_A;*.Swr_Obs_B)|*.Swr_Obs' +
        '_A;*.Swr_Obs_B'
      DialogOptions = [ofHideReadOnly, ofFileMustExist]
      DialogTitle = 'Browse for SWR Observation Output Files'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = fedObservationFileChange
    end
    object comboObservationType: TComboBox
      Left = 7
      Top = 68
      Width = 113
      Height = 24
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Stage'
      OnChange = comboObservationTypeChange
      Items.Strings = (
        'Stage'
        'Depth'
        'Bottom'
        'Flow'
        'Structure'
        'Base Flow')
    end
    object btnGetSelectedReaches: TButton
      Left = 7
      Top = 99
      Width = 170
      Height = 24
      Caption = 'Get selected reaches'
      TabOrder = 4
      OnClick = btnGetSelectedReachesClick
    end
    object seIncrement: TJvSpinEdit
      Left = 495
      Top = 99
      Width = 73
      Height = 21
      Increment = 0.100000000000000000
      MaxValue = 2147483647.000000000000000000
      ValueType = vtFloat
      Value = 0.100000000000000000
      TabOrder = 5
    end
    object btnAnimate: TButton
      Left = 280
      Top = 68
      Width = 113
      Height = 24
      Caption = 'Animate'
      TabOrder = 2
      OnClick = btnAnimateClick
    end
    object btnStopAnimation: TButton
      Left = 399
      Top = 68
      Width = 169
      Height = 25
      Caption = 'Stop animation'
      Enabled = False
      TabOrder = 3
      OnClick = btnStopAnimationClick
    end
  end
  object rdgTimes: TRbwDataGrid4
    Left = 0
    Top = 129
    Width = 121
    Height = 111
    Align = alLeft
    ColCount = 1
    DefaultColWidth = 50
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goAlwaysShowEditor]
    TabOrder = 1
    OnSelectCell = rdgTimesSelectCell
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
        WordWrapCaptions = True
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitTop = 97
    ExplicitHeight = 143
  end
  object jvtmrAnimate: TJvTimer
    Enabled = False
    OnTimer = jvtmrAnimateTimer
    Left = 200
    Top = 104
  end
end
