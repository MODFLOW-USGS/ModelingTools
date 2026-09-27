inherited frmDisvQualityMetrics: TfrmDisvQualityMetrics
  Caption = 'DISV Quality Metrics'
  ClientHeight = 440
  StyleElements = [seFont, seClient, seBorder]
  ExplicitHeight = 479
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 398
    Width = 764
    Height = 42
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -101
    ExplicitTop = 159
    ExplicitWidth = 865
    DesignSize = (
      764
      42)
    object btnHelp: TBitBtn
      Left = 588
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 677
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 1
    end
  end
  object pcMain: TPageControl
    Left = 0
    Top = 73
    Width = 764
    Height = 325
    ActivePage = tabCrossingFractions
    Align = alClient
    TabOrder = 1
    object tabCrossingAngles: TTabSheet
      Caption = 'Crossing Angles'
      object splCrossingAngles: TSplitter
        Left = 451
        Top = 0
        Width = 5
        Height = 292
        Align = alRight
        ExplicitLeft = 325
        ExplicitTop = 4
      end
      object pbCrossingAngles: TPaintBox
        Left = 0
        Top = 0
        Width = 451
        Height = 292
        Align = alClient
        OnPaint = pbCrossingAnglesPaint
        ExplicitLeft = 5
        ExplicitWidth = 751
      end
      object rdgCrossingAngles: TRbwDataGrid4
        Left = 456
        Top = 0
        Width = 300
        Height = 292
        Align = alRight
        ColCount = 3
        DefaultColWidth = 80
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnButtonClick = rdgCrossingAnglesButtonClick
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'Go to'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 60
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
            ButtonCaption = 'Go to'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 60
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
            ButtonCaption = 'Go to'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 60
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
        ExplicitLeft = 457
        ExplicitTop = -2
        ColWidths = (
          74
          95
          80)
      end
    end
    object tabCrossingFractions: TTabSheet
      Caption = 'Crossing Fractions'
      ImageIndex = 1
      object pbCrossingFractions: TPaintBox
        Left = 0
        Top = 0
        Width = 451
        Height = 292
        Align = alClient
        OnPaint = pbCrossingFractionsPaint
        ExplicitLeft = 149
        ExplicitTop = -2
        ExplicitWidth = 157
        ExplicitHeight = 250
      end
      object splCrossingFractions: TSplitter
        Left = 451
        Top = 0
        Width = 5
        Height = 292
        Align = alRight
        ExplicitLeft = 325
        ExplicitTop = 4
      end
      object rdgCrossingFractions: TRbwDataGrid4
        Left = 456
        Top = 0
        Width = 300
        Height = 292
        Align = alRight
        ColCount = 3
        DefaultColWidth = 80
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 0
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnButtonClick = rdgCrossingAnglesButtonClick
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = 'Go to'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 60
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
            ButtonCaption = 'Go to'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 60
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
            ButtonFont.Height = -12
            ButtonFont.Name = 'Segoe UI'
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
        ExplicitLeft = 457
        ExplicitTop = -2
        ColWidths = (
          74
          95
          80)
      end
    end
  end
  object pnlOverall: TPanel
    Left = 0
    Top = 0
    Width = 764
    Height = 73
    Align = alTop
    TabOrder = 2
    object lblMinAngle: TLabel
      Left = 8
      Top = 16
      Width = 568
      Height = 18
      Caption = 'Minimum Crossing  Angle = ?'
    end
    object lblMinMidFraction: TLabel
      Left = 8
      Top = 40
      Width = 609
      Height = 18
      Caption = 'Minimum Crossing Fraction = ?'
    end
  end
end
