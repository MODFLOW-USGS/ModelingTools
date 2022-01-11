object framePestObservationResults: TframePestObservationResults
  Left = 0
  Top = 0
  Width = 555
  Height = 490
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 454
    Width = 555
    Height = 36
    Align = alBottom
    TabOrder = 0
    object lblRMS: TLabel
      Left = 8
      Top = 9
      Width = 200
      Height = 13
      Caption = 'Root Mean Square Weighted Residual = ?'
    end
    object comboModels: TComboBox
      Left = 8
      Top = 6
      Width = 180
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      Visible = False
    end
  end
  object pgcObservations: TPageControl
    Left = 0
    Top = 0
    Width = 555
    Height = 454
    ActivePage = tabValues
    Align = alClient
    TabOrder = 1
    OnChange = pgcObservationsChange
    object tabControls: TTabSheet
      Caption = 'Controls'
      object lblNegativeColor: TLabel
        Left = 3
        Top = 279
        Width = 132
        Height = 26
        Caption = 'Color for negative residuals'#13#10'(Simulated head too high)'
      end
      object lblColorPositive: TLabel
        Left = 255
        Top = 279
        Width = 127
        Height = 26
        Caption = 'Color for positive residuals'#13#10'(Simulated head too low)'
      end
      object lblMaxSymbolSize: TLabel
        Left = 130
        Top = 357
        Width = 139
        Height = 13
        Caption = 'Maximum symbol size (pixels)'
      end
      object lblHeadObsResults: TLabel
        Left = 3
        Top = 3
        Width = 45
        Height = 13
        Caption = 'File name'
      end
      object flnmedHeadObsResults: TJvFilenameEdit
        Left = 3
        Top = 22
        Width = 505
        Height = 21
        Filter = 'PEST Residuals file (*.res)|*.res'
        TabOrder = 0
        Text = ''
        OnChange = flnmedHeadObsResultsChange
      end
      object grpbxFilter: TGroupBox
        Left = 3
        Top = 82
        Width = 505
        Height = 191
        Caption = 'Filters'
        TabOrder = 2
        object lblMaximumTime: TLabel
          Left = 252
          Top = 86
          Width = 67
          Height = 13
          Caption = 'Maximum time'
        end
        object lblMaxResidual: TLabel
          Left = 252
          Top = 24
          Width = 84
          Height = 13
          Caption = 'Maximum residual'
        end
        object lblMinimumTime: TLabel
          Left = 3
          Top = 86
          Width = 63
          Height = 13
          Caption = 'Minimum time'
        end
        object lblMinResidual: TLabel
          Left = 3
          Top = 24
          Width = 80
          Height = 13
          Caption = 'Minimum residual'
        end
        object lblMinWeightedResidual: TLabel
          Left = 3
          Top = 138
          Width = 127
          Height = 13
          Caption = 'Minimum weighted residual'
        end
        object lblMaxWeightedResidual: TLabel
          Left = 252
          Top = 138
          Width = 131
          Height = 13
          Caption = 'Maximum weighted residual'
        end
        inline framelmtMinimumTime: TframeDisplayLimit
          Left = 3
          Top = 105
          Width = 243
          Height = 35
          TabOrder = 2
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 105
          inherited cbCheck: TCheckBox
            Height = 23
            Enabled = True
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMaxResidual: TframeDisplayLimit
          Left = 252
          Top = 48
          Width = 243
          Height = 35
          TabOrder = 1
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 48
          inherited cbCheck: TCheckBox
            Height = 23
            Enabled = True
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMaximumTime: TframeDisplayLimit
          Left = 252
          Top = 107
          Width = 243
          Height = 35
          TabOrder = 3
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 107
          inherited cbCheck: TCheckBox
            Height = 23
            Enabled = True
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMinResidual: TframeDisplayLimit
          Left = 3
          Top = 48
          Width = 243
          Height = 35
          TabOrder = 0
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 48
          inherited cbCheck: TCheckBox
            Height = 23
            Enabled = True
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMinWeightedResidual: TframeDisplayLimit
          Left = 3
          Top = 160
          Width = 243
          Height = 35
          TabOrder = 4
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 160
          inherited cbCheck: TCheckBox
            Top = 3
            Enabled = True
            ExplicitTop = 3
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMaxWeightedResidual: TframeDisplayLimit
          Left = 252
          Top = 160
          Width = 243
          Height = 35
          TabOrder = 5
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 160
          inherited cbCheck: TCheckBox
            Enabled = True
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
      end
      object clrbtnNegative: TJvColorButton
        Left = 3
        Top = 319
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        Color = clRed
        TabOrder = 3
        TabStop = False
      end
      object clrbtnPositive: TJvColorButton
        Left = 255
        Top = 319
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        Color = clBlue
        TabOrder = 4
        TabStop = False
      end
      object spinSymbolSize: TJvSpinEdit
        Left = 3
        Top = 354
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 20.000000000000000000
        TabOrder = 5
        OnChange = spinSymbolSizeChange
      end
      object cbShow: TCheckBox
        Left = 3
        Top = 59
        Width = 190
        Height = 17
        Caption = 'Show weighted residuals'
        TabOrder = 1
      end
      object rgDrawChoice: TRadioGroup
        Left = 0
        Top = 381
        Width = 347
        Height = 42
        Caption = 'Plot choice'
        Columns = 2
        ItemIndex = 1
        Items.Strings = (
          'Residuals'
          'Weighted residuals')
        TabOrder = 6
        OnClick = rgDrawChoiceClick
      end
    end
    object tabValues: TTabSheet
      Caption = 'Values (read only)'
      ImageIndex = 1
      object rdgPestObs: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 547
        Height = 361
        Align = alClient
        ColCount = 13
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnMouseUp = rdgPestObsMouseUp
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = True
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
      end
      object pnlValueControls: TPanel
        Left = 0
        Top = 361
        Width = 547
        Height = 65
        Align = alBottom
        ParentBackground = False
        TabOrder = 1
        object btnCopy: TButton
          Left = 135
          Top = 32
          Width = 138
          Height = 25
          Caption = 'Copy to clipboard'
          TabOrder = 2
          OnClick = btnCopyClick
        end
        object btnHightlightObjects: TButton
          Left = 4
          Top = 6
          Width = 125
          Height = 51
          Caption = 'Highlight selected objects'
          TabOrder = 0
          WordWrap = True
          OnClick = btnHightlightObjectsClick
        end
        object btnRestore: TButton
          Left = 135
          Top = 6
          Width = 138
          Height = 25
          Caption = 'Original order'
          TabOrder = 1
          OnClick = btnRestoreClick
        end
      end
    end
    object tabLegend: TTabSheet
      Caption = 'Legend'
      ImageIndex = 2
      object shpMax: TShape
        Left = 3
        Top = 3
        Width = 65
        Height = 22
        Shape = stCircle
      end
      object shpHalfMax: TShape
        Left = 3
        Top = 31
        Width = 65
        Height = 26
        Shape = stCircle
      end
      object lblMax: TLabel
        Left = 56
        Top = 9
        Width = 30
        Height = 13
        Caption = 'lblMax'
      end
      object lblHalfMax: TLabel
        Left = 56
        Top = 41
        Width = 49
        Height = 13
        Caption = 'lblHalfMax'
      end
    end
    object tabGraph: TTabSheet
      Caption = 'Graph'
      ImageIndex = 3
      object pbObservations: TPaintBox
        Left = 0
        Top = 0
        Width = 547
        Height = 328
        Align = alClient
        OnMouseDown = pbObservationsMouseDown
        OnMouseMove = pbObservationsMouseMove
        OnMouseUp = pbObservationsMouseUp
        OnPaint = pbObservationsPaint
        ExplicitWidth = 520
        ExplicitHeight = 233
      end
      object pnlGraphControls: TPanel
        Left = 0
        Top = 328
        Width = 547
        Height = 98
        Align = alBottom
        ParentBackground = False
        TabOrder = 0
        object lblGraphInstructions: TLabel
          Left = 279
          Top = 6
          Width = 141
          Height = 13
          Caption = 'Click on a point to highlight it.'
          WordWrap = True
        end
        object rgGraphType: TRadioGroup
          Left = 4
          Top = 6
          Width = 269
          Height = 90
          Caption = 'Graph type'
          ItemIndex = 2
          Items.Strings = (
            'Simulated vs. Observed'
            'Residual vs. Observed'
            'Weighted Residual vs. Observed')
          TabOrder = 0
          OnClick = rgGraphTypeClick
        end
      end
    end
  end
  object qtreeObservations: TRbwQuadTree
    MaxPoints = 100
    Left = 280
    Top = 16
  end
end
