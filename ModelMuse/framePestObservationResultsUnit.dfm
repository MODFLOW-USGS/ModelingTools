object framePestObservationResults: TframePestObservationResults
  Left = 0
  Top = 0
  Width = 555
  Height = 468
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 432
    Width = 555
    Height = 36
    Align = alBottom
    TabOrder = 0
    object lblRMS: TLabel
      Left = 194
      Top = 9
      Width = 151
      Height = 13
      Caption = 'Root Mean Square Residual = ?'
    end
    object comboModels: TComboBox
      Left = 8
      Top = 6
      Width = 180
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object pgcObservations: TPageControl
    Left = 0
    Top = 0
    Width = 555
    Height = 432
    ActivePage = tabValues
    Align = alClient
    TabOrder = 1
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
        Top = 341
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
        Top = 27
        Width = 505
        Height = 21
        Filter = 'Head Observation Output files (*.hob_out)|*.hob_out'
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
        object lblMinLayer: TLabel
          Left = 3
          Top = 138
          Width = 67
          Height = 13
          Caption = 'Minimum layer'
        end
        object lblMaxLayer: TLabel
          Left = 252
          Top = 138
          Width = 71
          Height = 13
          Caption = 'Maximum layer'
        end
        inline framelmtMinimumTime: TframeDisplayLimit
          Left = 3
          Top = 107
          Width = 243
          Height = 35
          TabOrder = 2
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 107
          inherited cbCheck: TCheckBox
            Height = 23
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
            ExplicitHeight = 23
          end
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMinLayer: TframeDisplayLimit
          Left = 3
          Top = 160
          Width = 243
          Height = 35
          TabOrder = 4
          TabStop = True
          ExplicitLeft = 3
          ExplicitTop = 160
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
        inline framelmtMaxLayer: TframeDisplayLimit
          Left = 252
          Top = 160
          Width = 243
          Height = 35
          TabOrder = 5
          TabStop = True
          ExplicitLeft = 252
          ExplicitTop = 160
          inherited comboBoolLimit: TComboBox
            ExplicitHeight = 21
          end
        end
      end
      object clrbtnNegative: TJvColorButton
        Left = 3
        Top = 311
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 3
        TabStop = False
      end
      object clrbtnPositive: TJvColorButton
        Left = 255
        Top = 311
        Width = 92
        OtherCaption = '&Other...'
        Options = []
        TabOrder = 4
        TabStop = False
      end
      object spinSymbolSize: TJvSpinEdit
        Left = 3
        Top = 338
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 5
      end
      object cbShow: TCheckBox
        Left = 6
        Top = 59
        Width = 158
        Height = 17
        Caption = 'Show residuals'
        TabOrder = 1
      end
    end
    object tabValues: TTabSheet
      Caption = 'Values (read only)'
      ImageIndex = 1
      object rdgPestObs: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 547
        Height = 339
        Align = alClient
        ColCount = 12
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
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
      end
      object pnlValueControls: TPanel
        Left = 0
        Top = 339
        Width = 547
        Height = 65
        Align = alBottom
        ParentBackground = False
        TabOrder = 1
        object btnCopy: TButton
          Left = 111
          Top = 32
          Width = 107
          Height = 25
          Caption = 'Copy to clipboard'
          TabOrder = 2
        end
        object btnHightlightObjects: TButton
          Left = 4
          Top = 6
          Width = 101
          Height = 51
          Caption = 'Highlight selected objects'
          TabOrder = 0
          WordWrap = True
        end
        object btnRestore: TButton
          Left = 111
          Top = 6
          Width = 107
          Height = 25
          Caption = 'Original order'
          TabOrder = 1
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
      object pbHeadObs: TPaintBox
        Left = 0
        Top = 0
        Width = 547
        Height = 323
        Align = alClient
        ExplicitWidth = 520
        ExplicitHeight = 233
      end
      object pnlGraphControls: TPanel
        Left = 0
        Top = 323
        Width = 547
        Height = 81
        Align = alBottom
        ParentBackground = False
        TabOrder = 0
        object lblGraphInstructions: TLabel
          Left = 255
          Top = 6
          Width = 48
          Height = 52
          Caption = 'Click on a point to highlight it.'
          WordWrap = True
        end
        object rgGraphType: TRadioGroup
          Left = 4
          Top = 0
          Width = 245
          Height = 73
          Caption = 'Graph type'
          ItemIndex = 0
          Items.Strings = (
            'Simulated vs. Observed'
            'Residual vs. Observed')
          TabOrder = 0
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
