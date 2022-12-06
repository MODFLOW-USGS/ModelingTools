inherited framePkgFarm: TframePkgFarm
  Width = 504
  Height = 462
  ExplicitWidth = 504
  ExplicitHeight = 462
  DesignSize = (
    504
    462)
  inherited lblPackage: TLabel [0]
  end
  inherited lblComments: TLabel [1]
  end
  object splttrFarm: TJvNetscapeSplitter [2]
    Left = 94
    Top = 0
    Height = 462
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 96
    ExplicitTop = 184
    ExplicitHeight = 100
  end
  inherited memoComments: TMemo
    Width = 72
    Height = 149
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    ExplicitWidth = 72
    ExplicitHeight = 149
  end
  object jvplFarm: TJvPageList [4]
    Left = 104
    Top = 0
    Width = 400
    Height = 462
    ActivePage = jvspMnwNwtOptions
    PropagateEnable = False
    Align = alClient
    object jvspOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Farm_Process_Options'
      Caption = 'jvspOptions'
      DesignSize = (
        400
        462)
      object lblCropIrrigationRequirement: TLabel
        Left = 6
        Top = 318
        Width = 245
        Height = 15
        Anchors = [akLeft, akBottom]
        Caption = 'Crop irrigation requirement (AUX NOCIRNOQ)'
      end
      object lblRecomputeFlows: TLabel
        Left = 6
        Top = 379
        Width = 311
        Height = 15
        Anchors = [akLeft, akBottom]
        Caption = 'Recompute farm flows for each time step (RECOMP_Q_BD)'
      end
      object rgAssignmentMethod: TRadioGroup
        Left = 6
        Top = 253
        Width = 379
        Height = 59
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Precipitation assignment method'
        Enabled = False
        ItemIndex = 1
        Items.Strings = (
          'Objects overwrite values of previous objects'
          'Sum values of all objects')
        TabOrder = 0
      end
      object comboCropIrrigationRequirement: TComboBox
        Left = 6
        Top = 340
        Width = 369
        Height = 23
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'Irrigate continuously'
          'Irrigate only when needed')
      end
      object comboRecomputeFlows: TComboBox
        Left = 6
        Top = 401
        Width = 369
        Height = 23
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Don'#39't recompute'
          'Recompute')
      end
      object cbResetQMax: TCheckBox
        Left = 6
        Top = 428
        Width = 363
        Height = 35
        Caption = 'Reset QMAX in MNW packages in each stress period (AUX QMAXRESET)'
        Enabled = False
        TabOrder = 3
        WordWrap = True
      end
    end
    object jvspParameters: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'FarmParameters'
      Caption = 'jvspParameters'
    end
    object jvspWhenToRead: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'When_to_Read_Flags'
      Caption = 'jvspWhenToRead'
      object lblRootingDepth: TLabel
        Left = 16
        Top = 14
        Width = 114
        Height = 15
        Caption = 'Rooting depth (IRTFL)'
      end
      object lblConsumptiveUse: TLabel
        Left = 16
        Top = 75
        Width = 134
        Height = 15
        Caption = 'Consumptive use (ICUFL)'
      end
      object lblPrecipitation: TLabel
        Left = 16
        Top = 134
        Width = 100
        Height = 15
        Caption = 'Precipitation (IPFL)'
      end
      object lblInefficiencyLosses: TLabel
        Left = 16
        Top = 195
        Width = 207
        Height = 15
        Caption = 'Fraction of inefficiency losses (IIESWFL)'
      end
      object comboRootingDepth: TComboBox
        Left = 16
        Top = 36
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 0
        Items.Strings = (
          'Specified (1 or 2)'
          'Calculated (3)')
      end
      object comboConsumptiveUse: TComboBox
        Left = 16
        Top = 97
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'Calculated (3)'
          'Potential ET (2)'
          'Potential and reference ET (1)'
          'Crop coefficient (-1)')
      end
      object comboInefficiencyLosses: TComboBox
        Left = 16
        Top = 217
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 3
        Items.Strings = (
          'Calculated (0)'
          'Specified (1 or 2)')
      end
      object comboPrecipitation: TComboBox
        Left = 16
        Top = 156
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Spatially distributed (2)'
          'Climate time series (3)')
      end
    end
    object jvspWaterPolicy: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Water_Policy_Flags'
      Caption = 'jvspWaterPolicy'
      object lblDeficiency: TLabel
        Left = 6
        Top = 287
        Width = 136
        Height = 15
        Caption = 'Deficiency policy (IDEFFL)'
      end
      object comboDeficiency: TComboBox
        Left = 6
        Top = 309
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        OnChange = comboDeficiencyChange
        Items.Strings = (
          'Water stacking (-2)'
          'Deficit irrigation (-1)'
          'No policy (0)'
          'Acreage optimization (1)'
          'Acreage optimization with conservation pool (2)')
      end
      inline frameEfficiencyBehavior: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 281
        Align = alTop
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 281
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 278
          Caption = 'Efficiency behavior (IEBFL)'
          ExplicitWidth = 397
          ExplicitHeight = 278
          inherited lblTop: TLabel
            Left = 56
            Width = 170
            Caption = 'Efficiency groundwater function'
            ExplicitLeft = 56
            ExplicitWidth = 170
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 62
            Width = 52
            Height = 13
            AutoSize = False
            Caption = 'Efficiency reset'
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 62
            ExplicitWidth = 13
            ExplicitHeight = 74
          end
          inherited rdgGrid: TRbwDataGrid4
            Left = 32
            Top = 57
            Width = 360
            Height = 216
            Margins.Left = 30
            Margins.Top = 40
            ColCount = 3
            FixedCols = 0
            RowCount = 3
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
                CaptionAlignment = taLeftJustify
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
                CheckStyle = csRadio
                AutoAdjustColWidths = False
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = True
            ExplicitLeft = 32
            ExplicitTop = 57
            ExplicitWidth = 360
            ExplicitHeight = 216
            ColWidths = (
              178
              108
              64)
            RowHeights = (
              24
              24
              24)
          end
        end
      end
      object cbGroundwaterAllotments: TCheckBox
        Left = 6
        Top = 336
        Width = 371
        Height = 41
        Caption = 'Farm well pumping limited by groundwater allotments (IALLOTGW)'
        Enabled = False
        TabOrder = 2
        WordWrap = True
      end
    end
    object jvspCropConsumptiveUse: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Crop_Consumptive_Use_Flags'
      Caption = 'jvspCropConsumptiveUse'
      inline frameCropConsumptiveUse: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 462
        Align = alClient
        Enabled = False
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 462
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 459
          Caption = 'Crop Consumptive-Use Flag (ICCFL)'
          ExplicitWidth = 397
          ExplicitHeight = 459
          inherited lblTop: TLabel
            Width = 144
            Caption = 'Crop consumptive concept'
            ExplicitWidth = 144
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 96
            Width = 52
            Height = 13
            AutoSize = False
            Caption = 'Crop consumptive linkage  '
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 96
            ExplicitWidth = 13
            ExplicitHeight = 128
          end
          inherited rdgGrid: TRbwDataGrid4
            Left = 32
            Width = 360
            Height = 417
            Margins.Left = 30
            ColCount = 3
            RowCount = 3
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
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            ExplicitLeft = 32
            ExplicitWidth = 360
            ExplicitHeight = 417
          end
        end
      end
    end
    object jvspSurfaceWater: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Surface_Water_Flags'
      Caption = 'jvspSurfaceWater'
      object lblRoutedDelivery: TLabel
        Left = 16
        Top = 14
        Width = 198
        Height = 15
        Caption = 'Routed surface-water delivery (IRDFL)'
      end
      object lblRoutedReturnFlow: TLabel
        Left = 16
        Top = 75
        Width = 247
        Height = 15
        Caption = 'Routed surface-water runoff returnflow (IRRFL)'
      end
      object lblAllotment: TLabel
        Left = 16
        Top = 134
        Width = 190
        Height = 15
        Caption = 'Surface water allotment (IALLOTSW)'
      end
      object lblDiversionCriterion: TLabel
        Left = 16
        Top = 195
        Width = 190
        Height = 15
        Caption = 'Diversion closure criterion (PCLOSE)'
      end
      object comboRoutedDelivery: TComboBox
        Left = 16
        Top = 36
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 0
        Items.Strings = (
          'None (0)'
          'From a diversion segment (1)'
          'From any segment (-1)')
      end
      object comboRoutedReturnFlow: TComboBox
        Left = 16
        Top = 97
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'To a nondiversion segment (1)'
          'To any segment (-1)')
      end
      object comboAllotment: TComboBox
        Left = 16
        Top = 156
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        OnChange = comboAllotmentChange
        Items.Strings = (
          'None (0)'
          'Equal (1)'
          'Prior appropriation with calls (2)'
          'Prior appropriation without calls (3)')
      end
      object rdeDiversionCriterion: TRbwDataEntry
        Left = 16
        Top = 217
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspMandatoryPrintFlags1: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Mandatory_Print_Flags'
      Caption = 'jvspMandatoryPrintFlags1'
      object lblSaveWellFlowRates: TLabel
        Left = 16
        Top = 14
        Width = 182
        Height = 15
        Caption = 'Save farm well flow rates (IFWLCB)'
      end
      object lblSaveRecharge: TLabel
        Left = 16
        Top = 75
        Width = 144
        Height = 15
        Caption = 'Save net recharge (IFNRCB)'
      end
      object lblSupplyAndDemand: TLabel
        Left = 16
        Top = 134
        Width = 153
        Height = 15
        Caption = 'Supply and demand (ISDPFL)'
      end
      object comboSaveWellFlowRates: TComboBox
        Left = 16
        Top = 36
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 0
        Items.Strings = (
          'Default behavior'
          'Text file '#8220'FWELLS.OUT'#8221' (1)')
      end
      object comboSaveRecharge: TComboBox
        Left = 16
        Top = 97
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 1
        Items.Strings = (
          'Default behavior'
          'Text file by cell '#8220'FNRCH_ARRAY OUT'#8221' (1)'
          'Text file by farm '#8220'FNRCH_LIST.OUT'#8221' (2)'
          'Binary file '#8220'FNRCH_LIST_BIN.OUT'#8221' (3)')
      end
      object comboSupplyAndDemand: TComboBox
        Left = 16
        Top = 156
        Width = 369
        Height = 23
        Style = csDropDownList
        Enabled = False
        TabOrder = 2
        Items.Strings = (
          'Default behavior'
          'Listing every iteration ('#8211'3)'
          'Listing each time step (-2)'
          'Text file '#8220'FDS.OUT'#8221' (1)')
      end
    end
    object jvspOptionalPrintFlags: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Optional_Print_Flags'
      Caption = 'jvspOptionalPrintFlags'
      object lblDiversionBudgetLocation: TLabel
        Left = 16
        Top = 151
        Width = 183
        Height = 15
        Caption = 'Diversion budget location (IPAPFL)'
      end
      inline frameAcreageOptimizationPrintSettings: TframeRadioGrid
        Left = 0
        Top = 256
        Width = 400
        Height = 206
        Align = alBottom
        TabOrder = 2
        ExplicitTop = 256
        ExplicitWidth = 400
        ExplicitHeight = 206
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 203
          Caption = 'Acreage-Optimization print settings (IOPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 203
          inherited lblTop: TLabel
            Width = 71
            Caption = 'Print location'
            ExplicitWidth = 71
          end
          inherited lblLeft: TMMJLabel
            Left = 13
            Top = 80
            Width = 52
            Height = 13
            AutoSize = False
            Caption = 'Print choice '
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 13
            ExplicitTop = 80
            ExplicitWidth = 13
            ExplicitHeight = 58
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 161
            ColCount = 3
            RowCount = 6
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
                CheckStyle = csRadio
                AutoAdjustColWidths = False
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = True
            ExplicitWidth = 344
            ExplicitHeight = 161
            ColWidths = (
              191
              64
              64)
          end
        end
      end
      object comboDiversionBudgetLocation: TComboBox
        Left = 16
        Top = 173
        Width = 369
        Height = 23
        Style = csDropDownList
        TabOrder = 1
        Items.Strings = (
          'Listing (-1)'
          'Text file '#8220'PRIOR.OUT'#8221' (1)')
      end
      inline frameRoutingInformationPrintFlag: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 145
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 145
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 142
          Caption = 'Routing information print flag (IRTPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 142
          inherited lblTop: TLabel
            Left = 200
            Width = 46
            Caption = 'Location'
            ExplicitLeft = 200
            ExplicitWidth = 46
          end
          inherited lblLeft: TMMJLabel
            Left = 13
            Top = 36
            Width = 58
            Caption = 'Frequency '
            ExplicitLeft = 13
            ExplicitTop = 36
            ExplicitWidth = 58
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 100
            ColCount = 4
            RowCount = 3
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
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            ExplicitWidth = 344
            ExplicitHeight = 100
          end
        end
      end
    end
    object jvspMandatoryPrintFlags2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'Mandatory_Print_Flags_2'
      Caption = 'jvspMandatoryPrintFlags2'
      inline frameET_PrintFlag: TframeRadioGrid
        Left = 0
        Top = 0
        Width = 400
        Height = 211
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 400
        ExplicitHeight = 211
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 208
          Caption = 'Farm total evapotranspiration print flag (IETPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 208
          inherited lblTop: TLabel
            Width = 70
            Caption = 'What to print'
            ExplicitWidth = 70
          end
          inherited lblLeft: TMMJLabel
            Left = 13
            Top = 62
            Width = 74
            Caption = 'Print location '
            ExplicitLeft = 13
            ExplicitTop = 62
            ExplicitWidth = 74
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 166
            ColCount = 6
            RowCount = 3
            OnSelectCell = frameET_PrintFlagrdgGridSelectCell
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
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = True
              end>
            ExplicitWidth = 344
            ExplicitHeight = 166
          end
        end
      end
      inline frameFarmBudgetPrintFlag: TframeRadioGrid
        Left = 0
        Top = 256
        Width = 400
        Height = 206
        Align = alBottom
        Enabled = False
        TabOrder = 1
        ExplicitTop = 256
        ExplicitWidth = 400
        ExplicitHeight = 206
        inherited grpDescription: TGroupBox
          Width = 397
          Height = 203
          Caption = 'Farm budget print flags (IFBPFL)'
          ExplicitWidth = 397
          ExplicitHeight = 203
          inherited lblTop: TLabel
            Width = 51
            Caption = 'Print type'
            ExplicitWidth = 51
          end
          inherited lblLeft: TMMJLabel
            Left = 3
            Top = 38
            Width = 52
            Height = 13
            AutoSize = False
            Caption = 'Compact or detailed '
            ButtonWidth = 52
            ButtonHeight = 13
            ExplicitLeft = 3
            ExplicitTop = 38
            ExplicitWidth = 13
            ExplicitHeight = 99
          end
          inherited rdgGrid: TRbwDataGrid4
            Width = 344
            Height = 161
            DefaultColWidth = 60
            RowCount = 3
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
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                Format = rcf4Boolean
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = True
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
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
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csRadio
                AutoAdjustColWidths = False
              end>
            WordWrapRowCaptions = True
            ExplicitWidth = 344
            ExplicitHeight = 161
          end
        end
      end
    end
    object jvspMnwNwtOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 400
      Height = 462
      HelpType = htKeyword
      HelpKeyword = 'MNW_and_NWT_Options'
      Caption = 'jvspMnwNwtOptions'
      object lblMnwExplanation: TLabel
        Left = 16
        Top = 12
        Width = 242
        Height = 30
        Caption = 
          'These options apply to the interation with the MNW1 and MNW2 pac' +
          'kages'
        WordWrap = True
      end
      object grpMNWOptions: TGroupBox
        Left = 0
        Top = 0
        Width = 400
        Height = 305
        Align = alTop
        Caption = 'MNW1 and MNW2 Options'
        TabOrder = 0
        object lblRPCT: TLabel
          Left = 74
          Top = 237
          Width = 216
          Height = 45
          Caption = 
            'Fraction of reduction of residual-change closure criterion if QC' +
            'LOSE was not met (RPCT)'
          WordWrap = True
        end
        object lblHPCT: TLabel
          Left = 74
          Top = 175
          Width = 242
          Height = 30
          Caption = 
            'Fraction of reduction of head-change closure criterion if QCLOSE' +
            ' was not met (HPCT)'
          WordWrap = True
        end
        object lblQClose: TLabel
          Left = 74
          Top = 114
          Width = 225
          Height = 45
          Caption = 
            'Criterion for actual MNW pumping rate to converge to FMP pumping' +
            ' requirement (QCLOSE) '
          WordWrap = True
        end
        object cbMnwClose: TCheckBox
          Left = 3
          Top = 21
          Width = 394
          Height = 75
          Caption = 
            'Adjust solver closure criteria to allow convergence of the FMP p' +
            'umping requirement to pumping simulated by the linked MNW1 or MN' +
            'W2 packages (MNWCLOSE option)'
          Enabled = False
          TabOrder = 0
          WordWrap = True
          OnClick = cbMnwCloseClick
        end
        object rdeRPCT: TRbwDataEntry
          Left = 3
          Top = 237
          Width = 65
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 3
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMax = True
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeHPCT: TRbwDataEntry
          Left = 3
          Top = 175
          Width = 65
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMax = True
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeQClose: TRbwDataEntry
          Left = 3
          Top = 111
          Width = 65
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
      object grpNwtOptions: TGroupBox
        Left = 0
        Top = 305
        Width = 400
        Height = 144
        Align = alTop
        Caption = 'NWT Options'
        TabOrder = 1
        object lblPSIRAMPF: TLabel
          Left = 74
          Top = 26
          Width = 224
          Height = 45
          Caption = 
            'Minimum fraction of model cell thickness before pumping reductio' +
            'n is initiated (PSIRAMPF)'
          WordWrap = True
        end
        object lblSATTHK: TLabel
          Left = 74
          Top = 82
          Width = 232
          Height = 45
          Caption = 
            'Minimum saturated thickness of model cell before pumping reducti' +
            'on is initiated. (SATTHK)'
          WordWrap = True
        end
        object rdePSIRAMPF: TRbwDataEntry
          Left = 3
          Top = 26
          Width = 65
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeSATTHK: TRbwDataEntry
          Left = 3
          Top = 82
          Width = 65
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
  end
  object tvpglstFarm: TJvPageListTreeView [5]
    Left = 0
    Top = 0
    Width = 94
    Height = 462
    PageDefault = 0
    PageList = jvplFarm
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnCustomDrawItem = tvpglstFarmCustomDrawItem
    Items.Links = {00000000}
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
        Control = comboCropIrrigationRequirement
      end
      item
        Control = rgAssignmentMethod
      end
      item
        Control = comboRecomputeFlows
      end
      item
        Control = frameCropConsumptiveUse
      end
      item
        Control = comboSaveWellFlowRates
      end
      item
        Control = comboSaveRecharge
      end
      item
        Control = comboSupplyAndDemand
      end
      item
        Control = frameFarmBudgetPrintFlag
      end
      item
        Control = rgAssignmentMethod
      end
      item
        Control = comboRoutedDelivery
      end
      item
        Control = comboRoutedReturnFlow
      end
      item
        Control = comboAllotment
      end
      item
        Control = frameEfficiencyBehavior
      end
      item
        Control = comboDeficiency
      end
      item
        Control = comboRootingDepth
      end
      item
        Control = comboConsumptiveUse
      end
      item
        Control = comboPrecipitation
      end
      item
        Control = comboInefficiencyLosses
      end
      item
      end
      item
        Control = cbGroundwaterAllotments
      end
      item
        Control = cbResetQMax
      end
      item
        Control = cbMnwClose
      end
      item
        Control = rdePSIRAMPF
      end
      item
        Control = rdeSATTHK
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
    Left = 56
    Top = 24
  end
end
