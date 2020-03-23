inherited frmModflowTime: TfrmModflowTime
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Time_Dialog_Box'
  Caption = 'MODFLOW Time'
  ClientHeight = 327
  ClientWidth = 754
  OnResize = FormResize
  ExplicitWidth = 770
  ExplicitHeight = 366
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 292
    Width = 754
    Height = 35
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      754
      35)
    object btnCancel: TBitBtn
      Left = 666
      Top = 2
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 578
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 490
      Top = 2
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnConvertTimeUnits: TButton
      Left = 12
      Top = 2
      Width = 161
      Height = 27
      Caption = 'Convert time units'
      TabOrder = 3
      OnClick = btnConvertTimeUnitsClick
    end
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 754
    Height = 292
    ActivePage = tabMt3dms
    Align = alClient
    TabOrder = 0
    OnChange = pgcMainChange
    object tabModflow: TTabSheet
      Caption = 'MODFLOW'
      object pnlTop: TPanel
        Left = 0
        Top = 0
        Width = 746
        Height = 81
        Align = alTop
        TabOrder = 0
        object lblPeriodLength: TLabel
          Left = 183
          Top = 8
          Width = 35
          Height = 18
          Alignment = taCenter
          Caption = 'lblPe'
        end
        object lblMaxFirstTimeStepLength: TLabel
          Left = 262
          Top = 8
          Width = 47
          Height = 18
          Alignment = taCenter
          Caption = 'lblNum'
          WordWrap = True
        end
        object lblMultiplier: TLabel
          Left = 336
          Top = 8
          Width = 77
          Height = 18
          Alignment = taCenter
          Caption = 'lblMultiplier'
        end
        object lblSteadyTransient: TLabel
          Left = 412
          Top = 8
          Width = 127
          Height = 18
          Alignment = taCenter
          Caption = 'lblSteadyTransient'
          WordWrap = True
        end
        object rdePeriodLength: TRbwDataEntry
          Left = 191
          Top = 44
          Width = 65
          Height = 27
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          Text = '0'
          OnChange = rdePeriodLengthChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeMaxFirstStepLength: TRbwDataEntry
          Left = 262
          Top = 44
          Width = 65
          Height = 27
          Color = clBtnFace
          Enabled = False
          TabOrder = 1
          Text = '0'
          OnChange = rdeMaxFirstStepLengthChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeMultiplier: TRbwDataEntry
          Left = 341
          Top = 44
          Width = 65
          Height = 27
          Color = clBtnFace
          Enabled = False
          TabOrder = 2
          Text = '0'
          OnChange = rdeMultiplierChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object comboSteadyTransient: TJvImageComboBox
          Left = 421
          Top = 44
          Width = 91
          Height = 28
          Style = csOwnerDrawVariable
          ButtonStyle = fsLighter
          Color = clBtnFace
          DroppedWidth = 91
          Enabled = False
          ImageHeight = 0
          ImageWidth = 0
          ItemHeight = 22
          ItemIndex = -1
          TabOrder = 3
          OnChange = comboSteadyTransientChange
          Items = <
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Steady state'
            end
            item
              Brush.Style = bsClear
              Indent = 0
              Text = 'Transient'
            end>
        end
      end
      object dgTime: TRbwDataGrid4
        Left = 0
        Top = 81
        Width = 746
        Height = 137
        Align = alClient
        ColCount = 9
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 1
        OnMouseUp = dgTimeMouseUp
        OnSelectCell = dgTimeSelectCell
        OnSetEditText = dgTimeSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = dgTimeBeforeDrawCell
        OnButtonClick = dgTimeButtonClick
        OnColSize = dgTimeColSize
        ColorRangeSelection = False
        OnHorizontalScroll = dgTimeHorizontalScroll
        Columns = <
          item
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = True
            ButtonWidth = 35
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
            AutoAdjustRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
            ComboUsed = False
            Format = rcf4Real
            LimitToList = False
            Max = 1.000000000000000000
            MaxLength = 0
            Min = 1.000000000000000000
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
            ButtonFont.Pitch = fpVariable
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
              'Steady state'
              'Transient')
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
            ButtonFont.Pitch = fpVariable
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
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Pitch = fpVariable
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
        ColWidths = (
          64
          64
          64
          64
          74
          64
          64
          64
          64)
      end
      object pnlModflowBottom: TPanel
        Left = 0
        Top = 218
        Width = 746
        Height = 41
        Align = alBottom
        TabOrder = 2
        DesignSize = (
          746
          41)
        object lblNumPeriods: TLabel
          Left = 67
          Top = 10
          Width = 175
          Height = 18
          Caption = 'Number of stress periods'
        end
        object lblTimeUnit: TLabel
          Left = 403
          Top = 9
          Width = 126
          Height = 18
          Caption = 'Time unit (ITMUNI)'
        end
        object seNumPeriods: TJvSpinEdit
          Left = 4
          Top = 6
          Width = 57
          Height = 26
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 0
          OnChange = seNumPeriodsChange
        end
        object comboTimeUnit: TJvComboBox
          Left = 252
          Top = 6
          Width = 145
          Height = 26
          Style = csDropDownList
          TabOrder = 1
          Text = 'seconds (1)'
          OnChange = comboTimeUnitChange
          Items.Strings = (
            'undefined (0)'
            'seconds (1)'
            'minutes (2)'
            'hours (3)'
            'days (4)'
            'years (5)')
          ItemIndex = 1
        end
        object btnDelete: TButton
          Left = 574
          Top = 6
          Width = 82
          Height = 27
          Anchors = [akTop, akRight]
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnDeleteClick
        end
        object btnInsert: TButton
          Left = 662
          Top = 6
          Width = 83
          Height = 27
          Anchors = [akTop, akRight]
          Caption = 'Insert'
          TabOrder = 3
          OnClick = btnInsertClick
        end
      end
    end
    object tabMt3dms: TTabSheet
      Caption = 'MT3DMS or MT3D-USGS'
      ImageIndex = 1
      inline frameGrid: TframeGrid
        Left = 0
        Top = 0
        Width = 746
        Height = 259
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 746
        ExplicitHeight = 259
        inherited Panel: TPanel
          Top = 218
          Width = 746
          ExplicitTop = 218
          ExplicitWidth = 746
          inherited lbNumber: TLabel
            Width = 200
            Height = 18
            Caption = 'Number of MT3DMS periods'
            ExplicitWidth = 200
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 393
            Hint = 'Add row|Add a row below the bottom row.'
            ExplicitLeft = 393
          end
          inherited sbInsert: TSpeedButton
            Left = 465
            Hint = 'Insert row|Insert a row above the selected row.'
            ExplicitLeft = 465
          end
          inherited sbDelete: TSpeedButton
            Left = 537
            Hint = 'Delete row|Delete the selected row.'
            ExplicitLeft = 537
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            Value = 1.000000000000000000
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 746
          Height = 218
          ColCount = 8
          FixedCols = 1
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
          OnSelectCell = frameGridGridSelectCell
          OnBeforeDrawCell = frameGridGridBeforeDrawCell
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
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
              ComboUsed = False
              Format = rcf4Integer
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
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
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
              ButtonFont.Height = -11
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
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 746
          ExplicitHeight = 218
          ColWidths = (
            20
            64
            64
            64
            64
            64
            64
            64)
        end
      end
    end
  end
end
