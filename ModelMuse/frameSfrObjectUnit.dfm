object frameSfrObject: TframeSfrObject
  Left = 0
  Top = 0
  Width = 561
  Height = 638
  TabOrder = 0
  TabStop = True
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 561
    Height = 265
    Align = alTop
    ColumnCollection = <
      item
        SizeStyle = ssAbsolute
        Value = 200.000000000000000000
      end
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = Label1
        Row = 0
      end
      item
        Column = 1
        Control = Label2
        Row = 1
      end
      item
        Column = 0
        Control = RbwDataEntry2
        Row = 2
      end
      item
        Column = 1
        Control = Label3
        Row = 2
      end
      item
        Column = 0
        Control = ComboBox2
        Row = 4
      end
      item
        Column = 1
        Control = Label4
        Row = 3
      end
      item
        Column = 0
        Control = RbwDataEntry3
        Row = 3
      end
      item
        Column = 1
        Control = Label5
        Row = 4
      end
      item
        Column = 0
        Control = RbwDataEntry4
        Row = 5
      end
      item
        Column = 1
        Control = Label6
        Row = 5
      end
      item
        Column = 0
        Control = RbwDataEntry1
        Row = 0
      end
      item
        Column = 0
        Control = RbwDataEntry5
        Row = 6
      end
      item
        Column = 1
        Control = Label7
        Row = 6
      end
      item
        Column = 0
        Control = RbwDataEntry6
        Row = 7
      end
      item
        Column = 1
        Control = Label8
        Row = 7
      end
      item
        Column = 0
        Control = RbwDataEntry7
        Row = 8
      end
      item
        Column = 1
        Control = Label9
        Row = 8
      end
      item
        Column = 0
        Control = comboIcalc
        Row = 1
      end
      item
        Column = 0
        Control = comboParam
        Row = 9
      end
      item
        Column = 1
        Control = Label13
        Row = 9
      end>
    RowCollection = <
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end
      item
        Value = 10.000000000000000000
      end>
    TabOrder = 0
    DesignSize = (
      561
      265)
    object Label1: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 7
      Width = 118
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Segment number (NSEG)'
    end
    object Label2: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 33
      Width = 122
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Stage calculation (ICALC)'
    end
    object RbwDataEntry2: TRbwDataEntry
      Left = 1
      Top = 53
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 0
      Text = '0'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 39
      ExplicitHeight = 19
    end
    object Label3: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 59
      Width = 133
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Outflow segment (OUTSEG)'
    end
    object ComboBox2: TComboBox
      Left = 1
      Top = 105
      Width = 200
      Height = 21
      Align = alClient
      Style = csDropDownList
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      ItemIndex = 1
      TabOrder = 1
      Text = 'Only if diversion exceeds flow (-1)'
      Items.Strings = (
        'Limit diversion to available flow (0)'
        'Only if diversion exceeds flow (-1)'
        'Diversion is a fraction of flow (-2)'
        'Diverted amount is overflow (-3)')
      ExplicitTop = 77
    end
    object Label4: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 85
      Width = 135
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Diversion segment (IUPSEG)'
    end
    object RbwDataEntry3: TRbwDataEntry
      Left = 1
      Top = 79
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 2
      Text = '0'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 58
      ExplicitHeight = 19
    end
    object Label5: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 111
      Width = 128
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Diversion priority (IPRIOR)'
    end
    object RbwDataEntry4: TRbwDataEntry
      Left = 1
      Top = 131
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 3
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 96
      ExplicitHeight = 19
    end
    object Label6: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 137
      Width = 123
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Diversion amount (FLOW)'
    end
    object RbwDataEntry1: TRbwDataEntry
      Left = 1
      Top = 1
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 4
      Text = '1'
      DataType = dtInteger
      Max = 1.000000000000000000
      Min = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitHeight = 19
    end
    object RbwDataEntry5: TRbwDataEntry
      Left = 1
      Top = 157
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 5
      Text = '0'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 115
      ExplicitHeight = 19
    end
    object Label7: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 163
      Width = 122
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Runoff volume (RUNOFF)'
    end
    object RbwDataEntry6: TRbwDataEntry
      Left = 1
      Top = 183
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 6
      Text = '0'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 134
      ExplicitHeight = 19
    end
    object Label8: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 189
      Width = 167
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Evapotranspiration volume (ETSW)'
    end
    object RbwDataEntry7: TRbwDataEntry
      Left = 1
      Top = 209
      Width = 200
      Height = 26
      Align = alClient
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      TabOrder = 7
      Text = '0'
      DataType = dtInteger
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
      ExplicitTop = 153
      ExplicitHeight = 19
    end
    object Label9: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 215
      Width = 135
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Precipitation volume (PTSW)'
    end
    object comboIcalc: TComboBox
      Left = 1
      Top = 27
      Width = 200
      Height = 21
      Align = alClient
      Style = csDropDownList
      Anchors = [akLeft, akRight]
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 8
      Text = 'Specified stage (0)'
      Items.Strings = (
        'Specified stage (0)'
        'Rectangular channel (1)'
        'Eight-point chanel (2)'
        'Power function (3)'
        'Table (4)')
      ExplicitTop = 20
    end
    object comboParam: TComboBox
      Left = 1
      Top = 235
      Width = 200
      Height = 21
      Align = alClient
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 9
      Text = 'none'
      Items.Strings = (
        'none')
      ExplicitTop = 172
    end
    object Label13: TLabel
      AlignWithMargins = True
      Left = 204
      Top = 243
      Width = 50
      Height = 13
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Parameter'
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 265
    Width = 561
    Height = 373
    ActivePage = tabTable
    Align = alClient
    TabOrder = 1
    object tabUpDown: TTabSheet
      Caption = 'tabUpDown'
      ExplicitHeight = 332
      object Splitter1: TSplitter
        Left = 0
        Top = 153
        Width = 553
        Height = 8
        Cursor = crVSplit
        Align = alTop
        ExplicitTop = 113
      end
      object Panel1: TPanel
        Left = 0
        Top = 304
        Width = 553
        Height = 41
        Align = alBottom
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 248
        ExplicitWidth = 518
        DesignSize = (
          553
          41)
        object Label16: TLabel
          Left = 74
          Top = 10
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seSfrUpDown: TJvSpinEdit
          Left = 11
          Top = 6
          Width = 57
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 0
          OnChange = seSfrUpDownChange
        end
        object btnInsertUpDownTime: TBitBtn
          Left = 203
          Top = 1
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Insert'
          TabOrder = 1
          OnClick = btnInsertUpDownTimeClick
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ExplicitLeft = 168
        end
        object btnDeleteUpDownTime: TBitBtn
          Left = 291
          Top = 1
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Delete'
          Enabled = False
          TabOrder = 2
          OnClick = btnDeleteUpDownTimeClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
            0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          ExplicitLeft = 256
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 553
        Height = 153
        Align = alTop
        TabOrder = 1
        object Label10: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 8
          Width = 46
          Height = 13
          Caption = 'Upstream'
        end
        object dgUp: TRbwDataGrid4
          Left = 1
          Top = 27
          Width = 551
          Height = 125
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 11
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 0
          AutoDistributeText = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
          ExplicitHeight = 118
          ColWidths = (
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64
            64)
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 161
        Width = 553
        Height = 143
        Align = alClient
        TabOrder = 2
        ExplicitLeft = 29
        ExplicitTop = 129
        ExplicitWidth = 521
        ExplicitHeight = 169
        object Label11: TLabel
          Left = 8
          Top = 8
          Width = 60
          Height = 13
          Caption = 'Downstream'
        end
        object dgDown: TRbwDataGrid4
          Left = 1
          Top = 27
          Width = 551
          Height = 115
          Align = alBottom
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 11
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 0
          AutoDistributeText = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 519
          ExplicitHeight = 141
        end
      end
    end
    object tabRoughness: TTabSheet
      Caption = 'Channel properties'
      ImageIndex = 2
      ExplicitHeight = 332
      object Splitter3: TSplitter
        Left = 379
        Top = 0
        Width = 8
        Height = 345
        Align = alRight
        ExplicitLeft = 384
      end
      object jvplCrossSection: TJvPageList
        Left = 387
        Top = 0
        Width = 166
        Height = 345
        PropagateEnable = False
        Align = alRight
        ExplicitHeight = 332
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 379
        Height = 345
        Align = alClient
        TabOrder = 1
        ExplicitLeft = 200
        ExplicitWidth = 257
        DesignSize = (
          379
          345)
        object Label14: TLabel
          Left = 74
          Top = 313
          Width = 78
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Number of times'
        end
        object dgSfrRough: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 377
          Height = 297
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 4
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 0
          OnSelectCell = dgSfrRoughSelectCell
          AutoDistributeText = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 382
        end
        object seRoughnessTimes: TJvSpinEdit
          Left = 11
          Top = 310
          Width = 57
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          Anchors = [akLeft, akBottom]
          TabOrder = 1
          OnChange = seRoughnessTimesChange
        end
        object btnInsertRoughnessTime: TBitBtn
          Left = 205
          Top = 304
          Width = 82
          Height = 33
          Anchors = [akRight, akBottom]
          Cancel = True
          Caption = '&Insert'
          TabOrder = 2
          OnClick = btnInsertRoughnessTimeClick
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ExplicitLeft = 210
        end
        object btnDeleteRoughnessTime: TBitBtn
          Left = 293
          Top = 304
          Width = 82
          Height = 33
          Anchors = [akRight, akBottom]
          Cancel = True
          Caption = '&Delete'
          Enabled = False
          TabOrder = 3
          OnClick = btnDeleteRoughnessTimeClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
            0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          ExplicitLeft = 298
        end
      end
    end
    object tabTable: TTabSheet
      Caption = 'Flow Table'
      ImageIndex = 1
      ExplicitHeight = 332
      object Splitter2: TSplitter
        Left = 381
        Top = 0
        Width = 8
        Height = 345
        Align = alRight
        ExplicitLeft = 386
      end
      object jvplTable: TJvPageList
        Left = 389
        Top = 0
        Width = 164
        Height = 345
        PropagateEnable = False
        Align = alRight
        ExplicitLeft = 368
        ExplicitTop = 24
        ExplicitHeight = 264
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 381
        Height = 345
        Align = alClient
        TabOrder = 1
        ExplicitLeft = 184
        ExplicitTop = 3
        ExplicitWidth = 273
        ExplicitHeight = 342
        DesignSize = (
          381
          345)
        object Label12: TLabel
          Left = 74
          Top = 313
          Width = 73
          Height = 13
          Anchors = [akLeft, akRight]
          Caption = 'Number of times'
          ExplicitWidth = 78
        end
        object dgTableTime: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 379
          Height = 297
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 2
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
          TabOrder = 0
          OnSelectCell = dgTableTimeSelectCell
          AutoDistributeText = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          ColorSelectedRow = True
          Columns = <
            item
              AutoAdjustRowHeights = False
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = False
            end
            item
              AutoAdjustRowHeights = False
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 35
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = False
              WordWrapCells = False
              AutoAdjustColWidths = False
            end>
          ExplicitWidth = 384
        end
        object spTableTimes: TJvSpinEdit
          Left = 11
          Top = 310
          Width = 52
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          Anchors = [akLeft, akRight]
          TabOrder = 1
          OnChange = spTableTimesChange
          ExplicitWidth = 57
        end
        object btnInsertTableTime: TBitBtn
          Left = 207
          Top = 304
          Width = 82
          Height = 33
          Anchors = [akRight, akBottom]
          Cancel = True
          Caption = '&Insert'
          TabOrder = 2
          OnClick = btnInsertTableTimeClick
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          ExplicitLeft = 212
        end
        object btnDeleteTableTime: TBitBtn
          Left = 295
          Top = 304
          Width = 82
          Height = 33
          Anchors = [akRight, akBottom]
          Cancel = True
          Caption = '&Delete'
          Enabled = False
          TabOrder = 3
          OnClick = btnDeleteTableTimeClick
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
            0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
            0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
            00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
            000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          ExplicitLeft = 300
        end
      end
    end
    object tabEquation: TTabSheet
      Caption = 'Equation'
      ImageIndex = 3
      DesignSize = (
        553
        345)
      object Label15: TLabel
        Left = 66
        Top = 318
        Width = 78
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Number of times'
      end
      object dgSfrEquation: TRbwDataGrid4
        Left = 3
        Top = 3
        Width = 547
        Height = 303
        ColCount = 6
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 0
        AutoDistributeText = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        ColorRangeSelection = False
        ColorSelectedRow = True
        Columns = <
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = True
            ButtonCaption = 'F()'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 35
            CheckMax = False
            CheckMin = False
            ComboUsed = False
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            AutoAdjustColWidths = True
          end>
      end
      object btnInsertEquationTime: TBitBtn
        Left = 155
        Top = 312
        Width = 82
        Height = 33
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Insert'
        TabOrder = 1
        OnClick = btnInsertEquationTimeClick
        Glyph.Data = {
          F6000000424DF600000000000000760000002800000010000000100000000100
          0400000000008000000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
          CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
          FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
      end
      object btnDeleteEquationTime: TBitBtn
        Left = 243
        Top = 312
        Width = 82
        Height = 33
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Delete'
        Enabled = False
        TabOrder = 2
        OnClick = btnDeleteEquationTimeClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFF000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
          0000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000
          0000FFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000FFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFF
          000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      object seSfrEquation: TJvSpinEdit
        Left = 3
        Top = 315
        Width = 57
        Height = 21
        ButtonKind = bkClassic
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Anchors = [akLeft, akBottom]
        TabOrder = 3
        OnChange = seSfrEquationChange
      end
    end
  end
end
