inherited frameScreenObjectMAW: TframeScreenObjectMAW
  Width = 549
  Height = 293
  OnResize = FrameResize
  ExplicitWidth = 549
  ExplicitHeight = 293
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 549
    Height = 33
    Align = alTop
    TabOrder = 0
    object pnlCaption: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 541
      Height = 25
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 33
    Width = 549
    Height = 260
    ActivePage = tabTransient
    Align = alClient
    TabOrder = 1
    object tabWell: TTabSheet
      Caption = 'Well definition'
      object lblRadius: TLabel
        Left = 3
        Top = 19
        Width = 32
        Height = 13
        Caption = 'Radius'
      end
      object lblBottom: TLabel
        Left = 3
        Top = 46
        Width = 34
        Height = 13
        Caption = 'Bottom'
      end
      object lblStartingHead: TLabel
        Left = 3
        Top = 73
        Width = 93
        Height = 13
        Caption = 'Starting head (strt)'
      end
      object lblConductanceEquation: TLabel
        Left = 3
        Top = 100
        Width = 160
        Height = 13
        Caption = 'Conductance equation (condeqn)'
      end
      object edWellRadius: TJvComboEdit
        Left = 108
        Top = 16
        Width = 322
        Height = 21
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 0
        Text = ''
        OnChange = edWellRadiusChange
      end
      object edBottom: TJvComboEdit
        Left = 108
        Top = 43
        Width = 322
        Height = 21
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 1
        Text = ''
        OnChange = edBottomChange
      end
      object edStartingHead: TJvComboEdit
        Left = 141
        Top = 70
        Width = 289
        Height = 21
        ButtonWidth = 50
        DisabledColor = clBtnFace
        Glyph.Data = {
          26020000424D260200000000000076000000280000002D000000120000000100
          040000000000B001000000000000000000001000000000000000000000000000
          8000008000000080800080000000800080008080000080808000C0C0C0000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          F000FF000000000FFFF000F0FF0FFF000FF0FFF0FFF0FFFFF000FF0FFFFFFFFF
          FF0FFF00FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFF0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          F0FFFFF0FF0FFF0FFFFFFFFFFFFFFFFFF000FF00000000FFF0FFFFF0FF0FFF0F
          FFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFF0FFF00FF0FFF0FFFFFFFFFFFFFFFFF
          F000FF0FFFFFFFFFFFF000F0FF0FF0000FFFFFFFFFFFFFFFF000FF0FFFFFFFFF
          FFFFFFF0FFFFFF0FFFFFFFFFFFFFFFFFF000FF0FFFFFFFFFFFFFFFF0FFFFFF0F
          FFFFFFFFFFFFFFFFF000FF000000000FFFFFFFF0FF0FFFFFFFFFFFFFFFFFFFFF
          F000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFF000}
        TabOrder = 2
        Text = ''
        OnChange = edStartingHeadChange
      end
      object comboConductEq: TJvImageComboBox
        Left = 255
        Top = 97
        Width = 175
        Height = 23
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 175
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 17
        ItemIndex = 0
        TabOrder = 3
        OnChange = comboConductEqChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'SPECIFIED'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'THIEM'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'SKIN'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'CUMULATIVE'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'MEAN'
          end>
      end
    end
    object tabWellScreens: TTabSheet
      Caption = 'Well screens'
      ImageIndex = 2
      inline frameWellScreens: TframeGrid
        Left = 0
        Top = 0
        Width = 541
        Height = 232
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 541
        ExplicitHeight = 232
        inherited Panel: TPanel
          Top = 191
          Width = 541
          ExplicitTop = 191
          ExplicitWidth = 541
          inherited lbNumber: TLabel
            Width = 111
            Caption = 'Number of well screens'
            ExplicitWidth = 111
          end
          inherited sbAdd: TSpeedButton
            Left = 382
            ExplicitLeft = 269
          end
          inherited sbInsert: TSpeedButton
            Left = 436
            ExplicitLeft = 307
          end
          inherited sbDelete: TSpeedButton
            Left = 488
            ExplicitLeft = 344
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 541
          Height = 191
          ColCount = 4
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
          OnSetEditText = frameWellScreensGridSetEditText
          Columns = <
            item
              AutoAdjustRowHeights = False
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
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
              ButtonFont.Style = []
              ButtonUsed = True
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
              ButtonFont.Style = []
              ButtonUsed = True
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
              ButtonFont.Style = []
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 541
          ExplicitHeight = 191
        end
      end
    end
    object tabTransient: TTabSheet
      Caption = 'Transient'
      ImageIndex = 1
      object pnlBottom: TPanel
        Left = 0
        Top = 186
        Width = 541
        Height = 46
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          541
          46)
        object lblNumTimes: TLabel
          Left = 64
          Top = 15
          Width = 78
          Height = 13
          Caption = 'Number of times'
        end
        object seNumberOfTimes: TJvSpinEdit
          Left = 9
          Top = 5
          Width = 49
          Height = 21
          ButtonKind = bkClassic
          MaxValue = 2147483647.000000000000000000
          TabOrder = 0
          OnChange = seNumberOfTimesChange
        end
        object btnDelete: TBitBtn
          Left = 453
          Top = 6
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Delete'
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
          TabOrder = 2
          OnClick = btnDeleteClick
        end
        object btnInsert: TBitBtn
          Left = 365
          Top = 6
          Width = 82
          Height = 33
          Anchors = [akTop, akRight]
          Cancel = True
          Caption = '&Insert'
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
            CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
            FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
          TabOrder = 1
          OnClick = btnInsertClick
        end
      end
      object pnlGrid: TPanel
        Left = 0
        Top = 0
        Width = 541
        Height = 186
        Align = alClient
        TabOrder = 0
        object pnlEditGrid: TPanel
          Left = 1
          Top = 1
          Width = 539
          Height = 56
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFormula: TLabel
            Left = 232
            Top = 5
            Width = 38
            Height = 13
            Alignment = taCenter
            Caption = 'Formula'
          end
          object lblStatus: TLabel
            Left = 128
            Top = 5
            Width = 31
            Height = 13
            Caption = 'Status'
          end
          object lblRateLimitation: TLabel
            Left = 352
            Top = 5
            Width = 68
            Height = 13
            Caption = 'Rate limitation'
          end
          object rdeFormula: TRbwDataEntry
            Left = 232
            Top = 27
            Width = 57
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 1
            Text = ''
            OnChange = rdeFormulaChange
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
          object comboStatus: TJvImageComboBox
            Left = 128
            Top = 27
            Width = 98
            Height = 23
            Style = csOwnerDrawVariable
            ButtonStyle = fsLighter
            Color = clBtnFace
            DroppedWidth = 98
            Enabled = False
            ImageHeight = 0
            ImageWidth = 0
            ItemHeight = 17
            ItemIndex = -1
            TabOrder = 0
            OnChange = comboStatusChange
            Items = <>
          end
          object cbFlowingWell: TCheckBox
            Left = 304
            Top = 33
            Width = 49
            Height = 17
            Caption = 'FW'
            Enabled = False
            TabOrder = 3
            OnClick = cbFlowingWellClick
          end
          object comboRateLimitation: TJvImageComboBox
            Left = 352
            Top = 27
            Width = 70
            Height = 23
            Style = csOwnerDrawVariable
            ButtonStyle = fsLighter
            Color = clBtnFace
            DroppedWidth = 98
            Enabled = False
            ImageHeight = 0
            ImageWidth = 0
            ItemHeight = 17
            ItemIndex = -1
            TabOrder = 2
            OnChange = comboRateLimitationChange
            Items = <>
          end
          object cbHeadLimit: TCheckBox
            Left = 437
            Top = 33
            Width = 97
            Height = 17
            Caption = 'Head limit'
            Enabled = False
            TabOrder = 4
            OnClick = cbHeadLimitClick
          end
        end
        object rdgModflowBoundary: TRbwDataGrid4
          Left = 1
          Top = 57
          Width = 539
          Height = 128
          Align = alClient
          ColCount = 16
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goTabs]
          TabOrder = 1
          OnMouseUp = rdgModflowBoundaryMouseUp
          OnSelectCell = rdgModflowBoundarySelectCell
          OnSetEditText = rdgModflowBoundarySetEditText
          ExtendedAutoDistributeText = False
          AutoMultiEdit = True
          AutoDistributeText = True
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = True
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          OnBeforeDrawCell = rdgModflowBoundaryBeforeDrawCell
          OnColSize = rdgModflowBoundaryColSize
          OnStateChange = rdgModflowBoundaryStateChange
          ColorRangeSelection = False
          OnHorizontalScroll = rdgModflowBoundaryHorizontalScroll
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
              ComboUsed = True
              Format = rcf4String
              LimitToList = True
              MaxLength = 0
              ParentButtonFont = False
              PickList.Strings = (
                'Active'
                'Inactive'
                'Constant Head')
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
              ButtonWidth = 35
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
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ComboUsed = True
              Format = rcf4String
              LimitToList = True
              MaxLength = 0
              ParentButtonFont = False
              PickList.Strings = (
                'None'
                'Scaling'
                'Shutoff')
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
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
              ButtonUsed = True
              ButtonWidth = 35
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
              ButtonCaption = 'F()'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
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
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
          ExplicitTop = 58
          ColWidths = (
            64
            64
            109
            64
            64
            64
            64
            64
            66
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
