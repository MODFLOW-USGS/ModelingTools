inherited frmImportGriddedData: TfrmImportGriddedData
  HelpType = htKeyword
  HelpKeyword = 'Import_Gridded_Data_Dialog_Box'
  Caption = 'Import Gridded Data'
  ClientHeight = 445
  ClientWidth = 432
  Constraints.MinHeight = 432
  Constraints.MinWidth = 444
  KeyPreview = True
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  ExplicitWidth = 444
  ExplicitHeight = 483
  TextHeight = 18
  object jvplCellGrid: TJvPageList
    Left = 0
    Top = 185
    Width = 432
    Height = 213
    ActivePage = jvspGrid
    PropagateEnable = False
    Align = alClient
    ExplicitWidth = 426
    ExplicitHeight = 204
    object jvspCellList: TJvStandardPage
      Left = 0
      Top = 0
      Width = 432
      Height = 213
      Caption = 'jvspCellList'
      object rdgList: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 432
        Height = 172
        Align = alClient
        ColCount = 4
        FixedCols = 1
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnMouseUp = GridMouseDown
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = True
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = True
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnEndUpdate = rdgListEndUpdate
        ColorRangeSelection = False
        Columns = <
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
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
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
            Format = rcf4String
            LimitToList = False
            MaxLength = 0
            ParentButtonFont = False
            WordWrapCaptions = False
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = False
          end>
        WordWrapRowCaptions = False
      end
      object pnlListControls: TPanel
        Left = 0
        Top = 172
        Width = 432
        Height = 41
        Align = alBottom
        TabOrder = 1
        object lblNumberOfRows: TLabel
          Left = 79
          Top = 12
          Width = 153
          Height = 18
          Caption = 'Number of data points'
        end
        object GridPanel1: TGridPanel
          Left = 286
          Top = 3
          Width = 135
          Height = 32
          BevelOuter = bvNone
          ColumnCollection = <
            item
              Value = 33.333333333333330000
            end
            item
              Value = 33.333333333333330000
            end
            item
              Value = 33.333333333333340000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = sbAddRow
              Row = 0
            end
            item
              Column = 1
              Control = sbInsertRow
              Row = 0
            end
            item
              Column = 2
              Control = sbDeleteRow
              Row = 0
            end>
          RowCollection = <
            item
              Value = 100.000000000000000000
            end>
          TabOrder = 0
          DesignSize = (
            135
            32)
          object sbAddRow: TSpeedButton
            Left = 11
            Top = 5
            Width = 23
            Height = 22
            Hint = 'Add row|Add a row below the bottom row.'
            Anchors = []
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
              CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            OnClick = sbAddRowClick
            ExplicitTop = 6
          end
          object sbInsertRow: TSpeedButton
            Left = 56
            Top = 5
            Width = 23
            Height = 22
            Hint = 'Insert row|Insert a row above the selected row.'
            Anchors = []
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              8000008000000080800080000000800080008080000080808000C0C0C0000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FF0FF0CCCCCCCCC0F000F0CCCCCCC
              CC0FF0FF0CCCCCCCCC0FFFFF00000000000FFFFF0FFFFFFFFF0FFFFF0FFFFFFF
              FF0FFFFF0FFFFFFFFF0FFFFF00000000000FFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            OnClick = sbInsertRowClick
            ExplicitLeft = 54
            ExplicitTop = 6
          end
          object sbDeleteRow: TSpeedButton
            Left = 101
            Top = 5
            Width = 23
            Height = 22
            Hint = 'Delete row|Delete the selected row.'
            Anchors = []
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
            ParentShowHint = False
            ShowHint = True
            OnClick = sbDeleteRowClick
            ExplicitLeft = 107
            ExplicitTop = 6
          end
        end
        object seNumberOfRows: TJvSpinEdit
          Left = 8
          Top = 8
          Width = 65
          Height = 26
          CheckMaxValue = False
          ButtonKind = bkClassic
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 1
          OnChange = seNumberOfRowsChange
        end
      end
    end
    object jvspGrid: TJvStandardPage
      Left = 0
      Top = 0
      Width = 432
      Height = 213
      Caption = 'jvspGrid'
      ExplicitWidth = 426
      ExplicitHeight = 204
      object lblColumns: TLabel
        Left = 190
        Top = 16
        Width = 61
        Height = 18
        Caption = 'Columns'
      end
      object lblRows: TJvLabel
        Left = 8
        Top = 88
        Width = 21
        Height = 50
        AutoSize = False
        Caption = 'Rows'
        Transparent = True
        Angle = 90
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = 17
        HotTrackFont.Name = 'Microsoft Sans Serif'
        HotTrackFont.Pitch = fpVariable
        HotTrackFont.Style = []
      end
      object pcGriddedData: TPageControl
        AlignWithMargins = True
        Left = 40
        Top = 40
        Width = 392
        Height = 173
        Margins.Left = 40
        Margins.Top = 40
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 386
        ExplicitHeight = 164
      end
    end
  end
  object pnlMethodControls: TPanel
    Left = 0
    Top = 0
    Width = 432
    Height = 185
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 426
    DesignSize = (
      432
      185)
    object lblMethod: TLabel
      Left = 8
      Top = 45
      Width = 52
      Height = 18
      Caption = 'Method'
    end
    object lblDataSet: TLabel
      Left = 8
      Top = 98
      Width = 59
      Height = 18
      Caption = 'Data set'
    end
    object lblIgnoreValueCount: TLabel
      Left = 96
      Top = 45
      Width = 185
      Height = 18
      Caption = 'Number of values to ignore'
    end
    object lblModel: TLabel
      Left = 8
      Top = 14
      Width = 43
      Height = 18
      Caption = 'Model'
    end
    object comboMethod: TComboBox
      Left = 8
      Top = 66
      Width = 73
      Height = 26
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 2
      Text = 'Array'
      OnChange = comboMethodChange
      Items.Strings = (
        'List'
        'Array')
    end
    object rdgIgnoreValues: TRbwDataGrid4
      Left = 277
      Top = 45
      Width = 124
      Height = 98
      Anchors = [akTop, akRight]
      ColCount = 1
      DefaultColWidth = 100
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
      TabOrder = 1
      ExtendedAutoDistributeText = False
      AutoMultiEdit = True
      AutoDistributeText = True
      AutoIncreaseColCount = False
      AutoIncreaseRowCount = True
      SelectedRowOrColumnColor = clAqua
      UnselectableColor = clBtnFace
      OnEndUpdate = rdgIgnoreValuesEndUpdate
      ColorRangeSelection = False
      ColorSelectedRow = False
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
    object seIgnoreValueCount: TJvSpinEdit
      Left = 96
      Top = 67
      Width = 65
      Height = 26
      CheckMinValue = True
      ButtonKind = bkClassic
      TabOrder = 3
      OnChange = seIgnoreValueCountChange
    end
    object combotreeDataSets: TRbwStringTreeCombo
      Left = 8
      Top = 122
      Width = 263
      Height = 26
      Tree.Left = 0
      Tree.Top = 0
      Tree.Width = 628
      Tree.Height = 442
      Tree.Align = alClient
      Tree.Colors.BorderColor = 15987699
      Tree.Colors.DisabledColor = clGray
      Tree.Colors.DropMarkColor = 15385233
      Tree.Colors.DropTargetColor = 15385233
      Tree.Colors.DropTargetBorderColor = 15385233
      Tree.Colors.FocusedSelectionColor = 15385233
      Tree.Colors.FocusedSelectionBorderColor = 15385233
      Tree.Colors.GridLineColor = 15987699
      Tree.Colors.HeaderHotColor = clBlack
      Tree.Colors.HotColor = clBlack
      Tree.Colors.SelectionRectangleBlendColor = 15385233
      Tree.Colors.SelectionRectangleBorderColor = 15385233
      Tree.Colors.SelectionTextColor = clBlack
      Tree.Colors.TreeLineColor = 9471874
      Tree.Colors.UnfocusedColor = clGray
      Tree.Colors.UnfocusedSelectionColor = 13421772
      Tree.Colors.UnfocusedSelectionBorderColor = 13421772
      Tree.DefaultNodeHeight = 20
      Tree.Header.AutoSizeIndex = 0
      Tree.Header.DefaultHeight = 17
      Tree.Header.Height = 18
      Tree.Header.MainColumn = -1
      Tree.TabOrder = 0
      Tree.TreeOptions.SelectionOptions = [toFullRowSelect]
      Tree.OnChange = combotreeDataSetsDropDownTreeChange
      Tree.OnGetText = combotreeDataSetsDropDownTreeGetText
      Tree.OnGetNodeDataSize = combotreeDataSetsDropDownTreeGetNodeDataSize
      Tree.OnInitNode = combotreeDataSets1TreeInitNode
      Tree.Touch.InteractiveGestures = [igPan, igPressAndTap]
      Tree.Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
      Tree.ExplicitWidth = 200
      Tree.ExplicitHeight = 100
      Tree.Columns = <>
      Anchors = [akLeft, akTop, akRight]
      Enabled = True
      Glyph.Data = {
        36020000424D3602000000000000360000002800000010000000080000000100
        2000000000000002000000000000000000000000000000000000D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC0000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00000000000000000000000000D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9EC00D8E9EC000000
        000000000000000000000000000000000000D8E9EC00D8E9EC00D8E9EC00C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00000000000000
        00000000000000000000000000000000000000000000D8E9EC00C0C0C000C0C0
        C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9
        EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00D8E9EC00}
      NumGlyphs = 2
      TabOrder = 4
      ExplicitWidth = 257
    end
    object cbMultipleDataRows: TCheckBox
      Left = 8
      Top = 153
      Width = 297
      Height = 17
      Caption = 'Multiple data rows per grid row'
      TabOrder = 5
      OnClick = cbMultipleDataRowsClick
    end
    object comboModel: TComboBox
      Left = 64
      Top = 11
      Width = 363
      Height = 26
      Style = csDropDownList
      TabOrder = 0
      OnChange = comboModelChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 398
    Width = 432
    Height = 47
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 389
    ExplicitWidth = 426
    DesignSize = (
      432
      47)
    object btnHelp: TBitBtn
      Left = 117
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 111
    end
    object btnOK: TBitBtn
      Left = 214
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      Enabled = False
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      ModalResult = 1
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 208
    end
    object btnCancel: TBitBtn
      Left = 310
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 304
    end
    object btnPasteData: TButton
      Left = 20
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Caption = 'Paste Data'
      TabOrder = 3
      OnClick = btnPasteDataClick
      ExplicitLeft = 14
    end
  end
end
