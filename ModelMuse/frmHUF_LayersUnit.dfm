inherited frmHUF_Layers: TfrmHUF_Layers
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Hydrogeologic_Units'
  Caption = 'MODFLOW Hydrogeologic Units'
  ClientHeight = 353
  ClientWidth = 525
  ExplicitWidth = 537
  ExplicitHeight = 391
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 137
    Top = 0
    Width = 5
    Height = 312
    ExplicitTop = -38
    ExplicitHeight = 278
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 137
    Height = 312
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 311
    object GridPanel1: TGridPanel
      Left = 1
      Top = 279
      Width = 135
      Height = 32
      Align = alBottom
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
          Control = sbAddUnit
          Row = 0
        end
        item
          Column = 1
          Control = sbInsertUnit
          Row = 0
        end
        item
          Column = 2
          Control = sbDeleteUnit
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitTop = 278
      DesignSize = (
        135
        32)
      object sbAddUnit: TSpeedButton
        Left = 11
        Top = 5
        Width = 23
        Height = 22
        Hint = 
          'Add hydrogeologic unit|Add a hydrogeologic unit below the bottom' +
          ' hydrogeologic unit.'
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
        OnClick = sbAddUnitClick
        ExplicitTop = 6
      end
      object sbInsertUnit: TSpeedButton
        Left = 56
        Top = 5
        Width = 23
        Height = 22
        Hint = 
          'Insert hydrogeologic unit|Insert a hydrogeologic unit above the ' +
          'selected hydrogeologic unit.'
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
        OnClick = sbInsertUnitClick
        ExplicitLeft = 48
        ExplicitTop = 15
      end
      object sbDeleteUnit: TSpeedButton
        Left = 100
        Top = 5
        Width = 23
        Height = 22
        Hint = 
          'Delete hydrogeologic unit|Delete the selected hydrogeologic unit' +
          '.'
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
        OnClick = sbDeleteUnitClick
        ExplicitLeft = 107
        ExplicitTop = 6
      end
    end
    object tvHufLayers: TTreeView
      Left = 1
      Top = 1
      Width = 135
      Height = 278
      Align = alClient
      HideSelection = False
      Indent = 20
      MultiSelect = True
      ReadOnly = True
      TabOrder = 0
      OnChange = tvHufLayersChange
      ExplicitHeight = 277
    end
  end
  object pcMain: TPageControl
    Left = 142
    Top = 0
    Width = 383
    Height = 312
    ActivePage = tabParameters
    Align = alClient
    TabOrder = 1
    OnChange = pcMainChange
    ExplicitWidth = 379
    ExplicitHeight = 311
    object Properties: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Properties_TabHUF'
      Caption = 'Properties'
      DesignSize = (
        375
        279)
      object lblHydrogeologicUnitName: TLabel
        Left = 3
        Top = 7
        Width = 255
        Height = 18
        Caption = 'Hydrogeologic unit name (HGUNAM)'
      end
      object lblHorizontalAnisotropy: TLabel
        Left = 3
        Top = 65
        Width = 323
        Height = 36
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Ratio of hydraulic conductivity along columns to hydraulic condu' +
          'ctivity along rows (HGUHANI)  '
        WordWrap = True
        ExplicitWidth = 327
      end
      object lblVK_Method: TLabel
        Left = 3
        Top = 137
        Width = 349
        Height = 36
        Caption = 'Method of specifying vertical hydraulic conductivity (HGUVANI) '
        WordWrap = True
      end
      object lblVerticalAnisotropy: TLabel
        Left = 3
        Top = 214
        Width = 263
        Height = 36
        Caption = 'Ratio of horizontal to vertical hydraulic conductivity (HGUVANI)'
        WordWrap = True
      end
      object edHydrogeologicUnitName: TEdit
        Left = 3
        Top = 27
        Width = 121
        Height = 26
        MaxLength = 10
        TabOrder = 0
        OnChange = edHydrogeologicUnitNameChange
        OnExit = edHydrogeologicUnitNameExit
      end
      object rdeHorizontalAnisotropy: TRbwDataEntry
        Left = 3
        Top = 104
        Width = 145
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = '1'
        OnChange = rdeHorizontalAnisotropyChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object comboVK_Method: TJvImageComboBox
        Left = 3
        Top = 176
        Width = 365
        Height = 28
        Style = csOwnerDrawVariable
        Anchors = [akLeft, akTop, akRight]
        ButtonStyle = fsLighter
        DroppedWidth = 369
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 22
        ItemIndex = 0
        TabOrder = 2
        OnChange = comboVK_MethodChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Vertical hydraulic conductivity'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Ratio of horizontal to vertical hydraulic conductivity '
          end>
      end
      object rdeVerticalAnisotropy: TRbwDataEntry
        Left = 3
        Top = 253
        Width = 145
        Height = 22
        Font.Charset = ANSI_CHARSET
        Font.Color = clBlack
        Font.Height = -16
        Font.Name = 'Arial'
        Font.Pitch = fpVariable
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '1'
        OnChange = rdeVerticalAnisotropyChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object tabParameters: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Parameters_Tab'
      Caption = 'Parameters'
      ImageIndex = 1
      object rdgParameters: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 375
        Height = 279
        Align = alClient
        ColCount = 4
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
        TabOrder = 0
        OnSelectCell = rdgParametersSelectCell
        ExtendedAutoDistributeText = False
        AutoMultiEdit = True
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnStateChange = rdgParametersStateChange
        ColorRangeSelection = False
        ColorSelectedRow = False
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
            Format = rcf4Boolean
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
            PickList.Strings = (
              'HK'
              'HANI'
              'VK'
              'VANI'
              'SS'
              'SY'
              'SYTP ')
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
            Format = rcf4Boolean
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
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        ExplicitWidth = 371
        ExplicitHeight = 278
      end
    end
    object tabPrint: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Print_Tab'
      Caption = 'Print'
      ImageIndex = 2
      object lblPrintFormat: TLabel
        Left = 103
        Top = 7
        Width = 186
        Height = 18
        Caption = 'Print format (PRINTCODE)'
      end
      object clbItemsToPrint: TCheckListBox
        Left = 0
        Top = 35
        Width = 379
        Height = 244
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 18
        Items.Strings = (
          'Hydraulic conductivity (HK)'
          'Horizontal anisotropy (HANI)'
          'Vertical hydraulic conductivity (VK)'
          'Specific storage (SS)'
          'Specific yield (SY)')
        TabOrder = 1
        OnClickCheck = clbItemsToPrintClickCheck
      end
      object comboPrintFormat: TJvImageComboBox
        Left = 2
        Top = 3
        Width = 94
        Height = 28
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 94
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 22
        ItemIndex = 0
        TabOrder = 0
        OnChange = comboPrintFormatChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '11G10.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '9G13.6'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.1'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.2'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '15F7.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.0'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.1'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.2'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '20F5.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10G11.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10F6.0'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10F6.1'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10F6.2'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10F6.3'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10F6.4'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = '10F6.5'
          end>
      end
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 312
    Width = 525
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 311
    ExplicitWidth = 521
    DesignSize = (
      525
      41)
    object btnHelp: TBitBtn
      Left = 180
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 176
    end
    object btnOK: TBitBtn
      Left = 294
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 290
    end
    object btnCancel: TBitBtn
      Left = 408
      Top = 4
      Width = 108
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 404
    end
  end
end
