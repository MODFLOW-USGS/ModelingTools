inherited frmPrintFrequency: TfrmPrintFrequency
  Left = 499
  Top = 181
  HelpType = htKeyword
  HelpKeyword = 'Print_Frequency_Dialog_Box'
  Caption = 'PHAST Print Frequency'
  ClientHeight = 516
  ClientWidth = 606
  ExplicitWidth = 622
  ExplicitHeight = 555
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 438
    Width = 606
    Height = 78
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 148
    ExplicitWidth = 424
    DesignSize = (
      606
      78)
    object btnInsert: TButton
      Left = 103
      Top = 6
      Width = 89
      Height = 33
      Caption = 'Insert'
      TabOrder = 1
      OnClick = btnInsertClick
    end
    object btnAdd: TButton
      Left = 8
      Top = 6
      Width = 89
      Height = 33
      Caption = 'Add'
      TabOrder = 0
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 198
      Top = 6
      Width = 89
      Height = 33
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
    end
    object btnOK: TBitBtn
      Left = 414
      Top = 36
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 4
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 511
      Top = 36
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 5
    end
    object cbSaveFinalHeads: TCheckBox
      Left = 8
      Top = 40
      Width = 273
      Height = 30
      Caption = 'Save final heads in *.head.dat'
      TabOrder = 6
    end
    object btnHelp: TBitBtn
      Left = 317
      Top = 36
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
  end
  object rdgPrintFrequency: TRbwDataGrid4
    Left = 0
    Top = 41
    Width = 606
    Height = 397
    Align = alClient
    ColCount = 4
    FixedCols = 2
    RowCount = 22
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 1
    OnMouseUp = rdgPrintFrequencyMouseUp
    OnSelectCell = rdgPrintFrequencySelectCell
    OnSetEditText = rdgPrintFrequencySetEditText
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = True
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnBeforeDrawCell = rdgPrintFrequencyBeforeDrawCell
    OnColSize = rdgPrintFrequencyColSize
    ColorRangeSelection = False
    OnHorizontalScroll = rdgPrintFrequencyHorizontalScroll
    OnDistributeTextProgress = rdgPrintFrequencyDistributeTextProgress
    ColorSelectedRow = False
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
          'default'
          'seconds'
          'minutes'
          'hours'
          'days'
          'years'
          'step'
          'end')
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitWidth = 424
    ExplicitHeight = 107
    ColWidths = (
      64
      64
      64
      75)
    RowHeights = (
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24
      24)
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 606
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 424
    object rdeTime: TRbwDataEntry
      Left = 136
      Top = 10
      Width = 56
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 1
      Text = '0'
      OnChange = rdeTimeChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
    object comboUnits: TJvImageComboBox
      Left = 198
      Top = 8
      Width = 73
      Height = 28
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 145
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 22
      ItemIndex = 0
      TabOrder = 0
      OnChange = comboUnitsChange
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'default'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'seconds'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'minutes'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'hours'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'days'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'years'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'step'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'end'
        end>
    end
  end
end
