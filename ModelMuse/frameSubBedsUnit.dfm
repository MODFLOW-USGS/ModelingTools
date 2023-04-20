object frameSubBeds: TframeSubBeds
  Left = 0
  Top = 0
  Width = 494
  Height = 350
  TabOrder = 0
  TabStop = True
  DesignSize = (
    494
    350)
  object lblCount: TLabel
    Left = 63
    Top = 324
    Width = 54
    Height = 20
    Anchors = [akLeft, akBottom]
    Caption = 'Number'
  end
  object sbAdd: TSpeedButton
    Left = 410
    Top = 321
    Width = 23
    Height = 22
    Hint = 'Add layer group|Add a layer group below the bottom layer group.'
    Anchors = [akRight, akBottom]
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
    OnClick = sbAddClick
  end
  object sbInsert: TSpeedButton
    Left = 439
    Top = 321
    Width = 23
    Height = 22
    Hint = 
      'Insert layer group|Insert a layer group above the selected layer' +
      ' group.'
    Anchors = [akRight, akBottom]
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
    OnClick = sbInsertClick
  end
  object sbDelete: TSpeedButton
    Left = 468
    Top = 321
    Width = 23
    Height = 22
    Hint = 'Delete layer group|Delete the selected layer group.'
    Anchors = [akRight, akBottom]
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
    OnClick = sbDeleteClick
  end
  object rdgSubBed: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 494
    Height = 311
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnSelectCell = rdgSubBedSelectCell
    OnSetEditText = rdgSubBedSetEditText
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnBeforeDrawCell = rdgSubBedBeforeDrawCell
    OnStateChange = rdgSubBedStateChange
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
  object seCount: TJvSpinEdit
    Left = 3
    Top = 321
    Width = 54
    Height = 25
    CheckMinValue = True
    ButtonKind = bkClassic
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    OnChange = seCountChange
    OnEnter = seCountEnter
  end
end
