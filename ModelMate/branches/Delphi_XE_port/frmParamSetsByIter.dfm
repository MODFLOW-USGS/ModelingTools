object FormParamSetsByIter: TFormParamSetsByIter
  Left = 0
  Top = 0
  Caption = 'Parameter Sets By Iteration'
  ClientHeight = 566
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    792
    566)
  PixelsPerInch = 96
  TextHeight = 16
  object RbwDataGrid41: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 792
    Height = 479
    Align = alTop
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect, goAlwaysShowEditor]
    TabOrder = 0
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    ColorSelectedRow = True
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
        Format = rcf4Integer
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        PickList.Strings = (
          'Iteration')
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
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
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
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
        Format = rcf4Real
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
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
        CellAlignment = taCenter
        CheckMax = False
        CheckMin = False
        ComboUsed = False
        Format = rcf4Boolean
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
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
        Format = rcf4String
        LimitToList = False
        MaxLength = 0
        ParentButtonFont = False
        WordWrapCaptions = False
        WordWrapCells = False
        AutoAdjustColWidths = False
      end>
    ColWidths = (
      71
      78
      90
      62
      84)
    RowHeights = (
      24
      24
      24
      24
      24)
  end
  object btnReplace: TBitBtn
    Left = 119
    Top = 485
    Width = 272
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = 'Replace Parameter Values with Selected Set'
    TabOrder = 1
  end
  object btnGWChart: TBitBtn
    Left = 397
    Top = 485
    Width = 214
    Height = 31
    Anchors = [akLeft, akBottom]
    Caption = 'View Parameter Sets in GW_Chart'
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 397
    Top = 532
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 316
    Top = 532
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Kind = bkOK
  end
end
