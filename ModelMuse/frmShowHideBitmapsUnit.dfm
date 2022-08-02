inherited frmShowHideBitmaps: TfrmShowHideBitmaps
  Left = 546
  Top = 310
  HelpType = htKeyword
  HelpKeyword = 'Show_or_Hide_Bitmaps_Dialog_Box'
  Caption = 'Show or Hide Images'
  ClientWidth = 467
  ExplicitWidth = 485
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 181
    Width = 467
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      467
      45)
    object btnClose: TBitBtn
      Left = 367
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 4
    end
    object btnHelp: TBitBtn
      Left = 278
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
    end
    object btnShowAll: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Show all'
      TabOrder = 0
      OnClick = btnShowClick
    end
    object btnShowNone: TButton
      Left = 88
      Top = 6
      Width = 97
      Height = 33
      Caption = 'Show none'
      TabOrder = 1
      OnClick = btnShowClick
    end
    object btnToggle: TButton
      Left = 192
      Top = 6
      Width = 75
      Height = 33
      Caption = 'Toggle'
      TabOrder = 2
      OnClick = btnToggleClick
    end
  end
  object rdgBitmaps: TRbwDataGrid4
    Left = 0
    Top = 0
    Width = 467
    Height = 181
    Align = alClient
    ColCount = 2
    FixedCols = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnRowMoved = rdgBitmapsRowMoved
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnStateChange = rdgBitmapsStateChange
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
    ColWidths = (
      27
      64)
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmr1Timer
    Left = 192
    Top = 24
  end
end
