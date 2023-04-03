inherited frmTimeControl: TfrmTimeControl
  Left = 728
  Top = 245
  HelpType = htKeyword
  HelpKeyword = 'Time_Control_Dialog_Box'
  Caption = 'PHAST Time Control'
  ClientHeight = 248
  ClientWidth = 396
  KeyPreview = True
  OnKeyDown = FormKeyDown
  ExplicitWidth = 408
  ExplicitHeight = 286
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 142
    Width = 396
    Height = 106
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 141
    ExplicitWidth = 392
    DesignSize = (
      396
      106)
    object lblNumberOfPeriods: TLabel
      Left = 95
      Top = 9
      Width = 129
      Height = 18
      Caption = 'Number of periods'
    end
    object lblStartTime: TLabel
      Left = 95
      Top = 40
      Width = 67
      Height = 18
      Caption = 'Start time'
    end
    object seTimeCount: TJvSpinEdit
      Left = 5
      Top = 6
      Width = 84
      Height = 26
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      TabOrder = 0
      OnChange = seTimeCountChange
    end
    object btnOK: TBitBtn
      Left = 195
      Top = 64
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 191
    end
    object btnCancel: TBitBtn
      Left = 291
      Top = 64
      Width = 89
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
      ExplicitLeft = 287
    end
    object btnHelp: TBitBtn
      Left = 99
      Top = 64
      Width = 89
      Height = 33
      HelpType = htKeyword
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 95
    end
    object rdeStartTime: TRbwDataEntry
      Left = 5
      Top = 37
      Width = 84
      Height = 22
      TabOrder = 4
      Text = '0'
      DataType = dtReal
      Max = 1.000000000000000000
      ChangeDisabledColor = True
    end
  end
  object dgTime: TRbwDataGrid4
    Left = 0
    Top = 33
    Width = 396
    Height = 109
    Align = alClient
    ColCount = 3
    DefaultColWidth = 180
    FixedCols = 1
    RowCount = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = 17
    Font.Name = 'Microsoft Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goTabs]
    ParentFont = False
    TabOrder = 0
    OnMouseUp = dgTimeMouseUp
    OnSetEditText = dgTimeSetEditText
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnColSize = dgTimeColSize
    ColorRangeSelection = False
    OnHorizontalScroll = dgTimeHorizontalScroll
    Columns = <
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 25
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
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 25
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
        AutoAdjustColWidths = False
      end
      item
        AutoAdjustRowHeights = False
        AutoAdjustCaptionRowHeights = False
        ButtonCaption = '...'
        ButtonFont.Charset = DEFAULT_CHARSET
        ButtonFont.Color = clWindowText
        ButtonFont.Height = -11
        ButtonFont.Name = 'MS Sans Serif'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = False
        ButtonWidth = 25
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
        AutoAdjustColWidths = False
      end>
    WordWrapRowCaptions = False
    ExplicitWidth = 392
    ExplicitHeight = 108
    ColWidths = (
      180
      180
      180)
    RowHeights = (
      24
      24)
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 396
    Height = 33
    Align = alTop
    ParentColor = True
    TabOrder = 2
    ExplicitWidth = 392
    object rdeLength: TRbwDataEntry
      Left = 136
      Top = 6
      Width = 145
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 0
      Text = '0'
      OnChange = rdeLengthChange
      DataType = dtReal
      Max = 1.000000000000000000
      CheckMin = True
      ChangeDisabledColor = True
    end
  end
end
