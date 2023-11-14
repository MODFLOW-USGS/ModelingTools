inherited frmFormulaErrors: TfrmFormulaErrors
  Left = 496
  Top = 282
  HelpType = htKeyword
  HelpKeyword = 'Formula_Errors_Dialog_Box'
  ActiveControl = btnClose
  Caption = 'Formula Errors'
  ClientHeight = 218
  ClientWidth = 534
  Font.Height = 19
  FormStyle = fsStayOnTop
  OnResize = FormResize
  ExplicitWidth = 546
  ExplicitHeight = 256
  PixelsPerInch = 120
  TextHeight = 19
  object pnlBottom: TPanel
    Left = 0
    Top = 173
    Width = 534
    Height = 45
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 172
    ExplicitWidth = 530
    DesignSize = (
      534
      45)
    object btnClose: TBitBtn
      Left = 401
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 4
      ExplicitLeft = 397
    end
    object btnCopy: TButton
      Left = 8
      Top = 6
      Width = 81
      Height = 33
      Hint = 'Copy table to clipboard'
      Caption = 'Copy'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnCopyClick
    end
    object btnHelp: TBitBtn
      Left = 312
      Top = 6
      Width = 83
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 3
      OnClick = btnHelpClick
      ExplicitLeft = 308
    end
    object btnClear: TButton
      Left = 182
      Top = 6
      Width = 81
      Height = 33
      Caption = 'Clear'
      TabOrder = 2
      OnClick = btnClearClick
    end
    object btnSave: TButton
      Left = 95
      Top = 6
      Width = 81
      Height = 33
      Hint = 'Copy table to clipboard'
      Caption = 'Save'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnSaveClick
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 534
    Height = 65
    Align = alTop
    ParentColor = True
    TabOrder = 0
    ExplicitWidth = 530
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 532
      Height = 63
      Align = alClient
      Caption = 'The following formulas were invalid. They may have been reset.'
      WordWrap = True
      ExplicitWidth = 459
      ExplicitHeight = 19
    end
  end
  object sgErrors: TRbwDataGrid4
    Left = 0
    Top = 65
    Width = 534
    Height = 108
    Align = alClient
    ColCount = 4
    DefaultColWidth = 100
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 1
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
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
        AutoAdjustColWidths = True
      end
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
        WordWrapCaptions = False
        WordWrapCells = True
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end
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
        WordWrapCaptions = False
        WordWrapCells = True
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitWidth = 530
    ExplicitHeight = 107
    ColWidths = (
      100
      100
      100
      100)
    RowHeights = (
      24
      25)
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 144
    Top = 112
  end
  object sdErrors: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 360
    Top = 104
  end
end
