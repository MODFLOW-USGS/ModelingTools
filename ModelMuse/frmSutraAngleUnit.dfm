inherited frmSutraAngle: TfrmSutraAngle
  HelpType = htKeyword
  HelpKeyword = 'SUTRA_Cross_Section_Dialog_Box'
  Caption = 'SUTRA/DISV Cross Section'
  ClientHeight = 272
  ClientWidth = 368
  ExplicitWidth = 380
  ExplicitHeight = 310
  TextHeight = 18
  object lblAngle: TLabel
    Left = 297
    Top = 75
    Width = 40
    Height = 18
    Anchors = [akTop, akRight]
    Caption = 'Angle'
    ExplicitLeft = 207
  end
  object seAngle: TJvSpinEdit
    Left = 8
    Top = 72
    Width = 283
    Height = 26
    Decimal = 5
    MaxValue = 90.000000000000000000
    MinValue = -90.000000000000000000
    ValueType = vtFloat
    OnBottomClick = seAngleButtonClick
    OnTopClick = seAngleButtonClick
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = seAngleChange
    ExplicitWidth = 279
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 231
    Width = 368
    Height = 41
    Align = alBottom
    TabOrder = 3
    ExplicitTop = 230
    ExplicitWidth = 364
    DesignSize = (
      368
      41)
    object btnHelp: TBitBtn
      Left = 100
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 96
    end
    object btnOK: TBitBtn
      Left = 188
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 184
    end
    object btnCancel: TBitBtn
      Left = 276
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 272
    end
  end
  object rgSpecify: TRadioGroup
    Left = 8
    Top = 8
    Width = 348
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Specify'
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      'Angle'
      'End points')
    TabOrder = 0
    OnClick = rgSpecifyClick
    ExplicitWidth = 344
  end
  object rdgEndPoints: TRbwDataGrid4
    Left = 8
    Top = 110
    Width = 348
    Height = 113
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 3
    DefaultColWidth = 10
    Enabled = False
    FixedCols = 1
    RowCount = 3
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing, goAlwaysShowEditor]
    TabOrder = 2
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnEndUpdate = rdgEndPointsEndUpdate
    ColorRangeSelection = True
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
        Format = rcf4Real
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
        Format = rcf4Real
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
    ExplicitWidth = 344
    ExplicitHeight = 112
  end
end
