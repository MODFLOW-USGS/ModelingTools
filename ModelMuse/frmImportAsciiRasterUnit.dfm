inherited frmImportAsciiRaster: TfrmImportAsciiRaster
  HelpKeyword = 'Import_ASCII_Raster_File'
  Caption = 'Import ASCII Raster File'
  ClientHeight = 333
  ClientWidth = 628
  ExplicitLeft = 3
  ExplicitTop = 3
  ExplicitWidth = 640
  ExplicitHeight = 371
  TextHeight = 18
  inherited lblDataSet: TLabel
    Top = 13
    ExplicitTop = 13
  end
  inherited lblInterpolator: TLabel
    Top = 67
    ExplicitTop = 67
  end
  object lblConvert: TLabel [2]
    Left = 8
    Top = 270
    Width = 233
    Height = 18
    Caption = 'Convert X and Y coordinates from'
  end
  object lblTo: TLabel [3]
    Left = 422
    Top = 270
    Width = 13
    Height = 18
    Caption = 'to'
  end
  object lblWarning: TLabel [4]
    Left = 8
    Top = 299
    Width = 249
    Height = 38
    Caption = 'When importing multiple files, all must have the same points. '
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Pitch = fpVariable
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  inherited comboDataSets: TComboBox
    Top = 37
    Width = 517
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitTop = 37
    ExplicitWidth = 513
  end
  inherited comboInterpolators: TComboBox
    Top = 91
    TabOrder = 4
    ExplicitTop = 91
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 132
    Top = 0
    TabOrder = 0
    Visible = False
    ExplicitLeft = 132
    ExplicitTop = 0
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 118
    TabOrder = 5
    ExplicitTop = 118
  end
  inherited cbInterpolation: TCheckBox
    Top = 150
    TabOrder = 6
    ExplicitTop = 150
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 215
    TabOrder = 8
    ExplicitTop = 215
  end
  inherited btnOK: TBitBtn
    Left = 445
    Top = 299
    TabOrder = 12
    OnClick = btnOKClick
    ExplicitLeft = 445
    ExplicitTop = 299
  end
  inherited btnCancel: TBitBtn
    Left = 542
    Top = 299
    TabOrder = 13
    ExplicitLeft = 542
    ExplicitTop = 299
  end
  inherited btnHelp: TBitBtn
    Left = 350
    Top = 299
    TabOrder = 11
    ExplicitLeft = 350
    ExplicitTop = 299
  end
  object rgFilterMethod: TRadioGroup [14]
    Left = 360
    Top = 91
    Width = 240
    Height = 140
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Filter method'
    ItemIndex = 2
    Items.Strings = (
      'Lowest point in cell'
      'Highest point in cell'
      'Average of points in cell'
      'Point closest to cell center'
      'None')
    TabOrder = 3
    ExplicitWidth = 236
  end
  object rdgFilesAndDataSets: TRbwDataGrid4 [15]
    Left = 8
    Top = 8
    Width = 592
    Height = 53
    Anchors = [akLeft, akTop, akRight]
    ColCount = 3
    DefaultColWidth = 20
    FixedCols = 1
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    Visible = False
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    OnButtonClick = rdgFilesAndDataSetsButtonClick
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
        ButtonFont.Charset = ANSI_CHARSET
        ButtonFont.Color = clBlack
        ButtonFont.Height = -16
        ButtonFont.Name = 'Arial'
        ButtonFont.Pitch = fpVariable
        ButtonFont.Style = []
        ButtonUsed = True
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
        ButtonFont.Charset = ANSI_CHARSET
        ButtonFont.Color = clBlack
        ButtonFont.Height = -16
        ButtonFont.Name = 'Arial'
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = True
      end>
    WordWrapRowCaptions = False
    ExplicitWidth = 588
    ColWidths = (
      20
      20
      22)
    RowHeights = (
      24
      24)
  end
  object comboModel: TComboBox [16]
    Left = 8
    Top = 187
    Width = 281
    Height = 26
    Style = csDropDownList
    TabOrder = 7
  end
  object comboFromUnits: TComboBox [17]
    Left = 271
    Top = 267
    Width = 145
    Height = 26
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 9
    Text = 'No conversion'
    Items.Strings = (
      'No conversion'
      'cm'
      'm'
      'km'
      'feet'
      'miles')
  end
  object comboToUnits: TComboBox [18]
    Left = 441
    Top = 267
    Width = 145
    Height = 26
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 10
    Text = 'No conversion'
    Items.Strings = (
      'No conversion'
      'cm'
      'm'
      'km'
      'feet'
      'miles')
  end
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Text files (*.txt; *.asc)|*.txt;*.asc|All files (*.*)|*.*'
    FilterIndex = 1
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Open a ASCII raster file(s)'
    Left = 528
    Top = 23
  end
end
