inherited frmImportSurferGrdFile: TfrmImportSurferGrdFile
  HelpKeyword = 'Import_Surfer_Grid_File'
  Caption = 'Import Surfer Grid File'
  ClientHeight = 473
  ClientWidth = 594
  ExplicitWidth = 612
  ExplicitHeight = 518
  PixelsPerInch = 96
  TextHeight = 18
  inherited lblDataSet: TLabel
    Top = 156
    ExplicitTop = 156
  end
  inherited lblInterpolator: TLabel
    Top = 224
    ExplicitTop = 224
  end
  object Label1: TLabel [2]
    Left = 8
    Top = 8
    Width = 99
    Height = 18
    Caption = 'Grid file extent'
  end
  object lblConvert: TLabel [3]
    Left = 8
    Top = 403
    Width = 233
    Height = 18
    Caption = 'Convert X and Y coordinates from'
  end
  object lblTo: TLabel [4]
    Left = 422
    Top = 403
    Width = 13
    Height = 18
    Caption = 'to'
  end
  inherited comboDataSets: TComboBox
    Top = 178
    Width = 578
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitTop = 178
    ExplicitWidth = 578
  end
  inherited comboInterpolators: TComboBox
    Top = 245
    TabOrder = 3
    ExplicitTop = 245
  end
  inherited cbEnclosedCells: TCheckBox
    Left = 496
    Top = 309
    Width = 41
    TabOrder = 6
    Visible = False
    ExplicitLeft = 496
    ExplicitTop = 309
    ExplicitWidth = 41
  end
  inherited cbIntersectedCells: TCheckBox
    Top = 277
    Width = 329
    TabOrder = 4
    ExplicitTop = 277
    ExplicitWidth = 329
  end
  inherited cbInterpolation: TCheckBox
    Top = 308
    Width = 321
    TabOrder = 5
    ExplicitTop = 308
    ExplicitWidth = 321
  end
  inherited rgEvaluatedAt: TRadioGroup
    Top = 345
    TabOrder = 7
    ExplicitTop = 345
  end
  inherited btnOK: TBitBtn
    Left = 398
    Top = 432
    TabOrder = 11
    OnClick = btnOKClick
    ExplicitLeft = 398
    ExplicitTop = 432
  end
  inherited btnCancel: TBitBtn
    Left = 495
    Top = 432
    TabOrder = 12
    ExplicitLeft = 495
    ExplicitTop = 432
  end
  inherited btnHelp: TBitBtn
    Left = 301
    Top = 432
    TabOrder = 10
    ExplicitLeft = 301
    ExplicitTop = 432
  end
  object rdgLimits: TRbwDataGrid4 [14]
    Left = 8
    Top = 29
    Width = 578
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    ColCount = 4
    FixedCols = 1
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = False
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = False
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
    ColorSelectedRow = False
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
      end>
    WordWrapRowCaptions = False
  end
  object rgFilterMethod: TRadioGroup [15]
    Left = 343
    Top = 210
    Width = 243
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
    TabOrder = 2
  end
  object comboFromUnits: TComboBox [16]
    Left = 271
    Top = 400
    Width = 145
    Height = 26
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 8
    Text = 'No conversion'
    Items.Strings = (
      'No conversion'
      'cm'
      'm'
      'km'
      'feet'
      'miles')
  end
  object comboToUnits: TComboBox [17]
    Left = 441
    Top = 400
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
  inherited OpenDialogFile: TOpenDialog
    Filter = 'Surfer grid file (*.grd, *.dat)|*.grd;*.dat|All files (*.*)|*.*'
    Title = 'Open a Surfer grid file'
    Top = 216
  end
end
