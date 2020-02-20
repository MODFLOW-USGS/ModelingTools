object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 482
  ClientWidth = 790
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    790
    482)
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 32
    Top = 464
    Width = 15
    Height = 12
    Margins.Bottom = 2
    Caption = 'lbl1'
  end
  object btnTestPriorityQueue: TButton
    Left = 8
    Top = 8
    Width = 73
    Height = 33
    Caption = 'Test Priortiy Queue'
    TabOrder = 0
    WordWrap = True
    OnClick = btnTestPriorityQueueClick
  end
  object memo2: TMemo
    Left = 210
    Top = 42
    Width = 257
    Height = 432
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 8
  end
  object btnSimplePitRemoval: TButton
    Left = 8
    Top = 47
    Width = 75
    Height = 33
    Caption = 'Test Simple Pit Removal'
    TabOrder = 10
    WordWrap = True
    OnClick = btnSimplePitRemovalClick
  end
  object btnTestCompoundPitRemoval: TButton
    Left = 8
    Top = 86
    Width = 75
    Height = 49
    Caption = 'Test Compound Pit Removal'
    TabOrder = 13
    WordWrap = True
    OnClick = btnTestCompoundPitRemovalClick
  end
  object btnTestMultipleSimplePits: TButton
    Left = 8
    Top = 141
    Width = 75
    Height = 33
    Caption = 'Test Multiple Simple Pits'
    TabOrder = 15
    WordWrap = True
    OnClick = btnTestMultipleSimplePitsClick
  end
  object btnAnotherSimpleTest: TButton
    Left = 8
    Top = 180
    Width = 75
    Height = 25
    Caption = 'Another Simple Test'
    TabOrder = 17
    WordWrap = True
    OnClick = btnAnotherSimpleTestClick
  end
  object btnTransformSurferGridFile: TButton
    Left = 8
    Top = 211
    Width = 75
    Height = 30
    Caption = 'Transform Surfer Grid File'
    TabOrder = 20
    WordWrap = True
    OnClick = btnTransformSurferGridFileClick
  end
  object fedInput: TJvFilenameEdit
    Left = 8
    Top = 319
    Width = 196
    Height = 20
    Filter = 'Surfer Grid files (*.grd)|*.grd'
    ButtonWidth = 28
    TabOrder = 26
    OnChange = fedInputChange
  end
  object fedOutput: TJvFilenameEdit
    Left = 8
    Top = 343
    Width = 196
    Height = 20
    Filter = 'Surfer Grid files (*.grd)|*.grd'
    ButtonWidth = 28
    TabOrder = 27
  end
  object btnCalculateAccumulation: TButton
    Left = 8
    Top = 247
    Width = 75
    Height = 34
    Caption = 'Calculate Accumulation'
    TabOrder = 22
    WordWrap = True
    OnClick = btnCalculateAccumulationClick
  end
  object pb1: TProgressBar
    Left = 8
    Top = 442
    Width = 197
    Height = 16
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Max = 1000
    Step = 1
    TabOrder = 34
  end
  object btnExportStreams: TButton
    Left = 113
    Top = 47
    Width = 75
    Height = 21
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Export Streams'
    TabOrder = 11
    WordWrap = True
    OnClick = btnExportStreamsClick
  end
  object btnTestSimpleAccumulation: TButton
    Left = 113
    Top = 8
    Width = 75
    Height = 33
    Caption = 'Test Simple Accumulation'
    TabOrder = 1
    WordWrap = True
    OnClick = btnTestSimpleAccumulationClick
  end
  object btnExportStreamsRunoffStreamGrid: TButton
    Left = 114
    Top = 72
    Width = 74
    Height = 49
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Export Streams Shapefile and Runoff Stream Grid'
    TabOrder = 12
    WordWrap = True
    OnClick = btnExportStreamsRunoffStreamGridClick
  end
  object btn1: TButton
    Left = 114
    Top = 125
    Width = 74
    Height = 49
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Export Streams Shapefile and Runoff Stream Grid'
    TabOrder = 14
    WordWrap = True
    OnClick = btn1Click
  end
  object btnAssignMValuesFromRaster: TButton
    Left = 114
    Top = 179
    Width = 75
    Height = 49
    Caption = 'Assign M Values From Raster'
    TabOrder = 16
    WordWrap = True
    OnClick = btnAssignMValuesFromRasterClick
  end
  object fedInputShapefile: TJvFilenameEdit
    Left = 8
    Top = 369
    Width = 196
    Height = 20
    Filter = 'Surfer Grid files (*.shp)|*.shp'
    ButtonWidth = 28
    TabOrder = 28
  end
  object fedOutputShapeFile: TJvFilenameEdit
    Left = 8
    Top = 395
    Width = 196
    Height = 20
    Filter = 'Surfer Grid files (*.shp)|*.shp'
    ButtonWidth = 28
    TabOrder = 29
  end
  object rdgValues: TRbwDataGrid4
    Left = 473
    Top = 42
    Width = 309
    Height = 132
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 9
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = True
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end>
    WordWrapRowCaptions = False
  end
  object seRow: TJvSpinEdit
    Left = 473
    Top = 16
    Width = 80
    Height = 24
    Value = 5.000000000000000000
    TabOrder = 5
    OnChange = seRowChange
  end
  object seColumn: TJvSpinEdit
    Left = 559
    Top = 16
    Width = 80
    Height = 24
    Value = 5.000000000000000000
    TabOrder = 6
    OnChange = seColumnChange
  end
  object rdgStartingPoints: TRbwDataGrid4
    Left = 473
    Top = 180
    Width = 152
    Height = 109
    ColCount = 2
    FixedCols = 0
    RowCount = 20
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 18
    ExtendedAutoDistributeText = False
    AutoMultiEdit = False
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        Format = rcf4Integer
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
  object rdgPits: TRbwDataGrid4
    Left = 631
    Top = 180
    Width = 152
    Height = 109
    ColCount = 2
    FixedCols = 0
    RowCount = 20
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 19
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = False
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
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
        WordWrapCaptions = False
        WordWrapCells = False
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        Format = rcf4Integer
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
  object rdgPitless: TRbwDataGrid4
    Left = 473
    Top = 295
    Width = 309
    Height = 132
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    TabOrder = 25
    ExtendedAutoDistributeText = False
    AutoMultiEdit = True
    AutoDistributeText = True
    AutoIncreaseColCount = True
    AutoIncreaseRowCount = True
    SelectedRowOrColumnColor = clAqua
    UnselectableColor = clBtnFace
    ColorRangeSelection = False
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
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
        CaseSensitivePicklist = False
        CheckStyle = csCheck
        AutoAdjustColWidths = False
      end>
    WordWrapRowCaptions = False
  end
  object btnTestPitFilling: TButton
    Left = 473
    Top = 433
    Width = 75
    Height = 25
    Caption = 'Test Pit Filling'
    TabOrder = 33
    OnClick = btnTestPitFillingClick
  end
  object seX: TJvSpinEdit
    Left = 210
    Top = 16
    Width = 71
    Height = 24
    TabOrder = 3
  end
  object seY: TJvSpinEdit
    Left = 303
    Top = 16
    Width = 66
    Height = 24
    TabOrder = 4
  end
  object btnExtract: TButton
    Left = 375
    Top = 11
    Width = 75
    Height = 25
    Caption = 'btnExtract'
    TabOrder = 2
    OnClick = btnExtractClick
  end
  object fedClipShapeFile: TJvFilenameEdit
    Left = 9
    Top = 419
    Width = 196
    Height = 20
    Filter = 'Surfer Grid files (*.shp)|*.shp'
    ButtonWidth = 28
    TabOrder = 30
  end
  object btnRandomGrids: TButton
    Left = 113
    Top = 233
    Width = 74
    Height = 32
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Test Random Grids'
    TabOrder = 21
    WordWrap = True
    OnClick = btnRandomGridsClick
  end
  object seRandSeed: TJvSpinEdit
    Left = 648
    Top = 18
    Width = 55
    Height = 24
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    TabOrder = 7
  end
  object btnTestRndGrd: TButton
    Left = 114
    Top = 270
    Width = 73
    Height = 19
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Test a rnd grid'
    TabOrder = 23
    OnClick = btnTestRndGrdClick
  end
  object btnTestOutline: TButton
    Left = 114
    Top = 294
    Width = 75
    Height = 25
    Caption = 'btnTestOutline'
    TabOrder = 24
    OnClick = btnTestOutlineClick
  end
  object btnTestFileQueue: TButton
    Left = 558
    Top = 432
    Width = 81
    Height = 26
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Test File Queue'
    TabOrder = 31
    OnClick = btnTestFileQueueClick
  end
  object btnTestFileQueue2: TButton
    Left = 644
    Top = 432
    Width = 81
    Height = 26
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Test File Queue'
    TabOrder = 32
    OnClick = btnTestFileQueue2Click
  end
  object sdFileQueueFile: TSaveDialog
    Left = 864
    Top = 608
  end
end
