object frmAutomate: TfrmAutomate
  Left = 0
  Top = 0
  Caption = 'Automater 2'
  ClientHeight = 604
  ClientWidth = 688
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 19
  object pcMain: TPageControl
    Left = 0
    Top = 23
    Width = 688
    Height = 581
    ActivePage = tabRunModels
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 684
    ExplicitHeight = 575
    object tabRunModels: TTabSheet
      Caption = 'Run Models'
      object Label1: TLabel
        Left = 8
        Top = 10
        Width = 137
        Height = 19
        Margins.Bottom = 2
        Caption = 'MODFLOW location'
      end
      object Label2: TLabel
        Left = 8
        Top = 70
        Width = 138
        Height = 19
        Margins.Bottom = 2
        Caption = 'ModelMuse location'
      end
      object Label3: TLabel
        Left = 8
        Top = 126
        Width = 164
        Height = 19
        Margins.Bottom = 2
        Caption = 'ModelMuse file location'
      end
      object Label4: TLabel
        Left = 8
        Top = 180
        Width = 236
        Height = 19
        Margins.Bottom = 2
        Caption = 'Ratio of rectangle length to width'
      end
      object Label9: TLabel
        Left = 291
        Top = 175
        Width = 248
        Height = 19
        Margins.Bottom = 2
        Caption = 'Time-out time for ModelMuse (sec)'
      end
      object Label10: TLabel
        Left = 161
        Top = 391
        Width = 132
        Height = 19
        Margins.Bottom = 2
        Caption = 'Warning messages'
      end
      object Label5: TLabel
        Left = 548
        Top = 404
        Width = 117
        Height = 19
        Margins.Bottom = 2
        Caption = 'NSTP in first SP '
      end
      object Label6: TLabel
        Left = 548
        Top = 454
        Width = 123
        Height = 19
        Margins.Bottom = 2
        Caption = 'Processors to use'
      end
      object lblStart: TLabel
        Left = 0
        Top = 430
        Width = 164
        Height = 19
        Margins.Bottom = 2
        Caption = 'Starting model number'
      end
      object lblEnd: TLabel
        Left = 3
        Top = 485
        Width = 158
        Height = 19
        Margins.Bottom = 2
        Caption = 'Ending model number'
      end
      object feMODFLOW: TJvFilenameEdit
        Left = 8
        Top = 32
        Width = 666
        Height = 27
        Filter = 'Executables (*.exe)|*.exe'
        ButtonWidth = 27
        TabOrder = 0
        Text = 'C:\WRDAPP\MF2005.1_10\bin\mf2005dbl.exe'
      end
      object feModelMuseApplication: TJvFilenameEdit
        Left = 8
        Top = 92
        Width = 666
        Height = 27
        Filter = 'Executables (*.exe)|*.exe'
        ButtonWidth = 27
        TabOrder = 1
        Text = '"C:\Program Files\USGS\ModelMuse2_19_1\bin\ModelMuse.exe"'
      end
      object feModelMuseFile: TJvFilenameEdit
        Left = 9
        Top = 147
        Width = 666
        Height = 27
        Filter = 'ModelMuse file (*.gpt)|*.gpt'
        ButtonWidth = 27
        TabOrder = 2
        Text = ''
      end
      object rdeRatio: TRbwDataEntry
        Left = 8
        Top = 202
        Width = 58
        Height = 22
        TabOrder = 5
        Text = '2'
        DataType = dtReal
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdgBasinAreas: TRbwDataGrid4
        Left = 8
        Top = 230
        Width = 146
        Height = 193
        ColCount = 1
        DefaultColWidth = 51
        DefaultRowHeight = 19
        FixedCols = 0
        RowCount = 21
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 7
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
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        RowHeights = (
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23
          23)
      end
      object seTimeOut: TJvSpinEdit
        Left = 291
        Top = 197
        Width = 107
        Height = 27
        Increment = 60.000000000000000000
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 120.000000000000000000
        TabOrder = 3
      end
      object rdgKv: TRbwDataGrid4
        Left = 162
        Top = 230
        Width = 146
        Height = 155
        ColCount = 1
        DefaultColWidth = 51
        DefaultRowHeight = 19
        FixedCols = 0
        RowCount = 6
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 8
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
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        RowHeights = (
          23
          23
          23
          23
          23
          23)
      end
      object cbRunModflow: TCheckBox
        Left = 422
        Top = 197
        Width = 252
        Height = 17
        Caption = 'Run MODFLOW automatically'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object rdgSy: TRbwDataGrid4
        Left = 314
        Top = 228
        Width = 146
        Height = 155
        ColCount = 1
        DefaultColWidth = 51
        DefaultRowHeight = 19
        FixedCols = 0
        RowCount = 3
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 6
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
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        RowHeights = (
          23
          23
          23)
      end
      object rdgBasinDepth: TRbwDataGrid4
        Left = 466
        Top = 230
        Width = 146
        Height = 155
        ColCount = 1
        DefaultColWidth = 51
        DefaultRowHeight = 19
        FixedCols = 0
        RowCount = 6
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 9
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
            AutoAdjustColWidths = True
          end>
        WordWrapRowCaptions = False
        RowHeights = (
          23
          23
          23
          23
          23
          23)
      end
      object btnRun: TButton
        Left = 338
        Top = 508
        Width = 76
        Height = 26
        Caption = 'Run'
        TabOrder = 18
        OnClick = btnTestThreadClick
      end
      object btnAbort: TBitBtn
        Left = 420
        Top = 507
        Width = 76
        Height = 26
        Enabled = False
        Kind = bkAbort
        NumGlyphs = 2
        TabOrder = 19
        OnClick = btnAbortClick
      end
      object memoErrors: TMemo
        Left = 172
        Top = 414
        Width = 370
        Height = 69
        ScrollBars = ssBoth
        TabOrder = 10
      end
      object FillTable: TButton
        Left = 303
        Top = 507
        Width = 17
        Height = 26
        Caption = 'Fill results table without generating output'
        Enabled = False
        TabOrder = 17
        Visible = False
        OnClick = btnRunClick
      end
      object rdeNSTP: TRbwDataEntry
        Left = 548
        Top = 426
        Width = 100
        Height = 23
        TabOrder = 11
        Text = '40'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object pb1: TProgressBar
        Left = 338
        Top = 484
        Width = 152
        Height = 18
        Max = 2
        Step = 1
        TabOrder = 14
      end
      object seProcessors: TJvSpinEdit
        Left = 548
        Top = 474
        Width = 96
        Height = 27
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        TabOrder = 13
      end
      object cbRestart: TCheckBox
        Left = 514
        Top = 511
        Width = 98
        Height = 18
        Caption = 'Restart'
        TabOrder = 20
      end
      object seStart: TJvSpinEdit
        Left = 0
        Top = 452
        Width = 130
        Height = 27
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 12
      end
      object seEnd: TJvSpinEdit
        Left = 3
        Top = 507
        Width = 127
        Height = 27
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 2147483647.000000000000000000
        TabOrder = 16
      end
      object rgMethod: TRadioGroup
        Left = 172
        Top = 488
        Width = 126
        Height = 54
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 2
        Caption = 'Method'
        ItemIndex = 0
        Items.Strings = (
          'MODFLOW'
          'Analytic')
        TabOrder = 15
      end
    end
    object tabAnalyzeResults: TTabSheet
      Caption = 'Analyze Results'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 505
        Width = 680
        Height = 42
        Align = alBottom
        TabOrder = 1
        ExplicitTop = 500
        object btnAnalyze: TButton
          Left = 8
          Top = 5
          Width = 98
          Height = 25
          Caption = 'Get results'
          TabOrder = 0
          OnClick = btnAnalyzeClick
        end
        object Button2: TButton
          Left = 112
          Top = 6
          Width = 180
          Height = 25
          Caption = 'Copy table to clipboard'
          TabOrder = 1
          OnClick = Button2Click
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 680
        Height = 505
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 500
        object Splitter1: TSplitter
          Left = 122
          Top = 1
          Width = 5
          Height = 503
          ExplicitHeight = 501
        end
        object rdgResults: TRbwDataGrid4
          Left = 127
          Top = 1
          Width = 552
          Height = 503
          Align = alClient
          ColCount = 13
          DefaultColWidth = 51
          DefaultRowHeight = 19
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 1
          OnSelectCell = rdgResultsSelectCell
          ExtendedAutoDistributeText = False
          AutoMultiEdit = False
          AutoDistributeText = False
          AutoIncreaseColCount = False
          AutoIncreaseRowCount = False
          SelectedRowOrColumnColor = clAqua
          UnselectableColor = clBtnFace
          ColorRangeSelection = False
          Columns = <
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
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
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
          ExplicitHeight = 498
          RowHeights = (
            23
            23)
        end
        object rdgFixedDistances: TRbwDataGrid4
          Left = 1
          Top = 1
          Width = 121
          Height = 503
          Align = alLeft
          ColCount = 1
          DefaultColWidth = 51
          DefaultRowHeight = 19
          FixedCols = 0
          RowCount = 20
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
          TabOrder = 0
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
              CheckMin = True
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          WordWrapRowCaptions = False
          ExplicitHeight = 498
          RowHeights = (
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23
            23)
        end
      end
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 688
    Height = 23
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'ToolBar'
    Images = ImageList1
    TabOrder = 0
    ExplicitWidth = 684
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Hint = 'Open Automater file'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Open'
      ImageIndex = 0
      MenuItem = Open1
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 0
      Hint = 'Save Automater file'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Save'
      ImageIndex = 1
      MenuItem = Save1
    end
    object ToolButton3: TToolButton
      Left = 46
      Top = 0
      Hint = 'Exit'
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Caption = 'Exit'
      ImageIndex = 2
      MenuItem = Exit1
    end
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 480
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
        Hint = 'Open Automater file'
        ImageIndex = 0
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = 'Save'
        Hint = 'Save Automater file'
        ImageIndex = 1
        OnClick = Save1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        Hint = 'Exit'
        ImageIndex = 2
        OnClick = Exit1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Automater Files (*.af)|*.af'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 416
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.af'
    Filter = 'Automater Files (*.af)|*.af'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 448
  end
  object ImageList1: TImageList
    Left = 536
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000009A0000009A00020DA400020DA40000009A0000009A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008BD4EE006BD3F80028B0
      E000019ACF00019ACF00019ACF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000990000009900000099
      000076765700D5CCCC00D1C0BF00F6F0ED00F6F1EF00E3E7E600E3E7E600406D
      080040790D000099000000660000000000000000000000000000000000000107
      9F000526B8000732C2000732C2000732C2000732C2000732C2000732C2000526
      B80001079F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000076C8E500A9E9FE0075DB
      FF0077DCFF0063D1F90030B3E300019ACF00019ACF00019ACF00000000000000
      0000000000000000000000000000000000000000000000990000009900000099
      00004E7D28000066000000660000F7EDEA00FFFFFD00F3F8F600F3F8F6004169
      080040790D00009900000066000000000000000000000000000000009A00062F
      C5000732C5000732C2000732C2000732C2000732C2000732C2000732C2000732
      C200072FC00000009A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001FA9D6008FD3EB006FD9
      FE0071D9FE0071D9FE0071D9FE0071D9FE0073DAFE007BDFFF007ADEFF0077DC
      FF00019ACF000000000000000000000000000000000000990000009900000099
      0000517D2C000066000000660000DACECD00F0EFED00FFFFFF00FDFFFF004169
      080040790D000099000000660000000000000000000001079F000530D7000633
      D5000633D0000732C7000732C3000732C2000732C2000732C2000732C2000732
      C2000732C200072FC00001079F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000031B1DC0049B7DE0071DD
      FE0077DEFE0077DEFE0077DEFE0077DEFE0077DEFE0076DEFE0076DEFE007CE1
      FF0065D2F8000000000000000000000000000000000000990000009900000099
      000051802D000066000000660000C1B4B300DEDEDC00FFFFFF00FDFFFF004266
      0800407A0C00009900000066000000000000000000000427CC000534E4000533
      DB00022FD5004868DA00042FC7000732C2000732C200002BC0003E5FD000012C
      C0000732C2000732C2000526B800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000060CAEF001FA8D800BBF4
      FE007DE7FE0081E8FE0081E8FE0081E8FE0081E8FE0081E8FE0081E8FE0084E9
      FE005ED3F100019ACF0000000000000000000000000000990000009900000099
      00004E8C2500E9E2E200E9E2E200E9E2E200E9E2E200E9E2E200E9E2E2004179
      090040830F0000990000006600000000000000009A000335F5000434F0000534
      E600224AE500FFFFFF00FFFFFF000029C8000024C200F9FAFE00FFFFFF004464
      D1000732C2000732C2000732C20000009A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000065CFF5003EB7E500BFEE
      F8008DEFFF0085EDFF0085EDFF0086EDFF0086EDFF0086EDFF0086EDFF0089EE
      FF0065D9F300019ACF0000000000000000000000000000990000009900000099
      0000009900000099000000990000009900000099000000990000009900000099
      00000099000000990000006600000000000000009A000335FA000335F6000434
      EC00002DE600EBEEFC00FFFFFF00718AE3005875DC00FFFFFF00FCFCFE00022C
      C0000732C2000732C2000732C20000009A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000077D5FC005CC8FB0020A7
      D500B6E6F300D1F5FA00D2F6FA00B9FBFE009BF8FF0091F6FF0092F6FF006BD0
      B7000C851800A4FFFF0043C1E20000000000000000000099000000990000B1D0
      B100B1D0B100B1D0B100B1D0B100B1D0B100B1D0B100B1D0B100B1D0B100B1D0
      B100B1D0B100009900000066000000000000010DAC001E4AFB000C3CFB000335
      F8000435F3000027E700385EEB00FFFFFF00FFFFFF005F7CDF000025C5000732
      C2000732C2000732C2000732C200020DA4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008BDBFF005FCDFF002CAF
      E3000D9FD30010A0D30010A0D3008ED7EC00E2FDFE00A5F8FF00A3F8FF000C85
      180038B55700ABF3EB00B5FCFD0000000000000000000099000000990000FEFD
      FD00FEFDFD00FFFEFE00FFFEFE00FFFEFE00FFFEFE00FFFEFE00FFFEFE00FDFC
      FC00B1D0B100009900000066000000000000020EAD00456AFC00204CFB000335
      FB000335F9000029EF005272F100FFFFFF00FFFFFF00718BE500002ACD000732
      C5000732C2000732C2000732C200020DA4000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009FE9FF0070DCFF0076DD
      FE0076DDFE0074DCFE0073DCFE0061CEF6001CA8D9008CCED7000C8518005BE6
      8C0059E189003EBD60000C851800019ACF00000000000099000000990000FEFE
      FE00FEFEFE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00FEFE
      FE00B1D0B10000990000006600000000000000009A007792FD006B88FD000335
      FB000130FB00F9FAFF00FFFFFF005978F300395DED00FFFFFF00FFFFFF000631
      D1000632CC000732C3000732C20000009A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7EFFF0076E5FF007CE5
      FF007CE5FF007DE5FF007DE5FF007DE3FF0072DDFB000C8518002DAD470050D9
      7B0055DE83005AE38B0033AF51000197C300000000000099000000990000FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00B1D0B10000990000006600000000000000009A006B88FD009AAEFD00093A
      FB00214CFB00FFFFFF00FCFCFF00002AF2000027EC00EBEEFD00FFFFFF004466
      E0000633D2000732C9000732C40000009A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C7FFFF0082F5FF008FF5
      FF008FF5FF008DF4FF00A0FDFF00AFFFFF00AEFFFF00A5FBF800A3FCFA0032B7
      4F0048D670000C8518000000000000000000000000000099000000990000FEFE
      FE00FEFEFE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00FEFE
      FE00B1D0B100009900000066000000000000000000001B3DE5007A94FD0089A1
      FD002A54FC002652FC000030FB000335FB000335FA00002DF0001F4AEE000130
      E2000533DE000633D4000526BF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A4E0F000A0FDFF0090FC
      FF0090FCFF0099FDFF0086E8F500019ACF00019ACF00019ACF00019ACF002CB3
      470041D166000C8518000000000000000000000000000099000000990000FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00B1D0B100009900000066000000000000000000000108A7002F56F700CDD7
      FE0097ACFD001745FB000637FB000335FB000335FB000335F7000434F2000535
      E8000534E4000530D30001079F00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000019ACF00019A
      CF00019ACF00019ACF00000000000000000000000000000000000000000028BB
      410026B13E00000000000000000000000000000000000099000000990000FEFE
      FE00FEFEFE00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00CDCDCD00FEFE
      FE00B1D0B100009900000066000000000000000000000000000000009A003158
      F700839CFD00CDD7FE00B8C6FE007E97FD006C89FD00607FFC005073FC000F3D
      F4000431E60000009A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000C8518001CAE
      31000C851800000000000000000000000000000000000099000000990000FEFE
      FE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFEFE00FEFE
      FE00B1D0B1000099000000660000000000000000000000000000000000000209
      A8001F40E600819AFD00A1B4FE00A9BAFE009AAEFD006281FD003860FC000529
      D9000107A2000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000C8518000C8518000C8518000C8518000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000009A0000009A00020EB100010DB00000009A0000009A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFF81F000081FF8001E0070000
      803F8001C0030000800780018001000080078001800100008003800100000000
      8003800100000000800180010000000080018001000000008000800100000000
      800080010000000080038001800100008003800180010000C3E78001C0030000
      FFC78001E0070000FE1FFFFFF81F000000000000000000000000000000000000
      000000000000}
  end
  object jvcrtprcs1: TJvCreateProcess
    Left = 256
    Top = 184
  end
  object SaveDialogResults: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text file (*.txt)|*.txt'
    Left = 352
    Top = 16
  end
end
