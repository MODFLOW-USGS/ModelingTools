object frmExtract: TfrmExtract
  Left = 256
  Top = 94
  HelpContext = 400
  Caption = 
    'GW_Chart Hydrograph Extractor: Plots of model output through tim' +
    'e'
  ClientHeight = 541
  ClientWidth = 835
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 265
    Width = 835
    Height = 4
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 835
    Height = 265
    Align = alTop
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 397
      Top = 153
      Width = 4
      Height = 111
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 833
      Height = 152
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      DesignSize = (
        833
        152)
      object lblCount: TLabel
        Left = 5
        Top = 3
        Width = 74
        Height = 13
        Alignment = taCenter
        Caption = 'Number of Cells'
      end
      object sbFormat: TSpeedButton
        Left = 216
        Top = 20
        Width = 19
        Height = 21
        Hint = 'Format Chart'
        Anchors = [akTop, akRight]
        Enabled = False
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555550FF0559
          1950555FF75F7557F7F757000FF055591903557775F75557F77570FFFF055559
          1933575FF57F5557F7FF0F00FF05555919337F775F7F5557F7F700550F055559
          193577557F7F55F7577F07550F0555999995755575755F7FFF7F5570F0755011
          11155557F755F777777555000755033305555577755F75F77F55555555503335
          0555555FF5F75F757F5555005503335505555577FF75F7557F55505050333555
          05555757F75F75557F5505000333555505557F777FF755557F55000000355557
          07557777777F55557F5555000005555707555577777FF5557F55553000075557
          0755557F7777FFF5755555335000005555555577577777555555}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = sbFormatClick
        OnMouseMove = ToolBar1MouseMove
      end
      object sbImage: TSpeedButton
        Left = 239
        Top = 20
        Width = 19
        Height = 21
        Hint = 'Save as image'
        Enabled = False
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033BBBBBBBBBB
          BB33337777777777777F33BB00BBBBBBBB33337F77333333F37F33BB0BBBBBB0
          BB33337F73F33337FF7F33BBB0BBBB000B33337F37FF3377737F33BBB00BB00B
          BB33337F377F3773337F33BBBB0B00BBBB33337F337F7733337F33BBBB000BBB
          BB33337F33777F33337F33EEEE000EEEEE33337F3F777FFF337F33EE0E80000E
          EE33337F73F77773337F33EEE0800EEEEE33337F37377F33337F33EEEE000EEE
          EE33337F33777F33337F33EEEEE00EEEEE33337F33377FF3337F33EEEEEE00EE
          EE33337F333377F3337F33EEEEEE00EEEE33337F33337733337F33EEEEEEEEEE
          EE33337FFFFFFFFFFF7F33EEEEEEEEEEEE333377777777777773}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        OnClick = sbImageClick
      end
      object lblLegend: TLabel
        Left = 653
        Top = 128
        Width = 60
        Height = 13
        Caption = 'Legend Text'
      end
      object Label1: TLabel
        Left = 726
        Top = 46
        Width = 79
        Height = 13
        Caption = 'Density of Water'
      end
      object Label2: TLabel
        Left = 726
        Top = 66
        Width = 33
        Height = 13
        Caption = 'Gravity'
      end
      object btnRead: TButton
        Left = 5
        Top = 48
        Width = 60
        Height = 22
        Caption = 'Read'
        TabOrder = 3
        OnClick = btnReadClick
      end
      object btnSave: TButton
        Left = 69
        Top = 48
        Width = 60
        Height = 23
        Caption = 'Save'
        Enabled = False
        TabOrder = 4
        OnClick = btnSaveClick
      end
      object btnCancel: TButton
        Left = 133
        Top = 48
        Width = 60
        Height = 23
        Caption = 'Cancel'
        Enabled = False
        TabOrder = 5
        OnClick = btnCancelClick
      end
      object BitBtn1: TBitBtn
        Left = 197
        Top = 48
        Width = 60
        Height = 23
        Caption = 'Close'
        Kind = bkClose
        NumGlyphs = 2
        TabOrder = 6
      end
      object adeCellCount: TArgusDataEntry
        Left = 5
        Top = 24
        Width = 60
        Height = 22
        TabOrder = 2
        Text = '1'
        OnChange = adeCellCountChange
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgProgramChoice: TRadioGroup
        Left = 261
        Top = 8
        Width = 282
        Height = 137
        Caption = 'Data'
        ItemIndex = 0
        Items.Strings = (
          'MODFLOW head, drawdown, or concentration file'
          'MT3DMS Observations'
          'MT3DMS Concentrations'
          'GWT and MOC3D Observations'
          'SUTRA 09.97'
          'SUTRA 2D/3D'
          'SUTRA 2.1, 2.2, 3.0, or 4.0'
          'MODFLOW Hydmod package'
          'MODFLOW 6 Obs Utility output')
        TabOrder = 9
        OnClick = rgProgramChoiceClick
      end
      object rgMOC3D: TRadioGroup
        Left = 670
        Top = 8
        Width = 135
        Height = 39
        Caption = 'MOC3D Data'
        Enabled = False
        ItemIndex = 1
        Items.Strings = (
          'Head'
          'Concentration')
        TabOrder = 11
        OnClick = rgMOC3DClick
      end
      object rgSutra: TRadioGroup
        Left = 549
        Top = 5
        Width = 119
        Height = 114
        Caption = 'SUTRA Data'
        Enabled = False
        ItemIndex = 0
        Items.Strings = (
          'Pressure'
          'Concentration'
          'Saturation'
          'Equivalent Head')
        TabOrder = 10
        OnClick = rgSutraClick
      end
      object cbPlot: TCheckBox95
        Left = 66
        Top = 20
        Width = 97
        Height = 26
        Alignment = taLeftJustify
        Caption = 'Plot immediately'
        TabOrder = 0
        WordWrap = True
        AlignmentBtn = taLeftJustify
        LikePushButton = False
        VerticalAlignment = vaTop
      end
      object comboUnits: TComboBox
        Left = 168
        Top = 20
        Width = 47
        Height = 21
        Hint = 'Time Units'
        Style = csDropDownList
        Color = clBtnFace
        Enabled = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnChange = comboUnitsChange
        Items.Strings = (
          'sec'
          'min'
          'hour'
          'day'
          'week'
          'month'
          'year')
      end
      object edExplanation: TEdit
        Left = 549
        Top = 125
        Width = 98
        Height = 21
        TabOrder = 14
        Text = 'EXPLANATION'
        OnChange = edExplanationChange
      end
      object adeDensity: TArgusDataEntry
        Left = 674
        Top = 44
        Width = 46
        Height = 18
        Color = clBtnFace
        Enabled = False
        TabOrder = 12
        Text = '999.73'
        OnExit = dgDataNodesExit
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object adeG: TArgusDataEntry
        Left = 674
        Top = 63
        Width = 46
        Height = 18
        Color = clBtnFace
        Enabled = False
        TabOrder = 13
        Text = '9.80665'
        OnExit = dgDataNodesExit
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object btnPlotAll: TButton
        Left = 5
        Top = 76
        Width = 60
        Height = 25
        Caption = 'Plot All'
        TabOrder = 7
        OnClick = btnPlotAllClick
      end
      object btnPlotNone: TButton
        Left = 69
        Top = 76
        Width = 60
        Height = 25
        Caption = 'Plot None'
        TabOrder = 8
        OnClick = btnPlotNoneClick
      end
    end
    object dgDataPoints: TEcDataGrid
      Left = 1
      Top = 153
      Width = 227
      Height = 111
      Align = alLeft
      ColCount = 4
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
      TabOrder = 0
      OnSetEditText = dgDataPointsSetEditText
      Columns = <
        item
          Title.Caption = 'Column'
          Title.WordWrap = False
        end
        item
          Title.Caption = 'Row'
          Title.WordWrap = False
        end
        item
          Title.Caption = 'Layer'
          Title.WordWrap = False
        end
        item
          LimitToList = True
          PickList.Strings = (
            'No'
            'Yes')
          Title.Caption = 'Plot'
          Title.WordWrap = False
        end>
      RowCountMin = 0
      SelectedIndex = 0
      Version = '2.0'
    end
    object dgDataNodes: TEcDataGrid
      Left = 228
      Top = 153
      Width = 169
      Height = 111
      Align = alLeft
      ColCount = 3
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
      TabOrder = 1
      Visible = False
      OnExit = dgDataNodesExit
      OnSetEditText = dgDataNodesSetEditText
      Columns = <
        item
          Title.Caption = 'Node'
          Title.WordWrap = False
        end
        item
          LimitToList = True
          PickList.Strings = (
            'No'
            'Yes')
          Title.Caption = 'Plot'
          Title.WordWrap = False
        end
        item
          Format = cfNumber
          Title.Caption = 'Elevation'
          Title.WordWrap = False
        end>
      RowCountMin = 0
      SelectedIndex = 0
      Version = '2.0'
      ColWidths = (
        57
        48
        64)
    end
    object RichEdit1: TJvRichEdit
      Left = 401
      Top = 153
      Width = 433
      Height = 111
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 2147483632
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      WordWrap = False
    end
  end
  object ChartHydExtractor: TChart
    Left = 0
    Top = 269
    Width = 835
    Height = 272
    BackWall.Brush.Style = bsClear
    Legend.LegendStyle = lsSeries
    MarginRight = 10
    Title.Text.Strings = (
      '')
    OnGetLegendPos = ChartHydExtractorGetLegendPos
    OnGetLegendRect = ChartHydExtractorGetLegendRect
    BottomAxis.TickInnerLength = 4
    BottomAxis.TickLength = 0
    BottomAxis.Title.Caption = 'time'
    LeftAxis.TickInnerLength = 4
    LeftAxis.TickLength = 0
    LeftAxis.Title.Caption = 'head or drawdown'
    View3D = False
    OnAfterDraw = ChartHydExtractorAfterDraw
    Align = alClient
    Color = clWindow
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 0
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'text files|*.txt|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 440
    Top = 64
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'formatted head files (*.fhd)|*.fhd|formatted drawdown files (*.f' +
      'dn)|*.fdn|formatted HUF head files (*.hhd)|*.hhd|All Files|*.*'
    Left = 376
    Top = 48
  end
  object MainMenu1: TMainMenu
    Left = 416
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object ReadCellLocations1: TMenuItem
        Caption = '&Read Cell Locations'
        OnClick = ReadCellLocations1Click
      end
      object ReadHeadsorDrawdown1: TMenuItem
        Caption = 'R&ead Heads, Drawdown, or Concentration'
        GroupIndex = 2
        OnClick = btnReadClick
      end
      object SaveCellLocations1: TMenuItem
        Caption = '&Save Cell Locations'
        GroupIndex = 2
        OnClick = SaveCellLocations1Click
      end
      object SaveHeadsorDrawdown1: TMenuItem
        Caption = 'S&ave Heads, Drawdown, or Concentratin'
        GroupIndex = 2
        OnClick = btnSaveClick
      end
      object SaveasImage1: TMenuItem
        Caption = 'Save as &Image'
        Enabled = False
        GroupIndex = 2
        OnClick = sbImageClick
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
        GroupIndex = 2
        OnClick = Exit1Click
      end
    end
    object FormatChart1: TMenuItem
      Caption = 'Format &Chart'
      Enabled = False
      GroupIndex = 2
      OnClick = sbFormatClick
    end
    object Help2: TMenuItem
      Caption = '&Help'
      GroupIndex = 2
      object Help1: TMenuItem
        Caption = '&Help'
        OnClick = Help1Click
      end
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'hge'
    Filter = 'cell locations file (*.hge)|*.hge|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 480
    Top = 8
  end
  object OpenDialog2: TOpenDialog
    Filter = 'cell locations file (*.hge)|*.hge|All Files|*.*'
    Left = 352
    Top = 40
  end
  object ChartEditor1: TChartEditor
    Chart = ChartHydExtractor
    Options = [ceChange, ceTitle, ceHelp]
    Title = 'Editing Hydrograph Plot'
    GalleryHeight = 0
    GalleryWidth = 0
    Height = 0
    Width = 0
    Left = 152
    Top = 9
  end
end
