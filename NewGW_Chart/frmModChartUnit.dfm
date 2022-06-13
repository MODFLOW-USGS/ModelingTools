object frmModChart: TfrmModChart
  Left = 301
  Top = 127
  HelpContext = 100
  Caption = 'GW_Chart Calibration Plots: A Graphing Tool for Model Analysis'
  ClientHeight = 497
  ClientWidth = 704
  Color = clBtnFace
  DockSite = True
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDockDrop = FormDockDrop
  OnDockOver = Panel1DockOver
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 21
  object chartModflow: TChart
    Left = 0
    Top = 41
    Width = 472
    Height = 437
    BackWall.Brush.Style = bsClear
    Legend.ColorWidth = 50
    Legend.Font.Charset = ANSI_CHARSET
    Legend.Font.Name = 'Times New Roman'
    Legend.Shadow.HorizSize = 0
    Legend.Shadow.VertSize = 0
    Legend.Symbol.Width = 50
    MarginLeft = 5
    Title.Font.Charset = ANSI_CHARSET
    Title.Font.Color = clBlack
    Title.Font.Height = -24
    Title.Font.Name = 'Times New Roman'
    Title.Text.Strings = (
      ' ')
    OnGetLegendPos = chartModflowGetLegendPos
    OnGetLegendRect = chartModflowGetLegendRect
    BottomAxis.AxisValuesFormat = '#,##0.###,###'
    BottomAxis.ExactDateTime = False
    BottomAxis.Grid.Color = -1
    BottomAxis.Grid.Visible = False
    BottomAxis.LabelsFormat.Font.Charset = ANSI_CHARSET
    BottomAxis.LabelsFormat.Font.Height = -16
    BottomAxis.LabelsFormat.Font.Name = 'Times New Roman'
    BottomAxis.MinorTicks.Visible = False
    BottomAxis.TickInnerLength = 4
    BottomAxis.TickLength = 0
    BottomAxis.Ticks.Color = -1
    BottomAxis.TickOnLabelsOnly = False
    BottomAxis.Title.Caption = 'TEST'
    BottomAxis.Title.Font.Charset = ANSI_CHARSET
    BottomAxis.Title.Font.Height = -21
    BottomAxis.Title.Font.Name = 'Times New Roman'
    LeftAxis.Grid.Color = -1
    LeftAxis.Grid.Visible = False
    LeftAxis.LabelsFormat.Font.Charset = ANSI_CHARSET
    LeftAxis.LabelsFormat.Font.Height = -16
    LeftAxis.LabelsFormat.Font.Name = 'Times New Roman'
    LeftAxis.MinorTicks.Visible = False
    LeftAxis.TickInnerLength = 4
    LeftAxis.TickLength = 0
    LeftAxis.Ticks.Color = clBlack
    LeftAxis.Title.Caption = 'test'
    LeftAxis.Title.Font.Charset = ANSI_CHARSET
    LeftAxis.Title.Font.Height = -21
    LeftAxis.Title.Font.Name = 'Times New Roman'
    RightAxis.Grid.Visible = False
    RightAxis.LabelsFormat.Font.Charset = ANSI_CHARSET
    RightAxis.LabelsFormat.Font.Height = -16
    RightAxis.LabelsFormat.Font.Name = 'Times New Roman'
    RightAxis.MinorTicks.Visible = False
    RightAxis.TickInnerLength = 4
    RightAxis.TickLength = 0
    RightAxis.Title.Font.Charset = ANSI_CHARSET
    RightAxis.Title.Font.Height = -21
    RightAxis.Title.Font.Name = 'Times New Roman'
    TopAxis.Grid.Visible = False
    TopAxis.TickInnerLength = 4
    View3D = False
    Zoom.Allow = False
    OnAfterDraw = chartModflowAfterDraw
    OnGetAxisLabel = chartModflowGetAxisLabel
    OnGetNextAxisLabel = chartModflowGetNextAxisLabel
    Align = alClient
    Color = clWindow
    TabOrder = 1
    OnClick = chartModflowClick
    OnMouseDown = chartModflowMouseDown
    OnMouseMove = chartModflowMouseMove
    OnMouseUp = chartModflowMouseUp
    ExplicitLeft = 2
    ExplicitTop = 35
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 0
    object SerDataPoints: TPointSeries
      HoverElement = [heCurrent]
      Legend.Visible = False
      OnGetPointerStyle = SerDataPointsGetPointerStyle
      Marks.Visible = True
      Marks.Callout.Length = 8
      ShowInLegend = False
      Title = 'data points'
      OnGetMarkText = SerDataPointsGetMarkText
      ClickableLine = False
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      OnClickPointer = SerDataPointsClickPointer
    end
    object ser1to1: TLineSeries
      HoverElement = [heCurrent]
      Legend.Visible = False
      SeriesColor = clBlack
      ShowInLegend = False
      Title = '1 to 1 line'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object serBarChart: TBarSeries
      HoverElement = []
      Legend.Visible = False
      Marks.Visible = False
      SeriesColor = clYellow
      ShowInLegend = False
      Title = 'Bar chart data'
      MultiBar = mbNone
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object LineAt1: TLineSeries
      HoverElement = [heCurrent]
      Legend.Visible = False
      ShowInLegend = False
      Title = 'LineAt1'
      Brush.BackColor = clDefault
      Pointer.InflateMargins = True
      Pointer.Style = psRectangle
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
    end
    object serBubbleLegend: TBubbleSeries
      HoverElement = [heCurrent]
      Active = False
      Marks.Frame.Visible = False
      SeriesColor = clWhite
      Title = 'Bubble Legend'
      ClickableLine = False
      Pointer.HorizSize = 16
      Pointer.InflateMargins = False
      Pointer.Style = psCircle
      Pointer.VertSize = 16
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Y'
      YValues.Order = loNone
      RadiusValues.Name = 'Radius'
      RadiusValues.Order = loNone
    end
    object serBarChart2: TBarSeries
      HoverElement = []
      Legend.Visible = False
      Active = False
      SeriesColor = clGray
      ShowInLegend = False
      Title = 'SeriesBar2'
      MultiBar = mbNone
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Bar'
      YValues.Order = loNone
    end
    object TMarksTipTool
      Format.CustomPosition = True
      Format.Left = 0
      Format.TextAlignment = taCenter
      Format.Top = 0
      Format.Visible = False
      MouseAction = mtmClick
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 478
    Width = 704
    Height = 19
    Panels = <
      item
        Width = 350
      end
      item
        Width = 50
      end>
    OnMouseMove = ToolBar1MouseMove
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 704
    Height = 41
    ButtonHeight = 29
    Caption = 'Toolbar'
    DragKind = dkDock
    DragMode = dmAutomatic
    TabOrder = 0
    OnEndDock = ToolBar1EndDock
    OnMouseMove = ToolBar1MouseMove
    DesignSize = (
      704
      41)
    object sbZoomIn: TSpeedButton
      Left = 0
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Zoom In'
      AllowAllUp = True
      GroupIndex = 1
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33033333333333333F7F3333333333333000333333333333F777333333333333
        000333333333333F777333333333333000333333333333F77733333333333300
        033333333FFF3F777333333700073B703333333F7773F77733333307777700B3
        33333377333777733333307F8F8F7033333337F333F337F3333377F8F9F8F773
        3333373337F3373F3333078F898F870333337F33F7FFF37F333307F99999F703
        33337F377777337F3333078F898F8703333373F337F33373333377F8F9F8F773
        333337F3373337F33333307F8F8F70333333373FF333F7333333330777770333
        333333773FF77333333333370007333333333333777333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbZoomInClick
      OnMouseMove = ToolBar1MouseMove
    end
    object sbZoomOut: TSpeedButton
      Left = 23
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Zoom Out'
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33033333333333333F7F3333333333333000333333333333F777333333333333
        000333333333333F777333333333333000333333333333F77733333333333300
        033333333FFF3F777333333700073B703333333F7773F77733333307777700B3
        333333773337777333333078F8F87033333337F3333337F33333778F8F8F8773
        333337333333373F333307F8F8F8F70333337F33FFFFF37F3333078999998703
        33337F377777337F333307F8F8F8F703333373F3333333733333778F8F8F8773
        333337F3333337F333333078F8F870333333373FF333F7333333330777770333
        333333773FF77333333333370007333333333333777333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbZoomOutClick
      OnMouseMove = ToolBar1MouseMove
    end
    object sbPan: TSpeedButton
      Left = 46
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Pan'
      AllowAllUp = True
      GroupIndex = 1
      Enabled = False
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00DDDDDDDDDDDD
        DDDDDDDDD44444444DDDDDDDD0FFFFFF0DDDDDDD0FFFFFFFF0DDDDD0FFFFFFFF
        F0DDDDD0FFFFFFFFF0DDDD0FF0FFFFFFF0DDD0FF00F0F0F0F0DDD000D0F0F0F0
        F0DDDDDDD0F0F0F0F0DDDDDDD0F0F0F0F0DDDDDDD0F0F0F00DDDDDDDD0F0F0F0
        DDDDDDDDDD00F00DDDDDDDDDDDD00DDDDDDDDDDDDDDDDDDDDDDD}
      ParentShowHint = False
      ShowHint = True
      OnClick = sbPanClick
      OnMouseMove = ToolBar1MouseMove
    end
    object cbLabelPoints: TCheckBox
      Left = 69
      Top = 0
      Width = 116
      Height = 29
      Hint = 'If checked, points will be labelled when you click on them.'
      Caption = 'Label Points'
      Checked = True
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbLabelPointsClick
      OnMouseMove = ToolBar1MouseMove
    end
    object pnlFile: TPanel
      Left = 185
      Top = 0
      Width = 49
      Height = 29
      BevelOuter = bvNone
      Caption = 'File:'
      TabOrder = 1
      OnMouseMove = ToolBar1MouseMove
    end
    object treecomboFileNames: TksoTreeComboBox
      Left = 234
      Top = 0
      Width = 128
      Height = 29
      BorderStyle = kbsSunken
      BorderStyleFlat = kbsNone
      BorderStyleFocused = kbsNone
      DropDownHeight = 270
      Flat = False
      AutoExpand = False
      ChangeDelay = 0
      HideSelection = True
      HotTrack = False
      Indent = 23
      ReadOnly = False
      RightClickSelect = False
      RowSelect = False
      ShowButtons = True
      ShowLines = True
      ShowRoot = True
      SortType = stNone
      ToolTips = True
      Enabled = False
      ParentColor = False
      TabOrder = 2
      TabStop = True
      OnChange = treecomboFileNamesChange
    end
    object sbOpenFile: TSpeedButton
      Left = 362
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Open File'
      Anchors = [akTop, akRight]
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0033333333B333
        333B33FF33337F3333F73BB3777BB7777BB3377FFFF77FFFF77333B000000000
        0B3333777777777777333330FFFFFFFF07333337F33333337F333330FFFFFFFF
        07333337F3FF3FFF7F333330F00F000F07333337F77377737F333330FFFFFFFF
        07333FF7F3FFFF3F7FFFBBB0F0000F0F0BB37777F7777373777F3BB0FFFFFFFF
        0BBB3777F3FF3FFF77773330F00F000003333337F773777773333330FFFF0FF0
        33333337F3FF7F37F3333330F08F0F0B33333337F7737F77FF333330FFFF003B
        B3333337FFFF77377FF333B000000333BB33337777777F3377FF3BB3333BB333
        3BB33773333773333773B333333B3333333B7333333733333337}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = Button1Click
      OnMouseMove = ToolBar1MouseMove
    end
    object sbRefresh: TSpeedButton
      Left = 385
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Refresh'
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333FFFFF3333333333999993333333333F77777FFF333333999999999
        3333333777333777FF33339993707399933333773337F3777FF3399933000339
        9933377333777F3377F3399333707333993337733337333337FF993333333333
        399377F33333F333377F993333303333399377F33337FF333373993333707333
        333377F333777F333333993333101333333377F333777F3FFFFF993333000399
        999377FF33777F77777F3993330003399993373FF3777F37777F399933000333
        99933773FF777F3F777F339993707399999333773F373F77777F333999999999
        3393333777333777337333333999993333333333377777333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbRefreshClick
    end
    object sbPrint: TSpeedButton
      Left = 408
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Print'
      Anchors = [akTop, akRight]
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
        0003377777777777777308888888888888807F33333333333337088888888888
        88807FFFFFFFFFFFFFF7000000000000000077777777777777770F8F8F8F8F8F
        8F807F333333333333F708F8F8F8F8F8F9F07F333333333337370F8F8F8F8F8F
        8F807FFFFFFFFFFFFFF7000000000000000077777777777777773330FFFFFFFF
        03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
        03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
        33333337F3F37F3733333330F08F0F0333333337F7337F7333333330FFFF0033
        33333337FFFF7733333333300000033333333337777773333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = miPrintClick
      OnMouseMove = ToolBar1MouseMove
    end
    object sbFormat: TSpeedButton
      Left = 431
      Top = 0
      Width = 23
      Height = 29
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
    object sbOldFormatChart: TSpeedButton
      Left = 454
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Format Chart (old style)'
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
      OnClick = sbOldFormatChartClick
      OnMouseMove = ToolBar1MouseMove
    end
    object sbSaveImage: TSpeedButton
      Left = 477
      Top = 0
      Width = 23
      Height = 29
      Hint = 'Save Image'
      Anchors = [akTop, akRight]
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
      OnClick = sbSaveImageClick
      OnMouseMove = ToolBar1MouseMove
    end
    object BitBtnClose: TBitBtn
      Left = 500
      Top = 0
      Width = 75
      Height = 29
      Hint = 'Close Program'
      Anchors = [akTop, akRight]
      Caption = '&Close'
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00388888888877
        F7F787F8888888888333333F00004444400888FFF444448888888888F333FF8F
        000033334D5007FFF4333388888888883338888F0000333345D50FFFF4333333
        338F888F3338F33F000033334D5D0FFFF43333333388788F3338F33F00003333
        45D50FEFE4333333338F878F3338F33F000033334D5D0FFFF43333333388788F
        3338F33F0000333345D50FEFE4333333338F878F3338F33F000033334D5D0FFF
        F43333333388788F3338F33F0000333345D50FEFE4333333338F878F3338F33F
        000033334D5D0EFEF43333333388788F3338F33F0000333345D50FEFE4333333
        338F878F3338F33F000033334D5D0EFEF43333333388788F3338F33F00003333
        4444444444333333338F8F8FFFF8F33F00003333333333333333333333888888
        8888333F00003333330000003333333333333FFFFFF3333F00003333330AAAA0
        333333333333888888F3333F00003333330000003333333333338FFFF8F3333F
        0000}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = BitBtnCloseClick
      OnMouseMove = ToolBar1MouseMove
    end
  end
  object jvplChartControls: TJvPageList
    Left = 472
    Top = 41
    Width = 232
    Height = 437
    ActivePage = jvsp_nm
    PropagateEnable = False
    Align = alRight
    Visible = False
    object jvspChartType: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      object pcPPR: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabGroups
        Align = alClient
        TabOrder = 0
        object tabControls: TTabSheet
          Caption = 'Controls'
          DesignSize = (
            224
            401)
          object lblN_PPR: TLabel
            Left = 72
            Top = 364
            Width = 151
            Height = 21
            Caption = 'N (number of items)'
          end
          object rbPlotSeries: TRadioButton95
            Left = 0
            Top = 128
            Width = 217
            Height = 73
            Alignment = taLeftJustify
            Caption = 'Plot a separate series for each observation or prediction'
            TabOrder = 1
            WordWrap = True
            OnClick = rbPlotSeriesClick
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
          object rbPlotAverage: TRadioButton95
            Left = 0
            Top = 8
            Width = 225
            Height = 113
            Alignment = taLeftJustify
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Plot average values for each parameter'
            Checked = True
            TabOrder = 0
            TabStop = True
            WordWrap = True
            OnClick = rbPlotAverageClick
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
          object rgOrderPPR: TRadioGroup
            Left = 6
            Top = 208
            Width = 218
            Height = 65
            Caption = 'Order of data points in plot'
            ItemIndex = 1
            Items.Strings = (
              'Original order'
              'Largest first')
            TabOrder = 2
            OnClick = RedrawChart
          end
          object rgItemsToPlotPPR: TRadioGroup
            Left = 7
            Top = 272
            Width = 217
            Height = 81
            Caption = 'Items To Plot'
            ItemIndex = 0
            Items.Strings = (
              'All'
              'First N items'
              'Last N items')
            TabOrder = 3
            OnClick = rgItemsToPlotPPRClick
          end
          object seN_PPR: TJvSpinEdit
            Left = 8
            Top = 360
            Width = 57
            Height = 29
            ButtonKind = bkClassic
            MaxValue = 1000000.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Enabled = False
            TabOrder = 4
            OnChange = spNChange
          end
        end
        object tabGroups: TTabSheet
          Caption = 'Groups'
          ImageIndex = 1
          object treeGroups1: TTreeView
            Left = 0
            Top = 0
            Width = 224
            Height = 401
            Align = alClient
            Indent = 23
            TabOrder = 0
          end
        end
      end
    end
    object jvspDataOrder: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      object pc_NM: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabSeries
        Align = alClient
        TabOrder = 0
        object tabDataOrderControls: TTabSheet
          Caption = 'Controls'
          object lblN: TLabel
            Left = 72
            Top = 332
            Width = 151
            Height = 21
            Caption = 'N (number of items)'
          end
          object lblDataOrder: TLabel
            Left = 8
            Top = 8
            Width = 126
            Height = 42
            Caption = 'Order of data points and series'
            WordWrap = True
          end
          object spN: TJvSpinEdit
            Left = 8
            Top = 328
            Width = 57
            Height = 29
            ButtonKind = bkClassic
            MaxValue = 1000000.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Enabled = False
            TabOrder = 4
            OnChange = spNChange
          end
          object rgItemsToPlot: TRadioGroup
            Left = 7
            Top = 240
            Width = 217
            Height = 81
            Caption = 'Items To Plot'
            ItemIndex = 0
            Items.Strings = (
              'All'
              'First N items'
              'Last N items')
            TabOrder = 3
            OnClick = rgItemsToPlotClick
          end
          object rgPointOrder: TRadioGroup
            Left = 6
            Top = 176
            Width = 218
            Height = 65
            Caption = 'Order of data points in plot'
            ItemIndex = 1
            Items.Strings = (
              'Original order'
              'Largest first')
            TabOrder = 2
            OnClick = RedrawChart
          end
          object rgOrder: TRadioGroup
            Left = 6
            Top = 112
            Width = 218
            Height = 65
            Caption = 'Order of series in legend'
            ItemIndex = 1
            Items.Strings = (
              'Original order'
              'Order from *._sc file')
            TabOrder = 1
            OnClick = RedrawChart
          end
          object rgWhatToPlot: TRadioGroup
            Left = 6
            Top = 48
            Width = 218
            Height = 65
            Caption = 'What to plot on X-axis'
            ItemIndex = 0
            Items.Strings = (
              'Observations'
              'Parameters')
            TabOrder = 0
            OnClick = rgWhatToPlotClick
          end
        end
        object tabSeries: TTabSheet
          Caption = 'Series'
          ImageIndex = 1
          object lblNumberOfSeries: TLabel
            Left = 72
            Top = 123
            Width = 127
            Height = 21
            Caption = 'Number of Series'
          end
          object rgSeriesToPlot: TRadioGroup
            Left = 0
            Top = 8
            Width = 217
            Height = 105
            Caption = 'Series to Plot'
            ItemIndex = 1
            Items.Strings = (
              'All'
              'Largest'
              'Selected')
            TabOrder = 0
            OnClick = rgSeriesToPlotClick
          end
          object seNumberOfSeries: TJvSpinEdit
            Left = 0
            Top = 120
            Width = 65
            Height = 29
            ButtonKind = bkClassic
            MaxValue = 10000000000.000000000000000000
            MinValue = 1.000000000000000000
            Value = 15.000000000000000000
            TabOrder = 1
            OnChange = DelayRedrawChart
          end
          object clbSeriesToPlot: TCheckListBox
            Left = 0
            Top = 163
            Width = 224
            Height = 238
            OnClickCheck = clbSeriesToPlotClickCheck
            Align = alBottom
            Anchors = [akLeft, akTop, akRight, akBottom]
            Enabled = False
            ItemHeight = 21
            TabOrder = 2
          end
        end
      end
    end
    object jvsp_b_pa: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_b_pa'
      object cbDivideParameterValues: TCheckBox95
        Left = 16
        Top = 16
        Width = 209
        Height = 49
        Alignment = taLeftJustify
        Caption = 'Divide parameter values by their initial values'
        Checked = True
        State = cbChecked
        TabOrder = 0
        WordWrap = True
        OnClick = adeStandardErrorChange
        AlignmentBtn = taLeftJustify
        LikePushButton = False
        VerticalAlignment = vaTop
      end
      object cbShowAllParameters: TCheckBox95
        Left = 16
        Top = 72
        Width = 209
        Height = 33
        Alignment = taLeftJustify
        Caption = 'Show All Parameters'
        TabOrder = 1
        WordWrap = False
        OnClick = adeStandardErrorChange
        AlignmentBtn = taLeftJustify
        LikePushButton = False
        VerticalAlignment = vaTop
      end
    end
    object jvsp_os: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_os'
      DesignSize = (
        232
        437)
      object Label1: TLabel
        Left = 8
        Top = 80
        Width = 142
        Height = 21
        Caption = 'PlotSymbols to plot'
      end
      object lblStandardErrorOfRegression: TLabel
        Left = 8
        Top = 182
        Width = 214
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Standard Error of Regression'
        ExplicitTop = 202
      end
      object lblIgnoreValueOS: TLabel
        Left = 8
        Top = 292
        Width = 112
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Value to ignore'
        ExplicitTop = 312
      end
      object cbRecentModflow: TCheckBox95
        Left = 8
        Top = 8
        Width = 217
        Height = 73
        Alignment = taLeftJustify
        Caption = '._os file created by MODFLOW-2000 version 1.12 or later'
        Checked = True
        State = cbChecked
        TabOrder = 0
        WordWrap = True
        OnClick = adeStandardErrorChange
        AlignmentBtn = taLeftJustify
        LikePushButton = False
        VerticalAlignment = vaTop
      end
      object clb_OsPlotsymbols: TCheckListBox
        Left = 8
        Top = 104
        Width = 217
        Height = 69
        OnClickCheck = clb_OsPlotsymbolsClickCheck
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 21
        TabOrder = 1
      end
      object adeStandardError: TArgusDataEntry
        Left = 8
        Top = 206
        Width = 209
        Height = 22
        Anchors = [akLeft, akBottom]
        TabOrder = 2
        Text = '0'
        OnChange = adeStandardErrorChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object btnRead: TButton
        Left = 8
        Top = 238
        Width = 209
        Height = 47
        Anchors = [akLeft, akBottom]
        Caption = 'Read from "global" or "list" file'
        TabOrder = 3
        WordWrap = True
        OnClick = btnReadClick
      end
      object rdgIgnoreValueOS: TRbwDataGrid4
        Left = 8
        Top = 324
        Width = 216
        Height = 64
        Anchors = [akLeft, akBottom]
        ColCount = 1
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 5
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
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'MS Sans Serif'
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
            AutoAdjustColWidths = False
          end>
        OnEndUpdate = rdgIgnoreValueOSEndUpdate
        WordWrapRowCaptions = False
        ColWidths = (
          184)
      end
      object seIgnoreValueOS: TJvSpinEdit
        Left = 144
        Top = 292
        Width = 65
        Height = 29
        MaxValue = 2147483647.000000000000000000
        Anchors = [akLeft, akBottom]
        TabOrder = 4
        OnChange = seIgnoreValueOSChange
      end
    end
    object jvsp_pc: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_pc'
      object pgc_PC: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabWhatToPlot
        Align = alClient
        TabOrder = 0
        object tabWhatToPlot: TTabSheet
          Caption = 'Plot choice'
          object gbWhatToPlotPC: TGroupBox
            Left = 7
            Top = 0
            Width = 217
            Height = 337
            Caption = 'What to plot'
            TabOrder = 0
            object rbMeanCiRangeForPC: TRadioButton95
              Left = 2
              Top = 16
              Width = 209
              Height = 65
              Alignment = taLeftJustify
              Caption = 'Mean, Confidence intervals, and reasonable range'
              Checked = True
              TabOrder = 0
              TabStop = True
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rbCoefVarReg: TRadioButton95
              Left = 2
              Top = 120
              Width = 201
              Height = 49
              Alignment = taLeftJustify
              Caption = 'Coefficients of variation for regression'
              TabOrder = 2
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rbCoerVarPar: TRadioButton95
              Left = 2
              Top = 168
              Width = 207
              Height = 49
              Alignment = taLeftJustify
              Caption = 'Coefficients of variation for native parameters'
              TabOrder = 3
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rbMeanCiForPC: TRadioButton95
              Left = 2
              Top = 80
              Width = 207
              Height = 41
              Alignment = taLeftJustify
              Caption = 'Mean and Confidence intervals'
              TabOrder = 1
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rbInverseCoef: TRadioButton95
              Left = 2
              Top = 216
              Width = 207
              Height = 49
              Alignment = taLeftJustify
              Caption = 'Parameter value/ standard deviation'
              TabOrder = 4
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rb_b_sd_s: TRadioButton95
              Left = 2
              Top = 264
              Width = 207
              Height = 73
              Alignment = taLeftJustify
              Caption = 'Parameter value/ standard deviation * standard error'
              TabOrder = 5
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
          end
          object cbScaleValues: TCheckBox95
            Left = 7
            Top = 344
            Width = 217
            Height = 41
            Alignment = taLeftJustify
            Caption = 'Scale all values by the estimated value'
            Checked = True
            State = cbChecked
            TabOrder = 1
            WordWrap = True
            OnClick = RedrawChart
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
        end
        object tabInfo: TTabSheet
          Caption = 'Information'
          ImageIndex = 1
          object lblPcExpl1: TLabel
            Left = 8
            Top = 0
            Width = 141
            Height = 42
            Caption = 'Gray bars are reasonable ranges '
            WordWrap = True
          end
          object lblPcExpl2: TLabel
            Left = 1
            Top = 48
            Width = 192
            Height = 42
            Caption = 'Black lines are calculated confidence intervals '
            WordWrap = True
          end
        end
      end
    end
    object jvsp_rdrg: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_rdrg'
      object cbPlot_NmFile: TCheckBox95
        Left = 8
        Top = 16
        Width = 209
        Height = 25
        Alignment = taLeftJustify
        Caption = 'Plot *._nm File'
        TabOrder = 0
        WordWrap = False
        OnClick = RedrawChart
        AlignmentBtn = taLeftJustify
        LikePushButton = False
        VerticalAlignment = vaTop
      end
      object clbSeriesList: TCheckListBox
        Left = 8
        Top = 48
        Width = 209
        Height = 113
        OnClickCheck = clbSeriesListClickCheck
        ItemHeight = 21
        TabOrder = 1
      end
    end
    object jvsp_linp: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_linp'
      object pcLinp: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabLinpControls
        Align = alClient
        TabOrder = 0
        object tabLinpControls: TTabSheet
          Caption = 'Controls'
          object cbScale_linp: TCheckBox95
            Left = 7
            Top = 0
            Width = 217
            Height = 49
            Alignment = taLeftJustify
            Caption = 'Scale all values by the predicted value'
            Checked = True
            State = cbChecked
            TabOrder = 0
            WordWrap = True
            OnClick = RedrawChart
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
          object rgIntervalType: TRadioGroup
            Left = 7
            Top = 40
            Width = 217
            Height = 65
            ItemIndex = 0
            Items.Strings = (
              'Confidence intervals'
              'Prediction intervals')
            TabOrder = 1
            OnClick = RedrawChart
          end
          object GroupBox1: TGroupBox
            Left = 7
            Top = 104
            Width = 217
            Height = 161
            TabOrder = 2
            object rbIndividual: TRadioButton95
              Left = 8
              Top = 24
              Width = 201
              Height = 25
              Alignment = taLeftJustify
              Caption = 'Individual 95% intervals'
              Checked = True
              TabOrder = 0
              TabStop = True
              WordWrap = False
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rbSimulataneous: TRadioButton95
              Left = 8
              Top = 48
              Width = 201
              Height = 41
              Alignment = taLeftJustify
              Caption = 'Simultaneous 95% intervals'
              TabOrder = 1
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
            object rbUndefinedSimulataneous: TRadioButton95
              Left = 8
              Top = 88
              Width = 201
              Height = 65
              Alignment = taLeftJustify
              Caption = 'Undefined number of simultaneous 95% intervals'
              TabOrder = 2
              WordWrap = True
              OnClick = RedrawChart
              AlignmentBtn = taLeftJustify
              LikePushButton = False
              VerticalAlignment = vaTop
            end
          end
          object cbLinpConfidenceIntervals: TCheckBox
            Left = 8
            Top = 272
            Width = 209
            Height = 49
            Caption = 'Plot Confidence Intervals (black lines)'
            Checked = True
            State = cbChecked
            TabOrder = 3
            WordWrap = True
            OnClick = cbLinpConfidenceIntervalsClick
          end
          object cbLinpPlotStandardDev: TCheckBox
            Left = 8
            Top = 320
            Width = 201
            Height = 49
            Caption = 'Plot Standard Deviation (gray bars)'
            Checked = True
            State = cbChecked
            TabOrder = 4
            WordWrap = True
            OnClick = cbLinpPlotStandardDevClick
          end
        end
        object tabLinpPlotSymbols: TTabSheet
          Caption = 'Plot Symbols'
          ImageIndex = 1
          object clbLinpPlotSymbol: TCheckListBox
            Left = 0
            Top = 0
            Width = 224
            Height = 401
            OnClickCheck = clbLinpPlotSymbolClickCheck
            Align = alClient
            Columns = 1
            ItemHeight = 21
            TabOrder = 0
          end
        end
      end
    end
    object jvsp_intconf: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_intconf'
      object pcIntConf: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabIntconfControls
        Align = alClient
        TabOrder = 0
        object tabIntconfControls: TTabSheet
          Caption = 'Controls'
          object lblAdditionalFiles: TLabel
            Left = 8
            Top = 168
            Width = 109
            Height = 21
            Caption = 'Additional files'
          end
          object cbScale_intconf: TCheckBox95
            Left = 7
            Top = 0
            Width = 217
            Height = 49
            Alignment = taLeftJustify
            Caption = 'Scale all values by the predicted value'
            TabOrder = 0
            WordWrap = True
            OnClick = RedrawChart
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
          object rgIntConfWhatToPlot: TRadioGroup
            Left = 8
            Top = 49
            Width = 209
            Height = 105
            Caption = 'What to plot'
            ItemIndex = 2
            Items.Strings = (
              'Parameters'
              'Predictions'
              'Both')
            TabOrder = 1
            OnClick = RedrawChart
          end
          object reIntconfFiles: TRichEdit
            Left = 8
            Top = 200
            Width = 209
            Height = 121
            Lines.Strings = (
              '')
            TabOrder = 3
            WordWrap = False
            OnExit = RedrawChart
          end
          object btnIntConfBrowse: TButton
            Left = 144
            Top = 168
            Width = 75
            Height = 25
            Caption = 'Browse'
            TabOrder = 2
            OnClick = btnIntConfBrowseClick
          end
        end
        object tabWarnings: TTabSheet
          Caption = 'Warnings'
          ImageIndex = 1
          object lblRedCI: TLabel
            Left = 8
            Top = 108
            Width = 207
            Height = 126
            Caption = 
              'Red confidence interval - indicates either lower interval is abo' +
              've predicted value or upper interval is below it'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object lblOrangeCI: TLabel
            Left = 8
            Top = 8
            Width = 213
            Height = 84
            Caption = 
              'Orange confidence interval - indicates at least one of the inter' +
              'vals did not converge'
            Font.Charset = ANSI_CHARSET
            Font.Color = clBlack
            Font.Height = -17
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
        end
      end
    end
    object jvsp_xyztwr: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_xyztwr'
      object pcXyzt: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabOptions
        Align = alClient
        TabOrder = 0
        object tabDataControls: TTabSheet
          Caption = 'Data'
          object lblBlueRedDescripton: TLabel
            Left = 8
            Top = 336
            Width = 178
            Height = 42
            Caption = 'Blue - Positive Residual'#13#10'Red - Negative Residual'
          end
          object rgXAxis: TRadioGroup
            Left = 8
            Top = 0
            Width = 209
            Height = 129
            Caption = 'X Axis'
            ItemIndex = 0
            Items.Strings = (
              'X'
              'Y'
              'Z'
              'Time'
              'Weighted Residuals'
              'Residuals')
            TabOrder = 0
            OnClick = rgXAxisClick
          end
          object rgYAxis: TRadioGroup
            Left = 8
            Top = 128
            Width = 209
            Height = 129
            Caption = 'Y Axis'
            ItemIndex = 1
            Items.Strings = (
              'X'
              'Y'
              'Z'
              'Time'
              'Weighted Residuals'
              'Residuals')
            TabOrder = 1
            OnClick = rgYAxisClick
          end
          object rgDataToPlot: TRadioGroup
            Left = 8
            Top = 264
            Width = 209
            Height = 65
            Caption = 'Data to Plot'
            ItemIndex = 0
            Items.Strings = (
              'Weighted Residuals'
              'Residuals')
            TabOrder = 2
            OnClick = RedrawChart
          end
        end
        object tabOptions: TTabSheet
          Caption = 'Options'
          ImageIndex = 1
          DesignSize = (
            224
            401)
          object lblPlotSymbolsToPlot: TLabel
            Left = 8
            Top = 0
            Width = 147
            Height = 21
            Caption = 'Plot Symbols to Plot'
          end
          object lblRadiusFactor: TLabel
            Left = 112
            Top = 112
            Width = 102
            Height = 21
            Anchors = [akLeft, akBottom]
            Caption = 'Radius Factor'
            ExplicitTop = 132
          end
          object clbItemsToPlot: TCheckListBox
            Left = 8
            Top = 24
            Width = 209
            Height = 77
            Anchors = [akLeft, akTop, akBottom]
            ItemHeight = 21
            TabOrder = 0
            OnClick = clbItemsToPlotClick
          end
          object seRadius: TJvSpinEdit
            Left = 8
            Top = 108
            Width = 97
            Height = 29
            CheckMaxValue = False
            ButtonKind = bkClassic
            Decimal = 7
            Increment = 0.010000000000000000
            MinValue = 0.000100000000000000
            ValueType = vtFloat
            Value = 0.050000000000000000
            Anchors = [akLeft, akBottom]
            TabOrder = 1
            OnChange = RedrawChart
          end
          object cbShowLabels: TCheckBox
            Left = 8
            Top = 148
            Width = 161
            Height = 17
            Anchors = [akLeft, akBottom]
            Caption = 'Show Labels'
            TabOrder = 2
            OnClick = RedrawChart
          end
          object cbEarliestTimePlotted: TCheckBox95
            Left = 8
            Top = 172
            Width = 145
            Height = 49
            Alignment = taLeftJustify
            Anchors = [akLeft, akBottom]
            Caption = 'Limit Earliest Time Plotted'
            TabOrder = 3
            WordWrap = True
            OnClick = cbEarliestTimePlottedClick
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
          object cbLatestTimePlotted: TCheckBox95
            Left = 8
            Top = 260
            Width = 145
            Height = 49
            Alignment = taLeftJustify
            Anchors = [akLeft, akBottom]
            Caption = 'Limit Latest Time Plotted'
            TabOrder = 5
            WordWrap = True
            OnClick = cbLatestTimePlottedClick
            AlignmentBtn = taLeftJustify
            LikePushButton = False
            VerticalAlignment = vaTop
          end
          object adeEarliest: TArgusDataEntry
            Left = 8
            Top = 220
            Width = 145
            Height = 22
            Anchors = [akLeft, akBottom]
            Color = clBtnFace
            Enabled = False
            TabOrder = 4
            Text = '0'
            OnChange = DelayRedrawChart
            DataType = dtReal
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
          object adeLatest: TArgusDataEntry
            Left = 8
            Top = 308
            Width = 145
            Height = 22
            Anchors = [akLeft, akBottom]
            Color = clBtnFace
            Enabled = False
            TabOrder = 6
            Text = '0'
            OnChange = DelayRedrawChart
            DataType = dtReal
            Max = 1.000000000000000000
            ChangeDisabledColor = True
          end
        end
        object tabLegend: TTabSheet
          Caption = 'Legend'
          ImageIndex = 2
          DesignSize = (
            224
            401)
          object lblXyztLegendCount: TLabel
            Left = 0
            Top = 278
            Width = 195
            Height = 21
            Anchors = [akLeft, akBottom]
            Caption = 'Number of items in legend'
            ExplicitTop = 298
          end
          object rdgXyztLegend: TRbwDataGrid4
            Left = 0
            Top = 32
            Width = 224
            Height = 237
            Anchors = [akLeft, akTop, akRight, akBottom]
            Color = clBtnFace
            ColCount = 1
            Enabled = False
            FixedCols = 0
            RowCount = 3
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
            TabOrder = 1
            OnSetEditText = rdgXyztLegendSetEditText
            ExtendedAutoDistributeText = False
            AutoMultiEdit = False
            AutoDistributeText = True
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = True
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
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
                ButtonFont.Name = 'MS Sans Serif'
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
            OnEndUpdate = rdgXyztLegendEndUpdate
            WordWrapRowCaptions = False
          end
          object cbXyztLegend: TCheckBox
            Left = 0
            Top = 0
            Width = 153
            Height = 25
            Caption = 'Show Legend'
            TabOrder = 0
            OnClick = cbXyztLegendClick
          end
          object seXyztLegendCount: TJvSpinEdit
            Left = 0
            Top = 310
            Width = 121
            Height = 29
            Value = 2.000000000000000000
            Enabled = False
            Anchors = [akLeft, akBottom]
            TabOrder = 2
            OnChange = seXyztLegendCountChange
          end
        end
      end
    end
    object jvspScSo: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvspScSo'
      object lblN_so: TLabel
        Left = 72
        Top = 212
        Width = 151
        Height = 21
        Caption = 'N (number of items)'
      end
      object rgScSoSorting: TRadioGroup
        Left = 8
        Top = 8
        Width = 209
        Height = 105
        Caption = 'Sorting'
        ItemIndex = 1
        Items.Strings = (
          'Use original order'
          'Largest values first')
        TabOrder = 0
        OnClick = sbRefreshClick
      end
      object rgItemsToPlot_So: TRadioGroup
        Left = 8
        Top = 120
        Width = 217
        Height = 81
        Caption = 'Items To Plot'
        ItemIndex = 0
        Items.Strings = (
          'All'
          'First N items'
          'Last N items')
        TabOrder = 1
        OnClick = rgItemsToPlot_SoClick
      end
      object seN_so: TJvSpinEdit
        Left = 8
        Top = 208
        Width = 57
        Height = 29
        ButtonKind = bkClassic
        MaxValue = 1000000.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 2
        OnChange = spNChange
      end
      object cbRatio: TCheckBox
        Left = 16
        Top = 248
        Width = 169
        Height = 25
        Caption = 'Ratio to largest'
        TabOrder = 3
        OnClick = cbRatioClick
      end
    end
    object jvspPcc: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvspPcc'
      object lblDescription: TLabel
        Left = 8
        Top = 8
        Width = 187
        Height = 42
        Caption = 'Positive values are red; Negative values are blue.'
        WordWrap = True
      end
    end
    object jvspPpaAbschg: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvspPpaAbschg'
      object pgcParameters: TPageControl
        Left = 0
        Top = 0
        Width = 232
        Height = 437
        ActivePage = tabParameters
        Align = alClient
        TabOrder = 0
        object tsBasic: TTabSheet
          Caption = 'Basic'
          object lblN_AbsChg: TLabel
            Left = 64
            Top = 356
            Width = 151
            Height = 21
            Caption = 'N (number of items)'
          end
          object treeGroups2: TTreeView
            Left = 0
            Top = 16
            Width = 217
            Height = 169
            Indent = 23
            TabOrder = 0
          end
          object rgOrder2: TRadioGroup
            Left = 0
            Top = 192
            Width = 218
            Height = 65
            Caption = 'Order of data points in plot'
            ItemIndex = 1
            Items.Strings = (
              'Original order'
              'Largest first')
            TabOrder = 1
            OnClick = RedrawChart
          end
          object rgItemsToPlotAbsChg: TRadioGroup
            Left = 0
            Top = 264
            Width = 217
            Height = 81
            Caption = 'Items To Plot'
            ItemIndex = 0
            Items.Strings = (
              'All'
              'First N items'
              'Last N items')
            TabOrder = 2
            OnClick = rgItemsToPlotAbsChgClick
          end
          object seN_AbsChg: TJvSpinEdit
            Left = 0
            Top = 352
            Width = 57
            Height = 29
            ButtonKind = bkClassic
            MaxValue = 1000000.000000000000000000
            MinValue = 1.000000000000000000
            Value = 1.000000000000000000
            Enabled = False
            TabOrder = 3
            OnChange = spNChange
          end
        end
        object tabParameters: TTabSheet
          Caption = 'Parameters'
          ImageIndex = 1
          object jvchklstParameters: TJvCheckListBox
            Left = 0
            Top = 0
            Width = 217
            Height = 377
            OnClickCheck = jvchklstParametersClickCheck
            DoubleBuffered = False
            ItemHeight = 21
            ParentDoubleBuffered = False
            TabOrder = 0
          end
        end
      end
    end
    object jvspRc: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvspRc'
      object lblN_Rc: TLabel
        Left = 72
        Top = 108
        Width = 151
        Height = 21
        Caption = 'N (number of items)'
      end
      object rgItemsToPlot_Rc: TRadioGroup
        Left = 8
        Top = 16
        Width = 217
        Height = 81
        Caption = 'Items To Plot'
        ItemIndex = 0
        Items.Strings = (
          'All'
          'First N items'
          'Last N items')
        TabOrder = 0
        OnClick = rgItemsToPlot_RcClick
      end
      object seN_Rc: TJvSpinEdit
        Left = 8
        Top = 104
        Width = 57
        Height = 29
        ButtonKind = bkClassic
        MaxValue = 1000000.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 1
        OnChange = spNChange
      end
      object rgRc_PlotOrder: TRadioGroup
        Left = 8
        Top = 144
        Width = 185
        Height = 105
        Caption = 'Plot Order'
        ItemIndex = 0
        Items.Strings = (
          'Largest first'
          'Original order')
        TabOrder = 2
        OnClick = RedrawChart
      end
    end
    object jvsp_nm: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_nm'
      object lblPlotSymbolsNM: TLabel
        Left = 8
        Top = 120
        Width = 193
        Height = 21
        Caption = 'PlotSymbols to emphasize'
      end
      object lblValuesToIgnoreNM: TLabel
        Left = 8
        Top = 8
        Width = 112
        Height = 21
        Caption = 'Value to ignore'
      end
      object clbNM: TCheckListBox
        Left = 0
        Top = 183
        Width = 232
        Height = 254
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = 3
        ItemHeight = 21
        TabOrder = 2
        OnClick = DelayRedrawChart
      end
      object seValuesToIgnoreNM: TJvSpinEdit
        Left = 160
        Top = 8
        Width = 65
        Height = 29
        MaxValue = 2147483647.000000000000000000
        TabOrder = 0
        OnChange = seValuesToIgnoreNMChange
      end
      object rdgValuesToIgnoreNM: TRbwDataGrid4
        Left = 8
        Top = 40
        Width = 216
        Height = 64
        ColCount = 1
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
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
            AutoAdjustCaptionRowHeights = False
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -13
            ButtonFont.Name = 'MS Sans Serif'
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
            AutoAdjustColWidths = False
          end>
        OnEndUpdate = rdgValuesToIgnoreNMEndUpdate
        WordWrapRowCaptions = False
        ColWidths = (
          184)
      end
    end
    object jvsp_mcmc_par: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_mcmc_par'
      object lblParameter: TLabel
        Left = 16
        Top = 216
        Width = 130
        Height = 21
        Caption = 'Parameter to plot'
      end
      object lblMcmcPercent: TLabel
        Left = 8
        Top = 80
        Width = 178
        Height = 21
        Caption = 'Percent of Points to Use'
      end
      object lblMcmcBins: TLabel
        Left = 8
        Top = 152
        Width = 113
        Height = 21
        Caption = 'Number of Bins'
      end
      object rgMcmcGraphType: TRadioGroup
        Left = 8
        Top = 8
        Width = 201
        Height = 65
        Caption = 'Graph Type'
        ItemIndex = 0
        Items.Strings = (
          'Trace Plots'
          'Histogram')
        TabOrder = 0
        OnClick = rgMcmcGraphTypeClick
      end
      object lst_mcmcParameters: TJvxCheckListBox
        Left = 0
        Top = 247
        Width = 232
        Height = 190
        CheckKind = ckRadioButtons
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 21
        TabOrder = 3
        OnClickCheck = lst_mcmcParametersClickCheck
        InternalVersion = 202
      end
      object seMcmcPercent: TJvSpinEdit
        Left = 8
        Top = 112
        Width = 121
        Height = 29
        Increment = 10.000000000000000000
        MaxValue = 100.000000000000000000
        ValueType = vtFloat
        Value = 50.000000000000000000
        Enabled = False
        TabOrder = 1
        OnChange = RedrawChart
      end
      object seMcmcBins: TJvSpinEdit
        Left = 8
        Top = 184
        Width = 121
        Height = 29
        Value = 20.000000000000000000
        Enabled = False
        TabOrder = 2
        OnChange = RedrawChart
      end
    end
    object jvsp_mcmc_grr: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_mcmc_grr'
      object lblRThreshold: TLabel
        Left = 16
        Top = 16
        Width = 90
        Height = 21
        Caption = 'R-Threshold'
      end
      object rdeRThreshold: TRbwDataEntry
        Left = 16
        Top = 40
        Width = 145
        Height = 22
        TabOrder = 0
        Text = '1'
        OnChange = RedrawChart
        DataType = dtReal
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvsp_mcmc_pred: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_mcmc_pred'
      object rgMcmc_predGraphType: TRadioGroup
        Left = 8
        Top = 16
        Width = 209
        Height = 89
        Caption = 'Graph Type'
        ItemIndex = 0
        Items.Strings = (
          'Histograms'
          'Probability Plot')
        TabOrder = 0
        OnClick = rgMcmc_predGraphTypeClick
      end
      object jvpl_mcmc_pred: TJvPageList
        Left = 0
        Top = 131
        Width = 232
        Height = 306
        ActivePage = jvsp_mcmc_predProbability
        PropagateEnable = False
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        object jvsp_mcmc_predHistogram: TJvStandardPage
          Left = 0
          Top = 0
          Width = 232
          Height = 306
          Caption = 'jvsp_mcmc_predHistogram'
          object lbl_mcmc_prdBinCount: TLabel
            Left = 8
            Top = 8
            Width = 113
            Height = 21
            Caption = 'Number of Bins'
          end
          object se_mcmc_prdBinCount: TJvSpinEdit
            Left = 8
            Top = 40
            Width = 121
            Height = 29
            Value = 20.000000000000000000
            TabOrder = 0
            OnChange = RedrawChart
          end
          object chcklstbx_Mcmc_pred: TJvxCheckListBox
            Left = 0
            Top = 88
            Width = 232
            Height = 218
            CheckKind = ckRadioButtons
            Align = alBottom
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 21
            TabOrder = 1
            OnClickCheck = chcklstbx_Mcmc_predClickCheck
            InternalVersion = 202
          end
        end
        object jvsp_mcmc_predProbability: TJvStandardPage
          Left = 0
          Top = 0
          Width = 232
          Height = 306
          Caption = 'jvsp_mcmc_predProbability'
          object rgConfidenceInterval: TRadioGroup
            Left = 16
            Top = 16
            Width = 201
            Height = 129
            Caption = 'Credible Interval'
            ItemIndex = 1
            Items.Strings = (
              '90%'
              '95%'
              '99%'
              'Other')
            TabOrder = 0
            OnClick = rgConfidenceIntervalClick
          end
          object rdeConfidenceInterval: TRbwDataEntry
            Left = 16
            Top = 176
            Width = 145
            Height = 22
            Color = clBtnFace
            Enabled = False
            TabOrder = 1
            Text = '99.9'
            OnChange = rdeConfidenceIntervalChange
            DataType = dtReal
            Max = 100.000000000000000000
            CheckMax = True
            CheckMin = True
            ChangeDisabledColor = True
          end
        end
      end
    end
    object jvsp_svd: TJvStandardPage
      Left = 0
      Top = 0
      Width = 232
      Height = 437
      Caption = 'jvsp_svd'
      object rgWhatToPlot_svd: TRadioGroup
        Left = 16
        Top = 8
        Width = 209
        Height = 89
        Caption = 'What to plot'
        ItemIndex = 0
        Items.Strings = (
          'Singular values'
          'Singular vectors')
        TabOrder = 0
        OnClick = RedrawChart
      end
    end
  end
  object OpenDialogOutputFile: TOpenDialog
    Filter = 
      'All supported file types|*._os;*._ww;*._ws;*._sc;*._sd;*._s1;*._' +
      'nm;*._rd;*._rg;*._rb;*._rc;*._b;*._pa;*._pc;*._ss;*._rdadv;*._so' +
      ';*._opr;*._ppr;*._ppr_abschg;*._ppa;*._ppa_abschg;*._opr_abschg;' +
      '*._scgrp;*._mcmc*|Unweighted sim. equivalents and obs. *._os|*._' +
      'os|Weighted sim. equivalents and obs. *._ww|*._ww|Weighted sim. ' +
      'equivalents and resid. *._ws|*._ws|Composite scaled sensitivitie' +
      's (*._sc)|*._sc|Dimensionless scaled sensitivities  (*._sd)|*._s' +
      'd|One-percent scaled sensitivities (*._s1)|*._s1|Weighted res. a' +
      'nd prob. plotting pos. (*._nm)|*._nm|uncorrelated deviates and p' +
      'rob. plot pos. (*._rd)|*._rd|correlated deviates and prob. plot ' +
      'pos. (*._rg)|*._rg|DFBeta statistics (*._rb)|*._rb|Cook'#39's D (*._' +
      'rc)|*._rc|Parameter Values (*._b)|*._b|Parameter iterations (*._' +
      'pa)|*._pa|Final Parameter Values with CI (*._pc)|*._pc|Sum of Sq' +
      'uares Weighted Residuals (*._ss)|*._ss|Residual Analysis (*._rda' +
      'dv)|*._rdadv|Leverage Statistics (*._so)|*._so|Observation-Predi' +
      'ction (*.opr)|*._opr|Parameter-Prediction (*.ppr)|*._ppr|Paramet' +
      'er-Change in Pred Std Dev|*._ppr_abschg|Parameter-PPA Statistic|' +
      '*._ppa|Parameter-Change in Param Std Dev|._ppa_abschg|Observatio' +
      'n-Change in Pred Std Dev|._opr_abschg'
    Left = 309
    Top = 34
  end
  object MainMenu1: TMainMenu
    Left = 339
    Top = 34
    object miFile1: TMenuItem
      Caption = '&File'
      object miOpen: TMenuItem
        Caption = '&Open File'
        ShortCut = 16463
        OnClick = Button1Click
      end
      object SelectFileType1: TMenuItem
        Caption = 'Select Graph Type'
        Enabled = False
      end
      object miPrintSetup: TMenuItem
        Caption = 'Print &setup'
        Enabled = False
        OnClick = miPrintSetupClick
      end
      object miPrintPreview: TMenuItem
        Caption = 'Print P&review'
        Enabled = False
        OnClick = miPrintPreviewClick
      end
      object miPrint: TMenuItem
        Caption = '&Print'
        Enabled = False
        ShortCut = 16464
        OnClick = miPrintClick
      end
      object miSaveChart: TMenuItem
        Caption = '&Save as image'
        Enabled = False
        ShortCut = 16467
        OnClick = sbSaveImageClick
      end
      object miExit1: TMenuItem
        Caption = '&Exit'
        OnClick = miExit1Click
      end
    end
    object miConfigure: TMenuItem
      Caption = '&Configure'
      Enabled = False
      GroupIndex = 2
      object miZoomin: TMenuItem
        Caption = 'Zoom &in'
        Enabled = False
        OnClick = sbZoomInClick
      end
      object miZoomextents: TMenuItem
        Caption = 'Zoom &extents'
        Enabled = False
        OnClick = sbZoomOutClick
      end
      object miPan: TMenuItem
        Caption = '&Pan'
        Enabled = False
        OnClick = sbPanClick
      end
      object miFormatchart: TMenuItem
        Caption = '&Format chart'
        Enabled = False
        OnClick = sbFormatClick
      end
      object Formatchartoldstyle1: TMenuItem
        Caption = 'Format chart (&old style)'
        OnClick = sbOldFormatChartClick
      end
    end
    object miDockToolbar: TMenuItem
      Caption = '&Undock Toolbar'
      GroupIndex = 2
      OnClick = miDockToolbarClick
    end
    object Help1: TMenuItem
      Caption = 'Help'
      GroupIndex = 2
      object Help2: TMenuItem
        Caption = 'Help'
        GroupIndex = 2
        OnClick = Help2Click
      end
      object miAbout1: TMenuItem
        Caption = '&About'
        GroupIndex = 2
        OnClick = miAbout1Click
      end
    end
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 248
    Top = 34
  end
  object PrintDialog1: TPrintDialog
    Left = 278
    Top = 34
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.bmp'
    Filter = 
      'bmp files (*.bmp)|*.bmp|enhanced windows metafile (*.emf)|*.emf|' +
      'Windows metafile (*.wmf)|*.wmf'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 370
    Top = 34
  end
  object mainMenuFormChoice: TMainMenu
    Left = 440
    Top = 50
    object FormType1: TMenuItem
      Caption = 'Chart Type/Convert'
      GroupIndex = 1
      object CalibrationPlots1: TMenuItem
        Caption = 'Calibration Plots'
        OnClick = FormChoiceClick
      end
      object WaterBudgets1: TMenuItem
        Caption = 'Water Budgets'
        OnClick = FormChoiceClick
      end
      object Hydrographs1: TMenuItem
        Caption = 'Hydrographs'
        OnClick = FormChoiceClick
      end
      object LakePlots1: TMenuItem
        Caption = 'Lake, Gage, and UZF Plots'
        OnClick = FormChoiceClick
      end
      object PiperDiagram1: TMenuItem
        Caption = 'Piper Diagram'
        OnClick = FormChoiceClick
      end
      object CellWaterBudgets1: TMenuItem
        Caption = 'Cell Water Budgets or Zeta'
        OnClick = FormChoiceClick
      end
      object miFarmBudgets: TMenuItem
        Caption = 'Farm Budgets'
        OnClick = FormChoiceClick
      end
      object ConvertCellWaterBudgets1: TMenuItem
        Caption = 'Convert Cell Budgets or Zeta'
        OnClick = FormChoiceClick
      end
    end
  end
  object ChartEditor1: TChartEditor
    Chart = chartModflow
    Options = [ceChange, ceTitle, ceHelp]
    Title = 'Editing Calibration Plot'
    GalleryHeight = 0
    GalleryWidth = 0
    Height = 0
    Width = 0
    Left = 400
    Top = 33
  end
  object ChartPreviewer1: TChartPreviewer
    Chart = chartModflow
    Left = 432
    Top = 33
  end
  object OpenDialog1: TOpenDialog
    Filter = '_nm files|*._nm'
    Title = 'Select "global" or "list" file'
    Left = 456
    Top = 144
  end
  object odIntConf: TOpenDialog
    Left = 628
    Top = 177
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 636
    Top = 257
  end
  object tmrRedrawDelay: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tmrRedrawDelayTimer
    Left = 248
    Top = 65
  end
end
