object frmLakePlot: TfrmLakePlot
  Left = 373
  Top = 181
  HelpContext = 500
  Caption = 
    'GW_Chart Lake and Gage Plot: Lake and stream output from numeric' +
    'al models'
  ClientHeight = 441
  ClientWidth = 663
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Times New Roman'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 17
  object chrtLake: TChart
    Left = 0
    Top = 29
    Width = 663
    Height = 412
    BackWall.Brush.Style = bsClear
    MarginRight = 15
    Title.Font.Charset = ANSI_CHARSET
    Title.Font.Color = clBlack
    Title.Font.Height = -27
    Title.Font.Name = 'Times New Roman'
    Title.Text.Strings = (
      'TChart')
    OnGetLegendPos = chrtLakeGetLegendPos
    OnGetLegendRect = chrtLakeGetLegendRect
    BottomAxis.Grid.Visible = False
    BottomAxis.MinorTicks.Visible = False
    BottomAxis.TickInnerLength = 4
    BottomAxis.TickLength = 0
    BottomAxis.Title.Font.Charset = ANSI_CHARSET
    BottomAxis.Title.Font.Height = -20
    BottomAxis.Title.Font.Name = 'Times New Roman'
    LeftAxis.Grid.Visible = False
    LeftAxis.MinorTicks.Visible = False
    LeftAxis.TickInnerLength = 4
    LeftAxis.TickLength = 0
    LeftAxis.Title.Font.Charset = ANSI_CHARSET
    LeftAxis.Title.Font.Height = -20
    LeftAxis.Title.Font.Name = 'Times New Roman'
    View3D = False
    OnAfterDraw = chrtLakeAfterDraw
    Align = alClient
    Color = clWindow
    TabOrder = 1
    ExplicitHeight = 392
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 663
    Height = 29
    ButtonHeight = 25
    ButtonWidth = 25
    Caption = 'ToolBar1'
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 2
      Width = 85
      Height = 25
      AutoSize = False
      Caption = ' Data Source: '
      Layout = tlCenter
    end
    object comboDataSource: TComboBox
      Left = 85
      Top = 2
      Width = 132
      Height = 25
      Style = csDropDownList
      TabOrder = 0
      OnChange = comboDataSourceChange
      Items.Strings = (
        'MODFLOW'
        'GWT or MOC3D'
        'Gage file')
    end
    object Label2: TLabel
      Left = 217
      Top = 0
      Width = 84
      Height = 25
      AutoSize = False
      Caption = '  Graph Type: '
      Layout = tlCenter
    end
    object comboGraphType: TsiComboBox
      Left = 301
      Top = 0
      Width = 212
      Height = 25
      Style = csDropDownList
      TabOrder = 1
      OnChange = comboGraphTypeChange
      Items.Strings = (
        'Stage vs. Volume'
        'Connected Lakes vs. Time'
        'Dry Lake Cells vs. Time'
        'Total Lake Area vs. Time'
        'Stage vs. Time'
        'Precipitation vs. Time'
        'Evaporation vs. Time'
        'Runoff vs. Time'
        'Groundwater Inflow vs. Time'
        'Groundwater Outflow vs. Time'
        'Surface Water Inflow vs. Time'
        'Surface Water Outflow vs. Time'
        'Water Use vs. Time'
        'Connected Lake Influx vs. Time'
        'Volume vs. Time'
        'Surface Area vs. Time'
        'Stage Change vs. Time'
        'Cumulative Stage Change vs. Time')
    end
    object SpeedButton1: TSpeedButton
      Left = 513
      Top = 0
      Width = 23
      Height = 25
      Hint = 'Open File'
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
      OnClick = Open1Click
    end
    object sbImage: TSpeedButton
      Left = 536
      Top = 0
      Width = 23
      Height = 25
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
    object sbFormat: TSpeedButton
      Left = 559
      Top = 0
      Width = 23
      Height = 25
      Hint = 'Format Chart'
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
    end
    object sbAdvancedFormat: TSpeedButton
      Left = 582
      Top = 0
      Width = 23
      Height = 25
      Hint = 'Format Chart (Advanced)'
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
      OnClick = sbAdvancedFormatClick
    end
    object sbSaveData: TSpeedButton
      Left = 605
      Top = 0
      Width = 20
      Height = 25
      Hint = 'Save data to disk'
      Enabled = False
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333330070
        7700333333337777777733333333008088003333333377F73377333333330088
        88003333333377FFFF7733333333000000003FFFFFFF77777777000000000000
        000077777777777777770FFFFFFF0FFFFFF07F3333337F3333370FFFFFFF0FFF
        FFF07F3FF3FF7FFFFFF70F00F0080CCC9CC07F773773777777770FFFFFFFF039
        99337F3FFFF3F7F777F30F0000F0F09999937F7777373777777F0FFFFFFFF999
        99997F3FF3FFF77777770F00F000003999337F773777773777F30FFFF0FF0339
        99337F3FF7F3733777F30F08F0F0337999337F7737F73F7777330FFFF0039999
        93337FFFF7737777733300000033333333337777773333333333}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      OnClick = sbSaveDataClick
    end
    object sbPrint: TSpeedButton
      Left = 625
      Top = 0
      Width = 23
      Height = 25
      Hint = 'Print'
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
      OnClick = sbPrintClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 48
    object File1: TMenuItem
      Caption = 'File'
      object Open1: TMenuItem
        Caption = 'Open'
        OnClick = Open1Click
      end
      object SaveImage1: TMenuItem
        Caption = 'Save Image'
        Enabled = False
        OnClick = sbImageClick
      end
      object SaveData1: TMenuItem
        Caption = 'Save Data'
        Enabled = False
        OnClick = sbSaveDataClick
      end
      object PrintPreview1: TMenuItem
        Caption = 'Print Preview'
        Enabled = False
        OnClick = PrintPreview1Click
      end
      object Print1: TMenuItem
        Caption = 'Print'
        Enabled = False
        OnClick = sbPrintClick
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Format1: TMenuItem
      Caption = 'Format'
      Enabled = False
      GroupIndex = 2
      object Format2: TMenuItem
        Caption = 'Format (Simple)'
        OnClick = sbFormatClick
      end
      object FormatAdvanced1: TMenuItem
        Caption = 'Format (Advanced)'
        OnClick = sbAdvancedFormatClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      GroupIndex = 2
      object Help2: TMenuItem
        Caption = 'Help'
        OnClick = Help2Click
      end
      object About1: TMenuItem
        Caption = 'About'
        GroupIndex = 2
        OnClick = About1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Modflow Listing files (*.lst)|*.lst;*.out|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 72
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 8
    Top = 48
  end
  object StrSetModflowStrings: TStrSet
    Strings.Strings = (
      'Stage vs. Volume'
      'Stage vs. Area'
      'Volume vs. Area'
      'Connected Lakes vs. Time'
      'Dry Lake Cells vs. Time'
      'Total Lake Area vs. Time'
      'Stage vs. Time'
      'Precipitation Rate vs. Time'
      'Evaporation Rate vs. Time'
      'Runoff Rate vs. Time'
      'Ground-Water Inflow Rate vs. Time'
      'Ground-Water Outflow Rate vs. Time'
      'Surface-Water Inflow Rate vs. Time'
      'Surface-Water Outflow Rate vs. Time'
      'Water Use Rate vs. Time'
      'Connected Lake Influx Rate vs. Time'
      'Volume vs. Time'
      'Surface Area vs. Time'
      'Stage Change vs. Time'
      'Cumulative Stage Change vs. Time'
      'Volume Change vs. Time'
      'Percent Discrepancy vs. Time')
    Left = 104
    Top = 48
  end
  object StrSetMoc3dStrings: TStrSet
    Strings.Strings = (
      'Lake Volume'
      'Concentration'
      'Precipitation Solute Mass In'
      'Stream Solute Mass In'
      'Stream Solute Mass Out'
      'Net Solute  Withdrawal'
      'Runoff Solute Mass In'
      'Ground-Water Solute Mass In'
      'Ground-Water Solute Mass Out'
      'Solute Mass In Lake')
    Left = 136
    Top = 48
  end
  object PrintDialog1: TPrintDialog
    Options = [poDisablePrintToFile]
    Left = 168
    Top = 48
  end
  object ChartEditor1: TChartEditor
    Chart = chrtLake
    Options = [ceChange, ceTitle, ceHelp]
    Title = 'Editing Lake Plot'
    GalleryHeight = 0
    GalleryWidth = 0
    Height = 0
    Width = 0
    Left = 136
    Top = 17
  end
  object ChartPreviewer1: TChartPreviewer
    Chart = chrtLake
    Left = 176
    Top = 17
  end
end
