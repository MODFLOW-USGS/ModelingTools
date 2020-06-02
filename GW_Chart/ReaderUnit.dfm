object frmZoneBdgtReader: TfrmZoneBdgtReader
  Left = 296
  Top = 95
  Width = 622
  Height = 575
  HelpContext = 350
  Caption = 
    'GW_Chart Water Budgets: Water and solute budgets for numerical m' +
    'odels'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Times New Roman'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 0
    Top = 232
    Width = 606
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    OnMoved = Splitter1Moved
  end
  object chartZONEBDGT: TChart
    Left = 0
    Top = 0
    Width = 606
    Height = 232
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Legend.Font.Charset = ANSI_CHARSET
    Legend.Font.Name = 'Times New Roman'
    Title.Font.Charset = ANSI_CHARSET
    Title.Font.Height = -15
    Title.Font.Name = 'Times New Roman'
    Title.Text.Strings = (
      'ZONEBDGT Data')
    OnGetLegendPos = chartZONEBDGTGetLegendPos
    OnGetLegendRect = chartZONEBDGTGetLegendRect
    BottomAxis.LabelsFont.Charset = ANSI_CHARSET
    BottomAxis.LabelsFont.Height = -15
    BottomAxis.LabelsFont.Name = 'Times New Roman'
    BottomAxis.LabelStyle = talValue
    BottomAxis.MinorTicks.Visible = False
    BottomAxis.TickInnerLength = 4
    BottomAxis.TickLength = 0
    BottomAxis.Title.Font.Charset = ANSI_CHARSET
    BottomAxis.Title.Font.Height = -15
    BottomAxis.Title.Font.Name = 'Times New Roman'
    DepthAxis.MinorTicks.Visible = False
    DepthAxis.TickInnerLength = 4
    DepthAxis.TickLength = 0
    LeftAxis.AxisValuesFormat = '#,##0.###,###,###'
    LeftAxis.LabelsFont.Charset = ANSI_CHARSET
    LeftAxis.LabelsFont.Height = -15
    LeftAxis.LabelsFont.Name = 'Times New Roman'
    LeftAxis.MinorTicks.Visible = False
    LeftAxis.TickInnerLength = 4
    LeftAxis.TickLength = 0
    LeftAxis.Title.Font.Charset = ANSI_CHARSET
    LeftAxis.Title.Font.Height = -12
    LeftAxis.Title.Font.Name = 'Times New Roman'
    RightAxis.MinorTicks.Visible = False
    RightAxis.TickInnerLength = 4
    RightAxis.TickLength = 0
    TopAxis.MinorTicks.Visible = False
    TopAxis.TickInnerLength = 4
    TopAxis.TickLength = 0
    View3D = False
    OnAfterDraw = chartZONEBDGTAfterDraw
    Align = alClient
    Color = clWindow
    TabOrder = 0
    OnMouseDown = chartZONEBDGTMouseDown
    OnMouseMove = chartZONEBDGTMouseMove
    OnMouseUp = chartZONEBDGTMouseUp
    object ChartTool1: TMarksTipTool
      MouseAction = mtmClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 235
    Width = 606
    Height = 281
    Align = alBottom
    TabOrder = 1
    object Panel1: TPanel
      Left = 460
      Top = 1
      Width = 145
      Height = 279
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        145
        279)
      object btnOpen: TSpeedButton
        Left = 9
        Top = 246
        Width = 24
        Height = 31
        Hint = 'Open ZONEBDGT file'
        Anchors = [akLeft, akBottom]
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
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
        OnClick = btnReadClick
      end
      object btnSave: TSpeedButton
        Left = 41
        Top = 246
        Width = 24
        Height = 31
        Hint = 'Save ZONEBDGT data in tab-delimited format'
        Anchors = [akLeft, akBottom]
        Enabled = False
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
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
        OnClick = btnSaveClick
      end
      object cbDecay: TCheckBox
        Left = 8
        Top = 2
        Width = 137
        Height = 17
        Caption = 'Source-Term Decay'
        Enabled = False
        TabOrder = 0
        OnClick = cbDecayClick
      end
      object cbStoredMass: TCheckBox
        Left = 8
        Top = 19
        Width = 97
        Height = 17
        Caption = 'Stored Mass'
        Enabled = False
        TabOrder = 1
        OnClick = cbStoredMassClick
      end
      object cbInOut: TCheckBox
        Left = 8
        Top = 36
        Width = 137
        Height = 17
        Caption = 'IN - OUT'
        Checked = True
        State = cbChecked
        TabOrder = 2
        OnClick = cbInOutClick
      end
      object cbDiscrepancy: TCheckBox
        Left = 8
        Top = 53
        Width = 137
        Height = 17
        Hint = 'Percent Discrepancy should normally be less than 1%'
        Caption = 'Percent Discrepancy'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 3
        OnClick = cbDiscrepancyClick
      end
      object rgDataSource: TRadioGroup
        Left = 8
        Top = 72
        Width = 129
        Height = 169
        Caption = 'Read Data From'
        ItemIndex = 0
        Items.Strings = (
          'ZONEBDGT'
          'MODFLOW'
          'MODFLOW 6'
          'MOC3D or GWT'
          'SUTRA 09.97'
          'SUTRA'
          'MT3D'
          'HST3D'
          'SEAWAT-2000'
          'GSFLOW')
        TabOrder = 4
        OnClick = rgDataSourceClick
      end
      object BitBtnClose: TBitBtn
        Left = 71
        Top = 246
        Width = 66
        Height = 31
        Hint = 'Exit from Budgeteer'
        Anchors = [akLeft, akBottom]
        Caption = 'Close'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Kind = bkClose
      end
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 459
      Height = 279
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Panel2: TPanel
        Left = 0
        Top = 180
        Width = 459
        Height = 99
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          459
          99)
        object lblTime: TLabel
          Left = 8
          Top = 6
          Width = 25
          Height = 15
          Caption = 'Time'
        end
        object lblZone: TLabel
          Left = 8
          Top = 38
          Width = 25
          Height = 15
          Caption = 'Zone'
        end
        object sbImage: TSpeedButton
          Left = 8
          Top = 64
          Width = 24
          Height = 31
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
          Left = 32
          Top = 64
          Width = 24
          Height = 31
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
          OnMouseMove = ToolBar1MouseMove
        end
        object sbZoomIn: TSpeedButton
          Left = 56
          Top = 64
          Width = 24
          Height = 31
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
          Left = 80
          Top = 64
          Width = 24
          Height = 31
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
          Left = 104
          Top = 64
          Width = 24
          Height = 31
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
        object comboTimeStep: TComboBox
          Left = 40
          Top = 2
          Width = 417
          Height = 23
          Hint = 'Stress period and time step to plot'
          Style = csDropDownList
          Anchors = [akTop, akRight]
          Color = clBtnFace
          Enabled = False
          ItemHeight = 15
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnChange = comboTimeStepChange
        end
        object comboZone: TComboBox
          Left = 40
          Top = 34
          Width = 160
          Height = 23
          Hint = 'Zone to plot'
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          Color = clBtnFace
          ItemHeight = 15
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = comboZoneChange
        end
        object rgPlotType: TRadioGroup
          Left = 208
          Top = 27
          Width = 257
          Height = 70
          Anchors = [akTop, akRight]
          Caption = 'Plot Type'
          ItemIndex = 1
          Items.Strings = (
            'Plot bar chart of one time step and zone'
            'Plot time series of one zone')
          TabOrder = 1
          OnClick = rgPlotTypeClick
        end
      end
      object pnlBudgetItems: TPanel
        Left = 0
        Top = 0
        Width = 459
        Height = 180
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object splBudget: TSplitter
          Left = 301
          Top = 0
          Height = 180
          Align = alRight
          Beveled = True
        end
        object splNet: TSplitter
          Left = 155
          Top = 0
          Height = 180
          Beveled = True
        end
        object pnlInBudget: TPanel
          Left = 158
          Top = 0
          Width = 143
          Height = 180
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object Panel7: TPanel
            Left = 0
            Top = 0
            Width = 143
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object cbIn: TCheckBox
              Left = 2
              Top = 4
              Width = 128
              Height = 21
              Hint = 
                'You can use this check box to enable or disable all "In" budget ' +
                'items'
              Caption = '"In" Budget Items'
              Checked = True
              ParentShowHint = False
              ShowHint = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbInClick
            end
          end
          object clbIn: TCheckListBox
            Left = 0
            Top = 24
            Width = 143
            Height = 156
            OnClickCheck = clbInClickCheck
            Align = alClient
            ItemHeight = 15
            TabOrder = 1
          end
        end
        object pnlOutBudget: TPanel
          Left = 304
          Top = 0
          Width = 155
          Height = 180
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 2
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 155
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object cbOut: TCheckBox
              Left = 2
              Top = 4
              Width = 113
              Height = 21
              Hint = 
                'You can use this check box to enable or disable all "Out" budget' +
                ' items'
              Caption = '"Out" Budget Items'
              Checked = True
              ParentShowHint = False
              ShowHint = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbOutClick
            end
          end
          object clbOut: TCheckListBox
            Left = 0
            Top = 25
            Width = 155
            Height = 155
            OnClickCheck = clbOutClickCheck
            Align = alClient
            ItemHeight = 15
            TabOrder = 1
          end
        end
        object pnlNetBudget: TPanel
          Left = 0
          Top = 0
          Width = 155
          Height = 180
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object pnl: TPanel
            Left = 0
            Top = 0
            Width = 155
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object cbNet: TCheckBox
              Left = 2
              Top = 4
              Width = 113
              Height = 21
              Hint = 
                'You can use this check box to enable or disable all "Out" budget' +
                ' items'
              Caption = '"Net" Budget Items'
              Checked = True
              ParentShowHint = False
              ShowHint = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbNetClick
            end
          end
          object clbNet: TCheckListBox
            Left = 0
            Top = 25
            Width = 155
            Height = 155
            OnClickCheck = clbNetClickCheck
            Align = alClient
            ItemHeight = 15
            TabOrder = 1
          end
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Zonebudget Listing files (*.zbl, *.zblst, *.zb.lst)|*.zbl;*.zbls' +
      't;*.zb.lst|All Files (*.*)|*.*'
    Left = 48
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 104
    Top = 16
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 16
    object File1: TMenuItem
      Caption = '&File'
      object Read1: TMenuItem
        Caption = '&Open'
        OnClick = btnReadClick
      end
      object Save1: TMenuItem
        Caption = '&Save'
        Enabled = False
        OnClick = btnSaveClick
      end
      object Saveasimage1: TMenuItem
        Caption = 'Save as &image'
        Enabled = False
        OnClick = sbImageClick
      end
      object FormatChart1: TMenuItem
        Caption = 'Format &Chart'
        Enabled = False
        OnClick = sbFormatClick
      end
      object miPrintSetup: TMenuItem
        Caption = 'Print &Setup'
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
        OnClick = miPrintClick
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Configure1: TMenuItem
      Caption = 'Configure'
      Enabled = False
      GroupIndex = 2
      object Zoomin1: TMenuItem
        Caption = 'Zoom &in'
        OnClick = sbZoomInClick
      end
      object Zoomextents1: TMenuItem
        Caption = 'Zoom &extents'
        OnClick = sbZoomOutClick
      end
      object miPan: TMenuItem
        Caption = '&Pan'
        OnClick = sbPanClick
      end
      object Formatchart2: TMenuItem
        Caption = '&Format chart'
        OnClick = sbFormatClick
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      GroupIndex = 2
      object Help2: TMenuItem
        Caption = 'Help'
        OnClick = Help2Click
      end
      object About2: TMenuItem
        Caption = '&About'
        GroupIndex = 2
        OnClick = About1Click
      end
    end
  end
  object ChartEditor1: TChartEditor
    Chart = chartZONEBDGT
    Options = [ceChange, ceTitle, ceHelp]
    Title = 'Editing Budget Plot'
    Left = 136
    Top = 17
  end
  object ChartPreviewer1: TChartPreviewer
    Chart = chartZONEBDGT
    Left = 176
    Top = 17
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 208
    Top = 18
  end
  object PrintDialog1: TPrintDialog
    Left = 238
    Top = 18
  end
end
