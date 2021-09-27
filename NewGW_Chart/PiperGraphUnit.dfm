object frmPiperGraph: TfrmPiperGraph
  Left = 285
  Top = 93
  Width = 819
  Height = 558
  HelpContext = 600
  Caption = 'GW_Chart Piper Graph: Visual display of chemical composition'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Times New Roman'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 19
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 801
    Height = 469
    ActivePage = tabData
    Align = alClient
    TabOrder = 0
    object tabData: TTabSheet
      HelpContext = 610
      Caption = 'Data'
      object Panel1: TPanel
        Left = 0
        Top = 373
        Width = 793
        Height = 62
        Align = alBottom
        TabOrder = 1
        object Label1: TLabel
          Left = 10
          Top = 3
          Width = 136
          Height = 19
          Caption = 'Number of data points'
        end
        object sbOpenFile: TSpeedButton
          Left = 624
          Top = 10
          Width = 31
          Height = 32
          Hint = 'Open File'
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
          OnClick = sbOpenFileClick
        end
        object sbSaveFile: TSpeedButton
          Left = 655
          Top = 10
          Width = 32
          Height = 32
          Hint = 'Save File'
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
          OnClick = sbSaveFileClick
        end
        object sbFormat2: TSpeedButton
          Left = 592
          Top = 10
          Width = 32
          Height = 32
          Hint = 'Format Chart'
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
        object seNumPoints: TSpinEdit
          Left = 8
          Top = 22
          Width = 107
          Height = 29
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 1
          OnChange = seNumPointsChange
        end
        object Button2: TButton
          Left = 687
          Top = 10
          Width = 65
          Height = 32
          Hint = 'Paste from clipboard'
          Caption = 'Paste'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = Button2Click
        end
        object rgDataType: TRadioGroup
          Left = 156
          Top = 0
          Width = 392
          Height = 57
          Hint = 'type of data entered in table'
          Caption = 'Data Type'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'proportions'
            'percents'
            'concentrations (meq/l)'
            'concentrations (mg/l)')
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = rgDataTypeClick
        end
      end
      object DataGrid1: TDataGrid
        Left = 0
        Top = 0
        Width = 793
        Height = 373
        Align = alClient
        ColCount = 11
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing, goTabs, goAlwaysShowEditor]
        TabOrder = 0
        OnDrawCell = DataGrid1DrawCell
        OnKeyUp = DataGrid1KeyUp
        OnSelectCell = DataGrid1SelectCell
        OnSetEditText = DataGrid1SetEditText
        Columns = <
          item
            Format = cfNumber
            Title.Caption = 'Ca'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'Mg'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'Na'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'K'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'CO3'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'HCO3'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'Cl'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'SO4'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'TDS'
            Title.WordWrap = False
          end
          item
            LimitToList = True
            PickList.Strings = (
              'Circle'
              'Open Circle'
              'Square'
              'Open Square'
              'Triangle'
              'Open Triangle'
              'Inverted Triangle'
              'Open Inverted Triangle'
              'Star'
              'Open Star'
              'Cross'
              'X')
            Title.Caption = 'Symbol'
            Title.WordWrap = False
          end
          item
            ButtonStyle = cbsEllipsis
            Title.Caption = 'Color'
            Title.WordWrap = False
          end>
        RowCountMin = 0
        OnEditButtonClick = DataGrid1EditButtonClick
        SelectedIndex = 0
        Version = '2.0'
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64
          47
          142
          64)
        RowHeights = (
          25
          20)
      end
    end
    object tabPlot: TTabSheet
      HelpContext = 620
      Caption = 'Plot'
      ImageIndex = 1
      OnResize = tabPlotResize
      object Panel2: TPanel
        Left = 0
        Top = 388
        Width = 793
        Height = 47
        Align = alBottom
        TabOrder = 1
        object sbSaveImage: TSpeedButton
          Left = 39
          Top = 8
          Width = 32
          Height = 34
          Hint = 'Export to image'
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
        end
        object sbFormat: TSpeedButton
          Left = 8
          Top = 8
          Width = 31
          Height = 34
          Hint = 'Format Chart'
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
        object BitBtn2: TBitBtn
          Left = 71
          Top = 8
          Width = 71
          Height = 34
          Hint = 'print graph'
          Caption = 'Print'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = BitBtn2Click
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00300000000000
            00033FFFFFFFFFFFFFFF0888888888888880777777777777777F088888888888
            8880777777777777777F0000000000000000FFFFFFFFFFFFFFFF0F8F8F8F8F8F
            8F80777777777777777F08F8F8F8F8F8F9F0777777777777777F0F8F8F8F8F8F
            8F807777777777777F7F0000000000000000777777777777777F3330FFFFFFFF
            03333337F3FFFF3F7F333330F0000F0F03333337F77773737F333330FFFFFFFF
            03333337F3FF3FFF7F333330F00F000003333337F773777773333330FFFF0FF0
            33333337F3FF7F3733333330F08F0F0333333337F7737F7333333330FFFF0033
            33333337FFFF7733333333300000033333333337777773333333}
          NumGlyphs = 2
        end
        object rgOrientation: TRadioGroup
          Left = 156
          Top = 0
          Width = 230
          Height = 41
          Caption = 'Orientation'
          Columns = 2
          ItemIndex = 0
          Items.Strings = (
            'Portrait'
            'Landscape')
          TabOrder = 0
          OnClick = rgOrientationClick
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 793
        Height = 388
        Align = alClient
        Color = clWindow
        TabOrder = 0
        object PaintBox1: TPaintBox
          Left = 66
          Top = 1
          Width = 661
          Height = 386
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Times New Roman'
          Font.Style = []
          ParentFont = False
          OnMouseDown = PaintBox1MouseDown
          OnMouseMove = PaintBox1MouseMove
          OnMouseUp = PaintBox1MouseUp
          OnPaint = PaintBox1Paint
        end
        object shpLeft: TShape
          Left = 1
          Top = 1
          Width = 65
          Height = 386
          Align = alLeft
          Brush.Color = clBtnFace
          Pen.Color = clBtnFace
          Visible = False
        end
        object shpRight: TShape
          Left = 727
          Top = 1
          Width = 65
          Height = 386
          Align = alRight
          Brush.Color = clBtnFace
          Pen.Color = clBtnFace
          Visible = False
        end
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 469
    Width = 801
    Height = 19
    Panels = <>
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 532
    Top = 5
  end
  object ColorDialog1: TColorDialog
    Left = 572
    Top = 5
  end
  object OpenDialog1: TOpenDialog
    Filter = '(*.pgr)|*.pgr'
    Left = 496
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'pgr'
    Filter = '(*.pgr)|*.pgr'
    Left = 464
  end
  object SaveDialog2: TSaveDialog
    DefaultExt = 'wmf'
    Filter = '(*.wmf)|*.wmf|(*.emf)|*.emf|(*.bmp)|*.bmp|All files (*.*)|*.*'
    Left = 424
  end
  object MainMenu1: TMainMenu
    Left = 380
    Top = 2
    object File1: TMenuItem
      Caption = 'File'
      object Opendatafile1: TMenuItem
        Caption = 'Open'
        OnClick = sbOpenFileClick
      end
      object Save1: TMenuItem
        Caption = 'Save'
        OnClick = sbSaveFileClick
      end
      object Saveasimage1: TMenuItem
        Caption = 'Save as image'
        OnClick = sbSaveImageClick
      end
      object Print1: TMenuItem
        Caption = 'Print'
        OnClick = BitBtn2Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
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
        OnClick = About1Click
      end
    end
  end
end
