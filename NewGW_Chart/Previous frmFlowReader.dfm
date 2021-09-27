object frmCellFlows: TfrmCellFlows
  Left = 348
  Top = 112
  Width = 630
  Height = 544
  HelpContext = 635
  Caption = 'Flow Rates vs. Time'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
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
  TextHeight = 19
  object Splitter1: TSplitter
    Left = 0
    Top = 161
    Width = 622
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 425
    Width = 622
    Height = 73
    Align = alBottom
    TabOrder = 0
    object lblCounts: TLabel
      Left = 184
      Top = 43
      Width = 190
      Height = 19
      Caption = 'Columns: 0, Rows: 0, Layers: 0'
    end
    object Label1: TLabel
      Left = 184
      Top = 11
      Width = 74
      Height = 19
      Caption = 'Cells to Plot'
    end
    object sbFormat: TSpeedButton
      Left = 527
      Top = 39
      Width = 23
      Height = 26
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
      OnClick = FormatChart1Click
    end
    object btnSelectFile: TButton
      Left = 7
      Top = 7
      Width = 122
      Height = 26
      Caption = 'Open Budget File'
      TabOrder = 0
      OnClick = btnSelectFileClick
    end
    object BitBtn1: TBitBtn
      Left = 553
      Top = 39
      Width = 64
      Height = 26
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkClose
    end
    object btnUpdatePlot: TButton
      Left = 7
      Top = 40
      Width = 122
      Height = 25
      Caption = 'Update Plot'
      Enabled = False
      TabOrder = 2
      OnClick = btnUpdatePlotClick
    end
    object seCells: TSpinEdit
      Left = 139
      Top = 6
      Width = 40
      Height = 29
      Enabled = False
      MaxValue = 1000000
      MinValue = 1
      TabOrder = 3
      Value = 1
      OnChange = seCellsChange
    end
    object comboModelChoice: TComboBox
      Left = 272
      Top = 8
      Width = 345
      Height = 27
      Style = csDropDownList
      ItemHeight = 19
      TabOrder = 4
      Items.Strings = (
        'MODFLOW (MF96 or MF2K Version 1.1 or earlier)'
        'MODFLOW-2000 (Version 1.2 or later)')
    end
    object BitBtn2: TBitBtn
      Left = 448
      Top = 40
      Width = 75
      Height = 25
      HelpContext = 635
      TabOrder = 5
      Kind = bkHelp
    end
  end
  object chartFlow: TChart
    Left = 0
    Top = 164
    Width = 622
    Height = 261
    BackWall.Brush.Color = clWhite
    BackWall.Brush.Style = bsClear
    Title.Font.Charset = ANSI_CHARSET
    Title.Font.Color = clBlue
    Title.Font.Height = -19
    Title.Font.Name = 'Times New Roman'
    Title.Font.Style = []
    Title.Text.Strings = (
      'Flow Rates')
    BottomAxis.Title.Caption = 'Time or Stored Time Step'
    BottomAxis.Title.Font.Charset = ANSI_CHARSET
    BottomAxis.Title.Font.Color = clBlack
    BottomAxis.Title.Font.Height = -16
    BottomAxis.Title.Font.Name = 'Times New Roman'
    BottomAxis.Title.Font.Style = []
    LeftAxis.Title.Caption = 'Flow Rate'
    LeftAxis.Title.Font.Charset = ANSI_CHARSET
    LeftAxis.Title.Font.Color = clBlack
    LeftAxis.Title.Font.Height = -16
    LeftAxis.Title.Font.Name = 'Times New Roman'
    LeftAxis.Title.Font.Style = []
    View3D = False
    Align = alClient
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 622
    Height = 161
    Align = alTop
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 349
      Top = 1
      Width = 3
      Height = 159
      Cursor = crHSplit
      Align = alRight
    end
    object clbDataSets: TCheckListBox
      Left = 1
      Top = 1
      Width = 348
      Height = 159
      OnClickCheck = clbDataSetsClickCheck
      Align = alClient
      ItemHeight = 19
      TabOrder = 0
    end
    object dgCells: TRbwDataGrid
      Left = 352
      Top = 1
      Width = 269
      Height = 159
      Align = alRight
      ColCount = 4
      Enabled = False
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      TabOrder = 1
      OnMouseUp = dgCellsMouseUp
      ColorSelectedRow = True
      Columns = <
        item
          CheckMax = False
          CheckMin = True
          CheckedString = 'Yes'
          Format = rcfInteger
          LimitToList = False
          MaxLength = 0
          Max = 1
          Min = 1
          UnCheckedString = 'No'
        end
        item
          CheckMax = True
          CheckMin = True
          CheckedString = 'Yes'
          Format = rcfInteger
          LimitToList = False
          MaxLength = 0
          Max = 1
          Min = 1
          UnCheckedString = 'No'
        end
        item
          CheckMax = True
          CheckMin = True
          CheckedString = 'Yes'
          Format = rcfInteger
          LimitToList = False
          MaxLength = 0
          Max = 1
          Min = 1
          UnCheckedString = 'No'
        end
        item
          CheckMax = False
          CheckMin = False
          CheckedString = 'Yes'
          Format = rcfBoolean
          LimitToList = False
          MaxLength = 0
          UnCheckedString = 'No'
        end>
      SelectedRowColor = clAqua
      WordWrapColTitles = False
      UnselectableColor = clBtnFace
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = '*.bud|*.bud|*.*|*.*'
    Left = 32
    Top = 16
  end
  object ChartEditor1: TChartEditor
    Chart = chartFlow
    HideTabs = []
    Left = 64
    Top = 20
  end
  object MainMenu1: TMainMenu
    Left = 64
    Top = 236
    object File1: TMenuItem
      Caption = '&File'
      object SelectBudgetFile1: TMenuItem
        Caption = '&Open Budget File'
        OnClick = btnSelectFileClick
      end
      object UpdatePlot1: TMenuItem
        Caption = '&Update Plot'
        OnClick = btnUpdatePlotClick
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object FormatChart1: TMenuItem
      Caption = '&Format Chart'
      GroupIndex = 2
      OnClick = FormatChart1Click
    end
    object Help1: TMenuItem
      Caption = '&Help'
      GroupIndex = 2
      object Help2: TMenuItem
        Caption = '&Help'
        OnClick = Help2Click
      end
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
  end
end
