object frmFarm: TfrmFarm
  Left = 554
  Top = 353
  Width = 928
  Height = 480
  HelpContext = 645
  Caption = 'Farm Budgets'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mmFarm
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object chtFarmPlot: TChart
    Left = 249
    Top = 0
    Width = 543
    Height = 421
    Title.Text.Strings = (
      'Farm Budget')
    OnGetLegendPos = chtFarmPlotGetLegendPos
    OnGetLegendRect = chtFarmPlotGetLegendRect
    BottomAxis.Title.Caption = 'Time'
    LeftAxis.Title.Caption = 'Value'
    View3D = False
    OnAfterDraw = chtFarmPlotAfterDraw
    Align = alClient
    Color = clWhite
    TabOrder = 0
    PrintMargins = (
      15
      20
      15
      20)
    object spl1: TSplitter
      Left = 1
      Top = 1
      Height = 419
    end
  end
  object pnlFarmBudgets: TPanel
    Left = 792
    Top = 0
    Width = 120
    Height = 421
    Align = alRight
    TabOrder = 1
    object spl5: TSplitter
      Left = 1
      Top = 204
      Width = 118
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object pnl12: TPanel
      Left = 1
      Top = 1
      Width = 118
      Height = 203
      Align = alClient
      Caption = 'pnl12'
      TabOrder = 0
      object pnl13: TPanel
        Left = 1
        Top = 1
        Width = 116
        Height = 201
        Align = alClient
        TabOrder = 0
        object clbFarm: TJvCheckListBox
          Left = 1
          Top = 25
          Width = 114
          Height = 175
          OnClickCheck = clbFarmClickCheck
          Align = alClient
          ItemHeight = 13
          TabOrder = 1
        end
        object pnl14: TPanel
          Left = 1
          Top = 1
          Width = 114
          Height = 24
          Align = alTop
          TabOrder = 0
          object cbFarm: TCheckBox
            Left = 3
            Top = 0
            Width = 97
            Height = 17
            Caption = 'Farm'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbFarmClick
          end
        end
      end
    end
    object pnl16: TPanel
      Left = 1
      Top = 207
      Width = 118
      Height = 213
      Align = alBottom
      Caption = 'pnl12'
      TabOrder = 1
      object pnl17: TPanel
        Left = 1
        Top = 1
        Width = 116
        Height = 211
        Align = alClient
        TabOrder = 0
        object clbFarmAggregates: TJvCheckListBox
          Left = 1
          Top = 25
          Width = 114
          Height = 185
          OnClickCheck = clbFarmClickCheck
          Align = alClient
          ItemHeight = 13
          TabOrder = 1
        end
        object pnl18: TPanel
          Left = 1
          Top = 1
          Width = 114
          Height = 24
          Align = alTop
          TabOrder = 0
          object cbFarmAggregates: TCheckBox
            Left = 3
            Top = 0
            Width = 110
            Height = 17
            Caption = 'Farm Aggregates'
            TabOrder = 0
            OnClick = cbFarmAggregatesClick
          end
        end
      end
    end
  end
  object jvplSeriestypes: TJvPageList
    Left = 0
    Top = 0
    Width = 249
    Height = 421
    ActivePage = jvspDemandSupply
    PropagateEnable = False
    Align = alLeft
    object jvspFarmBudgets: TJvStandardPage
      Left = 0
      Top = 0
      Width = 249
      Height = 421
      Caption = 'jvspFarmBudgets'
      object pnl1: TPanel
        Left = 0
        Top = 0
        Width = 249
        Height = 422
        Align = alClient
        TabOrder = 0
        object spl2: TSplitter
          Left = 1
          Top = 333
          Width = 247
          Height = 3
          Cursor = crVSplit
          Align = alBottom
        end
        object pnl2: TPanel
          Left = 1
          Top = 336
          Width = 247
          Height = 85
          Align = alBottom
          TabOrder = 1
          object spl3: TSplitter
            Left = 130
            Top = 1
            Height = 83
          end
          object pnl3: TPanel
            Left = 1
            Top = 1
            Width = 129
            Height = 83
            Align = alLeft
            TabOrder = 1
            object clbDiscrepancy: TJvCheckListBox
              Left = 1
              Top = 25
              Width = 127
              Height = 57
              OnClickCheck = clbDiscrepancyClickCheck
              Align = alClient
              ItemHeight = 13
              TabOrder = 1
            end
            object pnl4: TPanel
              Left = 1
              Top = 1
              Width = 127
              Height = 24
              Align = alTop
              TabOrder = 0
              object cbDiscrepancy: TCheckBox
                Left = 8
                Top = 0
                Width = 97
                Height = 17
                Caption = '% Discrepancy'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = cbDiscrepancyClick
              end
            end
          end
          object pnl5: TPanel
            Left = 133
            Top = 1
            Width = 113
            Height = 83
            Align = alClient
            TabOrder = 0
            object clbInMinusOut: TJvCheckListBox
              Left = 1
              Top = 25
              Width = 111
              Height = 57
              OnClickCheck = clbInMinusOutClickCheck
              Align = alClient
              ItemHeight = 13
              TabOrder = 1
            end
            object pnl6: TPanel
              Left = 1
              Top = 1
              Width = 111
              Height = 24
              Align = alTop
              TabOrder = 0
              object cbNet: TCheckBox
                Left = 8
                Top = 0
                Width = 97
                Height = 17
                Caption = 'In - Out'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = cbNetClick
              end
            end
          end
        end
        object pnl7: TPanel
          Left = 1
          Top = 1
          Width = 247
          Height = 331
          Align = alClient
          TabOrder = 0
          object spl4: TSplitter
            Left = 130
            Top = 1
            Height = 330
          end
          object pnl8: TPanel
            Left = 133
            Top = 1
            Width = 113
            Height = 330
            Align = alClient
            TabOrder = 1
            object clbOut: TJvCheckListBox
              Left = 1
              Top = 25
              Width = 111
              Height = 304
              OnClickCheck = clbOutClickCheck
              Align = alClient
              ItemHeight = 13
              TabOrder = 1
            end
            object pnl9: TPanel
              Left = 1
              Top = 1
              Width = 111
              Height = 24
              Align = alTop
              TabOrder = 0
              object cbOut: TCheckBox
                Left = 8
                Top = 0
                Width = 97
                Height = 17
                Caption = 'Out'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = cbOutClick
              end
            end
          end
          object pnl10: TPanel
            Left = 1
            Top = 1
            Width = 129
            Height = 329
            Align = alLeft
            TabOrder = 0
            object clbIn: TJvCheckListBox
              Left = 1
              Top = 25
              Width = 127
              Height = 304
              OnClickCheck = clbInClickCheck
              Align = alClient
              ItemHeight = 13
              TabOrder = 1
            end
            object pnl11: TPanel
              Left = 1
              Top = 1
              Width = 127
              Height = 24
              Align = alTop
              TabOrder = 0
              object cbIn: TCheckBox
                Left = 8
                Top = 0
                Width = 97
                Height = 17
                Caption = 'In'
                Checked = True
                State = cbChecked
                TabOrder = 0
                OnClick = cbInClick
              end
            end
          end
        end
      end
    end
    object jvspDemandSupply: TJvStandardPage
      Left = 0
      Top = 0
      Width = 249
      Height = 421
      Caption = 'jvspDemandSupply'
      object pnl19: TPanel
        Left = 0
        Top = 263
        Width = 249
        Height = 158
        Align = alBottom
        TabOrder = 0
        object spl6: TSplitter
          Left = 116
          Top = 1
          Height = 156
        end
        object pnl20: TPanel
          Left = 119
          Top = 1
          Width = 129
          Height = 156
          Align = alClient
          TabOrder = 1
          object clbDefFinal: TJvCheckListBox
            Left = 1
            Top = 25
            Width = 127
            Height = 130
            OnClickCheck = clbDefFinalClickCheck
            Align = alClient
            ItemHeight = 13
            TabOrder = 1
          end
          object pnl21: TPanel
            Left = 1
            Top = 1
            Width = 127
            Height = 24
            Align = alTop
            TabOrder = 0
            object cbDefFinal: TCheckBox
              Left = 8
              Top = 0
              Width = 97
              Height = 17
              Caption = 'Final'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbDefFinalClick
            end
          end
        end
        object pnl22: TPanel
          Left = 1
          Top = 1
          Width = 115
          Height = 156
          Align = alLeft
          TabOrder = 0
          object clbDefInitial: TJvCheckListBox
            Left = 1
            Top = 25
            Width = 113
            Height = 130
            OnClickCheck = clbDefInitialClickCheck
            Align = alClient
            ItemHeight = 13
            TabOrder = 1
          end
          object pnl23: TPanel
            Left = 1
            Top = 1
            Width = 113
            Height = 24
            Align = alTop
            TabOrder = 0
            object cbDefInitial: TCheckBox
              Left = 8
              Top = 0
              Width = 97
              Height = 17
              Caption = 'Initial'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbDefInitialClick
            end
          end
        end
      end
      object pnl24: TPanel
        Left = 0
        Top = 0
        Width = 249
        Height = 57
        Align = alTop
        TabOrder = 1
        object cbDefFlag: TCheckBox
          Left = 8
          Top = 32
          Width = 161
          Height = 17
          Caption = 'Deficiency scenario flag'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = cbDefFlagClick
        end
        object cbOFE: TCheckBox
          Left = 8
          Top = 8
          Width = 209
          Height = 17
          Caption = 'On-farm efficiency'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = cbOFEClick
        end
      end
      object pnl25: TPanel
        Left = 0
        Top = 57
        Width = 249
        Height = 206
        Align = alClient
        TabOrder = 2
        object clbFarmsSupplyDef: TJvCheckListBox
          Left = 1
          Top = 25
          Width = 247
          Height = 180
          OnClickCheck = clbFarmsSupplyDefClickCheck
          Align = alClient
          ItemHeight = 13
          TabOrder = 1
        end
        object pnl26: TPanel
          Left = 1
          Top = 1
          Width = 247
          Height = 24
          Align = alTop
          TabOrder = 0
          object cbFarmsSupplyDef: TCheckBox
            Left = 8
            Top = 0
            Width = 97
            Height = 17
            Caption = 'Farms'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = cbFarmsSupplyDefClick
          end
        end
      end
    end
  end
  object mmFarm: TMainMenu
    Left = 304
    object miFile1: TMenuItem
      Caption = 'File'
      object miOpen: TMenuItem
        Caption = '&Open File'
        OnClick = miOpenClick
      end
      object miPrintSetup1: TMenuItem
        Caption = 'Print &setup'
        OnClick = miPrintSetup1Click
      end
      object miPrintPreview1: TMenuItem
        Caption = 'Print P&review'
        OnClick = miPrintPreview1Click
      end
      object miPrint1: TMenuItem
        Caption = '&Print'
        OnClick = miPrint1Click
      end
      object miSaveasImage1: TMenuItem
        Caption = 'Save as Image'
        OnClick = miSaveasImage1Click
      end
      object miExit1: TMenuItem
        Caption = 'E&xit'
        OnClick = miExit1Click
      end
    end
    object miFormatChart1: TMenuItem
      Caption = 'Format Chart'
      GroupIndex = 2
      OnClick = miFormatChart1Click
    end
    object miHelp1: TMenuItem
      Caption = 'Help'
      GroupIndex = 2
      object miHelp2: TMenuItem
        Caption = 'Help'
        OnClick = miHelp2Click
      end
      object miAbout1: TMenuItem
        Caption = 'About'
        OnClick = miAbout1Click
      end
    end
  end
  object dlgOpenFarm: TOpenDialog
    Filter = 
      'FB_DETAILS.OUT, FB_COMPACT.OUT|FB_DETAILS*.OUT;FB_COMPACT*.OUT|F' +
      'DS.OUT|FDS*.OUT|Any file (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 336
  end
  object ChartEditor1: TChartEditor
    Chart = chtFarmPlot
    Options = [ceChange, ceTitle, ceHelp]
    Title = 'Editing Farm Budget Plot'
    Left = 368
    Top = 1
  end
  object dlgPntSet1: TPrinterSetupDialog
    Left = 400
    Top = 2
  end
  object dlgPnt1: TPrintDialog
    Left = 430
    Top = 2
  end
  object ChartPreviewer1: TChartPreviewer
    Chart = chtFarmPlot
    Left = 464
    Top = 1
  end
  object sd1: TSaveDialog
    DefaultExt = '*.bmp'
    Filter = 
      'bmp files (*.bmp)|*.bmp|enhanced windows metafile (*.emf)|*.emf|' +
      'Windows metafile (*.wmf)|*.wmf'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 498
    Top = 2
  end
end
