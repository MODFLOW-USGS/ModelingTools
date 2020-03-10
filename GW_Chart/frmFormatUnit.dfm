object frmFormat: TfrmFormat
  Left = 521
  Top = 182
  Width = 561
  Height = 642
  HelpContext = 20
  Caption = 'Format Chart'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object BitBtn1: TBitBtn
    Left = 336
    Top = 560
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object BitBtn2: TBitBtn
    Left = 416
    Top = 560
    Width = 75
    Height = 25
    TabOrder = 3
    OnClick = BitBtn2Click
    Kind = bkOK
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 545
    Height = 553
    ActivePage = tabGeneral
    Align = alTop
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      object Label2: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 19
        Caption = 'Title'
      end
      object Label10: TLabel
        Left = 80
        Top = 460
        Width = 79
        Height = 19
        Caption = 'Left Margin'
      end
      object Label11: TLabel
        Left = 80
        Top = 492
        Width = 90
        Height = 19
        Caption = 'Right Margin'
      end
      object Label12: TLabel
        Left = 248
        Top = 460
        Width = 81
        Height = 19
        Caption = 'Top Margin'
      end
      object Label13: TLabel
        Left = 248
        Top = 492
        Width = 104
        Height = 19
        Caption = 'Bottom Margin'
      end
      object memoTitle: TMemo
        Left = 8
        Top = 24
        Width = 417
        Height = 73
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object GroupBox1: TGroupBox
        Left = 8
        Top = 104
        Width = 473
        Height = 153
        Caption = 'X Axis'
        TabOrder = 2
        object Label3: TLabel
          Left = 8
          Top = 35
          Width = 31
          Height = 19
          Caption = 'Title'
        end
        object Label1: TLabel
          Left = 120
          Top = 69
          Width = 70
          Height = 19
          Caption = 'Beginning'
        end
        object lblXMax: TLabel
          Left = 200
          Top = 70
          Width = 27
          Height = 19
          Caption = 'End'
        end
        object Label6: TLabel
          Left = 280
          Top = 70
          Width = 86
          Height = 19
          Caption = 'Tick interval'
        end
        object Label8: TLabel
          Left = 176
          Top = 124
          Width = 50
          Height = 19
          Caption = 'Format'
        end
        object edXAxisTitle: TEdit
          Left = 48
          Top = 24
          Width = 361
          Height = 41
          AutoSize = False
          TabOrder = 0
          Text = 'edXAxisTitle'
        end
        object cbAutoX: TCheckBox
          Left = 8
          Top = 72
          Width = 105
          Height = 17
          Caption = 'Automatic'
          TabOrder = 2
          OnClick = cbAutoXClick
        end
        object adeXMin: TArgusDataEntry
          Left = 120
          Top = 88
          Width = 73
          Height = 22
          ItemHeight = 19
          TabOrder = 3
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object adeXMax: TArgusDataEntry
          Left = 200
          Top = 89
          Width = 73
          Height = 22
          ItemHeight = 19
          TabOrder = 5
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object adeXTickInterval: TArgusDataEntry
          Left = 280
          Top = 89
          Width = 73
          Height = 22
          ItemHeight = 19
          TabOrder = 6
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object cbXGrid: TCheckBox
          Left = 8
          Top = 91
          Width = 105
          Height = 17
          Caption = 'Show Grid'
          TabOrder = 7
        end
        object Button2: TButton
          Left = 416
          Top = 32
          Width = 49
          Height = 25
          Caption = 'Font'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 384
          Top = 88
          Width = 81
          Height = 25
          Caption = 'Tick Font'
          TabOrder = 4
          OnClick = Button3Click
        end
        object cbXTick: TCheckBox
          Left = 8
          Top = 110
          Width = 105
          Height = 17
          Caption = 'Show Ticks'
          TabOrder = 8
        end
        object cbXLog: TCheckBox
          Left = 8
          Top = 128
          Width = 105
          Height = 17
          Caption = 'Logarithmic'
          TabOrder = 10
        end
        object edXFormat: TEdit
          Left = 232
          Top = 120
          Width = 121
          Height = 27
          TabOrder = 9
          Text = 'edXFormat'
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 264
        Width = 473
        Height = 153
        Caption = 'Y Axis'
        TabOrder = 3
        object Label5: TLabel
          Left = 8
          Top = 35
          Width = 31
          Height = 19
          Caption = 'Title'
        end
        object lblYMin: TLabel
          Left = 120
          Top = 69
          Width = 70
          Height = 19
          Caption = 'Beginning'
        end
        object Label4: TLabel
          Left = 200
          Top = 69
          Width = 27
          Height = 19
          Caption = 'End'
        end
        object Label7: TLabel
          Left = 280
          Top = 72
          Width = 86
          Height = 19
          Caption = 'Tick interval'
        end
        object Label9: TLabel
          Left = 176
          Top = 124
          Width = 50
          Height = 19
          Caption = 'Format'
        end
        object edYAxisTitle: TEdit
          Left = 48
          Top = 24
          Width = 361
          Height = 41
          AutoSize = False
          TabOrder = 0
          Text = 'edYAxisTitle'
        end
        object cbAutoY: TCheckBox
          Left = 8
          Top = 72
          Width = 97
          Height = 17
          Caption = 'Automatic'
          TabOrder = 2
          OnClick = cbAutoYClick
        end
        object adeYMin: TArgusDataEntry
          Left = 120
          Top = 88
          Width = 73
          Height = 22
          ItemHeight = 19
          TabOrder = 3
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object adeYMax: TArgusDataEntry
          Left = 200
          Top = 88
          Width = 73
          Height = 22
          ItemHeight = 19
          TabOrder = 4
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
        object adeYTickInterval: TArgusDataEntry
          Left = 280
          Top = 89
          Width = 73
          Height = 22
          ItemHeight = 19
          TabOrder = 6
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object cbYGrid: TCheckBox
          Left = 8
          Top = 91
          Width = 97
          Height = 17
          Caption = 'Show Grid'
          TabOrder = 7
        end
        object Button4: TButton
          Left = 384
          Top = 88
          Width = 81
          Height = 25
          Caption = 'Tick Font'
          TabOrder = 5
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 416
          Top = 32
          Width = 49
          Height = 25
          Caption = 'Font'
          TabOrder = 1
          OnClick = Button5Click
        end
        object cbYTick: TCheckBox
          Left = 8
          Top = 110
          Width = 105
          Height = 17
          Caption = 'Show Ticks'
          TabOrder = 8
        end
        object cbYLog: TCheckBox
          Left = 8
          Top = 128
          Width = 105
          Height = 17
          Caption = 'Logarithmic'
          TabOrder = 10
        end
        object edYFormat: TEdit
          Left = 232
          Top = 120
          Width = 121
          Height = 27
          TabOrder = 9
          Text = 'edYFormat'
        end
      end
      object Button1: TButton
        Left = 432
        Top = 48
        Width = 49
        Height = 25
        Caption = 'Font'
        TabOrder = 1
        OnClick = Button1Click
      end
      object cbLegend: TCheckBox
        Left = 8
        Top = 424
        Width = 81
        Height = 25
        Caption = 'Legend'
        TabOrder = 4
      end
      object btnLegendFont: TButton
        Left = 88
        Top = 424
        Width = 105
        Height = 25
        Caption = 'Legend Font'
        TabOrder = 5
        OnClick = btnLegendFontClick
      end
      object seLeft: TSpinEdit
        Left = 8
        Top = 456
        Width = 65
        Height = 26
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 6
        Value = 0
      end
      object seRight: TSpinEdit
        Left = 8
        Top = 488
        Width = 65
        Height = 26
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 8
        Value = 0
      end
      object seTop: TSpinEdit
        Left = 176
        Top = 456
        Width = 65
        Height = 26
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 7
        Value = 0
      end
      object seBottom: TSpinEdit
        Left = 176
        Top = 488
        Width = 65
        Height = 26
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        MaxValue = 0
        MinValue = 0
        ParentFont = False
        TabOrder = 9
        Value = 0
      end
    end
    object tabLineSeries: TTabSheet
      Caption = 'Line Series'
      ImageIndex = 1
      object dgLineSeries: TDataGrid
        Left = 0
        Top = 41
        Width = 537
        Height = 478
        Align = alClient
        ColCount = 9
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
        TabOrder = 1
        OnDrawCell = dgLineSeriesDrawCell
        OnMouseDown = dgLineSeriesMouseDown
        OnSelectCell = dgLineSeriesSelectCell
        OnSetEditText = dgLineSeriesSetEditText
        Columns = <
          item
            Title.Caption = 'Series'
            Title.WordWrap = False
          end
          item
            LimitToList = True
            PickList.Strings = (
              'No'
              'Yes')
            Title.Caption = 'Visible'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'Width'
            Title.WordWrap = False
          end
          item
            ButtonStyle = cbsEllipsis
            Format = cfNumber
            Title.Caption = 'Line Color'
            Title.WordWrap = False
          end
          item
            LimitToList = True
            PickList.Strings = (
              'No'
              'Yes')
            Title.Caption = 'Show Symbol'
            Title.WordWrap = False
          end
          item
            LimitToList = True
            Title.Caption = 'Symbol'
            Title.WordWrap = False
          end
          item
            ButtonStyle = cbsEllipsis
            Format = cfNumber
            Title.Caption = 'Symbol Color'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'Symb. Hor. Size'
            Title.WordWrap = False
          end
          item
            Format = cfNumber
            Title.Caption = 'Symb. Vert. Size'
            Title.WordWrap = False
          end>
        RowCountMin = 0
        OnEditButtonClick = dgLineSeriesEditButtonClick
        SelectedIndex = 0
        Version = '2.0'
        ColWidths = (
          64
          64
          103
          96
          123
          123
          122
          124
          64)
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 537
        Height = 41
        Align = alTop
        TabOrder = 0
        object cbMultiSelect: TCheckBox
          Left = 8
          Top = 8
          Width = 169
          Height = 17
          Caption = 'Select Multiple Cells'
          TabOrder = 0
          OnClick = cbMultiSelectClick
        end
        object adeValue: TArgusDataEntry
          Left = 184
          Top = 8
          Width = 145
          Height = 22
          Color = clBtnFace
          Enabled = False
          ItemHeight = 19
          TabOrder = 1
          Text = '0'
          OnChange = adeValueChange
          DataType = dtInteger
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
    end
  end
  object BitBtn3: TBitBtn
    Left = 256
    Top = 560
    Width = 75
    Height = 25
    TabOrder = 1
    Kind = bkHelp
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 40
    Top = 560
  end
  object ColorDialog1: TColorDialog
    Left = 8
    Top = 552
  end
end
