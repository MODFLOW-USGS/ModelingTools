object frmPiperFormat: TfrmPiperFormat
  Left = 0
  Top = 0
  HelpContext = 630
  BorderStyle = bsDialog
  Caption = 'Format Piper Diagram'
  ClientHeight = 524
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 387
    Height = 483
    ActivePage = tabLegend
    Align = alClient
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 85
        Top = 66
        Width = 70
        Height = 13
        Caption = 'Margin (Pixels)'
      end
      object Label2: TLabel
        Left = 85
        Top = 121
        Width = 68
        Height = 13
        Caption = 'Line Thickness'
      end
      object lblSymbolSize: TLabel
        Left = 85
        Top = 253
        Width = 56
        Height = 13
        Caption = 'Symbol Size'
      end
      object Label3: TLabel
        Left = 85
        Top = 88
        Width = 74
        Height = 26
        Caption = 'Interior Margin (Pixels)'
        WordWrap = True
      end
      object Label4: TLabel
        Left = 85
        Top = 185
        Width = 56
        Height = 26
        Hint = 
          'This is the size of the smallest symbol drawn at each data point' +
          '.'
        Caption = 'Minimum Symbol Size'
        WordWrap = True
      end
      object Label13: TLabel
        Left = 85
        Top = 217
        Width = 56
        Height = 26
        Hint = 'This is the size of the largest symbol drawn at each data point.'
        Caption = 'Maximum Symbol Size'
        WordWrap = True
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 379
        Height = 40
        Align = alTop
        Caption = 'Title'
        TabOrder = 0
        object edTitle: TEdit
          Left = 7
          Top = 13
          Width = 313
          Height = 21
          Hint = 'This is the title that will appear at the top of the graph'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'edTitle'
        end
        object BitBtn4: TBitBtn
          Left = 325
          Top = 13
          Width = 51
          Height = 21
          Hint = 'Click here to set the font for the title'
          Caption = 'Font'
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333333333333333333333333333FFF33FFFFF33333300033000
            00333337773377777333333330333300033333337FF33777F333333330733300
            0333333377FFF777F33333333700000073333333777777773333333333033000
            3333333337FF777F333333333307300033333333377F777F3333333333703007
            33333333377F7773333333333330000333333333337777F33333333333300003
            33333333337777F3333333333337007333333333337777333333333333330033
            3333333333377333333333333333033333333333333733333333333333333333
            3333333333333333333333333333333333333333333333333333}
          NumGlyphs = 2
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = BitBtn4Click
        end
      end
      object gbColors: TGroupBox
        Left = 168
        Top = 118
        Width = 204
        Height = 245
        Hint = 
          'Click one of the buttons to change the color associated with one' +
          ' of the symbols.'
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Symbol Colors'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        object Label5: TLabel
          Left = 86
          Top = 28
          Width = 26
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Circle'
        end
        object shpCircle: TShape
          Left = 61
          Top = 28
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clBlack
        end
        object shpSquare: TShape
          Left = 61
          Top = 61
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clGreen
        end
        object Label6: TLabel
          Left = 87
          Top = 61
          Width = 34
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Square'
        end
        object shpTriangle: TShape
          Left = 61
          Top = 95
          Width = 17
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clNavy
        end
        object Label7: TLabel
          Left = 87
          Top = 95
          Width = 38
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Triangle'
        end
        object shpInvertedTriangle: TShape
          Left = 61
          Top = 130
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clTeal
        end
        object Label8: TLabel
          Left = 87
          Top = 130
          Width = 83
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Inverted Triangle'
        end
        object shpStar: TShape
          Left = 61
          Top = 165
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clBlue
        end
        object Label9: TLabel
          Left = 85
          Top = 165
          Width = 20
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Star'
        end
        object shpCross: TShape
          Left = 61
          Top = 199
          Width = 17
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clLime
        end
        object Label10: TLabel
          Left = 85
          Top = 199
          Width = 27
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Cross'
        end
        object Label11: TLabel
          Left = 87
          Top = 217
          Width = 6
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'X'
        end
        object shpX: TShape
          Left = 61
          Top = 217
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clAqua
        end
        object shpOpenCircle: TShape
          Left = 61
          Top = 43
          Width = 17
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clMaroon
        end
        object Label14: TLabel
          Left = 87
          Top = 43
          Width = 55
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Open Circle'
        end
        object shpOpenSquare: TShape
          Left = 61
          Top = 78
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clOlive
        end
        object Label15: TLabel
          Left = 87
          Top = 78
          Width = 63
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Open Square'
        end
        object shpOpenTriangle: TShape
          Left = 61
          Top = 113
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clPurple
        end
        object Label16: TLabel
          Left = 87
          Top = 113
          Width = 67
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Open Triangle'
        end
        object shpOpenInvertedTriangle: TShape
          Left = 61
          Top = 147
          Width = 17
          Height = 18
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clFuchsia
        end
        object Label17: TLabel
          Left = 86
          Top = 147
          Width = 112
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Open Inverted Triangle'
        end
        object shpOpenStar: TShape
          Left = 61
          Top = 182
          Width = 17
          Height = 17
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Brush.Color = clRed
        end
        object Label18: TLabel
          Left = 85
          Top = 182
          Width = 49
          Height = 13
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Open Star'
        end
        object btnCircle: TButton
          Left = 8
          Top = 28
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 0
          OnClick = btnCircleClick
        end
        object btnOpenCircle: TButton
          Left = 8
          Top = 43
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 1
          OnClick = btnCircleClick
        end
        object btnSquare: TButton
          Left = 8
          Top = 61
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 2
          OnClick = btnCircleClick
        end
        object btnOpenSquare: TButton
          Left = 8
          Top = 78
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 3
          OnClick = btnCircleClick
        end
        object btnTriangle: TButton
          Left = 8
          Top = 95
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 4
          OnClick = btnCircleClick
        end
        object btnOpenTriangle: TButton
          Left = 8
          Top = 113
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 5
          OnClick = btnCircleClick
        end
        object btnInvertedTriangle: TButton
          Left = 8
          Top = 130
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 6
          OnClick = btnCircleClick
        end
        object btnOpenInvertedTriangle: TButton
          Left = 8
          Top = 147
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 7
          OnClick = btnCircleClick
        end
        object btnStar: TButton
          Left = 8
          Top = 165
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 8
          OnClick = btnCircleClick
        end
        object btnOpenStar: TButton
          Left = 8
          Top = 182
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 9
          OnClick = btnCircleClick
        end
        object btnCross: TButton
          Left = 8
          Top = 199
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 10
          OnClick = btnCircleClick
        end
        object btnX: TButton
          Left = 8
          Top = 217
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 11
          OnClick = btnCircleClick
        end
      end
      object rgColor: TRadioGroup
        Left = 168
        Top = 46
        Width = 208
        Height = 65
        Hint = 'This allows you to choose how to set the symbol color'
        Caption = 'Method of Setting Symbol Color'
        ItemIndex = 0
        Items.Strings = (
          'All Black'
          'Linked to Symbol'
          'Set Individually')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = rgColorClick
      end
      object seMargin: TSpinEdit
        Left = 3
        Top = 60
        Width = 76
        Height = 22
        Hint = 
          'This is the space between the chart and the edge of the form or ' +
          'the edge of the paper.'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Value = 20
      end
      object BitBtn3: TBitBtn
        Left = 3
        Top = 281
        Width = 89
        Height = 25
        Hint = 'Click here to set the font of the text on the graph.'
        Caption = 'Font'
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          0400000000000001000000000000000000001000000010000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          3333333333333333333333333333333333333333FFF33FFFFF33333300033000
          00333337773377777333333330333300033333337FF33777F333333330733300
          0333333377FFF777F33333333700000073333333777777773333333333033000
          3333333337FF777F333333333307300033333333377F777F3333333333703007
          33333333377F7773333333333330000333333333337777F33333333333300003
          33333333337777F3333333333337007333333333337777333333333333330033
          3333333333377333333333333333033333333333333733333333333333333333
          3333333333333333333333333333333333333333333333333333}
        NumGlyphs = 2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        OnClick = BitBtn3Click
      end
      object seLineThickness: TSpinEdit
        Left = 3
        Top = 118
        Width = 76
        Height = 22
        Hint = 'This is the size of the symbol drawn at each data point'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Value = 20
      end
      object seSymbolRadius: TSpinEdit
        Left = 3
        Top = 249
        Width = 76
        Height = 22
        Hint = 'This is the thickness of the lines'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        Value = 20
      end
      object cbGridLines: TCheckBox
        Left = 3
        Top = 312
        Width = 97
        Height = 17
        Hint = 'If this is checked, grid lines will be drawn on the graph.'
        Caption = 'Grid Lines'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        OnClick = cbGridLinesClick
      end
      object seInteriorMargin: TSpinEdit
        Left = 3
        Top = 88
        Width = 76
        Height = 22
        Hint = 'This is the space between the triangles and diamond'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Value = 20
      end
      object cbShowNumbers: TCheckBox
        Left = 3
        Top = 40
        Width = 97
        Height = 17
        Hint = 'If this is checked, numbers will be drawn on the axes.'
        Caption = 'Show Numbers'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbSymbolSize: TCheckBox95
        Left = 0
        Top = 152
        Width = 129
        Height = 27
        Hint = 'Make Symbol Size Proportional to TDS'
        Alignment = taLeftJustify
        Caption = 'Make Symbol Size Proportional to TDS'
        TabOrder = 7
        WordWrap = True
        OnClick = cbSymbolSizeClick
        AlignmentBtn = taLeftJustify
        LikePushButton = False
        VerticalAlignment = vaTop
      end
      object seMinSymb: TSpinEdit
        Left = 3
        Top = 185
        Width = 76
        Height = 22
        Hint = 'This is the size of the symbol drawn at each data point'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Value = 0
      end
      object seMaxSymb: TSpinEdit
        Left = 3
        Top = 217
        Width = 76
        Height = 22
        Hint = 'This is the size of the symbol drawn at each data point'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        Value = 0
      end
      object rgGridLines: TRadioGroup
        Left = 8
        Top = 368
        Width = 358
        Height = 81
        Caption = 'Grid Line Spacing'
        Columns = 2
        Enabled = False
        ItemIndex = 4
        Items.Strings = (
          '5%'
          '10%'
          '20%'
          '25%'
          '50%')
        TabOrder = 13
      end
    end
    object tabLegend: TTabSheet
      Caption = 'Legend'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 379
        Height = 41
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 0
        object Label12: TLabel
          Left = 135
          Top = 11
          Width = 129
          Height = 13
          Caption = 'Number of Items in Legend'
        end
        object seLegendCount: TSpinEdit
          Left = 8
          Top = 8
          Width = 121
          Height = 22
          Hint = 'Change this to set the number of items in the legend'
          MaxValue = 0
          MinValue = 0
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 0
          OnChange = seLegendCountChange
        end
      end
      object dgLegend: TEcDataGrid
        Left = 0
        Top = 41
        Width = 379
        Height = 414
        Hint = 'define the items in the legend here.'
        Align = alClient
        Color = clBtnFace
        ColCount = 3
        DefaultRowHeight = 20
        Enabled = False
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnDrawCell = dgLegendDrawCell
        OnSelectCell = dgLegendSelectCell
        OnSetEditText = dgLegendSetEditText
        Columns = <
          item
            ButtonStyle = cbsEllipsis
            Format = cfNumber
            Title.Caption = 'Color'
            Title.WordWrap = False
          end
          item
            LimitToList = True
            Title.Caption = 'Symbol'
            Title.WordWrap = False
          end
          item
            Title.Caption = 'Explanation'
            Title.WordWrap = False
          end>
        RowCountMin = 0
        OnEditButtonClick = dgLegendEditButtonClick
        SelectedIndex = 0
        Version = '2.0'
        ColWidths = (
          77
          150
          216)
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 483
    Width = 387
    Height = 41
    Align = alBottom
    Caption = 'Panel2'
    TabOrder = 1
    DesignSize = (
      387
      41)
    object BitBtn5: TBitBtn
      Left = 139
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object BitBtn1: TBitBtn
      Left = 219
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 1
    end
    object BitBtn2: TBitBtn
      Left = 299
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 256
    Top = 8
  end
  object ColorDialog1: TColorDialog
    Left = 288
    Top = 8
  end
end
