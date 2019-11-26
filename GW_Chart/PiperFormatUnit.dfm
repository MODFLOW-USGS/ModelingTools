object frmPiperFormat: TfrmPiperFormat
  Left = 410
  Top = 140
  HelpContext = 630
  BorderStyle = bsDialog
  Caption = 'Format Piper Diagram'
  ClientHeight = 523
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 479
    Height = 482
    ActivePage = tabLegend
    Align = alClient
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 104
        Top = 85
        Width = 85
        Height = 16
        Caption = 'Margin (Pixels)'
      end
      object Label2: TLabel
        Left = 104
        Top = 149
        Width = 83
        Height = 16
        Caption = 'Line Thickness'
      end
      object lblSymbolSize: TLabel
        Left = 104
        Top = 285
        Width = 70
        Height = 16
        Caption = 'Symbol Size'
      end
      object Label3: TLabel
        Left = 104
        Top = 109
        Width = 85
        Height = 32
        Caption = 'Interior Margin (Pixels)'
        WordWrap = True
      end
      object Label4: TLabel
        Left = 104
        Top = 211
        Width = 70
        Height = 32
        Caption = 'Minimum Symbol Size'
        WordWrap = True
      end
      object Label13: TLabel
        Left = 104
        Top = 245
        Width = 70
        Height = 32
        Caption = 'Maximum Symbol Size'
        WordWrap = True
      end
      object GroupBox2: TGroupBox
        Left = 0
        Top = 0
        Width = 471
        Height = 49
        Align = alTop
        Caption = 'Title'
        TabOrder = 0
        object edTitle: TEdit
          Left = 8
          Top = 16
          Width = 385
          Height = 24
          Hint = 'This is the title that will appear at the top of the graph'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'edTitle'
        end
        object BitBtn4: TBitBtn
          Left = 400
          Top = 16
          Width = 57
          Height = 25
          Hint = 'Click here to set the font for the title'
          Caption = 'Font'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = BitBtn4Click
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
        end
      end
      object gbColors: TGroupBox
        Left = 248
        Top = 129
        Width = 217
        Height = 225
        Hint = 
          'Click one of the buttons to change the color associated with one' +
          ' of the symbols.'
        Caption = 'Symbol Colors'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        object Label5: TLabel
          Left = 80
          Top = 24
          Width = 32
          Height = 16
          Caption = 'Circle'
        end
        object shpCircle: TShape
          Left = 56
          Top = 24
          Width = 16
          Height = 16
          Brush.Color = clBlack
        end
        object Label6: TLabel
          Left = 80
          Top = 56
          Width = 41
          Height = 16
          Caption = 'Square'
        end
        object shpSquare: TShape
          Left = 56
          Top = 56
          Width = 16
          Height = 16
          Brush.Color = clGreen
        end
        object Label7: TLabel
          Left = 80
          Top = 88
          Width = 47
          Height = 16
          Caption = 'Triangle'
        end
        object shpTriangle: TShape
          Left = 56
          Top = 88
          Width = 16
          Height = 16
          Brush.Color = clNavy
        end
        object Label8: TLabel
          Left = 80
          Top = 120
          Width = 98
          Height = 16
          Caption = 'Inverted Triangle'
        end
        object shpInvertedTriangle: TShape
          Left = 56
          Top = 120
          Width = 16
          Height = 16
          Brush.Color = clTeal
        end
        object Label9: TLabel
          Left = 80
          Top = 152
          Width = 24
          Height = 16
          Caption = 'Star'
        end
        object shpStar: TShape
          Left = 56
          Top = 152
          Width = 16
          Height = 16
          Brush.Color = clBlue
        end
        object Label10: TLabel
          Left = 80
          Top = 184
          Width = 32
          Height = 16
          Caption = 'Cross'
        end
        object Label11: TLabel
          Left = 80
          Top = 200
          Width = 8
          Height = 16
          Caption = 'X'
        end
        object shpX: TShape
          Left = 56
          Top = 200
          Width = 16
          Height = 16
          Brush.Color = clAqua
        end
        object shpCross: TShape
          Left = 56
          Top = 184
          Width = 16
          Height = 16
          Brush.Color = clLime
        end
        object shpOpenCircle: TShape
          Left = 56
          Top = 40
          Width = 16
          Height = 16
          Brush.Color = clMaroon
        end
        object Label14: TLabel
          Left = 80
          Top = 40
          Width = 66
          Height = 16
          Caption = 'Open Circle'
        end
        object shpOpenSquare: TShape
          Left = 56
          Top = 72
          Width = 16
          Height = 16
          Brush.Color = clOlive
        end
        object Label15: TLabel
          Left = 80
          Top = 72
          Width = 75
          Height = 16
          Caption = 'Open Square'
        end
        object shpOpenTriangle: TShape
          Left = 56
          Top = 104
          Width = 16
          Height = 16
          Brush.Color = clPurple
        end
        object Label16: TLabel
          Left = 80
          Top = 104
          Width = 81
          Height = 16
          Caption = 'Open Triangle'
        end
        object shpOpenInvertedTriangle: TShape
          Left = 56
          Top = 136
          Width = 16
          Height = 16
          Brush.Color = clFuchsia
        end
        object Label17: TLabel
          Left = 80
          Top = 136
          Width = 132
          Height = 16
          Caption = 'Open Inverted Triangle'
        end
        object shpOpenStar: TShape
          Left = 56
          Top = 168
          Width = 16
          Height = 16
          Brush.Color = clRed
        end
        object Label18: TLabel
          Left = 80
          Top = 168
          Width = 58
          Height = 16
          Caption = 'Open Star'
        end
        object btnCircle: TButton
          Left = 8
          Top = 24
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 0
          OnClick = btnCircleClick
        end
        object btnSquare: TButton
          Left = 8
          Top = 56
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 1
          OnClick = btnCircleClick
        end
        object btnTriangle: TButton
          Left = 8
          Top = 88
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 2
          OnClick = btnCircleClick
        end
        object btnInvertedTriangle: TButton
          Left = 8
          Top = 120
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 3
          OnClick = btnCircleClick
        end
        object btnStar: TButton
          Left = 8
          Top = 152
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 4
          OnClick = btnCircleClick
        end
        object btnCross: TButton
          Left = 8
          Top = 184
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 5
          OnClick = btnCircleClick
        end
        object btnX: TButton
          Left = 8
          Top = 200
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 6
          OnClick = btnCircleClick
        end
        object btnOpenCircle: TButton
          Left = 8
          Top = 40
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 7
          OnClick = btnCircleClick
        end
        object btnOpenSquare: TButton
          Left = 8
          Top = 72
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 8
          OnClick = btnCircleClick
        end
        object btnOpenTriangle: TButton
          Left = 8
          Top = 104
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 9
          OnClick = btnCircleClick
        end
        object btnOpenInvertedTriangle: TButton
          Left = 8
          Top = 136
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 10
          OnClick = btnCircleClick
        end
        object btnOpenStar: TButton
          Left = 8
          Top = 168
          Width = 41
          Height = 16
          Caption = 'Edit'
          Enabled = False
          TabOrder = 11
          OnClick = btnCircleClick
        end
      end
      object rgColor: TRadioGroup
        Left = 248
        Top = 56
        Width = 217
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
        Left = 8
        Top = 80
        Width = 89
        Height = 26
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
        Left = 8
        Top = 312
        Width = 89
        Height = 25
        Hint = 'Click here to set the font of the text on the graph.'
        Caption = 'Font'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        OnClick = BitBtn3Click
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
      end
      object seLineThickness: TSpinEdit
        Left = 8
        Top = 144
        Width = 89
        Height = 26
        Hint = 'This is the thickness of the lines'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        Value = 0
      end
      object seSymbolRadius: TSpinEdit
        Left = 8
        Top = 280
        Width = 89
        Height = 26
        Hint = 'This is the size of the symbol drawn at each data point'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        Value = 0
      end
      object cbGridLines: TCheckBox
        Left = 8
        Top = 344
        Width = 73
        Height = 13
        Hint = 'If this is checked, grid lines will be drawn on the graph.'
        Caption = 'Grid Lines'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        OnClick = cbGridLinesClick
      end
      object seInteriorMargin: TSpinEdit
        Left = 8
        Top = 112
        Width = 89
        Height = 26
        Hint = 'This is the space between the triangles and diamond'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Value = 20
      end
      object cbShowNumbers: TCheckBox
        Left = 8
        Top = 56
        Width = 105
        Height = 17
        Hint = 'If this is checked, numbers will be drawn on the axes.'
        Caption = 'Show Numbers'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object cbSymbolSize: TCheckBox95
        Left = 8
        Top = 176
        Width = 145
        Height = 33
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
        Left = 8
        Top = 216
        Width = 89
        Height = 26
        Hint = 'This is the size of the symbol drawn at each data point'
        MaxValue = 0
        MinValue = 0
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Value = 0
      end
      object seMaxSymb: TSpinEdit
        Left = 8
        Top = 248
        Width = 89
        Height = 26
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
        Top = 360
        Width = 457
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
      object dgLegend: TDataGrid
        Left = 0
        Top = 33
        Width = 471
        Height = 418
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
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 471
        Height = 33
        Align = alTop
        BevelOuter = bvLowered
        TabOrder = 0
        object Label12: TLabel
          Left = 96
          Top = 9
          Width = 155
          Height = 16
          Caption = 'Number of Items in Legend'
        end
        object seLegendCount: TSpinEdit
          Left = 4
          Top = 4
          Width = 89
          Height = 26
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
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 482
    Width = 479
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      479
      41)
    object BitBtn5: TBitBtn
      Left = 240
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 0
      Kind = bkHelp
    end
    object BitBtn1: TBitBtn
      Left = 320
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 1
      Kind = bkCancel
    end
    object BitBtn2: TBitBtn
      Left = 400
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 2
      Kind = bkOK
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
