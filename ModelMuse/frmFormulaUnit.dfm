inherited frmFormula: TfrmFormula
  Left = 273
  Top = 165
  HelpType = htKeyword
  HelpKeyword = 'Formula_Editor'
  ActiveControl = btnEquals
  Caption = 'Formula Editor'
  ClientHeight = 478
  ClientWidth = 765
  Font.Height = 16
  Position = poOwnerFormCenter
  ExplicitWidth = 777
  ExplicitHeight = 516
  TextHeight = 16
  object Splitter: TSplitter
    Left = 539
    Top = 0
    Width = 5
    Height = 437
    OnCanResize = SplitterCanResize
    ExplicitLeft = 476
    ExplicitHeight = 361
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 539
    Height = 437
    Align = alLeft
    ParentColor = True
    TabOrder = 0
    ExplicitHeight = 436
    object JvNetscapeSplitter1: TJvNetscapeSplitter
      Left = 1
      Top = 98
      Width = 537
      Height = 10
      Cursor = crVSplit
      Align = alTop
      MinSize = 1
      Maximized = False
      Minimized = False
      ButtonCursor = crDefault
      ExplicitLeft = 17
      ExplicitTop = 92
      ExplicitWidth = 527
    end
    object pnlButtons: TPanel
      Left = 1
      Top = 191
      Width = 537
      Height = 245
      HelpType = htKeyword
      HelpKeyword = 'Number_and_Operator_Buttons'
      Align = alBottom
      ParentColor = True
      TabOrder = 2
      ExplicitTop = 190
      object gbLogicalOperators: TGroupBox
        Left = 8
        Top = 6
        Width = 249
        Height = 150
        Caption = 'Logical operators'
        TabOrder = 0
        object btnOr: TButton
          Left = 176
          Top = 20
          Width = 63
          Height = 37
          Hint = 'or operator'
          Margins.Top = 20
          Caption = 'or'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = buttonClick
        end
        object btnAnd: TButton
          Left = 107
          Top = 20
          Width = 63
          Height = 37
          Hint = 'and operator'
          Margins.Left = 8
          Margins.Top = 20
          Caption = 'and'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = buttonClick
        end
        object btnNot: TButton
          Left = 107
          Top = 63
          Width = 63
          Height = 37
          Hint = 'not operator'
          Margins.Left = 8
          Caption = 'not'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
          OnClick = buttonClick
        end
        object btnNotEqual: TButton
          Left = 55
          Top = 20
          Width = 41
          Height = 37
          Hint = 'not equals operator'
          Margins.Top = 20
          Caption = '<>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = buttonClick
        end
        object btnEquals: TButton
          Left = 8
          Top = 20
          Width = 41
          Height = 37
          Hint = 'equals operator'
          Margins.Left = 8
          Margins.Top = 20
          Caption = '='
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = buttonClick
        end
        object btnGreaterThan: TButton
          Left = 8
          Top = 63
          Width = 41
          Height = 37
          Hint = 'greater than operator'
          Margins.Left = 8
          Caption = '>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = buttonClick
        end
        object btnLessThan: TButton
          Left = 55
          Top = 63
          Width = 41
          Height = 37
          Hint = 'less than operator'
          Caption = '<'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = buttonClick
        end
        object btnTrue: TButton
          Left = 107
          Top = 106
          Width = 63
          Height = 37
          Margins.Left = 8
          Caption = 'True'
          TabOrder = 10
          OnClick = buttonClick
        end
        object btnFalse: TButton
          Left = 176
          Top = 106
          Width = 63
          Height = 37
          Caption = 'False'
          TabOrder = 11
          OnClick = buttonClick
        end
        object btnLessEquals: TButton
          Left = 55
          Top = 106
          Width = 41
          Height = 37
          Hint = 'less than or equals operator'
          Caption = '<='
          ParentShowHint = False
          ShowHint = True
          TabOrder = 9
          OnClick = buttonClick
        end
        object btnGreaterOrEquals: TButton
          Left = 8
          Top = 106
          Width = 41
          Height = 37
          Hint = 'greater than or equals operator'
          Margins.Left = 8
          Caption = '>='
          ParentShowHint = False
          ShowHint = True
          TabOrder = 8
          OnClick = buttonClick
        end
        object btnXor: TButton
          Left = 176
          Top = 63
          Width = 63
          Height = 37
          Hint = 'xor operator (exclusive or)'
          Caption = 'xor'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
          OnClick = buttonClick
        end
      end
      object gbIntegerOperators: TGroupBox
        Left = 107
        Top = 159
        Width = 150
        Height = 66
        Caption = 'Integer operators'
        TabOrder = 3
        object btnMod: TButton
          Left = 8
          Top = 20
          Width = 63
          Height = 37
          Hint = 'mod operator (remainder)'
          Margins.Left = 8
          Margins.Top = 20
          Caption = 'mod'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = buttonClick
        end
        object btnDiv: TButton
          Left = 77
          Top = 20
          Width = 63
          Height = 37
          Hint = 'div operator (integer division)'
          Margins.Top = 20
          Caption = 'div'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = buttonClick
        end
      end
      object gbNumbers: TGroupBox
        Left = 263
        Top = 6
        Width = 156
        Height = 196
        Caption = 'Numbers'
        TabOrder = 1
        object btn7: TButton
          Left = 8
          Top = 20
          Width = 41
          Height = 37
          Margins.Left = 8
          Margins.Top = 20
          Caption = '7'
          TabOrder = 0
          OnClick = buttonClick
        end
        object btn8: TButton
          Left = 55
          Top = 20
          Width = 41
          Height = 37
          Margins.Top = 20
          Caption = '8'
          TabOrder = 1
          OnClick = buttonClick
        end
        object btn9: TButton
          Left = 102
          Top = 20
          Width = 41
          Height = 37
          Margins.Top = 20
          Caption = '9'
          TabOrder = 2
          OnClick = buttonClick
        end
        object btn6: TButton
          Left = 102
          Top = 63
          Width = 41
          Height = 37
          Caption = '6'
          TabOrder = 5
          OnClick = buttonClick
        end
        object btn5: TButton
          Left = 55
          Top = 63
          Width = 41
          Height = 37
          Caption = '5'
          TabOrder = 4
          OnClick = buttonClick
        end
        object btn4: TButton
          Left = 8
          Top = 63
          Width = 41
          Height = 37
          Margins.Left = 8
          Caption = '4'
          TabOrder = 3
          OnClick = buttonClick
        end
        object btn1: TButton
          Left = 8
          Top = 106
          Width = 41
          Height = 37
          Margins.Left = 8
          Caption = '1'
          TabOrder = 6
          OnClick = buttonClick
        end
        object btn2: TButton
          Left = 55
          Top = 106
          Width = 41
          Height = 37
          Caption = '2'
          TabOrder = 7
          OnClick = buttonClick
        end
        object btn3: TButton
          Left = 102
          Top = 106
          Width = 41
          Height = 37
          Caption = '3'
          TabOrder = 8
          OnClick = buttonClick
        end
        object btn0: TButton
          Left = 8
          Top = 149
          Width = 41
          Height = 37
          Margins.Left = 8
          Caption = '0'
          TabOrder = 9
          OnClick = buttonClick
        end
        object btnE: TButton
          Left = 55
          Top = 149
          Width = 41
          Height = 37
          Caption = 'E'
          TabOrder = 10
          OnClick = buttonClick
        end
        object btnDecimal: TButton
          Left = 102
          Top = 149
          Width = 41
          Height = 37
          Caption = '.'
          TabOrder = 11
          OnClick = buttonClick
        end
      end
      object gbOperators: TGroupBox
        Left = 425
        Top = 6
        Width = 104
        Height = 235
        Caption = 'Operators'
        TabOrder = 2
        object btnOpenParen: TButton
          Left = 8
          Top = 20
          Width = 41
          Height = 37
          Hint = 'Left parenthesis'
          Margins.Left = 8
          Margins.Top = 20
          Caption = '('
          TabOrder = 0
          OnClick = buttonClick
        end
        object btnCloseParen: TButton
          Left = 55
          Top = 20
          Width = 41
          Height = 37
          Hint = 'Right parenthesis'
          Margins.Top = 20
          Caption = ')'
          TabOrder = 1
          OnClick = buttonClick
        end
        object btnDivide: TButton
          Left = 55
          Top = 63
          Width = 41
          Height = 37
          Hint = 'division operator'
          Caption = '/'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnClick = buttonClick
        end
        object btnMultiply: TButton
          Left = 8
          Top = 63
          Width = 41
          Height = 37
          Hint = 'multiplication operator'
          Margins.Left = 8
          Caption = '*'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = buttonClick
        end
        object btnPlus: TButton
          Left = 8
          Top = 106
          Width = 41
          Height = 37
          Hint = 'plus operator'
          Margins.Left = 8
          Caption = '+'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = buttonClick
        end
        object btnMinus: TButton
          Left = 55
          Top = 106
          Width = 41
          Height = 37
          Hint = 'minus operator'
          Caption = '-'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          OnClick = buttonClick
        end
        object btnComma: TButton
          Left = 55
          Top = 149
          Width = 41
          Height = 37
          Hint = 'Comma'
          Caption = ', '
          TabOrder = 7
          OnClick = buttonClick
        end
        object btnQuote: TButton
          Left = 8
          Top = 149
          Width = 41
          Height = 37
          Hint = 'Quote'
          Margins.Left = 8
          Caption = '"'
          TabOrder = 6
          OnClick = buttonClick
        end
        object btnPower1: TButton
          Left = 8
          Top = 192
          Width = 41
          Height = 37
          Hint = 'Quote'
          Margins.Left = 8
          Caption = '^'
          TabOrder = 8
          OnClick = buttonClick
        end
        object btnPower2: TButton
          Left = 55
          Top = 192
          Width = 41
          Height = 37
          Hint = 'Quote'
          Margins.Left = 8
          Caption = '**'
          TabOrder = 9
          OnClick = buttonClick
        end
      end
    end
    object tvFormulaDiagram: TTreeView
      Left = 1
      Top = 1
      Width = 537
      Height = 97
      Hint = 'Diagram of current formula'
      Align = alTop
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnCollapsed = tvFormulaDiagramCollapsed
      OnExpanded = tvFormulaDiagramExpanded
    end
    object jreFormula: TJvRichEdit
      Left = 1
      Top = 108
      Width = 537
      Height = 83
      Hint = 'Type formula here'
      HelpType = htKeyword
      HelpKeyword = 'Formula_Text_Box'
      Align = alClient
      HideSelection = False
      SelText = ''
      TabOrder = 1
      OnChange = jreFormulaChange
      OnDblClick = jreFormulaDblClick
      OnMouseUp = jreFormulaMouseUp
      OnSelectionChange = jreFormulaSelectionChange
      ExplicitHeight = 82
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 437
    Width = 765
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 436
    ExplicitWidth = 761
    DesignSize = (
      765
      41)
    object btnCancel: TBitBtn
      Left = 637
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 3
      ExplicitLeft = 633
    end
    object btnOK: TBitBtn
      Left = 540
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 2
      OnClick = btnOKClick
      ExplicitLeft = 536
    end
    object btnHelp: TBitBtn
      Left = 445
      Top = 4
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnHelpClick
      ExplicitLeft = 441
    end
    object btnFunctionHelp: TBitBtn
      Left = 275
      Top = 4
      Width = 163
      Height = 33
      HelpType = htKeyword
      HelpKeyword = 'Function_Help_Button'
      Anchors = [akTop, akRight]
      Caption = '&Function help'
      Enabled = False
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        04000000000068010000120B0000120B00001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333336633
        3333333333333FF3333333330000333333364463333333333333388F33333333
        00003333333E66433333333333338F38F3333333000033333333E66333333333
        33338FF8F3333333000033333333333333333333333338833333333300003333
        3333446333333333333333FF3333333300003333333666433333333333333888
        F333333300003333333E66433333333333338F38F333333300003333333E6664
        3333333333338F38F3333333000033333333E6664333333333338F338F333333
        0000333333333E6664333333333338F338F3333300003333344333E666433333
        333F338F338F3333000033336664333E664333333388F338F338F33300003333
        E66644466643333338F38FFF8338F333000033333E6666666663333338F33888
        3338F3330000333333EE666666333333338FF33333383333000033333333EEEE
        E333333333388FFFFF8333330000333333333333333333333333388888333333
        0000}
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnFunctionHelpClick
      ExplicitLeft = 271
    end
  end
  object pnlRight: TPanel
    Left = 544
    Top = 0
    Width = 221
    Height = 437
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    ExplicitWidth = 217
    ExplicitHeight = 436
    object pnlLabelItemTree: TPanel
      Left = 0
      Top = 0
      Width = 221
      Height = 41
      Align = alTop
      ParentColor = True
      TabOrder = 0
      ExplicitWidth = 217
      object lbltems: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 213
        Height = 33
        Align = alClient
        Alignment = taCenter
        Caption = 'Double-click to insert into formula'
        WordWrap = True
        ExplicitWidth = 193
        ExplicitHeight = 16
      end
    end
    object tvItems: TTreeView
      Left = 0
      Top = 41
      Width = 221
      Height = 396
      Hint = 'Double-click to insert selected item into formula'
      HelpType = htKeyword
      HelpKeyword = 'List_of_Data_Sets_and_Function'
      Align = alClient
      Indent = 19
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 1
      OnChange = tvItemsChange
      OnDblClick = tvItemsDblClick
      OnMouseDown = tvItemsMouseDown
      ExplicitWidth = 217
      ExplicitHeight = 395
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerSetSelection
    Left = 8
    Top = 8
  end
  object rbFormulaParser: TRbwParser
    Left = 40
    Top = 8
  end
end
