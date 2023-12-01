object Form1: TForm1
  Left = 231
  Top = 114
  Width = 564
  Height = 735
  Caption = 'Linear System Solver'
  Color = clNavy
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 257
    Height = 20
    AutoSize = False
    Caption = 'Solve linear system : A * X = B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 10
    Top = 112
    Width = 121
    Height = 16
    Caption = 'System matrix (A)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 110
    Height = 16
    Caption = 'Order of system'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 418
    Top = 112
    Width = 119
    Height = 25
    AutoSize = False
    Caption = 'Constants (B)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label5: TLabel
    Left = 274
    Top = 416
    Width = 119
    Height = 25
    AutoSize = False
    Caption = 'Solution (X)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label6: TLabel
    Left = 418
    Top = 416
    Width = 119
    Height = 25
    AutoSize = False
    Caption = 'Check (A * X = B)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object SpinEdit1: TSpinEdit
    Left = 128
    Top = 55
    Width = 69
    Height = 26
    EditorEnabled = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'System'
    Font.Style = [fsBold]
    MaxValue = 20
    MinValue = 2
    ParentFont = False
    TabOrder = 0
    Value = 3
    OnChange = SpinEdit1Change
  end
  object Button3: TButton
    Left = 16
    Top = 656
    Width = 233
    Height = 33
    Caption = 'S&olve system'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = Button3Click
  end
  object Button5: TButton
    Left = 16
    Top = 608
    Width = 233
    Height = 33
    Caption = '&Numeric format'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 424
    Top = 656
    Width = 121
    Height = 33
    Caption = '&Quit'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = Button6Click
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 136
    Width = 388
    Height = 201
    ColCount = 3
    DefaultColWidth = 125
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 1
  end
  object StringGrid2: TStringGrid
    Left = 418
    Top = 136
    Width = 129
    Height = 201
    ColCount = 1
    DefaultColWidth = 125
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 352
    Width = 265
    Height = 33
    Caption = '&Load system'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 288
    Top = 352
    Width = 265
    Height = 33
    Caption = '&Save system'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 280
    Top = 656
    Width = 121
    Height = 33
    Caption = 'S&ave solution'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = Button4Click
  end
  object StringGrid3: TStringGrid
    Left = 274
    Top = 440
    Width = 129
    Height = 201
    ColCount = 1
    DefaultColWidth = 125
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    TabOrder = 10
  end
  object RadioGroup1: TRadioGroup
    Left = 16
    Top = 432
    Width = 233
    Height = 145
    Caption = 'Algorithm'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemIndex = 0
    Items.Strings = (
      'Gauss-Jordan'
      'LU'
      'QR'
      'SVD')
    ParentFont = False
    TabOrder = 5
  end
  object StringGrid4: TStringGrid
    Left = 418
    Top = 440
    Width = 129
    Height = 201
    ColCount = 1
    DefaultColWidth = 125
    FixedCols = 0
    RowCount = 3
    FixedRows = 0
    TabOrder = 11
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dat'
    Filter = 'Data files|*.dat|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 296
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Data files|*.dat|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
    Top = 8
  end
  object SaveDialog2: TSaveDialog
    Filter = 'Data files|*.dat|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 376
    Top = 8
  end
end
