object Form1: TForm1
  Left = 228
  Top = 114
  Width = 561
  Height = 518
  Caption = 'Polynomial Solver'
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
    Width = 521
    Height = 20
    AutoSize = False
    Caption = 
      'Solve polynomial : a(0) + a(1) * x + a(2) * x^2 + ... + a(n) * x' +
      '^n = 0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 10
    Top = 104
    Width = 82
    Height = 16
    Caption = 'Coefficients'
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
    Width = 150
    Height = 16
    Caption = 'Degree of polynomial'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 290
    Top = 104
    Width = 119
    Height = 25
    AutoSize = False
    Caption = 'Roots'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object SpinEdit1: TSpinEdit
    Left = 184
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
    MinValue = 1
    ParentFont = False
    TabOrder = 0
    Value = 6
    OnChange = SpinEdit1Change
  end
  object Button5: TButton
    Left = 288
    Top = 400
    Width = 257
    Height = 33
    Caption = 'S&olve polynomial'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button4: TButton
    Left = 288
    Top = 360
    Width = 257
    Height = 33
    Caption = '&Numeric format'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button6: TButton
    Left = 288
    Top = 440
    Width = 257
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
    Left = 5
    Top = 136
    Width = 248
    Height = 201
    ColCount = 2
    DefaultColWidth = 125
    RowCount = 6
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 3
  end
  object StringGrid2: TStringGrid
    Left = 288
    Top = 136
    Width = 255
    Height = 201
    ColCount = 4
    DefaultColWidth = 125
    FixedCols = 0
    RowCount = 6
    TabOrder = 7
  end
  object Button1: TButton
    Left = 120
    Top = 96
    Width = 65
    Height = 33
    Caption = '&Load'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 188
    Top = 96
    Width = 65
    Height = 33
    Caption = '&Save'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 416
    Top = 96
    Width = 129
    Height = 33
    Caption = 'S&ave roots'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = Button3Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 352
    Width = 249
    Height = 121
    Caption = 'Complex roots'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    object Label5: TLabel
      Left = 16
      Top = 21
      Width = 194
      Height = 48
      Caption = 'A complex root (x + i y) is considered real if abs(y / x) < 10^'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object SpinEdit2: TSpinEdit
      Left = 128
      Top = 82
      Width = 57
      Height = 26
      Font.Charset = ANSI_CHARSET
      Font.Color = clRed
      Font.Height = -16
      Font.Name = 'System'
      Font.Style = [fsBold]
      MaxValue = -1
      MinValue = -20
      ParentFont = False
      TabOrder = 0
      Value = -6
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dat'
    Filter = 'Data files|*.dat|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 416
    Top = 56
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Data files|*.dat|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 464
    Top = 56
  end
  object SaveDialog2: TSaveDialog
    Filter = 'Data files|*.dat|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 512
    Top = 56
  end
end
