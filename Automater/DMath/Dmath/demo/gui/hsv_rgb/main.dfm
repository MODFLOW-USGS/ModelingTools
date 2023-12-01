object Form1: TForm1
  Left = 472
  Top = 280
  Width = 498
  Height = 243
  Caption = 'HSV / RGB Converter'
  Color = clNavy
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 88
    Height = 16
    Caption = 'Hue (0 - 360)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 98
    Height = 16
    Caption = 'Saturation (%)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 96
    Width = 68
    Height = 16
    Caption = 'Value (%)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 440
    Top = 16
    Width = 30
    Height = 16
    Caption = 'Red'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 440
    Top = 56
    Width = 43
    Height = 16
    Caption = 'Green'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label6: TLabel
    Left = 440
    Top = 96
    Width = 32
    Height = 16
    Caption = 'Blue'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Shape1: TShape
    Left = 216
    Top = 16
    Width = 113
    Height = 105
    Shape = stRoundSquare
  end
  object Label7: TLabel
    Left = 229
    Top = 144
    Width = 92
    Height = 16
    Caption = 'Hexadecimal'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SpinEdit1: TSpinEdit
    Left = 112
    Top = 15
    Width = 69
    Height = 26
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 360
    MinValue = 0
    ParentFont = False
    TabOrder = 0
    Value = 0
  end
  object SpinEdit2: TSpinEdit
    Left = 112
    Top = 55
    Width = 69
    Height = 26
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 100
    MinValue = 0
    ParentFont = False
    TabOrder = 1
    Value = 0
  end
  object SpinEdit3: TSpinEdit
    Left = 112
    Top = 95
    Width = 69
    Height = 26
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 100
    MinValue = 0
    ParentFont = False
    TabOrder = 2
    Value = 100
  end
  object SpinEdit4: TSpinEdit
    Left = 360
    Top = 15
    Width = 69
    Height = 26
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 255
    MinValue = 0
    ParentFont = False
    TabOrder = 4
    Value = 255
  end
  object SpinEdit5: TSpinEdit
    Left = 360
    Top = 55
    Width = 69
    Height = 26
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 255
    MinValue = 0
    ParentFont = False
    TabOrder = 5
    Value = 255
  end
  object SpinEdit6: TSpinEdit
    Left = 360
    Top = 95
    Width = 69
    Height = 26
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    MaxValue = 255
    MinValue = 0
    ParentFont = False
    TabOrder = 6
    Value = 255
  end
  object Button1: TButton
    Left = 56
    Top = 160
    Width = 125
    Height = 33
    Caption = 'HSV ----> RGB'
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
    Left = 360
    Top = 160
    Width = 121
    Height = 33
    Caption = 'RGB ----> HSV'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 228
    Top = 168
    Width = 89
    Height = 28
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 8
    Text = '#FFFFFF'
  end
end
