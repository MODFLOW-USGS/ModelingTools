object Form1: TForm1
  Left = 198
  Top = 114
  Width = 1009
  Height = 563
  Caption = 'Function plotter'
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
    Top = 16
    Width = 209
    Height = 60
    Caption = 
      'Enter your functions here (9 functions maximum, one function per' +
      ' line)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 170
    Top = 304
    Width = 50
    Height = 20
    Caption = 'points'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 8
    Top = 304
    Width = 73
    Height = 20
    Caption = 'Compute'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Image1: TImage
    Left = 240
    Top = 16
    Width = 750
    Height = 500
  end
  object Memo1: TMemo
    Left = 8
    Top = 88
    Width = 217
    Height = 185
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    Lines.Strings = (
      'x'
      'x^2'
      'x^3')
    ParentFont = False
    ScrollBars = ssHorizontal
    TabOrder = 0
  end
  object SpinEdit1: TSpinEdit
    Left = 88
    Top = 303
    Width = 69
    Height = 26
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'System'
    Font.Style = [fsBold]
    MaxValue = 10000
    MinValue = 2
    ParentFont = False
    TabOrder = 1
    Value = 750
  end
  object Button1: TButton
    Left = 8
    Top = 360
    Width = 217
    Height = 33
    Caption = 'Graph &Options'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 400
    Width = 105
    Height = 33
    Caption = '&Plot'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 8
    Top = 480
    Width = 217
    Height = 33
    Caption = '&Quit'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button3: TButton
    Left = 120
    Top = 400
    Width = 105
    Height = 33
    Caption = '&Save'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = Button3Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'BMP'
    FileName = '*.BMP'
    Filter = 'Bitmap|BMP'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 160
    Top = 440
  end
end
