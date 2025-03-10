object Form1: TForm1
  Left = 198
  Top = 114
  Width = 916
  Height = 563
  Caption = 'Quadratic Iterator: x'#39' = a.x.(1 - x) - Bifurcation diagram'
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
  object Image1: TImage
    Left = 168
    Top = 16
    Width = 732
    Height = 500
  end
  object Button2: TButton
    Left = 8
    Top = 56
    Width = 137
    Height = 33
    Caption = '&Graph options'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 8
    Top = 136
    Width = 137
    Height = 33
    Caption = '&Quit'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button3: TButton
    Left = 8
    Top = 96
    Width = 137
    Height = 33
    Caption = '&Save'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 137
    Height = 33
    Caption = '&Bifurcation'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 184
    Width = 137
    Height = 169
    Caption = 'Parameters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 28
      Width = 56
      Height = 16
      Caption = 'a1 [1..4]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 68
      Width = 65
      Height = 16
      Caption = 'a2 (a1..4]'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label3: TLabel
      Left = 8
      Top = 108
      Width = 54
      Height = 16
      Caption = 'x0 (0..1)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Edit1: TEdit
      Left = 80
      Top = 24
      Width = 49
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      Text = '1'
    end
    object Edit2: TEdit
      Left = 80
      Top = 64
      Width = 49
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      Text = '4'
    end
    object Edit3: TEdit
      Left = 80
      Top = 104
      Width = 49
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Text = '0.5'
    end
    object CheckBox1: TCheckBox
      Left = 80
      Top = 136
      Width = 49
      Height = 17
      Caption = 'Rnd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 368
    Width = 137
    Height = 65
    Caption = 'Hide first points'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object Label4: TLabel
      Left = 8
      Top = 33
      Width = 54
      Height = 16
      Caption = 'Nb. pts.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SpinEdit1: TSpinEdit
      Left = 72
      Top = 23
      Width = 57
      Height = 26
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Increment = 100
      MaxValue = 1000
      MinValue = 100
      ParentFont = False
      TabOrder = 0
      Value = 200
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 448
    Width = 137
    Height = 65
    Caption = 'Show next points'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    object Label5: TLabel
      Left = 8
      Top = 33
      Width = 54
      Height = 16
      Caption = 'Nb. pts.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object SpinEdit2: TSpinEdit
      Left = 72
      Top = 23
      Width = 57
      Height = 26
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      Increment = 100
      MaxValue = 1000
      MinValue = 100
      ParentFont = False
      TabOrder = 0
      Value = 200
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'BMP'
    FileName = '*.BMP'
    Filter = 'Bitmap|BMP'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 200
    Top = 40
  end
end
