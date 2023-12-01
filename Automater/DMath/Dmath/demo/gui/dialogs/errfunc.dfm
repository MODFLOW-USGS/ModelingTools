object ErrFuncDlg: TErrFuncDlg
  Left = 226
  Top = 99
  ActiveControl = OKBtn
  BorderStyle = bsDialog
  Caption = 'Error model'
  ClientHeight = 322
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object OKBtn: TBitBtn
    Left = 204
    Top = 8
    Width = 77
    Height = 27
    TabOrder = 0
    OnClick = OKBtnClick
    Kind = bkOK
    Margin = 2
    Spacing = -1
    IsControl = True
  end
  object CancelBtn: TBitBtn
    Left = 204
    Top = 40
    Width = 77
    Height = 27
    TabOrder = 1
    Kind = bkCancel
    Margin = 2
    Spacing = -1
    IsControl = True
  end
  object HelpBtn: TBitBtn
    Left = 204
    Top = 72
    Width = 77
    Height = 27
    TabOrder = 2
    OnClick = HelpBtnClick
    Kind = bkHelp
    Margin = 2
    Spacing = -1
    IsControl = True
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 177
    Height = 145
    Caption = 'Function'
    ItemIndex = 0
    Items.Strings = (
      '&Constant'
      '&Linear'
      '&Polynomial, degree 2'
      'P&olynomial, degree 3'
      '&Exponential'
      'Po&wer')
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 160
    Width = 273
    Height = 41
    Caption = 'Formula'
    TabOrder = 4
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 249
      Height = 13
      AutoSize = False
      Caption = 's = e0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object GroupBoxVar: TGroupBox
    Left = 8
    Top = 208
    Width = 273
    Height = 105
    Caption = 'Parameters (e0 is estimated by the program)'
    TabOrder = 5
    Visible = False
    object LabelVar1: TLabel
      Left = 8
      Top = 24
      Width = 15
      Height = 13
      Caption = 'e1'
    end
    object LabelVar2: TLabel
      Left = 8
      Top = 48
      Width = 15
      Height = 13
      Caption = 'e2'
    end
    object LabelVar3: TLabel
      Left = 8
      Top = 72
      Width = 15
      Height = 13
      Caption = 'e3'
    end
    object Label2: TLabel
      Left = 96
      Top = 56
      Width = 115
      Height = 13
      Caption = 'Number of iterations'
      Visible = False
    end
    object EditVar1: TEdit
      Left = 32
      Top = 24
      Width = 49
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      Text = '1'
    end
    object EditVar2: TEdit
      Left = 32
      Top = 48
      Width = 49
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      Text = '1'
    end
    object EditVar3: TEdit
      Left = 32
      Top = 72
      Width = 49
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      Text = '1'
    end
    object CheckBox1: TCheckBox
      Left = 96
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Fit the parameter(s)'
      TabOrder = 3
      Visible = False
    end
    object SpinEdit1: TSpinEdit
      Left = 216
      Top = 48
      Width = 41
      Height = 22
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      MaxValue = 10
      MinValue = 1
      ParentFont = False
      TabOrder = 4
      Value = 5
      Visible = False
    end
  end
  object RadioGroup2: TRadioGroup
    Left = 200
    Top = 112
    Width = 81
    Height = 41
    Caption = 'Variable'
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      '&x'
      '&y')
    TabOrder = 6
    Visible = False
    OnClick = RadioGroup2Click
  end
end
