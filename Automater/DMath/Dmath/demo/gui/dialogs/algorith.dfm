object AlgorithmDlg: TAlgorithmDlg
  Left = 352
  Top = 222
  BorderStyle = bsDialog
  Caption = 'Algorithms'
  ClientHeight = 264
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object TabbedNotebook1: TTabbedNotebook
    Left = 0
    Top = 0
    Width = 427
    Height = 264
    Align = alClient
    TabFont.Charset = DEFAULT_CHARSET
    TabFont.Color = clBtnText
    TabFont.Height = -12
    TabFont.Name = 'MS Sans Serif'
    TabFont.Style = []
    TabOrder = 0
    object TTabPage
      Left = 4
      Top = 27
      Caption = '&Linear regression'
      object RadioGroup0: TRadioGroup
        Left = 8
        Top = 8
        Width = 241
        Height = 65
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ItemIndex = 1
        Items.Strings = (
          '&Gauss-Jordan method'
          '&Singular value decomposition (SVD)')
        ParentFont = False
        TabOrder = 0
        OnClick = RadioGroup0Click
      end
      object GroupBox0: TGroupBox
        Left = 8
        Top = 80
        Width = 241
        Height = 65
        Caption = 'SVD Options'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object Label0: TLabel
          Left = 8
          Top = 36
          Width = 91
          Height = 13
          Caption = 'Tolerance : 10^'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object SpinEdit0: TSpinEdit
          Left = 105
          Top = 31
          Width = 57
          Height = 22
          Font.Charset = ANSI_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = -1
          MinValue = -20
          ParentFont = False
          TabOrder = 0
          Value = -15
        end
      end
    end
    object TTabPage
      Left = 4
      Top = 27
      Caption = '&Nonlinear regression'
      object RadioGroup1: TRadioGroup
        Left = 8
        Top = 8
        Width = 153
        Height = 217
        Caption = 'Optimization'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ItemIndex = 0
        Items.Strings = (
          '&Marquardt'
          '&Simplex'
          '&BFGS'
          'Simulated &annealing'
          '&Genetic algorithm'
          'Metropolis-&Hastings')
        ParentFont = False
        TabOrder = 0
        OnClick = RadioGroup1Click
      end
      object GroupBox1: TGroupBox
        Left = 168
        Top = 144
        Width = 153
        Height = 81
        Caption = 'Initial parameters'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        object CheckBox1: TCheckBox
          Left = 8
          Top = 24
          Width = 73
          Height = 17
          Caption = '&Estimate'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object CheckBox2: TCheckBox
          Left = 8
          Top = 48
          Width = 89
          Height = 17
          Caption = 'Sh&ow'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
      end
      object GroupBox3: TGroupBox
        Left = 168
        Top = 8
        Width = 153
        Height = 217
        Caption = 'SA Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        Visible = False
        object Label6: TLabel
          Left = 8
          Top = 110
          Width = 70
          Height = 26
          Caption = 'T reduction factor (%)'
          WordWrap = True
        end
        object Label4: TLabel
          Left = 8
          Top = 46
          Width = 62
          Height = 26
          Caption = 'Loops at constant T'
          WordWrap = True
        end
        object Label5: TLabel
          Left = 8
          Top = 78
          Width = 79
          Height = 26
          Caption = 'Loops before step adjust.'
          WordWrap = True
        end
        object Label3: TLabel
          Left = 8
          Top = 24
          Width = 38
          Height = 13
          Caption = 'Cycles'
        end
        object SpinEdit6: TSpinEdit
          Left = 96
          Top = 113
          Width = 49
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 4
          Value = 90
        end
        object CheckBox4: TCheckBox
          Left = 8
          Top = 184
          Width = 105
          Height = 17
          Caption = '&Log file'
          TabOrder = 0
        end
        object SpinEdit3: TSpinEdit
          Left = 96
          Top = 19
          Width = 49
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = 10
          MinValue = 1
          ParentFont = False
          TabOrder = 1
          Value = 1
        end
        object SpinEdit4: TSpinEdit
          Left = 96
          Top = 49
          Width = 49
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 2
          Value = 5
        end
        object SpinEdit5: TSpinEdit
          Left = 96
          Top = 81
          Width = 49
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 3
          Value = 15
        end
      end
      object GroupBox2: TGroupBox
        Left = 168
        Top = 8
        Width = 153
        Height = 129
        Caption = 'Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        object Label1: TLabel
          Left = 8
          Top = 28
          Width = 54
          Height = 13
          Caption = 'Max. &iter.'
        end
        object Label2: TLabel
          Left = 8
          Top = 56
          Width = 83
          Height = 13
          Caption = '&Tolerance 10^'
        end
        object CheckBox3: TCheckBox
          Left = 8
          Top = 96
          Width = 105
          Height = 17
          Caption = '&Log file'
          TabOrder = 0
        end
        object SpinEdit1: TSpinEdit
          Left = 80
          Top = 23
          Width = 65
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 1000
          MaxValue = 10000
          MinValue = 0
          ParentFont = False
          TabOrder = 1
          Value = 1000
        end
        object SpinEdit2: TSpinEdit
          Left = 96
          Top = 51
          Width = 49
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = -1
          MinValue = -10
          ParentFont = False
          TabOrder = 2
          Value = -6
        end
      end
      object GroupBox4: TGroupBox
        Left = 168
        Top = 8
        Width = 153
        Height = 217
        Caption = 'GA Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        Visible = False
        object Label7: TLabel
          Left = 8
          Top = 22
          Width = 61
          Height = 26
          Caption = 'Population size'
          WordWrap = True
        end
        object Label8: TLabel
          Left = 8
          Top = 54
          Width = 67
          Height = 26
          Caption = 'Max. nb. of generations'
          WordWrap = True
        end
        object Label9: TLabel
          Left = 8
          Top = 86
          Width = 47
          Height = 26
          Caption = 'Survival rate (%)'
          WordWrap = True
        end
        object Label10: TLabel
          Left = 8
          Top = 118
          Width = 50
          Height = 26
          Caption = 'Mutation rate (%)'
          WordWrap = True
        end
        object Label11: TLabel
          Left = 8
          Top = 150
          Width = 76
          Height = 26
          Caption = 'Homozygotes (%)'
          WordWrap = True
        end
        object SpinEdit7: TSpinEdit
          Left = 88
          Top = 20
          Width = 57
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 1000
          MinValue = 100
          ParentFont = False
          TabOrder = 1
          Value = 200
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 192
          Width = 105
          Height = 17
          Caption = '&Log file'
          TabOrder = 0
        end
        object SpinEdit8: TSpinEdit
          Left = 88
          Top = 52
          Width = 57
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 100
          MinValue = 10
          ParentFont = False
          TabOrder = 2
          Value = 40
        end
        object SpinEdit9: TSpinEdit
          Left = 88
          Top = 88
          Width = 57
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 3
          Value = 60
        end
        object SpinEdit10: TSpinEdit
          Left = 88
          Top = 120
          Width = 57
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 4
          Value = 10
        end
        object SpinEdit11: TSpinEdit
          Left = 88
          Top = 152
          Width = 57
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 5
          Value = 50
        end
      end
      object GroupBox5: TGroupBox
        Left = 168
        Top = 8
        Width = 153
        Height = 217
        Caption = ' MCMC Options'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlack
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        Visible = False
        object Label12: TLabel
          Left = 8
          Top = 24
          Width = 38
          Height = 13
          Caption = 'Cycles'
        end
        object Label15: TLabel
          Left = 8
          Top = 160
          Width = 139
          Height = 13
          Caption = 'Save simulations in file :'
        end
        object Label13: TLabel
          Left = 8
          Top = 56
          Width = 54
          Height = 13
          Caption = 'Max. sim.'
        end
        object Label14: TLabel
          Left = 8
          Top = 88
          Width = 63
          Height = 13
          Caption = 'Saved sim.'
        end
        object Button1: TButton
          Left = 8
          Top = 176
          Width = 137
          Height = 25
          Caption = 'mcmc.txt'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clNavy
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 0
          OnClick = Button1Click
        end
        object SpinEdit12: TSpinEdit
          Left = 80
          Top = 19
          Width = 65
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          MaxValue = 100
          MinValue = 1
          ParentFont = False
          TabOrder = 1
          Value = 10
        end
        object SpinEdit13: TSpinEdit
          Left = 80
          Top = 51
          Width = 65
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 10000
          MinValue = 100
          ParentFont = False
          TabOrder = 2
          Value = 1000
        end
        object SpinEdit14: TSpinEdit
          Left = 80
          Top = 83
          Width = 65
          Height = 22
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          Increment = 100
          MaxValue = 10000
          MinValue = 100
          ParentFont = False
          TabOrder = 3
          Value = 1000
        end
      end
    end
  end
  object OKBtn: TBitBtn
    Left = 336
    Top = 50
    Width = 77
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = OKBtnClick
    Kind = bkOK
    Margin = 2
    Spacing = -1
  end
  object CancelBtn: TBitBtn
    Left = 336
    Top = 82
    Width = 77
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    Kind = bkCancel
    Margin = 2
    Spacing = -1
  end
  object HelpBtn: TBitBtn
    Left = 336
    Top = 114
    Width = 77
    Height = 27
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = HelpBtnClick
    Kind = bkHelp
    Margin = 2
    Spacing = -1
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'TXT'
    Filter = 'Text file|*.TXT'
    Options = [ofOverwritePrompt, ofHideReadOnly]
    Left = 360
    Top = 168
  end
end
