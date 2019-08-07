object FormParEstAdvanced: TFormParEstAdvanced
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'UCODE Parameter-Estimation Advanced Settings'
  ClientHeight = 478
  ClientWidth = 794
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object lblExpl: TLabel
    Left = 10
    Top = 10
    Width = 744
    Height = 36
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 
      'Changes to quasi-Newton updating and Marquardt Parameter rarely ' +
      'improve performance of the regression algorithm'
    WordWrap = True
  end
  object gpbxMqrt: TGroupBox
    Left = 10
    Top = 275
    Width = 775
    Height = 126
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Marquardt Parameter'
    TabOrder = 1
    object Label1: TLabel
      Left = 102
      Top = 26
      Width = 312
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MqrtDirection: Marquardt Direction (degrees)'
    end
    object Label7: TLabel
      Left = 102
      Top = 61
      Width = 204
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MqrtFactor: Marquardt Factor'
    end
    object lblMqrtInc: TLabel
      Left = 102
      Top = 93
      Width = 248
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MqrtIncrement: Marquardt Increment'
    end
    object edtMqrtDir: TEdit
      Left = 13
      Top = 21
      Width = 82
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 0
      Text = '85.4'
    end
    object edtMqrtFac: TEdit
      Left = 13
      Top = 53
      Width = 82
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
      Text = '1.5'
    end
    object edtMqrtInc: TEdit
      Left = 13
      Top = 86
      Width = 82
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
      Text = '0.001'
    end
  end
  object GroupBox1: TGroupBox
    Left = 9
    Top = 89
    Width = 776
    Height = 178
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Quasi-Newton Updating'
    TabOrder = 0
    object lblQNiter: TLabel
      Left = 102
      Top = 53
      Width = 599
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        'QNiter: Number of parameter-estimation iterations before using q' +
        'uasi-Newton updating'
      WordWrap = True
    end
    object lblQNsosr: TLabel
      Left = 103
      Top = 115
      Width = 522
      Height = 36
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        'QNsosr: Fractional change in sum-of-squares objective functionov' +
        'er two parameter-estimation iterations below which quasi-Newton ' +
        'updating is used'
      WordWrap = True
    end
    object ckbxQuasiNewton: TCheckBox
      Left = 13
      Top = 21
      Width = 219
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Use quasi-Newton updating'
      TabOrder = 0
      OnClick = ckbxQuasiNewtonClick
    end
    object edtQNsosr: TEdit
      Left = 13
      Top = 115
      Width = 82
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Enabled = False
      TabOrder = 1
      Text = '0.01'
    end
    object spedQNiter: TSpinEdit
      Left = 13
      Top = 50
      Width = 82
      Height = 28
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
  end
  object btnResetDefault: TButton
    Left = 10
    Top = 410
    Width = 232
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Reset All Values to Defaults'
    TabOrder = 2
    OnClick = btnResetDefaultClick
  end
  object btnCancel: TBitBtn
    Left = 395
    Top = 444
    Width = 90
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object btnOK: TBitBtn
    Left = 299
    Top = 444
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnOKClick
  end
end
