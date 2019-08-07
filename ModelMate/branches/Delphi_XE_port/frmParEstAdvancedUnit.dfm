object FormParEstAdvanced: TFormParEstAdvanced
  Left = 0
  Top = 0
  Caption = 'UCODE Parameter-Estimation Advanced Settings'
  ClientHeight = 335
  ClientWidth = 699
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object lblExpl: TLabel
    Left = 8
    Top = 8
    Width = 674
    Height = 16
    Caption = 
      'Changes to quasi-Newton updating and Marquardt Parameter rarely ' +
      'improve performance of the regression algorithm'
  end
  object gpbxMqrt: TGroupBox
    Left = 8
    Top = 161
    Width = 677
    Height = 106
    Caption = 'Marquardt Parameter'
    TabOrder = 1
    object Label1: TLabel
      Left = 86
      Top = 22
      Width = 258
      Height = 16
      Caption = 'MqrtDirection: Marquardt Direction (degrees)'
    end
    object Label7: TLabel
      Left = 86
      Top = 51
      Width = 170
      Height = 16
      Caption = 'MqrtFactor: Marquardt Factor'
    end
    object lblMqrtInc: TLabel
      Left = 86
      Top = 78
      Width = 214
      Height = 16
      Caption = 'MqrtIncrement: Marquardt Increment'
    end
    object edtMqrtDir: TEdit
      Left = 11
      Top = 18
      Width = 69
      Height = 24
      TabOrder = 0
      Text = '85.4'
    end
    object edtMqrtFac: TEdit
      Left = 11
      Top = 45
      Width = 69
      Height = 24
      TabOrder = 1
      Text = '1.5'
    end
    object edtMqrtInc: TEdit
      Left = 11
      Top = 72
      Width = 69
      Height = 24
      TabOrder = 2
      Text = '0.001'
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 34
    Width = 677
    Height = 114
    Caption = 'Quasi-Newton Updating'
    TabOrder = 0
    object lblQNiter: TLabel
      Left = 86
      Top = 45
      Width = 500
      Height = 16
      Caption = 
        'QNiter: Number of parameter-estimation iterations before using q' +
        'uasi-Newton updating'
      WordWrap = True
    end
    object lblQNsosr: TLabel
      Left = 86
      Top = 72
      Width = 435
      Height = 32
      Caption = 
        'QNsosr: Fractional change in sum-of-squares objective functionov' +
        'er two parameter-estimation iterations below which quasi-Newton ' +
        'updating is used'
      WordWrap = True
    end
    object ckbxQuasiNewton: TCheckBox
      Left = 11
      Top = 18
      Width = 184
      Height = 17
      Caption = 'Use quasi-Newton updating'
      TabOrder = 0
      OnClick = ckbxQuasiNewtonClick
    end
    object edtQNsosr: TEdit
      Left = 11
      Top = 76
      Width = 69
      Height = 24
      Enabled = False
      TabOrder = 1
      Text = '0.01'
    end
    object spedQNiter: TSpinEdit
      Left = 11
      Top = 42
      Width = 69
      Height = 26
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
    end
  end
  object btnResetDefault: TButton
    Left = 8
    Top = 287
    Width = 173
    Height = 30
    Caption = 'Reset All Values to Defaults'
    TabOrder = 2
    OnClick = btnResetDefaultClick
  end
  object btnCancel: TBitBtn
    Left = 333
    Top = 288
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 252
    Top = 288
    Width = 75
    Height = 25
    TabOrder = 3
    OnClick = btnOKClick
    Kind = bkOK
  end
end
