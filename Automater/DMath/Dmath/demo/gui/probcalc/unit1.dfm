object Form1: TForm1
  Left = 285
  Top = 146
  Width = 401
  Height = 438
  Caption = 'Probability Calculator'
  Color = clNavy
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'System'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 16
  object LabelFormula: TLabel
    Left = 15
    Top = 11
    Width = 378
    Height = 22
    AutoSize = False
    Caption = 'Enter a formula, then press <Return> or click "Evaluate"'
    Color = clNavy
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object LabelResult: TLabel
    Left = 15
    Top = 114
    Width = 280
    Height = 16
    AutoSize = False
    Caption = 'Result'
    Color = clNavy
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object EditFormula: TEdit
    Left = 7
    Top = 40
    Width = 378
    Height = 26
    AutoSelect = False
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnKeyDown = EditFormulaKeyDown
  end
  object EditResult: TEdit
    Left = 7
    Top = 137
    Width = 378
    Height = 24
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'System'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    TabOrder = 3
  end
  object BtnEval: TButton
    Left = 7
    Top = 72
    Width = 186
    Height = 25
    Caption = 'Evaluate'
    TabOrder = 1
    OnClick = BtnEvalClick
  end
  object BtnClear: TButton
    Left = 200
    Top = 72
    Width = 185
    Height = 25
    Caption = 'Clear'
    TabOrder = 2
    OnClick = BtnClearClick
  end
  object BtnPBinom: TButton
    Left = 8
    Top = 192
    Width = 67
    Height = 25
    Hint = 'pBinom(n, p, k) = Probability of binomial dist.'
    Caption = 'pBinom'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    OnClick = ButtonClick
  end
  object BtnFBinom: TButton
    Left = 8
    Top = 224
    Width = 67
    Height = 25
    Hint = 'fBinom(n, p, k) = Cumulative probability of binomial dist.'
    Caption = 'fBinom'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    OnClick = ButtonClick
  end
  object BtnPPoisson: TButton
    Left = 86
    Top = 192
    Width = 67
    Height = 25
    Hint = 'pPoisson(mu, k) = Probability of Poisson dist.'
    Caption = 'pPoisson'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = ButtonClick
  end
  object BtnFPoisson: TButton
    Left = 86
    Top = 224
    Width = 67
    Height = 25
    Hint = 'fPoisson(mu, k) = Cumulative probability of Poisson dist.'
    Caption = 'fPoisson'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = ButtonClick
  end
  object BtnDExpo: TButton
    Left = 164
    Top = 192
    Width = 67
    Height = 25
    Hint = 'dExpo(a, x) = PDF of exponential dist.'
    Caption = 'dExpo'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = ButtonClick
  end
  object BtnFExpo: TButton
    Left = 164
    Top = 224
    Width = 67
    Height = 25
    Hint = 'fExpo(a, x) = CDF of exponential dist.'
    Caption = 'fExpo'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnClick = ButtonClick
  end
  object BtnDGamma: TButton
    Left = 242
    Top = 192
    Width = 67
    Height = 25
    Hint = 'dGamma(a, b, x) = PDF of Gamma dist.'
    Caption = 'dGamma'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = ButtonClick
  end
  object BtnFGamma: TButton
    Left = 242
    Top = 224
    Width = 67
    Height = 25
    Hint = 'fGamma(a, b, x) = CDF of Gamma dist.'
    Caption = 'fGamma'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 11
    OnClick = ButtonClick
  end
  object BtnDBeta: TButton
    Left = 320
    Top = 192
    Width = 67
    Height = 25
    Hint = 'dBeta(a, b, x) = PDF of Beta dist.'
    Caption = 'dBeta'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 12
    OnClick = ButtonClick
  end
  object BtnFBeta: TButton
    Left = 320
    Top = 224
    Width = 67
    Height = 25
    Hint = 'fBeta(a, b, x) = CDF of Beta dist.'
    Caption = 'fBeta'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnClick = ButtonClick
  end
  object BtnDNorm: TButton
    Left = 8
    Top = 272
    Width = 90
    Height = 25
    Hint = 'dNorm(x) = PDF of standard normal dist.'
    Caption = 'dNorm'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 14
    OnClick = ButtonClick
  end
  object BtnFNorm: TButton
    Left = 8
    Top = 304
    Width = 90
    Height = 25
    Hint = 'fNorm(x) = CDF of standard normal dist.'
    Caption = 'fNorm'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 15
    OnClick = ButtonClick
  end
  object BtnPNorm: TButton
    Left = 8
    Top = 336
    Width = 90
    Height = 25
    Hint = 'pNorm(x) = Prob(abs(U) > x) for standard normal dist.'
    Caption = 'pNorm'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 16
    OnClick = ButtonClick
  end
  object BtnInvNorm: TButton
    Left = 8
    Top = 368
    Width = 90
    Height = 25
    Hint = 'invNorm(p) = inverse of CDF of standard normal dist.'
    Caption = 'invNorm'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 17
    OnClick = ButtonClick
  end
  object BtnDStudent: TButton
    Left = 104
    Top = 272
    Width = 90
    Height = 25
    Hint = 'dStudent(n, x) = PDF of Student dist. with n d.o.f.'
    Caption = 'dStudent'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 18
    OnClick = ButtonClick
  end
  object BtnFStudent: TButton
    Left = 104
    Top = 304
    Width = 90
    Height = 25
    Hint = 'fStudent(n, x) = CDF of Student dist. with n d.o.f.'
    Caption = 'fStudent'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 19
    OnClick = ButtonClick
  end
  object BtnPStudent: TButton
    Left = 104
    Top = 336
    Width = 90
    Height = 25
    Hint = 
      'pStudent(n, x) = Prob(abs(T) > x) for Student dist. with n d.o.f' +
      '.'
    Caption = 'pStudent'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 20
    OnClick = ButtonClick
  end
  object BtnInvStudent: TButton
    Left = 104
    Top = 368
    Width = 90
    Height = 25
    Hint = 'invStudent(n, p) = inverse of CDF of Student dist. with n d.o.f.'
    Caption = 'invStudent'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 21
    OnClick = ButtonClick
  end
  object BtnDKhi2: TButton
    Left = 200
    Top = 272
    Width = 90
    Height = 25
    Hint = 'dKhi2(n, x) = PDF of Khi2 dist. with n d.o.f.'
    Caption = 'dKhi2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 22
    OnClick = ButtonClick
  end
  object BtnFKhi2: TButton
    Left = 200
    Top = 304
    Width = 90
    Height = 25
    Hint = 'fKhi2(n, x) = CDF of Khi2 dist. with n d.o.f.'
    Caption = 'fKhi2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 23
    OnClick = ButtonClick
  end
  object BtnPKhi2: TButton
    Left = 200
    Top = 336
    Width = 90
    Height = 25
    Hint = 'pKhi2(n, x) = Prob(Khi2 > x) for Khi2 dist. with n d.o.f.'
    Caption = 'pKhi2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 24
    OnClick = ButtonClick
  end
  object BtnInvKhi2: TButton
    Left = 200
    Top = 368
    Width = 90
    Height = 25
    Hint = 'invKhi2(n, p) = inverse of CDF of Khi2 dist. with n d.o.f.'
    Caption = 'invKhi2'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 25
    OnClick = ButtonClick
  end
  object BtnDSnedecor: TButton
    Left = 296
    Top = 272
    Width = 90
    Height = 25
    Hint = 
      'dSnedecor(n1, n2, x) = PDF of Snedecor dist. with n1 and n2 d.o.' +
      'f.'
    Caption = 'dSnedecor'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 26
    OnClick = ButtonClick
  end
  object BtnFSnedecor: TButton
    Left = 296
    Top = 304
    Width = 90
    Height = 25
    Hint = 
      'fSnedecor(n1, n2, x) = CDF of Snedecor dist. with n1 and n2 d.o.' +
      'f.'
    Caption = 'fSnedecor'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 27
    OnClick = ButtonClick
  end
  object BtnPSnedecor: TButton
    Left = 296
    Top = 336
    Width = 90
    Height = 25
    Hint = 
      'pSnedecor(n1, n2, x) = Prob(F > x) for Snedecor dist. with n1 an' +
      'd n2 d.o.f.'
    Caption = 'pSnedecor'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 28
    OnClick = ButtonClick
  end
  object BtnInvSnedecor: TButton
    Left = 296
    Top = 368
    Width = 90
    Height = 25
    Hint = 
      'invSnedecor(n1, n2, p) = inverse of CDF of Snedecor dist. with n' +
      '1 and n2 d.o.f.'
    Caption = 'invSnedecor'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 29
    OnClick = ButtonClick
  end
end
