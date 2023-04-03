inherited frmSupCalc: TfrmSupCalc
  HelpType = htKeyword
  HelpKeyword = 'SupCalc_Options_Dialog_Box'
  Caption = 'SUPCALC Options'
  ClientHeight = 287
  ClientWidth = 369
  ExplicitWidth = 381
  ExplicitHeight = 325
  TextHeight = 18
  object lblPestFileName: TLabel
    Left = 8
    Top = 8
    Width = 159
    Height = 18
    Caption = 'PEST control file name'
  end
  object lblExpected: TLabel
    Left = 8
    Top = 136
    Width = 345
    Height = 18
    Caption = 'Expected value of measurement objective function'
  end
  object fedPestControlFile: TJvFilenameEdit
    Left = 8
    Top = 32
    Width = 349
    Height = 26
    DefaultExt = '.pst'
    Filter = 'Pest Control File (*.pst)|*.pst'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnChange = fedPestControlFileChange
    ExplicitWidth = 345
  end
  object rgSuperParameterCalculation: TRadioGroup
    Left = 8
    Top = 64
    Width = 353
    Height = 73
    Caption = 'For computation of super parameters use:'
    ItemIndex = 0
    Items.Strings = (
      'SVD on Q^(1/2)X'
      'SVD on XtQX')
    TabOrder = 1
  end
  object btnHelp: TBitBtn
    Left = 79
    Top = 246
    Width = 89
    Height = 33
    HelpType = htKeyword
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 2
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 174
    Top = 246
    Width = 89
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 270
    Top = 248
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 4
  end
  object cbRunPest: TCheckBox
    Left = 8
    Top = 188
    Width = 353
    Height = 26
    Caption = 'Run PEST before SupCalc to generate .jco file'
    TabOrder = 5
  end
  object cbRunSlupCalc: TCheckBox
    Left = 8
    Top = 220
    Width = 217
    Height = 17
    Caption = 'Run SUPCALC'
    TabOrder = 6
  end
  object rdeExpected: TRbwDataEntry
    Left = 8
    Top = 160
    Width = 145
    Height = 22
    TabOrder = 7
    Text = '0'
    OnChange = rdeExpectedChange
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
end
