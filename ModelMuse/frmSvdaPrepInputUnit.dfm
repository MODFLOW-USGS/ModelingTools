inherited frmSvdaPrepInput: TfrmSvdaPrepInput
  HelpType = htKeyword
  HelpKeyword = 'SVDAPrep_Input_Dialog_Box'
  Caption = 'SVDAPREP Input'
  ClientHeight = 316
  ClientWidth = 367
  ExplicitWidth = 383
  ExplicitHeight = 355
  PixelsPerInch = 96
  TextHeight = 18
  object lblNumSupParameters: TLabel
    Left = 134
    Top = 202
    Width = 200
    Height = 18
    Caption = 'Number of super-parameters'
  end
  object lblPestFileName: TLabel
    Left = 8
    Top = 8
    Width = 159
    Height = 18
    Caption = 'PEST control file name'
  end
  object rgSuperParameterCalculation: TRadioGroup
    Left = 6
    Top = 64
    Width = 353
    Height = 129
    Caption = 'For computation of super parameters use:'
    ItemIndex = 0
    Items.Strings = (
      'SVD on Q^(1/2)X'
      'SVD on XtQX'
      'LSQR without orthogonalization'
      'LSQR with orthogonalization')
    TabOrder = 0
  end
  object seNumSupParameters: TJvSpinEdit
    Left = 7
    Top = 199
    Width = 121
    Height = 26
    MaxValue = 2147483647.000000000000000000
    MinValue = 1.000000000000000000
    Value = 10.000000000000000000
    TabOrder = 1
  end
  object cbRunSvdaPrep: TCheckBox
    Left = 6
    Top = 231
    Width = 217
    Height = 17
    Caption = 'Run SVDAPREP'
    TabOrder = 2
  end
  object cbRunPest: TCheckBox
    Left = 6
    Top = 257
    Width = 257
    Height = 17
    Caption = 'Run PEST after SVDAPREP'
    TabOrder = 3
  end
  object btnHelp: TBitBtn
    Left = 78
    Top = 280
    Width = 89
    Height = 33
    HelpType = htKeyword
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 4
    OnClick = btnHelpClick
  end
  object btnOK: TBitBtn
    Left = 174
    Top = 280
    Width = 89
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 5
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 270
    Top = 280
    Width = 91
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 6
  end
  object fedPestControlFile: TJvFilenameEdit
    Left = 8
    Top = 32
    Width = 353
    Height = 26
    DefaultExt = '.pst'
    Filter = 'Pest Control File (*.pst)|*.pst'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    Text = ''
    OnChange = fedPestControlFileChange
  end
end
