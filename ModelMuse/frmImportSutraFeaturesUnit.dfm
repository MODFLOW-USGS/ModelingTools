inherited frmImportSutraFeatures: TfrmImportSutraFeatures
  HelpType = htKeyword
  HelpKeyword = 'Import_SUTRA_Feature_Modified_'
  Caption = 'Import SUTRA Feature Modified by PEST'
  ClientHeight = 297
  ExplicitWidth = 438
  ExplicitHeight = 335
  TextHeight = 18
  object lblModelFeatureFile: TLabel
    Left = 8
    Top = 8
    Width = 112
    Height = 18
    Caption = 'SUTRA Input file'
  end
  object lblTimeStep: TLabel
    Left = 8
    Top = 198
    Width = 68
    Height = 18
    Caption = 'Time step'
  end
  object fedModelFeatureFile: TJvFilenameEdit
    Left = 8
    Top = 32
    Width = 404
    Height = 26
    Filter = 
      'SUTRA input files (*.inp, sutra.fil,*.sutra.fil)|*.inp;sutra.fil' +
      ';*.sutra.fil'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnChange = fedModelFeatureFileChange
    ExplicitWidth = 400
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 256
    Width = 426
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 255
    ExplicitWidth = 422
    DesignSize = (
      426
      41)
    object btnCancel: TBitBtn
      Left = 320
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 316
    end
    object btnOK: TBitBtn
      Left = 224
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Enabled = False
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 220
    end
    object btnHelp: TBitBtn
      Left = 127
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 123
    end
  end
  object clbFeatures: TCheckListBox
    Left = 8
    Top = 64
    Width = 404
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 18
    Items.Strings = (
      'Specified Pressure'
      'Specified Concentration or Temperature'
      'Fluid Flux'
      'Mass or Energy Flux'
      'Generalized Pressure Boundary'
      'Generalized Mass or Energy Boundary')
    TabOrder = 2
    OnClickCheck = clbFeaturesClickCheck
    ExplicitWidth = 400
  end
  object seTimeStep: TJvSpinEdit
    Left = 8
    Top = 222
    Width = 121
    Height = 26
    MaxValue = 2147483647.000000000000000000
    Enabled = False
    TabOrder = 3
  end
end
