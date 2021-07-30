inherited frmImportSutraFeatures: TfrmImportSutraFeatures
  Caption = 'Import SUTRA Feature Modified by PEST'
  ClientHeight = 243
  ExplicitHeight = 282
  PixelsPerInch = 96
  TextHeight = 18
  object lblModelFeatureFile: TLabel
    Left = 8
    Top = 8
    Width = 112
    Height = 18
    Caption = 'SUTRA Input file'
  end
  object fedModelFeatureFile: TJvFilenameEdit
    Left = 8
    Top = 32
    Width = 408
    Height = 26
    Filter = 'SUTRA input files (*.inp)|*.inp'
    TabOrder = 0
    Text = ''
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 202
    Width = 424
    Height = 41
    Align = alBottom
    ParentColor = True
    TabOrder = 1
    ExplicitTop = 133
    DesignSize = (
      424
      41)
    object btnCancel: TBitBtn
      Left = 324
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 227
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 131
      Top = 2
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
  end
  object clbFeatures: TCheckListBox
    Left = 8
    Top = 64
    Width = 408
    Height = 121
    ItemHeight = 18
    Items.Strings = (
      'Specified Pressure'
      'Specified Concentration or Temperature'
      'Fluid Flux'
      'Mass or Energy Flux'
      'Generalized Pressure Boundary'
      'Generalized Mass or Energy Boundary')
    TabOrder = 2
  end
end
