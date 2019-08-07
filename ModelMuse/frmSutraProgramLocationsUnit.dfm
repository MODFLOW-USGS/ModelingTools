inherited frmSutraProgramLocations: TfrmSutraProgramLocations
  HelpType = htKeyword
  HelpKeyword = 'SUTRA_Program_Locations_Dialog'
  Caption = 'SUTRA Program Location'
  ClientHeight = 183
  ClientWidth = 613
  ExplicitWidth = 629
  ExplicitHeight = 222
  PixelsPerInch = 96
  TextHeight = 18
  object htlblSutra22: TJvHTLabel
    Left = 105
    Top = 17
    Width = 237
    Height = 19
    Caption = 
      '<a href="http://www.usgs.gov/software/sutra">http://www.usgs.gov' +
      '/software/sutra</a>'
    SuperSubScriptRatio = 0.666666666666666600
  end
  object lblSutra22: TLabel
    Left = 16
    Top = 17
    Width = 78
    Height = 18
    Caption = 'SUTRA 2.2'
  end
  object lblTextEditor: TLabel
    Left = 17
    Top = 73
    Width = 71
    Height = 18
    Caption = 'Text editor'
  end
  object fedSutra22: TJvFilenameEdit
    Left = 16
    Top = 38
    Width = 588
    Height = 26
    Filter = 
      'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
      '.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnChange = fedSutra22Change
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 142
    Width = 613
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      613
      41)
    object btnHelp: TBitBtn
      Left = 349
      Top = 6
      Width = 82
      Height = 27
      HelpType = htKeyword
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 437
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 525
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object fedTextEditor: TJvFilenameEdit
    Left = 17
    Top = 94
    Width = 588
    Height = 26
    Filter = 
      'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
      '.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = ''
    OnChange = fedTextEditorChange
  end
end
