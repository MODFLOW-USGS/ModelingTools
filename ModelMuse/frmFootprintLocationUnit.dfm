inherited frmFootprintLocation: TfrmFootprintLocation
  HelpType = htKeyword
  HelpKeyword = 'Footprint_Program_Location'
  Caption = 'WellFootprint Program Location'
  ClientHeight = 175
  ClientWidth = 604
  ExplicitWidth = 616
  ExplicitHeight = 213
  TextHeight = 18
  object htlblSutra22: TJvHTLabel
    Left = 121
    Top = 17
    Width = 239
    Height = 19
    Caption = 
      '<a href="https://doi.org/10.5066/F70C4TQ8">https://doi.org/10.50' +
      '66/F70C4TQ8</a>'
    SuperSubScriptRatio = 0.666666666666666600
  end
  object lblFootprint: TLabel
    Left = 16
    Top = 17
    Width = 92
    Height = 18
    Caption = 'WellFootprint'
  end
  object lblTextEditor: TLabel
    Left = 16
    Top = 73
    Width = 71
    Height = 18
    Caption = 'Text editor'
  end
  object fedFootprint: TJvFilenameEdit
    Left = 16
    Top = 41
    Width = 576
    Height = 26
    Filter = 
      'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
      '.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnChange = fedFootprintChange
    ExplicitWidth = 572
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 134
    Width = 604
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 133
    ExplicitWidth = 600
    DesignSize = (
      604
      41)
    object btnHelp: TBitBtn
      Left = 336
      Top = 6
      Width = 82
      Height = 27
      HelpType = htKeyword
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 332
    end
    object btnOK: TBitBtn
      Left = 424
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 420
    end
    object btnCancel: TBitBtn
      Left = 512
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 508
    end
  end
  object fedTextEditor: TJvFilenameEdit
    Left = 16
    Top = 94
    Width = 576
    Height = 26
    Filter = 
      'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
      '.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = ''
    OnChange = fedTextEditorChange
    ExplicitWidth = 572
  end
end
