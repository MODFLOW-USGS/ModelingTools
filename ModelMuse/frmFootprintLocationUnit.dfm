inherited frmFootprintLocation: TfrmFootprintLocation
  HelpType = htKeyword
  HelpKeyword = 'Footprint_Program_Location'
  Caption = 'WellFootprint Program Location'
  ClientHeight = 175
  ClientWidth = 604
  ExplicitWidth = 620
  ExplicitHeight = 213
  PixelsPerInch = 96
  TextHeight = 18
  object htlblSutra22: TJvHTLabel
    Left = 121
    Top = 17
    Width = 239
    Height = 19
    Caption = 
      '<a href="https://doi.org/10.5066/F70C4TQ8">https://doi.org/10.50' +
      '66/F70C4TQ8</a>'
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
    Width = 580
    Height = 26
    Filter = 
      'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
      '.*)|*.*'
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = ''
    OnChange = fedFootprintChange
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 134
    Width = 604
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      604
      41)
    object btnHelp: TBitBtn
      Left = 340
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
      Left = 428
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
      Left = 516
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
    Left = 16
    Top = 94
    Width = 580
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
