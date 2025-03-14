inherited frmPhastLocation: TfrmPhastLocation
  HelpType = htKeyword
  HelpKeyword = 'PHAST_Program_Location'
  Caption = 'PHAST Program Location'
  ClientHeight = 131
  ClientWidth = 947
  ExplicitWidth = 959
  ExplicitHeight = 169
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 90
    Width = 947
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 89
    ExplicitWidth = 943
    DesignSize = (
      947
      41)
    object btnHelp: TBitBtn
      Left = 679
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 675
    end
    object btnOK: TBitBtn
      Left = 767
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 763
    end
    object btnCancel: TBitBtn
      Left = 855
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 851
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 947
    Height = 90
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 943
    ExplicitHeight = 89
    DesignSize = (
      947
      90)
    object lblPhast: TLabel
      Left = 16
      Top = 17
      Width = 53
      Height = 18
      Caption = 'PHAST'
    end
    object htlblPhast: TJvHTLabel
      Left = 88
      Top = 17
      Width = 838
      Height = 19
      Caption = 
        '<a href="https://www.usgs.gov/software/phast-computer-program-si' +
        'mulating-groundwater-flow-solute-transport-and-multicomponent">h' +
        'ttps://www.usgs.gov/software/phast-computer-program-simulating-g' +
        'roundwater-flow-solute-transport-and-multicomponent</a>'
      SuperSubScriptRatio = 0.666666666666666600
    end
    object fedPhast: TJvFilenameEdit
      Left = 16
      Top = 40
      Width = 908
      Height = 26
      Filter = 'Batch Files (*.bat)|*.bat|All files (*.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = ''
      OnChange = fedPhastChange
      ExplicitWidth = 904
    end
  end
end
