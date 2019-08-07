object FormProgramLocations: TFormProgramLocations
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Program_Locations_Dialog_Box'
  Caption = 'Program Locations'
  ClientHeight = 436
  ClientWidth = 588
  Color = clBtnFace
  Constraints.MaxHeight = 474
  Constraints.MinHeight = 470
  Constraints.MinWidth = 596
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object pnlBottom: TPanel
    Left = 0
    Top = 398
    Width = 588
    Height = 38
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      588
      38)
    object btnHelp: TBitBtn
      Left = 497
      Top = 6
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      TabOrder = 2
      Visible = False
      OnClick = btnHelpClick
      Kind = bkHelp
    end
    object btnOK: TBitBtn
      Left = 193
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 0
      OnClick = btnOKClick
      Kind = bkOK
    end
    object btnCancel: TBitBtn
      Left = 275
      Top = 6
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 398
    Align = alClient
    TabOrder = 0
    DesignSize = (
      588
      398)
    object lblModflow: TLabel
      Left = 12
      Top = 105
      Width = 94
      Height = 16
      Caption = 'MODFLOW-2005'
    end
    object JvHTLabel1: TJvHTLabel
      Left = 154
      Top = 105
      Width = 413
      Height = 17
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2005/modflo' +
        'w2005.html">http://water.usgs.gov/nrp/gwsoftware/modflow2005/mod' +
        'flow2005.html</a>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 12
      Top = 47
      Width = 75
      Height = 16
      Caption = 'UCODE_2005'
    end
    object Label2: TLabel
      Left = 12
      Top = 163
      Width = 94
      Height = 16
      Caption = 'MODFLOW-2000'
    end
    object JvHTLabel2: TJvHTLabel
      Left = 154
      Top = 162
      Width = 413
      Height = 17
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2000/modflo' +
        'w2000.html">http://water.usgs.gov/nrp/gwsoftware/modflow2000/mod' +
        'flow2000.html</a>'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object JvHTLabel3: TJvHTLabel
      Left = 154
      Top = 47
      Width = 238
      Height = 17
      Caption = 
        '<a href="http://igwmc.mines.edu/freeware/ucode/">http://igwmc.mi' +
        'nes.edu/freeware/ucode/</a>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object JvHTLabel4: TJvHTLabel
      Left = 154
      Top = 221
      Width = 375
      Height = 17
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/GW_Chart/GW_Chart.' +
        'html">http://water.usgs.gov/nrp/gwsoftware/GW_Chart/GW_Chart.htm' +
        'l</a>'
    end
    object Label3: TLabel
      Left = 12
      Top = 221
      Width = 58
      Height = 16
      Caption = 'GW_Chart'
    end
    object Label4: TLabel
      Left = 12
      Top = 279
      Width = 100
      Height = 16
      Caption = 'Residual_analysis'
    end
    object JvHTLabel5: TJvHTLabel
      Left = 154
      Top = 279
      Width = 238
      Height = 17
      Caption = 
        '<a href="http://igwmc.mines.edu/freeware/ucode/">http://igwmc.mi' +
        'nes.edu/freeware/ucode/</a>'
    end
    object Label5: TLabel
      Left = 12
      Top = 337
      Width = 127
      Height = 16
      Caption = 'Residual_analysis_adv'
    end
    object JvHTLabel6: TJvHTLabel
      Left = 154
      Top = 337
      Width = 238
      Height = 17
      Caption = 
        '<a href="http://igwmc.mines.edu/freeware/ucode/">http://igwmc.mi' +
        'nes.edu/freeware/ucode/</a>'
    end
    object Label6: TLabel
      Left = 12
      Top = 18
      Width = 94
      Height = 16
      Caption = 'Program Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 154
      Top = 18
      Width = 371
      Height = 16
      Caption = 'Web Address for New Versions (click to open in browser)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object fedUcode: TJvFilenameEdit
      Left = 12
      Top = 69
      Width = 561
      Height = 24
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = fedUcodeChange
    end
    object fedModflow2005: TJvFilenameEdit
      Left = 12
      Top = 127
      Width = 561
      Height = 24
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = fedModflow2005Change
    end
    object fedModflow2000: TJvFilenameEdit
      Left = 12
      Top = 185
      Width = 561
      Height = 24
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Enabled = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = fedModflow2000Change
    end
    object fedGWChart: TJvFilenameEdit
      Left = 12
      Top = 243
      Width = 561
      Height = 24
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = fedGWChartChange
    end
    object fedResidAnalysis: TJvFilenameEdit
      Left = 12
      Top = 301
      Width = 561
      Height = 24
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = fedResidAnalysisChange
    end
    object fedResidAnalysisAdv: TJvFilenameEdit
      Left = 12
      Top = 359
      Width = 561
      Height = 24
      DefaultExt = '.exe'
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = fedResidAnalysisAdvChange
    end
  end
end
