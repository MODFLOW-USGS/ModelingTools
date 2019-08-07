object FormProgramLocations: TFormProgramLocations
  Left = 0
  Top = 0
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Program_Locations_Dialog_Box'
  Caption = 'Program Locations'
  ClientHeight = 526
  ClientWidth = 700
  Color = clBtnFace
  Constraints.MaxHeight = 663
  Constraints.MinHeight = 558
  Constraints.MinWidth = 708
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 481
    Width = 700
    Height = 45
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      700
      45)
    object btnHelp: TBitBtn
      Left = 592
      Top = 7
      Width = 90
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Anchors = [akTop, akRight]
      DoubleBuffered = True
      Kind = bkHelp
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 2
      Visible = False
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 229
      Top = 7
      Width = 89
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DoubleBuffered = True
      Kind = bkOK
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 327
      Top = 7
      Width = 89
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DoubleBuffered = True
      Kind = bkCancel
      NumGlyphs = 2
      ParentDoubleBuffered = False
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 700
    Height = 481
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    TabOrder = 0
    DesignSize = (
      700
      481)
    object lblModflow: TLabel
      Left = 14
      Top = 125
      Width = 124
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MODFLOW-2005'
    end
    object JvHTLabel1: TJvHTLabel
      Left = 183
      Top = 125
      Width = 498
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2005/modflo' +
        'w2005.html">http://water.usgs.gov/nrp/gwsoftware/modflow2005/mod' +
        'flow2005.html</a>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 14
      Top = 56
      Width = 103
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'UCODE'
    end
    object Label2: TLabel
      Left = 14
      Top = 194
      Width = 124
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'MODFLOW-2000'
    end
    object JvHTLabel2: TJvHTLabel
      Left = 183
      Top = 192
      Width = 498
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/modflow2000/modflo' +
        'w2000.html">http://water.usgs.gov/nrp/gwsoftware/modflow2000/mod' +
        'flow2000.html</a>'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object JvHTLabel3: TJvHTLabel
      Left = 183
      Top = 56
      Width = 291
      Height = 20
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://igwmc.mines.edu/freeware/ucode/">http://igwmc.mi' +
        'nes.edu/freeware/ucode/</a>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
    end
    object JvHTLabel4: TJvHTLabel
      Left = 183
      Top = 262
      Width = 438
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://water.usgs.gov/nrp/gwsoftware/GW_Chart/GW_Chart.' +
        'html">http://water.usgs.gov/nrp/gwsoftware/GW_Chart/GW_Chart.htm' +
        'l</a>'
    end
    object Label3: TLabel
      Left = 14
      Top = 262
      Width = 74
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'GW_Chart'
    end
    object Label4: TLabel
      Left = 14
      Top = 331
      Width = 126
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Residual_analysis'
    end
    object JvHTLabel5: TJvHTLabel
      Left = 183
      Top = 331
      Width = 274
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://igwmc.mines.edu/freeware/ucode/">http://igwmc.mi' +
        'nes.edu/freeware/ucode/</a>'
    end
    object Label5: TLabel
      Left = 14
      Top = 400
      Width = 160
      Height = 18
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Residual_analysis_adv'
    end
    object JvHTLabel6: TJvHTLabel
      Left = 183
      Top = 400
      Width = 274
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 
        '<a href="http://igwmc.mines.edu/freeware/ucode/">http://igwmc.mi' +
        'nes.edu/freeware/ucode/</a>'
    end
    object Label6: TLabel
      Left = 14
      Top = 21
      Width = 117
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Program Name'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label7: TLabel
      Left = 183
      Top = 21
      Width = 450
      Height = 19
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Web Address for New Versions (click to open in browser)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -17
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object fedUcode: TJvFilenameEdit
      Left = 14
      Top = 82
      Width = 668
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = fedUcodeChange
    end
    object fedModflow2005: TJvFilenameEdit
      Left = 14
      Top = 151
      Width = 668
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      OnChange = fedModflow2005Change
    end
    object fedModflow2000: TJvFilenameEdit
      Left = 14
      Top = 220
      Width = 668
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      Enabled = False
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = fedModflow2000Change
    end
    object fedGWChart: TJvFilenameEdit
      Left = 14
      Top = 289
      Width = 668
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      OnChange = fedGWChartChange
    end
    object fedResidAnalysis: TJvFilenameEdit
      Left = 14
      Top = 357
      Width = 668
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.exe'
      Filter = 
        'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
        '.*)|*.*'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = fedResidAnalysisChange
    end
    object fedResidAnalysisAdv: TJvFilenameEdit
      Left = 14
      Top = 426
      Width = 668
      Height = 26
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultExt = '.exe'
      ButtonWidth = 25
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      OnChange = fedResidAnalysisAdvChange
    end
  end
end
