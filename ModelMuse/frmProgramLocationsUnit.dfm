inherited frmProgramLocations: TfrmProgramLocations
  HelpType = htKeyword
  HelpKeyword = 'MODFLOW_Program_Locations_Dialog_Box'
  Caption = 'MODFLOW Program Locations'
  ClientHeight = 586
  ClientWidth = 722
  ExplicitTop = -108
  ExplicitWidth = 738
  ExplicitHeight = 625
  PixelsPerInch = 96
  TextHeight = 18
  object pnlBottom: TPanel
    Left = 0
    Top = 545
    Width = 722
    Height = 41
    Align = alBottom
    TabOrder = 1
    ExplicitWidth = 623
    DesignSize = (
      722
      41)
    object btnHelp: TBitBtn
      Left = 458
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
      ExplicitLeft = 359
    end
    object btnOK: TBitBtn
      Left = 546
      Top = 6
      Width = 82
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
      ExplicitLeft = 447
    end
    object btnCancel: TBitBtn
      Left = 634
      Top = 6
      Width = 83
      Height = 27
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
      ExplicitLeft = 535
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 722
    Height = 545
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 623
    object jvrltModflow2005: TJvRollOut
      Left = 1
      Top = 85
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'Modflow 2005'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 1
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 84
      FCWidth = 22
      FCHeight = 22
      object htlblModflow: TJvHTLabel
        Left = 15
        Top = 25
        Width = 733
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/modflow-2005-usgs-three-d' +
          'imensional-finite-difference-ground-water-model">https://www.usg' +
          's.gov/software/modflow-2005-usgs-three-dimensional-finite-differ' +
          'ence-ground-water-model</a>'
      end
      object fedModflow: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModflowLGR: TJvRollOut
      Left = 1
      Top = 107
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MODFLOW-LGR'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 2
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 82
      FCWidth = 22
      FCHeight = 22
      object JvHTLabel1: TJvHTLabel
        Left = 15
        Top = 25
        Width = 270
        Height = 19
        Caption = 
          '<a href="https://water.usgs.gov/ogw/modflow-lgr/">https://water.' +
          'usgs.gov/ogw/modflow-lgr/</a>'
      end
      object fedModflowLgr: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModflowLgr2: TJvRollOut
      Left = 1
      Top = 129
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MODFLOW-LGR V2'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 3
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 84
      FCWidth = 22
      FCHeight = 22
      object jvhtlblMfLgr2: TJvHTLabel
        Left = 15
        Top = 25
        Width = 270
        Height = 19
        Caption = 
          '<a href="https://water.usgs.gov/ogw/modflow-lgr/">https://water.' +
          'usgs.gov/ogw/modflow-lgr/</a>'
      end
      object fedModflowLgr2: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModflowNWT: TJvRollOut
      Left = 1
      Top = 151
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MODFLOW-NWT'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 4
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 80
      FCWidth = 22
      FCHeight = 22
      object htlblModflowNWT: TJvHTLabel
        Left = 15
        Top = 25
        Width = 550
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/modflow-nwt-a-newton-form' +
          'ulation-modflow-2005">https://www.usgs.gov/software/modflow-nwt-' +
          'a-newton-formulation-modflow-2005</a>'
      end
      object fedModflowNWT: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModpath: TJvRollOut
      Left = 1
      Top = 217
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MODPATH'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 7
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 80
      FCWidth = 22
      FCHeight = 22
      object htlblModPath: TJvHTLabel
        Left = 15
        Top = 25
        Width = 438
        Height = 19
        Caption = 
          '<a href="https://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5' +
          '.html">https://water.usgs.gov/nrp/gwsoftware/modpath5/modpath5.h' +
          'tml</a>'
      end
      object fedModpath: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltZoneBudget: TJvRollOut
      Left = 1
      Top = 239
      Width = 720
      Height = 84
      Align = alTop
      Caption = 'ZONEBUDGET 3'
      ImageOptions.Images = ilShowHide
      TabOrder = 8
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        84)
      FAWidth = 145
      FAHeight = 84
      FCWidth = 22
      FCHeight = 22
      object htlblZoneBudget: TJvHTLabel
        Left = 15
        Top = 25
        Width = 879
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/zonebudget-a-program-comp' +
          'uting-subregional-water-budgets-modflow-groundwater-flow-models"' +
          '>https://www.usgs.gov/software/zonebudget-a-program-computing-su' +
          'bregional-water-budgets-modflow-groundwater-flow-models</a>'
      end
      object fedZonebudget: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltMt3dms: TJvRollOut
      Left = 1
      Top = 407
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MT3DMS'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 9
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 82
      FCWidth = 22
      FCHeight = 22
      object htlblMt3dms: TJvHTLabel
        Left = 15
        Top = 25
        Width = 274
        Height = 19
        Caption = 
          '<a href="https://hydro.geo.ua.edu/mt3d/index.htm">https://hydro.' +
          'geo.ua.edu/mt3d/index.htm</a>'
      end
      object fedMt3dms: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModelMate: TJvRollOut
      Left = 1
      Top = 451
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'ModelMate'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 11
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 84
      FCWidth = 22
      FCHeight = 22
      object htlblModelMate: TJvHTLabel
        Left = 15
        Top = 25
        Width = 298
        Height = 19
        Caption = 
          '<a href="https://water.usgs.gov/software/ModelMate/">https://wat' +
          'er.usgs.gov/software/ModelMate/</a>'
      end
      object fedModelMate: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltTextEditor: TJvRollOut
      Left = 1
      Top = 473
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'Text editor'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 12
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 56
      FCWidth = 22
      FCHeight = 22
      object fedTextEditor: TJvFilenameEdit
        Left = 15
        Top = 24
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModelMonitor: TJvRollOut
      Left = 1
      Top = 495
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'ModelMonitor'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 13
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 56
      FCWidth = 22
      FCHeight = 22
      object fedModelMonitor: TJvFilenameEdit
        Left = 15
        Top = 24
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModflowFmp: TJvRollOut
      Left = 1
      Top = 195
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MF2005-OWHM'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 6
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 120
      FCWidth = 22
      FCHeight = 22
      object htlblModflowFmp: TJvHTLabel
        Left = 15
        Top = 25
        Width = 531
        Height = 19
        Caption = 
          '<a href="https://ca.water.usgs.gov/modeling-software/one-water-h' +
          'ydrologic-model.html">https://ca.water.usgs.gov/modeling-softwar' +
          'e/one-water-hydrologic-model.html</a>'
      end
      object htlbl1: TJvHTLabel
        Left = 15
        Top = 55
        Width = 356
        Height = 19
        Caption = 
          '<a href="https://sourceforge.net/projects/modflow-owhm/files/">h' +
          'ttps://sourceforge.net/projects/modflow-owhm/files/</a>'
      end
      object fedModflowFmp: TJvFilenameEdit
        Left = 15
        Top = 80
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModflowCFP: TJvRollOut
      Left = 1
      Top = 173
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MF2005-CFP'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 5
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 82
      FCWidth = 22
      FCHeight = 22
      object htlblModflowCFP: TJvHTLabel
        Left = 15
        Top = 25
        Width = 782
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/conduit-flow-process-cfp-' +
          'a-program-simulate-turbulent-or-laminar-groundwater-flow">https:' +
          '//www.usgs.gov/software/conduit-flow-process-cfp-a-program-simul' +
          'ate-turbulent-or-laminar-groundwater-flow</a>'
      end
      object fedModflowCFP: TJvFilenameEdit
        Left = 15
        Top = 48
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltModflow6: TJvRollOut
      Left = 1
      Top = 1
      Width = 720
      Height = 84
      Align = alTop
      Caption = 'Modflow 6'
      ImageOptions.Images = ilShowHide
      TabOrder = 0
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        84)
      FAWidth = 145
      FAHeight = 84
      FCWidth = 22
      FCHeight = 22
      object htlblModflow6: TJvHTLabel
        Left = 15
        Top = 25
        Width = 506
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/modflow-6-usgs-modular-hy' +
          'drologic-model">https://www.usgs.gov/software/modflow-6-usgs-mod' +
          'ular-hydrologic-model</a>'
      end
      object fedModflow6: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltMt3dUsgs: TJvRollOut
      Left = 1
      Top = 429
      Width = 720
      Height = 22
      Align = alTop
      Caption = 'MT3D-USGS'
      Collapsed = True
      ImageOptions.Images = ilShowHide
      TabOrder = 10
      ExplicitWidth = 621
      DesignSize = (
        720
        22)
      FAWidth = 145
      FAHeight = 88
      FCWidth = 22
      FCHeight = 22
      object htlblMt3dUSGS: TJvHTLabel
        Left = 15
        Top = 26
        Width = 618
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/mt3d-usgs-groundwater-sol' +
          'ute-transport-simulator-modflow">https://www.usgs.gov/software/m' +
          't3d-usgs-groundwater-solute-transport-simulator-modflow</a>'
      end
      object fedMt3dUsgs: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
    object jvrltZonebudget6: TJvRollOut
      Left = 1
      Top = 323
      Width = 720
      Height = 84
      Align = alTop
      Caption = 'ZONEBUDGET 6'
      ImageOptions.Images = ilShowHide
      TabOrder = 14
      OnExpand = jvrltExpand
      ExplicitWidth = 621
      DesignSize = (
        720
        84)
      FAWidth = 145
      FAHeight = 84
      FCWidth = 22
      FCHeight = 22
      object htlblZoneBudget6: TJvHTLabel
        Left = 15
        Top = 25
        Width = 506
        Height = 19
        Caption = 
          '<a href="https://www.usgs.gov/software/modflow-6-usgs-modular-hy' +
          'drologic-model">https://www.usgs.gov/software/modflow-6-usgs-mod' +
          'ular-hydrologic-model</a>'
      end
      object fedZonebudget6: TJvFilenameEdit
        Left = 15
        Top = 50
        Width = 687
        Height = 26
        Filter = 
          'Executables (*.exe)|*.exe|Batch Files (*.bat)|*.bat|All files (*' +
          '.*)|*.*'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = ''
        OnChange = fedModflowChange
        ExplicitWidth = 588
      end
    end
  end
  object ilShowHide: TImageList
    Height = 12
    Width = 12
    Left = 448
    Top = 48
    Bitmap = {
      494C01010200050004000C000C00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000300000000C00000001002000000000000009
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000808080008080
      80008080800080808000808080008080800080808000C0C0C000000000000000
      000000000000C0C0C00080808000808080008080800080808000808080008080
      800080808000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0C0C000808080008080
      80008080800080808000808080008080800080808000C0C0C000000000000000
      000000000000C0C0C00080808000808080008080800080808000808080008080
      800080808000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      28000000300000000C0000000100010000000000600000000000000000000000
      000000000000000000000000FFFFFF00FFFFFF0000000000FFFFFF0000000000
      8038030000000000BFBBFB0000000000BFBBBB0000000000BFBBBB0000000000
      A0BA0B0000000000BFBBBB0000000000BFBBBB0000000000BFBBFB0000000000
      8038030000000000FFFFFF000000000000000000000000000000000000000000
      000000000000}
  end
end
