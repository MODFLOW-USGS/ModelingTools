inherited frmSutraOutputControl: TfrmSutraOutputControl
  HelpType = htKeyword
  HelpKeyword = 'SUTRA_Output_Control_Dialog_Bo'
  Caption = 'SUTRA Output Control'
  ClientHeight = 438
  ClientWidth = 722
  ExplicitWidth = 738
  ExplicitHeight = 477
  PixelsPerInch = 96
  TextHeight = 18
  object spl1: TSplitter
    Left = 136
    Top = 0
    Width = 5
    Height = 388
    ExplicitLeft = 0
    ExplicitHeight = 344
  end
  object jvplMain: TJvPageList
    Left = 141
    Top = 0
    Width = 581
    Height = 388
    ActivePage = jvspObsBound
    PropagateEnable = False
    Align = alClient
    ExplicitHeight = 344
    object jvspListing: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 388
      Caption = 'jvspListing'
      DesignSize = (
        581
        388)
      object lblNprint: TLabel
        Left = 133
        Top = 19
        Width = 257
        Height = 18
        Caption = 'Print values every NPRINT time steps'
      end
      object seNprint: TJvSpinEdit
        Left = 6
        Top = 16
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 0
      end
      object chklstOptions: TCheckListBox
        Left = 6
        Top = 48
        Width = 572
        Height = 206
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 18
        Items.Strings = (
          'Print output for the first time step (transient models) (NPRINT)'
          
            'Print coordinates, element thicknesses, and porosities of nodes ' +
            '(CNODAL)'
          'Print permeabilities and dispersivities of elements (CELMNT)'
          'Print node incidences in elements (CINCID)'
          'Print pressure and saturation at nodes (CPANDS)'
          'Print fluid velocities (CVEL)'
          'Print concentrations or temperatures (CCORT)'
          'Print fluid mass budget and energy or solute mass budget (CBUDG)'
          'Write a summary of simulation progress to the screen (CSCRN)'
          'Pause for a user response at the end of the simulation (CPAUSE)')
        TabOrder = 1
      end
    end
    object jvspNodEle: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 388
      Caption = 'jvspNodEle'
      DesignSize = (
        581
        388)
      object lblNE_PrintFrequency: TLabel
        Left = 133
        Top = 11
        Width = 407
        Height = 36
        Caption = 
          'Print node and element data every N time steps in "nod" or "ele"' +
          ' file (NCOLPR, LCOLPR)'
        WordWrap = True
      end
      object lblNcol: TLabel
        Left = 6
        Top = 48
        Width = 119
        Height = 18
        Caption = 'NCOL and LCOL'
      end
      object seNE_PrintFrequency: TJvSpinEdit
        Left = 6
        Top = 16
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 0
      end
      object clbNcol: TCheckListBox
        Left = 6
        Top = 69
        Width = 572
        Height = 260
        Anchors = [akLeft, akTop, akRight]
        DoubleBuffered = False
        ItemHeight = 18
        Items.Strings = (
          'Print nodal data on first time step (transient models) (NCOLPR)'
          'Node and element numbers (N, E)'
          'Coordinates of node or element (X, Y, Z)'
          'Pressure (P)'
          'Concentration or temperature (U)'
          'Saturation (S)'
          'X, Y, and Z-components of fluid velocity (VX, VY, VZ)')
        ParentDoubleBuffered = False
        ScrollWidth = 332
        TabOrder = 1
      end
    end
    object jvspObsBound: TJvStandardPage
      Left = 0
      Top = 0
      Width = 581
      Height = 388
      ExplicitHeight = 344
      object lblNoblin: TLabel
        Left = 133
        Top = 6
        Width = 415
        Height = 36
        Caption = 
          'Maximum number of observations output to a single line in a '#8220'.ob' +
          's'#8221' file (NOBLIN) '
        WordWrap = True
      end
      object lblNbcfpr: TLabel
        Left = 133
        Top = 59
        Width = 372
        Height = 18
        Caption = 'Print values every NBCFPR time steps for fluid source'
      end
      object lblNbcspr: TLabel
        Left = 133
        Top = 93
        Width = 408
        Height = 36
        Caption = 'Print values every NBCSPR time steps for solute or energy source'
        WordWrap = True
      end
      object lblNbcppr: TLabel
        Left = 133
        Top = 135
        Width = 362
        Height = 36
        Caption = 'Print values every NBCPPR time steps for specified pressure'
        WordWrap = True
      end
      object lblNbcupr: TLabel
        Left = 133
        Top = 177
        Width = 362
        Height = 36
        Caption = 
          'Print values every NBCUPR time steps for specified concentration' +
          ' or temperature'
        WordWrap = True
      end
      object lblGenFlowFrequency: TLabel
        Left = 133
        Top = 219
        Width = 410
        Height = 36
        Caption = 
          'Print values every NBGPPR time steps for generalized flow bounda' +
          'ries'
        WordWrap = True
      end
      object lblGenTransportFrequency: TLabel
        Left = 133
        Top = 256
        Width = 379
        Height = 36
        Caption = 
          'Print values every NBGUPR time steps for generalized transport b' +
          'oundaries'
        WordWrap = True
      end
      object lblLakeOutputCycle: TLabel
        Left = 133
        Top = 300
        Width = 402
        Height = 18
        Caption = 'Print values every NLAKPR time steps for lake boundaries'
      end
      object seNoblin: TJvSpinEdit
        Left = 6
        Top = 11
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 0
      end
      object seNbcfpr: TJvSpinEdit
        Left = 6
        Top = 56
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 1
      end
      object seNbcspr: TJvSpinEdit
        Left = 6
        Top = 98
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 2
      end
      object seNbcppr: TJvSpinEdit
        Left = 6
        Top = 140
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 3
      end
      object seNbcupr: TJvSpinEdit
        Left = 6
        Top = 182
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 4
      end
      object cbCinact: TCheckBox
        Left = 6
        Top = 346
        Width = 467
        Height = 17
        Caption = 'List all sources and boundary conditions (CINACT)'
        TabOrder = 7
      end
      object seGenFlowFrequency: TJvSpinEdit
        Left = 6
        Top = 224
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 5
      end
      object seGenTransportFrequency: TJvSpinEdit
        Left = 6
        Top = 261
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        TabOrder = 6
      end
      object seLakeOutputCycle: TJvSpinEdit
        Left = 6
        Top = 297
        Width = 121
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 8
      end
    end
  end
  object jvpltvNavigator: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 136
    Height = 388
    PageDefault = 0
    PageList = jvplMain
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnCustomDrawItem = jvpltvNavigatorCustomDrawItem
    Items.Links = {00000000}
    ExplicitHeight = 344
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 388
    Width = 722
    Height = 50
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    ExplicitTop = 344
    DesignSize = (
      722
      50)
    object btnCancel: TBitBtn
      Left = 612
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 515
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 418
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
  end
end
