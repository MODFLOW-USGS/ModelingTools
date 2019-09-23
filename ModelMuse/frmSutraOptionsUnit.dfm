inherited frmSutraOptions: TfrmSutraOptions
  HelpType = htKeyword
  HelpKeyword = 'SUTRA_Options_Dialog_Box'
  Caption = 'SUTRA Options'
  ClientHeight = 562
  ClientWidth = 784
  ExplicitWidth = 800
  ExplicitHeight = 601
  PixelsPerInch = 96
  TextHeight = 18
  object splttrVertical: TJvNetscapeSplitter
    Left = 179
    Top = 0
    Height = 512
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 128
    ExplicitTop = 208
    ExplicitHeight = 100
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 512
    Width = 784
    Height = 50
    Align = alBottom
    ParentColor = True
    TabOrder = 2
    DesignSize = (
      784
      50)
    object btnCancel: TBitBtn
      Left = 674
      Top = 7
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
    object btnOK: TBitBtn
      Left = 577
      Top = 6
      Width = 91
      Height = 33
      Anchors = [akTop, akRight]
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnHelp: TBitBtn
      Left = 480
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
  object jplMain: TJvPageList
    Left = 189
    Top = 0
    Width = 595
    Height = 512
    ActivePage = jvspDefaultLakeInteractions
    PropagateEnable = False
    Align = alClient
    OnChange = jplMainChange
    object jvspConfiguration: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Configuration_Pane'
      DesignSize = (
        595
        512)
      object lblGravX: TLabel
        Left = 106
        Top = 66
        Width = 362
        Height = 18
        Caption = 'Component of gravity vector in +x direction (GRAVX)'
      end
      object lblGravY: TLabel
        Left = 106
        Top = 94
        Width = 362
        Height = 18
        Caption = 'Component of gravity vector in +Y direction (GRAVY)'
      end
      object lblGravZ: TLabel
        Left = 106
        Top = 121
        Width = 362
        Height = 18
        Caption = 'Component of gravity vector in +Z direction (GRAVZ)'
      end
      object rgMeshType: TRadioGroup
        Left = 6
        Top = 8
        Width = 331
        Height = 49
        Caption = 'Mesh type (MSHSTR)'
        Columns = 3
        ItemIndex = 2
        Items.Strings = (
          '2D areal'
          '2D profile'
          '3D')
        TabOrder = 0
        OnClick = rgMeshTypeClick
      end
      object rgSaturation: TRadioGroup
        Left = 263
        Top = 147
        Width = 319
        Height = 96
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Flow conditions (CUNSAT)'
        ItemIndex = 0
        Items.Strings = (
          'Saturated'
          'Unsaturated')
        TabOrder = 5
      end
      object rgTransport: TRadioGroup
        Left = 6
        Top = 146
        Width = 251
        Height = 97
        Caption = 'Transport (SIMULA)'
        ItemIndex = 0
        Items.Strings = (
          'Solute using pressure'
          'Solute using head'
          'Energy')
        TabOrder = 4
        OnClick = rgTransportClick
      end
      object rgSimulationType: TRadioGroup
        Left = 6
        Top = 249
        Width = 576
        Height = 105
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Simulation type (CSSFLO, CSSTRA)'
        ItemIndex = 0
        Items.Strings = (
          'Steady-state flow, steady-state transport'
          'Steady-state flow, transient transport'
          'Transient flow, transient transport')
        TabOrder = 6
      end
      object rdeGravX: TRbwDataEntry
        Left = 6
        Top = 63
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        ChangeDisabledColor = True
      end
      object rdeGravY: TRbwDataEntry
        Left = 6
        Top = 90
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        ChangeDisabledColor = True
      end
      object rdeGravZ: TRbwDataEntry
        Left = 6
        Top = 118
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        ChangeDisabledColor = True
      end
    end
    object jvspTitle: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Title_Pane'
      Caption = 'jvspTitle'
      object jvedTitle: TJvEditor
        Left = 0
        Top = 49
        Width = 595
        Height = 463
        Cursor = crIBeam
        Completion.ItemHeight = 13
        Completion.CRLF = '/n'
        Completion.Separator = '='
        TabStops = '3 5'
        KeepTrailingBlanks = True
        CursorBeyondEOL = False
        BracketHighlighting.StringEscape = #39#39
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Courier New'
        Font.Style = []
      end
      object pnlTitleCaption: TPanel
        Left = 0
        Top = 0
        Width = 595
        Height = 49
        Align = alTop
        Alignment = taLeftJustify
        TabOrder = 0
        object lblTitle: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 587
          Height = 41
          Align = alClient
          Caption = 
            'The first 80 characters of the first two lines are TITLE1 and TI' +
            'TLE2.'#13#10'The remaining lines will be treated as comments.'
          ExplicitWidth = 458
          ExplicitHeight = 36
        end
      end
    end
    object jvspInitialCondition: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Initial_Conditions_Pane'
      Caption = 'jvspInitialCondition'
      DesignSize = (
        595
        512)
      object lblRestartFile: TLabel
        Left = 6
        Top = 119
        Width = 172
        Height = 18
        Caption = 'Restart file for warm start'
      end
      object lblRestartFrequency: TLabel
        Left = 103
        Top = 90
        Width = 287
        Height = 18
        Caption = 'Frequency for saving restart file (ISTORE)'
      end
      object lblRestartInitialConditions: TLabel
        Left = 6
        Top = 334
        Width = 211
        Height = 18
        Caption = 'Restart file for initial conditions'
      end
      object rgStartType: TRadioGroup
        Left = 6
        Top = 3
        Width = 578
        Height = 78
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Starting type (CREAD)'
        ItemIndex = 0
        Items.Strings = (
          'Cold start (start from first time step)'
          'Warm start (start from restart file)')
        TabOrder = 0
        OnClick = rgStartTypeClick
      end
      object fedRestartFile: TJvFilenameEdit
        Left = 6
        Top = 143
        Width = 578
        Height = 26
        DefaultExt = '.rst'
        Filter = 'Restart Files (*.rst)|*.rst|All files (*.*)|*.*'
        Enabled = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = ''
        OnChange = fedRestartFileChange
      end
      object seRestartFrequency: TJvSpinEdit
        Left = 6
        Top = 87
        Width = 91
        Height = 26
        MaxValue = 2147483647.000000000000000000
        Value = 10000.000000000000000000
        TabOrder = 1
      end
      object rgInitialValues: TRadioGroup
        Left = 6
        Top = 175
        Width = 571
        Height = 153
        Caption = 'Read initial conditions from a restart file'
        ItemIndex = 0
        Items.Strings = (
          
            'read neither the pressure/head nor the concentration/temperature' +
            ' from the restart file'
          'read the pressure/head from the restart file'
          'read the concentration/temperature from the restart file'
          
            'read both the pressure/head and the concentration/temperature fr' +
            'om the restart file')
        TabOrder = 3
        WordWrap = True
        OnClick = rgInitialValuesClick
      end
      object fedRestartInitialConditions: TJvFilenameEdit
        Left = 6
        Top = 358
        Width = 578
        Height = 26
        DefaultExt = '.rst'
        Filter = 'Restart Files (*.rst)|*.rst|All files (*.*)|*.*'
        Enabled = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = ''
        OnChange = fedRestartInitialConditionsChange
      end
    end
    object jvspNumericalControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Numerical_Controls_Pane'
      Caption = 'jvspNumericalControls'
      object lblFractionalUpstreamWeight: TLabel
        Left = 79
        Top = 16
        Width = 223
        Height = 18
        Caption = 'Fractional upstream weight (UP)'
      end
      object lblPressureFactor: TLabel
        Left = 79
        Top = 166
        Width = 300
        Height = 18
        Caption = 'Pressure boundary condition factor (GNUP)'
      end
      object lblUFactor: TLabel
        Left = 79
        Top = 194
        Width = 422
        Height = 18
        Caption = 'Concentration/temperature boundary condition factor (GNUU)'
      end
      object lblMaxIterations: TLabel
        Left = 127
        Top = 40
        Width = 381
        Height = 36
        Caption = 
          'Maximum number of iterations allowed per time step to resolve no' +
          'nlinearities (ITRMAX)'
        WordWrap = True
      end
      object lblNonLinPressureCriterion: TLabel
        Left = 79
        Top = 79
        Width = 422
        Height = 36
        Caption = 
          'Absolute iteration convergence criterion for pressure solution (' +
          'RPMAX) '
        WordWrap = True
      end
      object lblUCriterion: TLabel
        Left = 79
        Top = 121
        Width = 422
        Height = 36
        Caption = 
          'Absolute iteration convergence criterion for transport solution ' +
          '(RUMAX) '
        WordWrap = True
      end
      object rdeFractionalUpstreamWeight: TRbwDataEntry
        Left = 16
        Top = 13
        Width = 57
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = rdeFractionalUpstreamWeightChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdePressureFactor: TRbwDataEntry
        Left = 16
        Top = 163
        Width = 57
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeUFactor: TRbwDataEntry
        Left = 16
        Top = 191
        Width = 57
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seMaxIterations: TJvSpinEdit
        Left = 16
        Top = 47
        Width = 105
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 3
        OnChange = seMaxIterationsChange
      end
      object rdeNonLinPressureCriterion: TRbwDataEntry
        Left = 16
        Top = 76
        Width = 57
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeUCriterion: TRbwDataEntry
        Left = 16
        Top = 128
        Width = 57
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspSolverControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Solver_Pane'
      Caption = 'jvspSolverControls'
      DesignSize = (
        595
        512)
      object lblMaxPressureIterations: TLabel
        Left = 111
        Top = 114
        Width = 426
        Height = 36
        Caption = 
          'Maximum number of solver iterations during pressure solution (IT' +
          'RMXP)'
        WordWrap = True
      end
      object lblPressureCriterion: TLabel
        Left = 111
        Top = 156
        Width = 465
        Height = 36
        Caption = 
          'Convergence tolerance for solver iterations during pressure solu' +
          'tion (TOLP)'
        WordWrap = True
      end
      object lblMaxTransportIterations: TLabel
        Left = 111
        Top = 303
        Width = 426
        Height = 36
        Caption = 
          'Maximum number of solver iterations during transport solution (I' +
          'TRMXU)'
        WordWrap = True
      end
      object lblTransportCriterion: TLabel
        Left = 111
        Top = 345
        Width = 465
        Height = 36
        Caption = 
          'Convergence tolerance for solver iterations during transport sol' +
          'ution (TOLU)'
        WordWrap = True
      end
      object rgPressureSolution: TRadioGroup
        Left = 6
        Top = 3
        Width = 586
        Height = 105
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Pressure solution solver (CSOLVP)'
        Items.Strings = (
          'Banded Gaussian elimination (DIRECT)'
          'IC-preconditioned conjugate gradient (CG)'
          'ILU-preconditioned generalized minimum residual (GMRES)'
          'ILU-preconditioned orthomin (ORTHOMIN)')
        TabOrder = 0
        OnClick = rgPressureSolutionClick
      end
      object seMaxPressureIterations: TJvSpinEdit
        Left = 6
        Top = 119
        Width = 99
        Height = 26
        TabOrder = 1
      end
      object rdePressureCriterion: TRbwDataEntry
        Left = 6
        Top = 163
        Width = 99
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rgUSolutionMethod: TRadioGroup
        Left = 6
        Top = 207
        Width = 586
        Height = 90
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Transport solution solver (CSOLVU)'
        Items.Strings = (
          'Banded Gaussian elimination (DIRECT)'
          'ILU-preconditioned generalized minimum residual (GMRES)'
          'ILU-preconditioned orthomin (ORTHOMIN)')
        TabOrder = 3
        OnClick = rgUSolutionMethodClick
      end
      object seMaxTransportIterations: TJvSpinEdit
        Left = 6
        Top = 313
        Width = 99
        Height = 26
        TabOrder = 4
      end
      object rdeTransportCriterion: TRbwDataEntry
        Left = 6
        Top = 345
        Width = 99
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspFluidProperties: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Fluid_Properties_Pane'
      Caption = 'jvspFluidProperties'
      object lblFluidCompressibility: TLabel
        Left = 106
        Top = 14
        Width = 223
        Height = 18
        Caption = 'Fluid compressibility (COMPFL)'
      end
      object lblFluidSpecificHeat: TLabel
        Left = 106
        Top = 42
        Width = 167
        Height = 18
        Caption = 'Fluid specific heat (CW)'
      end
      object lblFluidDiffusivity: TLabel
        Left = 106
        Top = 70
        Width = 179
        Height = 18
        Caption = 'Fluid diffusivity (SIGMAW)'
      end
      object lblBaseFluidDensity: TLabel
        Left = 106
        Top = 126
        Width = 435
        Height = 18
        Caption = 'Density of fluid at base concentration or temperature (RHOW'#216')'
      end
      object lblBaseU: TLabel
        Left = 106
        Top = 154
        Width = 322
        Height = 18
        Caption = 'Base value of solute concentration (URHOW'#216')'
      end
      object lblFluidDensityCoefficientConcentration: TLabel
        Left = 106
        Top = 205
        Width = 429
        Height = 36
        Caption = 
          'Coefficient of fluid density change with concentration (fraction' +
          ') (DRWDU) '
        WordWrap = True
      end
      object lblViscosityScaleFactor: TLabel
        Left = 106
        Top = 271
        Width = 160
        Height = 18
        Caption = 'Fluid viscosity (VISC'#216')'
      end
      object lblScaleFactor: TLabel
        Left = 106
        Top = 299
        Width = 146
        Height = 18
        Caption = 'Scale factor (VISC'#216')'
      end
      object lblFluidThermalConductivity: TLabel
        Left = 106
        Top = 98
        Width = 251
        Height = 18
        Caption = 'Fluid thermal conductivity (SIGMAW)'
      end
      object lblFluidDensityCoefficientTemperature: TLabel
        Left = 106
        Top = 243
        Width = 430
        Height = 18
        Caption = 'Coefficient of fluid density change with temperature (DRWDU) '
      end
      object lblBaseTemperature: TLabel
        Left = 106
        Top = 181
        Width = 268
        Height = 18
        Caption = 'Base value of temperature (URHOW'#216')'
      end
      object rdeFluidCompressibility: TRbwDataEntry
        Left = 6
        Top = 11
        Width = 94
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidSpecificHeat: TRbwDataEntry
        Left = 6
        Top = 39
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidDiffusivity: TRbwDataEntry
        Left = 6
        Top = 67
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeBaseFluidDensity: TRbwDataEntry
        Left = 6
        Top = 123
        Width = 94
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeBaseConcentration: TRbwDataEntry
        Left = 6
        Top = 151
        Width = 94
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidDensityCoefficientConcentration: TRbwDataEntry
        Left = 6
        Top = 212
        Width = 94
        Height = 22
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeViscosity: TRbwDataEntry
        Left = 6
        Top = 268
        Width = 94
        Height = 22
        TabOrder = 9
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeScaleFactor: TRbwDataEntry
        Left = 6
        Top = 296
        Width = 94
        Height = 22
        TabOrder = 10
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidThermalConductivity: TRbwDataEntry
        Left = 6
        Top = 95
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluidDensityCoefficientTemperature: TRbwDataEntry
        Left = 6
        Top = 240
        Width = 94
        Height = 22
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeBaseTemperature: TRbwDataEntry
        Left = 6
        Top = 178
        Width = 94
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspSolidAdsorption: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Solid_Matrix__Adsorption_Pane'
      Caption = 'jvspSolidAdsorption'
      object grpSolidMatrix: TGroupBox
        Left = 0
        Top = 0
        Width = 595
        Height = 145
        Align = alTop
        Caption = 'Solid matrix properties'
        TabOrder = 0
        object lblMatrixCompressibility: TLabel
          Left = 108
          Top = 30
          Width = 276
          Height = 18
          Caption = 'Solid matrix compressibility (COMPMA)'
        end
        object lblSolidGrainSpecificHeat: TLabel
          Left = 108
          Top = 58
          Width = 204
          Height = 18
          Caption = 'Solid grain specific heat (CS)'
        end
        object lblSolidGrainDiffusivity: TLabel
          Left = 108
          Top = 87
          Width = 217
          Height = 18
          Caption = 'Solid grain diffusivity (SIGMAS)'
        end
        object lblSolidGrainDensity: TLabel
          Left = 108
          Top = 114
          Width = 217
          Height = 18
          Caption = 'Density of a solid grain (RHOS)'
        end
        object rdeSolidGrainDensity: TRbwDataEntry
          Left = 8
          Top = 111
          Width = 94
          Height = 22
          TabOrder = 3
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeSolidGrainDiffusivity: TRbwDataEntry
          Left = 8
          Top = 83
          Width = 94
          Height = 22
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeSolidGrainSpecificHeat: TRbwDataEntry
          Left = 8
          Top = 55
          Width = 94
          Height = 22
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeMatrixCompressibility: TRbwDataEntry
          Left = 8
          Top = 27
          Width = 94
          Height = 22
          TabOrder = 0
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
      object grpAdsorption: TGroupBox
        Left = 0
        Top = 145
        Width = 595
        Height = 193
        Align = alTop
        Caption = 'Adsorption parameters'
        TabOrder = 1
        object lblFirstDistributionCoefficient: TLabel
          Left = 106
          Top = 132
          Width = 235
          Height = 18
          Caption = 'First distribution coefficient (CHI1)'
        end
        object lblSecondDistributionCoefficient: TLabel
          Left = 106
          Top = 159
          Width = 258
          Height = 18
          Caption = 'Second distribution coefficient (CHI2)'
        end
        object rgSorptionModel: TRadioGroup
          Left = 6
          Top = 21
          Width = 417
          Height = 105
          Caption = 'Sorption model (ADSMOD)'
          Items.Strings = (
            'None'
            'Linear'
            'Freundlich'
            'Langmuir')
          TabOrder = 0
          OnClick = rgSorptionModelClick
        end
        object rdeFirstDistributionCoefficient: TRbwDataEntry
          Left = 6
          Top = 129
          Width = 94
          Height = 22
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeSecondDistributionCoefficient: TRbwDataEntry
          Left = 6
          Top = 155
          Width = 94
          Height = 22
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
    object jvspProdGrav: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Production_Pane'
      Caption = 'jvspProdGrav'
      object lblZeroFluidProd: TLabel
        Left = 106
        Top = 14
        Width = 354
        Height = 18
        Caption = 'Zero-order rate of production in the fluid (PRODF'#216')'
      end
      object lblZeroImmobProd: TLabel
        Left = 106
        Top = 42
        Width = 438
        Height = 18
        Caption = 'Zero-order rate of production in the immobile phase (PRODS'#216')'
      end
      object lblFirstFluidProd: TLabel
        Left = 106
        Top = 69
        Width = 437
        Height = 18
        Caption = 'First-order rate of solute mass production in the fluid (PRODF1)'
      end
      object lblFirstImmobProd: TLabel
        Left = 106
        Top = 94
        Width = 428
        Height = 36
        Caption = 
          'First-order rate of adsorbate mass production in the immobile ph' +
          'ase (PRODS1)'
        WordWrap = True
      end
      object rdeZeroFluidProd: TRbwDataEntry
        Left = 6
        Top = 11
        Width = 94
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeZeroImmobProd: TRbwDataEntry
        Left = 6
        Top = 38
        Width = 94
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFirstFluidProd: TRbwDataEntry
        Left = 6
        Top = 66
        Width = 94
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeFirstImmobProd: TRbwDataEntry
        Left = 6
        Top = 101
        Width = 94
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object jvspLake: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Lakes_Pane'
      Caption = 'jvspLake'
      object grpLakeDataset2: TGroupBox
        Left = 0
        Top = 89
        Width = 595
        Height = 423
        Align = alClient
        Caption = 'Lake dataset 2'
        TabOrder = 0
        object lblDefaultRechargeFrac: TLabel
          Left = 6
          Top = 24
          Width = 304
          Height = 18
          Caption = 'Default runoff fraction for recharge (FRROD)'
        end
        object lblDefaultDischargeFrac: TLabel
          Left = 6
          Top = 80
          Width = 312
          Height = 18
          Caption = 'Default runoff fraction for discharge (FDROD)'
        end
        object lblLakeOutput: TLabel
          Left = 6
          Top = 132
          Width = 420
          Height = 18
          Caption = 'Output value for stage at non-submerged lake node (RNOLK)'
        end
        object rdeDefaultRechargeFrac: TRbwDataEntry
          Left = 6
          Top = 48
          Width = 145
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 0
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMax = True
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeDefaultDischargeFrac: TRbwDataEntry
          Left = 6
          Top = 104
          Width = 145
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMax = True
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeLakeOutput: TRbwDataEntry
          Left = 6
          Top = 156
          Width = 145
          Height = 22
          Color = clBtnFace
          Enabled = False
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          ChangeDisabledColor = True
        end
      end
      object grpGeneral: TGroupBox
        Left = 0
        Top = 0
        Width = 595
        Height = 89
        Align = alTop
        TabOrder = 1
        object cbUseLakes: TCheckBox
          Left = 6
          Top = 16
          Width = 97
          Height = 17
          Caption = 'Use lakes'
          TabOrder = 0
          OnClick = cbUseLakesClick
        end
        object cbAllNodesLakes: TCheckBox
          Left = 6
          Top = 39
          Width = 374
          Height = 17
          Caption = 'All nodes on the top layer can be lake nodes (LKAR)'
          Enabled = False
          TabOrder = 1
          OnClick = EnableLakeBottom
        end
        object cbSpecifyLakeBotton: TCheckBox
          Left = 6
          Top = 62
          Width = 273
          Height = 17
          Caption = 'Specify lake bottom (CBOT)'
          TabOrder = 2
        end
      end
    end
    object jvspDefaultLakeInteractions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 595
      Height = 512
      HelpType = htKeyword
      HelpKeyword = 'Lake_Boundary_Interaction'
      Caption = 'jvspDefaultLakeInteractions'
      object grpLakeFluidSources: TGroupBox
        Left = 3
        Top = 3
        Width = 589
        Height = 81
        Caption = 'Default effect of lakes on sources or sinks of fluid  (ILKF)'
        TabOrder = 0
        object lblFluidSourceInLakesPresent: TLabel
          Left = 11
          Top = 19
          Width = 207
          Height = 18
          Caption = 'If lake water present or absent'
        end
        object comboFluidSourceInLakesPresent: TComboBox
          Left = 11
          Top = 43
          Width = 286
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 1
          TabOrder = 0
          Text = 'No change (0)'
          Items.Strings = (
            'Apply if lake water absent (-1)'
            'No change (0)'
            'Apply if lake water present (1)')
        end
      end
      object grpLakeSoluteMassSources: TGroupBox
        Left = 3
        Top = 90
        Width = 589
        Height = 81
        Caption = 
          'Default effect of lakes on sources or sinks of solute mass or en' +
          'ergy (ILKS)'
        TabOrder = 1
        object lblSoluteSourceInLakesPresent: TLabel
          Left = 11
          Top = 19
          Width = 207
          Height = 18
          Caption = 'If lake water present or absent'
        end
        object comboSoluteSourceInLakesPresent: TComboBox
          Left = 11
          Top = 43
          Width = 286
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 1
          TabOrder = 0
          Text = 'No change (0)'
          Items.Strings = (
            'Apply if lake water absent (-1)'
            'No change (0)'
            'Apply if lake water present (1)')
        end
      end
      object grpLakeSpecifiedPressure: TGroupBox
        Left = 3
        Top = 177
        Width = 589
        Height = 81
        Caption = 'Default effect of lakes on specified pressure boundaries (ILKP)'
        TabOrder = 2
        object lblSpecifiedPressureInLakesPresent: TLabel
          Left = 11
          Top = 19
          Width = 207
          Height = 18
          Caption = 'If lake water present or absent'
        end
        object comboSpecifiedPressureInLakesPresent: TComboBox
          Left = 11
          Top = 43
          Width = 286
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 1
          TabOrder = 0
          Text = 'No change (0)'
          Items.Strings = (
            'Apply if lake water absent (-1)'
            'No change (0)'
            'Apply if lake water present (1)')
        end
      end
      object grpLakeSpecifiedU: TGroupBox
        Left = 3
        Top = 264
        Width = 589
        Height = 81
        Caption = 
          'Default effect of lakes on specified concentration or temperatur' +
          'e boundaries (ILKU)'
        TabOrder = 3
        object lblSpecifiedUInLakesPresent: TLabel
          Left = 11
          Top = 19
          Width = 207
          Height = 18
          Caption = 'If lake water present or absent'
        end
        object comboSpecifiedUInLakesPresent: TComboBox
          Left = 11
          Top = 43
          Width = 286
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 1
          TabOrder = 0
          Text = 'No change (0)'
          Items.Strings = (
            'Apply if lake water absent (-1)'
            'No change (0)'
            'Apply if lake water present (1)')
        end
      end
      object grpLakeGeneralizedFlow: TGroupBox
        Left = 3
        Top = 351
        Width = 589
        Height = 81
        Caption = 
          'Default effect of lakes on generalized-flow boundaries (ILKPG, C' +
          'TIPG)'
        TabOrder = 4
        object lblGeneralizedFlowPresent: TLabel
          Left = 11
          Top = 28
          Width = 207
          Height = 18
          Caption = 'If lake water present or absent'
        end
        object lblLakeGeneralizedFlowType: TLabel
          Left = 328
          Top = 28
          Width = 104
          Height = 18
          Caption = 'Interaction type'
        end
        object comboGeneralizedFlowPresent: TComboBox
          Left = 11
          Top = 52
          Width = 286
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 1
          TabOrder = 0
          Text = 'No change (0)'
          Items.Strings = (
            'Apply if lake water absent (-1)'
            'No change (0)'
            'Apply if lake water present (1)')
        end
        object comboLakeGeneralizedFlowType: TComboBox
          Left = 328
          Top = 52
          Width = 262
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 0
          TabOrder = 1
          Text = 'Like fluid source/sink (F)'
          Items.Strings = (
            'Like fluid source/sink (F)'
            'Like specified pressure (P)')
        end
      end
      object grp1: TGroupBox
        Left = 3
        Top = 438
        Width = 589
        Height = 81
        Caption = 
          'Default effect of lakes on generalized-transport boundaries (ILK' +
          'UG. CTIUG)'
        TabOrder = 5
        object lblGeneralizedTransportPresent: TLabel
          Left = 11
          Top = 19
          Width = 207
          Height = 18
          Caption = 'If lake water present or absent'
        end
        object lbl1: TLabel
          Left = 320
          Top = 19
          Width = 104
          Height = 18
          Caption = 'Interaction type'
        end
        object comboGeneralizedTransportPresent: TComboBox
          Left = 11
          Top = 42
          Width = 286
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 1
          TabOrder = 0
          Text = 'No change (0)'
          Items.Strings = (
            'Apply if lake water absent (-1)'
            'No change (0)'
            'Apply if lake water present (1)')
        end
        object comboLakeGeneralizedTransportType: TComboBox
          Left = 320
          Top = 43
          Width = 262
          Height = 26
          Style = csDropDownList
          Enabled = False
          ItemIndex = 0
          TabOrder = 1
          Text = 'Like solute/energy source/sink (S)'
          Items.Strings = (
            'Like solute/energy source/sink (S)'
            'Like spec. conc./temp. (U)')
        end
      end
    end
  end
  object jvpltvNavigation: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 179
    Height = 512
    PageDefault = 0
    PageList = jplMain
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnCustomDrawItem = jvpltvNavigationCustomDrawItem
    OnMouseDown = jvpltvNavigationMouseDown
    Items.NodeData = {
      0303000000380000000000000000000000FFFFFFFFFFFFFFFF00000000000000
      0000000000010D43006F006E00660069006700750072006100740069006F006E
      00280000000000000000000000FFFFFFFFFFFFFFFF0000000001000000000000
      0001055400690074006C006500400000000000000000000000FFFFFFFFFFFFFF
      FF000000000200000000000000011149006E0069007400690061006C0043006F
      006E0064006900740069006F006E007300}
    Items.Links = {03000000000000000100000002000000}
  end
  object rcLakes: TRbwController
    ControlList = <
      item
        Control = cbAllNodesLakes
      end
      item
        Control = rdeDefaultRechargeFrac
      end
      item
        Control = rdeDefaultDischargeFrac
      end
      item
        Control = rdeLakeOutput
      end
      item
        Control = comboFluidSourceInLakesPresent
      end
      item
        Control = comboSoluteSourceInLakesPresent
      end
      item
        Control = comboSpecifiedPressureInLakesPresent
      end
      item
        Control = comboSpecifiedUInLakesPresent
      end
      item
        Control = comboGeneralizedFlowPresent
      end
      item
        Control = comboLakeGeneralizedFlowType
      end
      item
        Control = comboGeneralizedTransportPresent
      end
      item
        Control = comboLakeGeneralizedTransportType
      end>
    Enabled = False
    OnEnabledChange = EnableLakeBottom
    Left = 96
    Top = 16
  end
end
