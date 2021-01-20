inherited frmPEST: TfrmPEST
  Caption = 'PEST Properties'
  ClientHeight = 461
  ClientWidth = 710
  ExplicitWidth = 726
  ExplicitHeight = 500
  PixelsPerInch = 96
  TextHeight = 18
  object splMain: TSplitter
    Left = 193
    Top = 0
    Width = 5
    Height = 419
    ExplicitLeft = 121
    ExplicitHeight = 287
  end
  object tvPEST: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 193
    Height = 419
    ShowButtons = True
    PageDefault = 0
    PageList = plMain
    Align = alLeft
    Indent = 19
    TabOrder = 0
    Items.Links = {00000000}
  end
  object plMain: TJvPageList
    Left = 198
    Top = 0
    Width = 512
    Height = 419
    ActivePage = jvspPilotPoints
    PropagateEnable = False
    Align = alClient
    OnChange = plMainChange
    object jvspBasic: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspBasic'
      object lblTemplateCharacter: TLabel
        Left = 16
        Top = 40
        Width = 132
        Height = 18
        Caption = 'Template character'
      end
      object lblFormulaMarker: TLabel
        Left = 16
        Top = 104
        Width = 110
        Height = 18
        Caption = 'Formula marker'
      end
      object lblPestDirectory: TLabel
        Left = 16
        Top = 168
        Width = 109
        Height = 18
        Caption = 'PEST Directory'
      end
      object cbPEST: TCheckBox
        Left = 16
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Use PEST'
        TabOrder = 0
      end
      object comboTemplateCharacter: TComboBox
        Left = 16
        Top = 64
        Width = 65
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 1
        Text = '~'
        OnChange = MarkerChange
        Items.Strings = (
          '~'
          '@'
          '$'
          '%'
          ':'
          ';'
          '?')
      end
      object comboFormulaMarker: TComboBox
        Left = 16
        Top = 128
        Width = 65
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = '~'
        OnChange = MarkerChange
        Items.Strings = (
          '~'
          '@'
          '$'
          '%'
          ':'
          ';'
          '?')
      end
      object diredPest: TJvDirectoryEdit
        Left = 16
        Top = 192
        Width = 305
        Height = 26
        TabOrder = 3
        Text = 'C:\PEST'
        OnChange = diredPestChange
      end
    end
    object jvspControlDataMode: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspControlDataMode'
      object lblPestMode: TLabel
        Left = 23
        Top = 40
        Width = 144
        Height = 18
        Caption = 'Mode (PESTMODE)'
      end
      object cbSaveRestart: TCheckBox
        Left = 23
        Top = 16
        Width = 277
        Height = 17
        Caption = 'Save restart information (RSTFLE)'
        TabOrder = 0
      end
      object comboPestMode: TComboBox
        Left = 23
        Top = 64
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 1
        Items.Strings = (
          'estimation'
          'prediction'
          'regularisation'
          'pareto')
      end
    end
    object jvspDimensions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspDimensions'
      object lblMaxCompDim: TLabel
        Left = 85
        Top = 6
        Width = 369
        Height = 18
        Caption = 'Maximum Compression Dimension (MAXCOMPDIM)'
      end
      object lblZeroLimit: TLabel
        Left = 85
        Top = 34
        Width = 306
        Height = 18
        Caption = 'Jacobian element threshold (DERZEROLIM)'
      end
      object rdeMaxCompDim: TRbwDataEntry
        Left = 6
        Top = 3
        Width = 73
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeZeroLimit: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 73
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspInversionControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspInversionControls'
      object lblInitialLambda: TLabel
        Left = 95
        Top = 6
        Width = 267
        Height = 18
        Caption = 'Initial Marquardt lambda (RLAMBDA1)'
      end
      object comboLambdaAdj: TLabel
        Left = 95
        Top = 34
        Width = 270
        Height = 18
        Caption = 'Lambda adjustment factor (RLAMFAC)'
      end
      object lblIterationClosure: TLabel
        Left = 95
        Top = 62
        Width = 382
        Height = 18
        Caption = 'Sufficient objective function improvement (PHIRATSUF)'
      end
      object lblLambdaTermination: TLabel
        Left = 95
        Top = 90
        Width = 356
        Height = 18
        Caption = 'Lambda search termination criterion (PHIREDLAM)'
      end
      object lblMaxLambdas: TLabel
        Left = 95
        Top = 118
        Width = 221
        Height = 18
        Caption = 'Max lambdas to test (NUMLAM)'
      end
      object lblJacobianUpdate: TLabel
        Left = 95
        Top = 146
        Width = 287
        Height = 18
        Caption = 'Broyden Jacobian update (JACUPDATE)'
      end
      object rdeInitialLambda: TRbwDataEntry
        Left = 6
        Top = 3
        Width = 83
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeLambdaAdj: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 83
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeIterationClosure: TRbwDataEntry
        Left = 6
        Top = 59
        Width = 83
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeLambdaTermination: TRbwDataEntry
        Left = 6
        Top = 87
        Width = 83
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxLambdas: TRbwDataEntry
        Left = 6
        Top = 115
        Width = 83
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeJacobianUpdate: TRbwDataEntry
        Left = 6
        Top = 143
        Width = 83
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbLamForgive: TCheckBox
        Left = 6
        Top = 171
        Width = 467
        Height = 22
        Caption = 'Ignore failed model runs when testing lambdas (LAMFORGIVE)'
        TabOrder = 6
      end
      object cbDerForgive: TCheckBox
        Left = 6
        Top = 199
        Width = 499
        Height = 18
        Caption = 
          'Ignore failed model runs when calculating derivatives (DERFORGIV' +
          'E)'
        TabOrder = 7
      end
    end
    object jvspParameterAdjustmentControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      object lblMaxRelParamChange: TLabel
        Left = 93
        Top = 7
        Width = 364
        Height = 18
        Caption = 'Maximum relative parameter change (RELPARMAX)'
      end
      object lblMaxFacParamChange: TLabel
        Left = 93
        Top = 35
        Width = 354
        Height = 18
        Caption = 'Maximum factor parameter change (FACPARMAX)'
      end
      object lblFactorOriginal: TLabel
        Left = 93
        Top = 62
        Width = 363
        Height = 18
        Caption = 'Minimum fraction of original param value (FACORIG)'
      end
      object lblBoundStick: TLabel
        Left = 93
        Top = 90
        Width = 353
        Height = 36
        Caption = 
          'Number of iterations a parameter can be at its limit before stic' +
          'king to limit (IBOUNDSTICK)'
        WordWrap = True
      end
      object rdeMaxRelParamChange: TRbwDataEntry
        Left = 6
        Top = 3
        Width = 81
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeMaxFacParamChange: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 81
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeFactorOriginal: TRbwDataEntry
        Left = 6
        Top = 59
        Width = 81
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeBoundStick: TRbwDataEntry
        Left = 6
        Top = 87
        Width = 81
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbParameterBending: TCheckBox
        Left = 6
        Top = 124
        Width = 467
        Height = 21
        Caption = 'Upgrade parameter vector bending (UPVECBEND)'
        TabOrder = 4
      end
    end
    object jvspInversionControls2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspInversionControls2'
      object lblSwitchCriterion: TLabel
        Left = 93
        Top = 6
        Width = 401
        Height = 18
        Caption = 'Criterion for using higher-order derivatives (PHIREDSWH)'
      end
      object lblSwitchCount: TLabel
        Left = 93
        Top = 31
        Width = 323
        Height = 36
        Caption = 
          'Number of iterations before using higher-order derivatives (NOPT' +
          'SWITCH)'
        WordWrap = True
      end
      object lblSplitSlopeCriterion: TLabel
        Left = 93
        Top = 71
        Width = 365
        Height = 18
        Caption = 'Criterion for starting split slope analysis (SPLITSWH)'
      end
      object lblAutomaticUserIntervation: TLabel
        Left = 8
        Top = 96
        Width = 242
        Height = 18
        Caption = 'Automatic user intervation (DOAUI)'
      end
      object rdeSwitchCriterion: TRbwDataEntry
        Left = 6
        Top = 2
        Width = 81
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = rdeSwitchCriterionChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeSwitchCount: TRbwDataEntry
        Left = 6
        Top = 30
        Width = 81
        Height = 22
        TabOrder = 1
        Text = '1'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSplitSlopeCriterion: TRbwDataEntry
        Left = 6
        Top = 68
        Width = 81
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object comboAutomaticUserIntervation: TComboBox
        Left = 6
        Top = 120
        Width = 267
        Height = 26
        Style = csDropDownList
        ItemIndex = 2
        TabOrder = 3
        Text = 'Most sensitive removed first (auid)'
        Items.Strings = (
          'none (noaui)'
          'Least sensitive removed first (aui)'
          'Most sensitive removed first (auid)')
      end
      object cbSensitivityReuse: TCheckBox
        Left = 6
        Top = 152
        Width = 353
        Height = 17
        Caption = 'Sensitivity reuse (DOSENREUSE)'
        TabOrder = 4
      end
      object cbBoundsScaling: TCheckBox
        Left = 6
        Top = 175
        Width = 306
        Height = 17
        Caption = 'Use bounds scaling (BOUNDSCALE)'
        TabOrder = 5
      end
    end
    object jvspIterationControls: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspIterationControls'
      object lblMaxIterations: TLabel
        Left = 93
        Top = 6
        Width = 294
        Height = 18
        Caption = 'Maximun number of Iterations (NOPTMAX)'
      end
      object lblPhiReductionCriterion: TLabel
        Left = 93
        Top = 34
        Width = 362
        Height = 18
        Caption = 'Small Phi reduction stopping criterion (PHIREDSTP)'
      end
      object lblPhiReductionCount: TLabel
        Left = 93
        Top = 62
        Width = 316
        Height = 18
        Caption = 'Small Phi reduction iteration count (NPHISTP)'
      end
      object lblNoReductionCount: TLabel
        Left = 93
        Top = 90
        Width = 350
        Height = 18
        Caption = 'No Phi reduction stopping critierion (NPHINORED)'
      end
      object lblSmallParameterReduction: TLabel
        Left = 93
        Top = 118
        Width = 361
        Height = 18
        Caption = 'Parameter change stopping criterion (RELPARSTP)'
      end
      object lblrdeSmallParameterReductionCount: TLabel
        Left = 93
        Top = 146
        Width = 318
        Height = 18
        Caption = 'Parameter change iteration count (NRELPAR)'
      end
      object lblPhiStoppingThreshold: TLabel
        Left = 93
        Top = 170
        Width = 293
        Height = 18
        Caption = 'Phi stopping criterion (PHISTOPTHRESH)'
      end
      object lblAbandon: TLabel
        Left = 93
        Top = 242
        Width = 310
        Height = 18
        Caption = 'Large Phi stopping criterion (PHIABANDON)'
      end
      object rdeMaxIterations: TRbwDataEntry
        Left = 6
        Top = 3
        Width = 81
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdePhiReductionCriterion: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 81
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdePhiReductionCount: TRbwDataEntry
        Left = 6
        Top = 58
        Width = 81
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeNoReductionCount: TRbwDataEntry
        Left = 6
        Top = 86
        Width = 81
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeSmallParameterReduction: TRbwDataEntry
        Left = 6
        Top = 114
        Width = 81
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeSmallParameterReductionCount: TRbwDataEntry
        Left = 6
        Top = 142
        Width = 81
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdePhiStoppingThreshold: TRbwDataEntry
        Left = 6
        Top = 172
        Width = 81
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object cbLastRun: TCheckBox
        Left = 6
        Top = 200
        Width = 457
        Height = 33
        Caption = 
          'After optimisation is complete, run model one more time with opt' +
          'imized parameter values (LASTRUN)'
        TabOrder = 7
        WordWrap = True
      end
      object rdeAbandon: TRbwDataEntry
        Left = 6
        Top = 239
        Width = 81
        Height = 22
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
    end
    object jvspOutputOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspOutputOptions'
      object cbWriteCov: TCheckBox
        Left = 6
        Top = 3
        Width = 395
        Height = 17
        Caption = 'Write posterior covariance matrix (ICOV)'
        TabOrder = 0
      end
      object cbWriteCorrCoef: TCheckBox
        Left = 6
        Top = 24
        Width = 345
        Height = 17
        Caption = 'Write correlation coeficients matrix (ICOR)'
        TabOrder = 1
      end
      object cbWriteEigenvectors: TCheckBox
        Left = 6
        Top = 47
        Width = 329
        Height = 17
        Caption = 'Write eigenvectors and eigenvalues (IEIG)'
        TabOrder = 2
      end
      object cbWriteResolution: TCheckBox
        Left = 6
        Top = 70
        Width = 304
        Height = 17
        Caption = 'Write resolution data file (IRES)'
        TabOrder = 3
      end
      object cbWriteJacobian: TCheckBox
        Left = 6
        Top = 93
        Width = 329
        Height = 17
        Caption = 'Write Jacobian matrix file (JCOSAVE)'
        TabOrder = 4
      end
      object cbWriteJacobianEveryIteration: TCheckBox
        Left = 6
        Top = 116
        Width = 475
        Height = 17
        Caption = 'Write a Jacobian matrix file after every iteration (JCOSAVEITN)'
        TabOrder = 5
      end
      object cbWriteVerboseRunRecord: TCheckBox
        Left = 6
        Top = 139
        Width = 403
        Height = 17
        Caption = 'Write verbose run record (VERBOSEREC)'
        TabOrder = 6
      end
      object cbWriteIntermResidualForEveryIteration: TCheckBox
        Left = 6
        Top = 162
        Width = 475
        Height = 39
        Caption = 
          'Write a separate interm residuals file for every iteration (REIS' +
          'AVEITN)'
        TabOrder = 7
        WordWrap = True
      end
      object cbSaveParamValuesIteration: TCheckBox
        Left = 6
        Top = 201
        Width = 473
        Height = 17
        Caption = 'Save a parameter values file after each iteration (PARSAVEITN)'
        TabOrder = 8
      end
      object cbSaveParamValuesModelRun: TCheckBox
        Left = 6
        Top = 224
        Width = 493
        Height = 17
        Caption = 'Save a parameter values file after each model run (PARSAVERUN)'
        TabOrder = 9
      end
    end
    object jvspSingularValueDecomp: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspSingularValueDecomp'
      object lblSvdMode: TLabel
        Left = 6
        Top = 8
        Width = 347
        Height = 18
        Caption = 'Singular value decomposition model (SVDMODE)'
      end
      object lblMaxSingularValues: TLabel
        Left = 159
        Top = 71
        Width = 299
        Height = 36
        Caption = 'Maximum number of singular values before truncation (MAXSING)'
        WordWrap = True
      end
      object lblEigenThreshold: TLabel
        Left = 159
        Top = 123
        Width = 307
        Height = 36
        Caption = 'Eigenvalue ratio for singular value truncation (EIGTHRESH)'
        WordWrap = True
      end
      object lblEigenWrite: TLabel
        Left = 6
        Top = 162
        Width = 216
        Height = 18
        Caption = 'SVD output option (EIGWRITE)'
      end
      object comboSvdMode: TComboBox
        Left = 6
        Top = 32
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 0
        OnChange = comboSvdModeChange
        Items.Strings = (
          'Deactivate (0)'
          'Normal (1)'
          'Damped (2)')
      end
      object rdeMaxSingularValues: TRbwDataEntry
        Left = 6
        Top = 83
        Width = 145
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeEigenThreshold: TRbwDataEntry
        Left = 6
        Top = 120
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboEigenWrite: TComboBox
        Left = 6
        Top = 186
        Width = 299
        Height = 26
        Style = csDropDownList
        TabOrder = 3
        Items.Strings = (
          'singular values (1)'
          'singular values and eigenvectors (2)')
      end
    end
    object jvspLqsr: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspLqsr'
      object lblMatrixTolerance: TLabel
        Left = 157
        Top = 29
        Width = 216
        Height = 18
        Caption = 'Matrix tolerance (LSQR_ATOL)'
      end
      object lblRightHandSideTolerance: TLabel
        Left = 157
        Top = 57
        Width = 283
        Height = 18
        Caption = 'Right hand side tolerance (LSQR_BTOL)'
      end
      object lblConditionNumberLimit: TLabel
        Left = 157
        Top = 82
        Width = 345
        Height = 18
        Caption = 'Upper limit on condition number (LSQR_CONLIM)'
      end
      object lblMaxLqsrIterations: TLabel
        Left = 157
        Top = 106
        Width = 322
        Height = 36
        Caption = 
          'Maximum number of iterations (LSQR_ITNLIM)'#13#10'(Use zero to set aut' +
          'omatically)'
      end
      object cbUseLqsr: TCheckBox
        Left = 6
        Top = 3
        Width = 299
        Height = 17
        Caption = 'Use LQSR (LSQRMODE)'
        TabOrder = 0
        OnClick = cbUseLqsrClick
      end
      object rdeMatrixTolerance: TRbwDataEntry
        Left = 6
        Top = 26
        Width = 145
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRightHandSideTolerance: TRbwDataEntry
        Left = 6
        Top = 53
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeConditionNumberLimit: TRbwDataEntry
        Left = 6
        Top = 81
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxLqsrIterations: TRbwDataEntry
        Left = 6
        Top = 113
        Width = 145
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbWriteLsqrOutput: TCheckBox
        Left = 6
        Top = 148
        Width = 353
        Height = 17
        Caption = 'Write LSQR output (LSQRWRITE)'
        TabOrder = 5
      end
    end
    object jvspObservationGroups: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspObservationGroups'
      inline frameObservationGroups: TframeGrid
        Left = 0
        Top = 0
        Width = 512
        Height = 419
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 512
        ExplicitHeight = 419
        inherited Panel: TPanel
          Top = 378
          Width = 512
          ExplicitTop = 378
          ExplicitWidth = 512
          DesignSize = (
            512
            41)
          inherited lbNumber: TLabel
            Width = 209
            Height = 18
            Caption = 'Number of observation groups'
            ExplicitWidth = 209
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 419
            ExplicitLeft = 419
          end
          inherited sbInsert: TSpeedButton
            Left = 448
            OnClick = frameObservationGroupssbInsertClick
            ExplicitLeft = 448
          end
          inherited sbDelete: TSpeedButton
            Left = 477
            OnClick = frameObservationGroupssbDeleteClick
            ExplicitLeft = 477
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            OnChange = frameObservationGroupsseNumberChange
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 512
          Height = 378
          ColCount = 4
          OnSelectCell = frameObservationGroupsGridSelectCell
          OnSetEditText = frameObservationGroupsGridSetEditText
          OnButtonClick = frameObservationGroupsGridButtonClick
          Columns = <
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 12
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Boolean
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4Real
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end
            item
              AutoAdjustRowHeights = True
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = True
              ButtonWidth = 20
              CheckMax = False
              CheckMin = False
              ComboUsed = False
              Format = rcf4String
              LimitToList = False
              MaxLength = 0
              ParentButtonFont = False
              WordWrapCaptions = True
              WordWrapCells = False
              CaseSensitivePicklist = False
              CheckStyle = csCheck
              AutoAdjustColWidths = True
            end>
          ExplicitWidth = 512
          ExplicitHeight = 378
        end
      end
    end
    object jvspObsGroupAssignments: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspObsGroupAssignments'
      inline frameParentObsGroups: TframeParentChild
        Left = 0
        Top = 0
        Width = 512
        Height = 419
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 512
        ExplicitHeight = 419
        inherited tvTree: TTreeView
          Width = 512
          Height = 419
          ExplicitWidth = 512
          ExplicitHeight = 419
        end
      end
    end
    object jvspPilotPoints: TJvStandardPage
      Left = 0
      Top = 0
      Width = 512
      Height = 419
      Caption = 'jvspPilotPoints'
      object Panel2: TPanel
        Left = 0
        Top = 124
        Width = 512
        Height = 295
        Align = alClient
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 242
          Top = 1
          Width = 5
          Height = 293
        end
        object gbIndividualPilotPoints: TGroupBox
          Left = 1
          Top = 1
          Width = 241
          Height = 293
          Align = alLeft
          Caption = 'Individually specified pilot points'
          TabOrder = 0
          inline framePilotPoints: TframeGrid
            Left = 2
            Top = 89
            Width = 237
            Height = 202
            Align = alClient
            TabOrder = 0
            ExplicitLeft = 2
            ExplicitTop = 89
            ExplicitWidth = 237
            ExplicitHeight = 202
            inherited Panel: TPanel
              Top = 161
              Width = 237
              ExplicitTop = 161
              ExplicitWidth = 237
              inherited lbNumber: TLabel
                Width = 151
                Height = 18
                Caption = 'Number of pilot points'
                ExplicitWidth = 151
                ExplicitHeight = 18
              end
              inherited sbAdd: TSpeedButton
                Left = 117
                ExplicitLeft = 252
              end
              inherited sbInsert: TSpeedButton
                Left = 140
                ExplicitLeft = 298
              end
              inherited sbDelete: TSpeedButton
                Left = 163
                ExplicitLeft = 345
              end
              inherited seNumber: TJvSpinEdit
                Height = 26
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 237
              Height = 161
              ColCount = 2
              Columns = <
                item
                  AutoAdjustRowHeights = False
                  ButtonCaption = '...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -11
                  ButtonFont.Name = 'Tahoma'
                  ButtonFont.Style = []
                  ButtonUsed = False
                  ButtonWidth = 20
                  CheckMax = False
                  CheckMin = False
                  ComboUsed = False
                  Format = rcf4Real
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end
                item
                  AutoAdjustRowHeights = False
                  ButtonCaption = '...'
                  ButtonFont.Charset = DEFAULT_CHARSET
                  ButtonFont.Color = clWindowText
                  ButtonFont.Height = -11
                  ButtonFont.Name = 'Tahoma'
                  ButtonFont.Style = []
                  ButtonUsed = False
                  ButtonWidth = 20
                  CheckMax = False
                  CheckMin = False
                  ComboUsed = False
                  Format = rcf4Real
                  LimitToList = False
                  MaxLength = 0
                  ParentButtonFont = False
                  WordWrapCaptions = True
                  WordWrapCells = False
                  CaseSensitivePicklist = False
                  CheckStyle = csCheck
                  AutoAdjustColWidths = True
                end>
              ExplicitWidth = 237
              ExplicitHeight = 161
            end
          end
          object Panel1: TPanel
            Left = 2
            Top = 20
            Width = 237
            Height = 69
            Align = alTop
            TabOrder = 1
            object btnImportShape: TButton
              Left = 8
              Top = 8
              Width = 177
              Height = 25
              Caption = 'Import from Shapefile'
              TabOrder = 0
              OnClick = btnImportShapeClick
            end
            object btnImportText: TButton
              Left = 8
              Top = 39
              Width = 177
              Height = 25
              Caption = 'Import from text file'
              TabOrder = 1
              OnClick = btnImportTextClick
            end
          end
        end
        object gbBetweenPointObs: TGroupBox
          Left = 247
          Top = 1
          Width = 264
          Height = 293
          Align = alClient
          Caption = 'Between point observations'
          TabOrder = 1
          object rdgBetweenObs: TRbwDataGrid4
            Left = 2
            Top = 113
            Width = 260
            Height = 178
            Align = alClient
            ColCount = 2
            FixedCols = 0
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goAlwaysShowEditor]
            TabOrder = 0
            ExtendedAutoDistributeText = False
            AutoMultiEdit = False
            AutoDistributeText = False
            AutoIncreaseColCount = False
            AutoIncreaseRowCount = False
            SelectedRowOrColumnColor = clAqua
            UnselectableColor = clBtnFace
            ColorRangeSelection = False
            Columns = <
              item
                AutoAdjustRowHeights = False
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end
              item
                AutoAdjustRowHeights = False
                ButtonCaption = '...'
                ButtonFont.Charset = DEFAULT_CHARSET
                ButtonFont.Color = clWindowText
                ButtonFont.Height = -11
                ButtonFont.Name = 'Tahoma'
                ButtonFont.Style = []
                ButtonUsed = False
                ButtonWidth = 20
                CheckMax = False
                CheckMin = False
                ComboUsed = False
                Format = rcf4Real
                LimitToList = False
                MaxLength = 0
                ParentButtonFont = False
                WordWrapCaptions = False
                WordWrapCells = False
                CaseSensitivePicklist = False
                CheckStyle = csCheck
                AutoAdjustColWidths = True
              end>
            WordWrapRowCaptions = False
            ColWidths = (
              64
              64)
          end
          object Panel3: TPanel
            Left = 2
            Top = 20
            Width = 260
            Height = 93
            Align = alTop
            TabOrder = 1
            object cbUseBetweenObs: TCheckBox
              Left = 4
              Top = 6
              Width = 221
              Height = 33
              Caption = 'Use pilot points '#13#10'between observations'
              TabOrder = 0
              WordWrap = True
            end
            object btnBetweenObservations: TButton
              Left = 4
              Top = 45
              Width = 253
              Height = 42
              Caption = 'Generate pilot points'#13#10'between point observations'
              TabOrder = 1
              WordWrap = True
              OnClick = btnBetweenObservationsClick
            end
          end
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 512
        Height = 124
        Align = alTop
        TabOrder = 1
        object lblPilotPointBuffer: TLabel
          Left = 6
          Top = 39
          Width = 112
          Height = 18
          Caption = 'Pilot point buffer'
        end
        object cbShowPilotPoints: TCheckBox
          Left = 6
          Top = 16
          Width = 227
          Height = 17
          Caption = 'Show candidate pilot points'
          TabOrder = 0
        end
        object gbArray: TGroupBox
          Left = 248
          Top = 1
          Width = 263
          Height = 122
          Align = alRight
          Caption = 'Regularly spaced pilot points'
          TabOrder = 1
          object lblArrayPattern: TLabel
            Left = 16
            Top = 11
            Width = 50
            Height = 18
            Caption = 'Pattern'
          end
          object lblPilotPointSpacing: TLabel
            Left = 14
            Top = 63
            Width = 128
            Height = 18
            Caption = 'Pilot point spacing'
          end
          object comboArrayPattern: TComboBox
            Left = 16
            Top = 35
            Width = 145
            Height = 26
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 0
            Text = 'none'
            Items.Strings = (
              'none'
              'Rectangular'
              'Triangular')
          end
          object rdePilotPointSpacing: TRbwDataEntry
            Left = 14
            Top = 87
            Width = 145
            Height = 22
            TabOrder = 1
            Text = '0'
            DataType = dtReal
            Max = 1.000000000000000000
            CheckMin = True
            ChangeDisabledColor = True
          end
        end
        object rdePilotPointBuffer: TRbwDataEntry
          Left = 6
          Top = 63
          Width = 145
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
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 419
    Width = 710
    Height = 42
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 446
      Top = 6
      Width = 83
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 535
      Top = 6
      Width = 83
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 624
      Top = 6
      Width = 83
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object dlgOpenCovarianceMatrixFile: TOpenDialog
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 430
    Top = 72
  end
  object dlgOpenPilotPoints: TOpenDialog
    Filter = 
      'Shapefile (*.shp)|*.shp|Text files (*.txt,*.csv)|*.csv;*.txt|Tex' +
      't file (*.txt)|*.txt|CSV file (*.csv)|*.csv'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 438
    Top = 16
  end
end
