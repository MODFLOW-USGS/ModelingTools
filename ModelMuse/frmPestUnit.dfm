inherited frmPEST: TfrmPEST
  HelpType = htKeyword
  HelpKeyword = 'Pest_Properties_Dialog_Box'
  Caption = 'PEST Properties'
  ClientHeight = 484
  ClientWidth = 760
  ExplicitWidth = 776
  ExplicitHeight = 523
  TextHeight = 18
  object splMain: TSplitter
    Left = 193
    Top = 0
    Width = 5
    Height = 442
    ExplicitLeft = 121
    ExplicitHeight = 287
  end
  object tvPEST: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 193
    Height = 442
    AutoExpand = False
    ShowButtons = True
    PageDefault = 0
    PageList = plMain
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    OnMouseDown = tvPESTMouseDown
    Items.Links = {00000000}
  end
  object plMain: TJvPageList
    Left = 198
    Top = 0
    Width = 562
    Height = 442
    ActivePage = jvspPriorInfoHorizContinuity
    PropagateEnable = False
    Align = alClient
    OnChange = plMainChange
    object jvspBasic: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Basic_Pane'
      Caption = 'jvspBasic'
      DesignSize = (
        562
        442)
      object lblTemplateCharacter: TLabel
        Left = 16
        Top = 72
        Width = 138
        Height = 18
        Caption = 'Parameter delimiter'
      end
      object lblFormulaMarker: TLabel
        Left = 16
        Top = 136
        Width = 121
        Height = 18
        Caption = 'Formula delimiter'
      end
      object lblPestDirectory: TLabel
        Left = 17
        Top = 264
        Width = 109
        Height = 18
        Caption = 'PEST Directory'
      end
      object lblArrayMarker: TLabel
        Left = 17
        Top = 200
        Width = 183
        Height = 18
        Caption = 'Array substitution delimiter'
      end
      object htlblZoneBudget6: TJvHTLabel
        Left = 17
        Top = 287
        Width = 181
        Height = 19
        Caption = 
          '<a href="https://pesthomepage.org/">https://pesthomepage.org/</a' +
          '>'
        SuperSubScriptRatio = 0.666666666666666600
      end
      object lblPestStatus: TLabel
        Left = 17
        Top = 16
        Width = 90
        Height = 18
        Caption = 'PEST Status'
      end
      object comboTemplateCharacter: TComboBox
        Left = 16
        Top = 96
        Width = 65
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 0
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
        Top = 160
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
      object diredPest: TJvDirectoryEdit
        Left = 17
        Top = 312
        Width = 508
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'C:\PEST'
        OnChange = diredPestChange
      end
      object comboArrayMarker: TComboBox
        Left = 17
        Top = 224
        Width = 65
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 3
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
      object comboPestStatus: TComboBox
        Left = 16
        Top = 40
        Width = 233
        Height = 26
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 4
        Text = 'Inactive'
        Items.Strings = (
          'Inactive'
          'Only define observations'
          'Active')
      end
    end
    object jvspControlDataMode: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Mode_and_Dimen'
      Caption = 'jvspControlDataMode'
      object lblPestMode: TLabel
        Left = 14
        Top = 40
        Width = 144
        Height = 18
        Caption = 'Mode (PESTMODE)'
      end
      object lblMaxCompDim: TLabel
        Left = 93
        Top = 102
        Width = 369
        Height = 18
        Caption = 'Maximum Compression Dimension (MAXCOMPDIM)'
      end
      object lblZeroLimit: TLabel
        Left = 93
        Top = 130
        Width = 306
        Height = 18
        Caption = 'Jacobian element threshold (DERZEROLIM)'
      end
      object cbSaveRestart: TCheckBox
        Left = 14
        Top = 17
        Width = 277
        Height = 17
        Caption = 'Save restart information (RSTFLE)'
        TabOrder = 0
      end
      object comboPestMode: TComboBox
        Left = 14
        Top = 64
        Width = 203
        Height = 26
        Style = csDropDownList
        TabOrder = 1
        OnChange = comboPestModeChange
        Items.Strings = (
          'estimation'
          'prediction analysis'
          'regularization'
          'pareto')
      end
      object rdeMaxCompDim: TRbwDataEntry
        Left = 14
        Top = 99
        Width = 73
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtInteger
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeZeroLimit: TRbwDataEntry
        Left = 14
        Top = 127
        Width = 73
        Height = 22
        TabOrder = 3
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
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Inversion_Cont'
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
        OnChange = rdeLambdaAdjChange
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
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Parameter_Adju'
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
        OnChange = rdeMaxRelParamChangeChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxFacParamChange: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 81
        Height = 22
        TabOrder = 1
        Text = '1'
        OnChange = rdeMaxFacParamChangeChange
        DataType = dtReal
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFactorOriginal: TRbwDataEntry
        Left = 6
        Top = 59
        Width = 81
        Height = 22
        TabOrder = 2
        Text = '0'
        OnChange = rdeFactorOriginalChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
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
        Top = 132
        Width = 467
        Height = 21
        Caption = 'Upgrade parameter vector bending (UPVECBEND)'
        TabOrder = 4
      end
    end
    object jvspInversionControls2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Inversion_Con2'
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
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Iteration_Cont'
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
        Min = -2.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdePhiReductionCriterion: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 81
        Height = 22
        TabOrder = 1
        Text = '0'
        OnChange = rdePhiReductionCriterionChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdePhiReductionCount: TRbwDataEntry
        Left = 6
        Top = 58
        Width = 81
        Height = 22
        TabOrder = 2
        Text = '1'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeNoReductionCount: TRbwDataEntry
        Left = 6
        Top = 86
        Width = 81
        Height = 22
        TabOrder = 3
        Text = '1'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSmallParameterReduction: TRbwDataEntry
        Left = 6
        Top = 114
        Width = 81
        Height = 22
        TabOrder = 4
        Text = '0'
        OnChange = rdeSmallParameterReductionChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSmallParameterReductionCount: TRbwDataEntry
        Left = 6
        Top = 142
        Width = 81
        Height = 22
        TabOrder = 5
        Text = '1'
        DataType = dtInteger
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdePhiStoppingThreshold: TRbwDataEntry
        Left = 6
        Top = 172
        Width = 81
        Height = 22
        TabOrder = 6
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
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
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspOutputOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Output_Pane'
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
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Singular_Value'
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
          'singular values (0)'
          'singular values and eigenvectors (1)')
      end
    end
    object jvspLsqr: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_LQSR_Pane'
      Caption = 'jvspLsqr'
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
      object lblMaxLsqrIterations: TLabel
        Left = 157
        Top = 106
        Width = 322
        Height = 36
        Caption = 
          'Maximum number of iterations (LSQR_ITNLIM)'#13#10'(Use zero to set aut' +
          'omatically)'
      end
      object cbUseLsqr: TCheckBox
        Left = 6
        Top = 3
        Width = 299
        Height = 17
        Caption = 'Use LSQR (LSQRMODE)'
        TabOrder = 0
        OnClick = cbUseLsqrClick
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
      object rdeMaxLsqrIterations: TRbwDataEntry
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
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Observation_Gr'
      Caption = 'jvspObservationGroups'
      inline frameObservationGroups: TframeGrid
        Left = 0
        Top = 0
        Width = 562
        Height = 442
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 562
        ExplicitHeight = 442
        inherited Panel: TPanel
          Top = 401
          Width = 562
          ExplicitTop = 401
          ExplicitWidth = 562
          DesignSize = (
            562
            41)
          inherited lbNumber: TLabel
            Width = 209
            Height = 18
            Caption = 'Number of observation groups'
            ExplicitWidth = 209
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 442
            ExplicitLeft = 419
          end
          inherited sbInsert: TSpeedButton
            Left = 473
            OnClick = frameObservationGroupssbInsertClick
            ExplicitLeft = 448
          end
          inherited sbDelete: TSpeedButton
            Left = 500
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
          Width = 562
          Height = 401
          ColCount = 5
          OnSelectCell = frameObservationGroupsGridSelectCell
          OnSetEditText = frameObservationGroupsGridSetEditText
          OnBeforeDrawCell = frameObservationGroupsGridBeforeDrawCell
          OnButtonClick = frameObservationGroupsGridButtonClick
          OnStateChange = frameObservationGroupsGridStateChange
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
              ButtonCaption = '...'
              ButtonFont.Charset = DEFAULT_CHARSET
              ButtonFont.Color = clWindowText
              ButtonFont.Height = -11
              ButtonFont.Name = 'Tahoma'
              ButtonFont.Style = []
              ButtonUsed = False
              ButtonWidth = 20
              CheckMax = False
              CheckMin = True
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
              AutoAdjustCaptionRowHeights = False
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
          ExplicitWidth = 562
          ExplicitHeight = 401
        end
      end
    end
    object jvspObsGroupAssignments: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Observation_G2'
      Caption = 'jvspObsGroupAssignments'
      ExplicitWidth = 426
      ExplicitHeight = 399
      inline frameParentObsGroups: TframeParentChild
        Left = 0
        Top = 0
        Width = 562
        Height = 442
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 426
        ExplicitHeight = 399
        inherited tvTree: TTreeView
          Width = 562
          Height = 442
          ExplicitWidth = 562
          ExplicitHeight = 442
        end
      end
    end
    object jvspPilotPoints: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'Pest_Properties_Pilot_Points_P'
      Caption = 'jvspPilotPoints'
      object Panel2: TPanel
        Left = 0
        Top = 140
        Width = 562
        Height = 302
        Align = alClient
        TabOrder = 0
        object Splitter1: TSplitter
          Left = 242
          Top = 1
          Width = 5
          Height = 300
          ExplicitHeight = 293
        end
        object gbIndividualPilotPoints: TGroupBox
          Left = 1
          Top = 1
          Width = 241
          Height = 300
          Align = alLeft
          Caption = 'Individually specified pilot points'
          TabOrder = 0
          inline framePilotPoints: TframeGrid
            Left = 2
            Top = 89
            Width = 237
            Height = 209
            Align = alClient
            TabOrder = 0
            ExplicitLeft = 2
            ExplicitTop = 89
            ExplicitWidth = 237
            ExplicitHeight = 209
            inherited Panel: TPanel
              Top = 143
              Width = 237
              Height = 66
              ExplicitTop = 143
              ExplicitWidth = 237
              ExplicitHeight = 66
              inherited lbNumber: TLabel
                Left = 82
                Top = 33
                Width = 151
                Height = 18
                Caption = 'Number of pilot points'
                ExplicitLeft = 82
                ExplicitTop = 33
                ExplicitWidth = 151
                ExplicitHeight = 18
              end
              inherited sbAdd: TSpeedButton
                Left = 117
                Hint = 'Add a pilot point|Add a pilot point after the last pilot point'
                ExplicitLeft = 117
              end
              inherited sbInsert: TSpeedButton
                Left = 140
                Hint = 
                  'Insert a pilot point|Insert a pilot point before the selected pi' +
                  'lot point'
                ExplicitLeft = 140
              end
              inherited sbDelete: TSpeedButton
                Left = 163
                Hint = 'Delete pilot point|Delete the selected pilot point'
                ExplicitLeft = 163
              end
              inherited seNumber: TJvSpinEdit
                Left = 11
                Top = 30
                Height = 26
                OnChange = framePilotPointsseNumberChange
                ExplicitLeft = 11
                ExplicitTop = 30
                ExplicitHeight = 26
              end
            end
            inherited Grid: TRbwDataGrid4
              Width = 237
              Height = 143
              ColCount = 2
              Columns = <
                item
                  AutoAdjustRowHeights = False
                  AutoAdjustCaptionRowHeights = False
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
                  AutoAdjustCaptionRowHeights = False
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
              ExplicitHeight = 143
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
          Width = 314
          Height = 300
          Align = alClient
          Caption = 'Between point observations'
          TabOrder = 1
          object rdgBetweenObs: TRbwDataGrid4
            Left = 2
            Top = 169
            Width = 310
            Height = 129
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
                AutoAdjustCaptionRowHeights = False
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
                AutoAdjustCaptionRowHeights = False
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
            Width = 310
            Height = 149
            Align = alTop
            TabOrder = 1
            object lblMinSeparation: TLabel
              Left = 4
              Top = 45
              Width = 141
              Height = 18
              Caption = 'Minimum separation'
            end
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
              Top = 97
              Width = 248
              Height = 42
              Caption = 'Generate pilot points'#13#10'between point observations'
              TabOrder = 1
              WordWrap = True
              OnClick = btnBetweenObservationsClick
            end
            object rdeMinSeparation: TRbwDataEntry
              Left = 4
              Top = 69
              Width = 109
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
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 140
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
          Left = 298
          Top = 1
          Width = 263
          Height = 138
          Align = alRight
          Caption = 'Regularly spaced pilot points'
          TabOrder = 1
          object lblArrayPattern: TLabel
            Left = 16
            Top = 27
            Width = 50
            Height = 18
            Caption = 'Pattern'
          end
          object lblPilotPointSpacing: TLabel
            Left = 14
            Top = 79
            Width = 128
            Height = 18
            Caption = 'Pilot point spacing'
          end
          object comboArrayPattern: TComboBox
            Left = 16
            Top = 51
            Width = 145
            Height = 26
            Style = csDropDownList
            TabOrder = 0
            OnChange = comboArrayPatternChange
            Items.Strings = (
              'none'
              'Square'
              'Triangular')
          end
          object rdePilotPointSpacing: TRbwDataEntry
            Left = 14
            Top = 103
            Width = 145
            Height = 22
            TabOrder = 1
            Text = '0'
            OnChange = rdePilotPointSpacingChange
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
          OnChange = rdePilotPointBufferChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
      end
    end
    object jvspRegularisation: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Regularization'
      object lblPhimLim: TLabel
        Left = 127
        Top = 19
        Width = 342
        Height = 18
        Caption = 'Target measurement objective function (PHIMLIM)'
      end
      object lblPhimAccept: TLabel
        Left = 127
        Top = 70
        Width = 419
        Height = 18
        Caption = 'Acceptable measurement objective function (PHIMACCEPT)'
      end
      object lblFRACPHIM: TLabel
        Left = 127
        Top = 98
        Width = 306
        Height = 18
        Caption = 'Fraction for updating PHIMLIM (FRACPHIM)'
      end
      object lblWFINIT: TLabel
        Left = 127
        Top = 148
        Width = 290
        Height = 18
        Caption = 'Initial regularisation weight factor (WFINIT)'
      end
      object lblWFMIN: TLabel
        Left = 127
        Top = 175
        Width = 319
        Height = 18
        Caption = 'Minimum regularisation weight factor (WFMIN)'
      end
      object LBLWFMAX: TLabel
        Left = 127
        Top = 202
        Width = 331
        Height = 18
        Caption = 'Maximum regularisation weight factor (WFMAX)'
      end
      object lblWFFAC: TLabel
        Left = 127
        Top = 229
        Width = 291
        Height = 18
        Caption = 'Weight Factor adjustment factor (WFFAC)'
      end
      object lblWFTOL: TLabel
        Left = 127
        Top = 256
        Width = 433
        Height = 18
        Caption = 'Convergence criterion for regularization weight factor (WFTOL)'
      end
      object rdePhimLim: TRbwDataEntry
        Left = 6
        Top = 16
        Width = 115
        Height = 22
        TabOrder = 0
        Text = '0'
        OnChange = rdePhimLimChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdePhimAccept: TRbwDataEntry
        Left = 6
        Top = 67
        Width = 115
        Height = 22
        TabOrder = 1
        Text = '0'
        OnChange = rdePhimAcceptChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbAutomaticallySetPHIMACCEPT: TCheckBox
        Left = 6
        Top = 44
        Width = 339
        Height = 17
        Caption = 'Automatically Set PHIMACCEPT'
        TabOrder = 2
        OnClick = cbAutomaticallySetPHIMACCEPTClick
      end
      object rdeFRACPHIM: TRbwDataEntry
        Left = 6
        Top = 95
        Width = 115
        Height = 22
        TabOrder = 3
        Text = '0'
        OnChange = rdeFRACPHIMChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbMemSave: TCheckBox
        Left = 6
        Top = 122
        Width = 403
        Height = 17
        Caption = 'Save memory at the expense of speed (MEMSAVE)'
        TabOrder = 4
      end
      object rdeWFINIT: TRbwDataEntry
        Left = 6
        Top = 145
        Width = 115
        Height = 21
        TabOrder = 5
        Text = '0'
        OnChange = rdeWFINITChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeWFMIN: TRbwDataEntry
        Left = 6
        Top = 172
        Width = 115
        Height = 21
        TabOrder = 6
        Text = '0'
        OnChange = rdeWFMINChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeWFMAX: TRbwDataEntry
        Left = 6
        Top = 199
        Width = 115
        Height = 21
        TabOrder = 7
        Text = '0'
        OnChange = rdeWFMAXChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeWFFAC: TRbwDataEntry
        Left = 6
        Top = 226
        Width = 115
        Height = 21
        TabOrder = 8
        Text = '1'
        OnChange = rdeWFFACChange
        DataType = dtReal
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeWFTOL: TRbwDataEntry
        Left = 6
        Top = 253
        Width = 115
        Height = 21
        TabOrder = 9
        Text = '0'
        OnChange = rdeWFTOLChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbLINREG: TCheckBox
        Left = 6
        Top = 280
        Width = 441
        Height = 17
        Caption = 'All regularization constraints are linear (LINREG)'
        TabOrder = 10
      end
      object cbREGCONTINUE: TCheckBox
        Left = 6
        Top = 303
        Width = 491
        Height = 17
        Caption = 'Continue regularization after reaching PHIMLIM (REGCONTINUE)'
        TabOrder = 11
      end
    end
    object jvspRegularizationOption: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Regularizatio2'
      Caption = 'jvspRegularizationOption'
      object lblIREGADJ: TLabel
        Left = 103
        Top = 299
        Width = 68
        Height = 18
        Caption = 'IREGADJ'
      end
      object lblNOPTREGADJ: TLabel
        Left = 103
        Top = 331
        Width = 444
        Height = 18
        Caption = 'Re-calculation of regularization weights interval (NOPTREGADJ)'
      end
      object lblREGWEIGHTRAT: TLabel
        Left = 103
        Top = 363
        Width = 271
        Height = 36
        Caption = 
          'Regularization weight ratio, logarithmic if negative (REGWEIGHTR' +
          'AT)'
        WordWrap = True
      end
      object lblREGSINGTHRESH: TLabel
        Left = 103
        Top = 410
        Width = 315
        Height = 18
        Caption = 'Regularization threshold (REGSINGTHRESH)'
      end
      object rgRegOption: TRadioGroup
        Left = 6
        Top = 3
        Width = 515
        Height = 105
        Caption = 'Regularization weight adjustment option (IREGADJ)'
        ItemIndex = 0
        Items.Strings = (
          'No inter-regularization-group weights adjustment takes place (0)'
          'Regularization is on a group by group basis (1,2,3)'
          'Regularization on an item by item basis. (4, 5)')
        TabOrder = 0
        OnClick = rgRegOptionClick
      end
      object rgGroupWeightMethod: TRadioGroup
        Left = 8
        Top = 114
        Width = 515
        Height = 71
        Caption = 'Group weight adjustment method (IREGADJ)'
        ItemIndex = 0
        Items.Strings = (
          'Equalize sensitivities (1,3)'
          'Equalize weights (2)')
        TabOrder = 1
        OnClick = rgGroupWeightMethodClick
      end
      object rgIndividualAdjustmentMethod: TRadioGroup
        Left = 8
        Top = 215
        Width = 515
        Height = 74
        Caption = 'Individual adjustment method (IREGADJ)'
        ItemIndex = 0
        Items.Strings = (
          'Weights calculated automatically (4)'
          'Items categorized into null space or solution space (5)')
        TabOrder = 2
        OnClick = rgIndividualAdjustmentMethodClick
      end
      object cbRegApplyGroupWeight: TCheckBox
        Left = 8
        Top = 192
        Width = 393
        Height = 17
        Caption = 'Also apply user specified group weight (3) (IREGADJ)'
        TabOrder = 3
        OnClick = cbRegApplyGroupWeightClick
      end
      object rdeIREGADJ: TRbwDataEntry
        Left = 8
        Top = 295
        Width = 89
        Height = 22
        TabOrder = 4
        Text = '0'
        OnChange = rdeIREGADJChange
        DataType = dtInteger
        Max = 5.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seNOPTREGADJ: TJvSpinEdit
        Left = 8
        Top = 328
        Width = 89
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 5
      end
      object rdeREGWEIGHTRAT: TRbwDataEntry
        Left = 8
        Top = 371
        Width = 89
        Height = 22
        TabOrder = 6
        Text = '0'
        OnChange = rdeREGWEIGHTRATChange
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object rdeREGSINGTHRESH: TRbwDataEntry
        Left = 6
        Top = 407
        Width = 91
        Height = 22
        TabOrder = 7
        Text = '0'
        OnChange = rdeREGSINGTHRESHChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspPriorInfoObsGroups: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Prior_Informat'
      Caption = 'jvspPriorInfoObsGroups'
      inline framePriorInfoObservationGroups: TframeGrid
        Left = 0
        Top = 57
        Width = 562
        Height = 385
        Align = alClient
        TabOrder = 0
        ExplicitTop = 57
        ExplicitWidth = 562
        ExplicitHeight = 385
        inherited Panel: TPanel
          Top = 344
          Width = 562
          ExplicitTop = 344
          ExplicitWidth = 562
          DesignSize = (
            562
            41)
          inherited lbNumber: TLabel
            Width = 241
            Height = 18
            Caption = 'Number of prior information groups'
            ExplicitWidth = 241
            ExplicitHeight = 18
          end
          inherited sbAdd: TSpeedButton
            Left = 330
            ExplicitLeft = 338
          end
          inherited sbInsert: TSpeedButton
            Left = 356
            ExplicitLeft = 367
          end
          inherited sbDelete: TSpeedButton
            Left = 384
            ExplicitLeft = 477
          end
          inherited seNumber: TJvSpinEdit
            Height = 26
            ExplicitHeight = 26
          end
        end
        inherited Grid: TRbwDataGrid4
          Width = 562
          Height = 344
          ColCount = 5
          OnExit = framePriorInfoObservationGroupsGridExit
          OnSelectCell = framePriorInfoObservationGroupsGridSelectCell
          OnSetEditText = framePriorInfoObservationGroupsGridSetEditText
          OnBeforeDrawCell = framePriorInfoObservationGroupsGridBeforeDrawCell
          OnButtonClick = framePriorInfoObservationGroupsGridButtonClick
          OnStateChange = framePriorInfoObservationGroupsGridStateChange
          Columns = <
            item
              AutoAdjustRowHeights = True
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
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
              AutoAdjustCaptionRowHeights = False
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
          ExplicitWidth = 562
          ExplicitHeight = 344
        end
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 57
        Align = alTop
        TabOrder = 1
        object btnMakeAllRegul: TButton
          Left = 6
          Top = 12
          Width = 171
          Height = 39
          Caption = 'Make all groups regularization groups'
          TabOrder = 0
          WordWrap = True
          OnClick = btnMakeAllRegulClick
        end
      end
    end
    object jvspPriorInfoInitialValue: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Initial_Value_'
      Caption = 'jvspPriorInfoInitialValue'
      object rdgPriorInfoInitialValue: TRbwDataGrid4
        Left = 0
        Top = 105
        Width = 562
        Height = 337
        Align = alClient
        ColCount = 4
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSetEditText = rdgPriorInfoInitialValueSetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = rdgPriorInfoInitialValueBeforeDrawCell
        OnStateChange = rdgPriorInfoInitialValueStateChange
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 12
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
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
        WordWrapRowCaptions = False
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 105
        Align = alTop
        TabOrder = 1
        object cbInitialValue: TCheckBox
          Left = 6
          Top = 12
          Width = 291
          Height = 17
          Caption = 'Use initial value prior information'
          TabOrder = 0
        end
        object btnCheckAllInitialValue: TButton
          Left = 6
          Top = 35
          Width = 235
          Height = 54
          Caption = 'Use initial value prior information for all parameters.'
          TabOrder = 1
          WordWrap = True
          OnClick = btnCheckAllInitialValueClick
        end
      end
    end
    object jvspPriorInfoHorizContinuity: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Within_Layer_C'
      Caption = 'jvspPriorInfoHorizContinuity'
      object pnlPriorInfoContinuity: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 161
        Align = alTop
        TabOrder = 0
        object lblSearchDistance: TLabel
          Left = 157
          Top = 36
          Width = 113
          Height = 18
          Caption = 'Search distance'
        end
        object lblMaxPilotPoints: TLabel
          Left = 160
          Top = 68
          Width = 219
          Height = 18
          Caption = 'Maximum number of pilot points'
        end
        object cbPriorInfoHorizContinuity: TCheckBox
          Left = 6
          Top = 10
          Width = 355
          Height = 17
          Caption = 'Use within-layer continuity prior information'
          TabOrder = 0
          OnClick = cbPriorInfoHorizContinuityClick
        end
        object rdeSearchDistance: TRbwDataEntry
          Left = 6
          Top = 33
          Width = 145
          Height = 22
          TabOrder = 1
          Text = '0'
          OnChange = rdeSearchDistanceChange
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object seMaxPilotPoints: TJvSpinEdit
          Left = 6
          Top = 65
          Width = 145
          Height = 26
          TabOrder = 2
        end
        object btnWithinLayerPrior: TButton
          Left = 6
          Top = 97
          Width = 235
          Height = 54
          Caption = 
            'Use within-layer continuity prior information for all parameters' +
            '.'
          TabOrder = 3
          WordWrap = True
          OnClick = btnWithinLayerPriorClick
        end
      end
      object rdgPriorInfoHorizContinuity: TRbwDataGrid4
        Left = 0
        Top = 161
        Width = 562
        Height = 281
        Align = alClient
        ColCount = 4
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        OnSetEditText = rdgPriorInfoHorizContinuitySetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = rdgPriorInfoHorizContinuityBeforeDrawCell
        OnStateChange = rdgPriorInfoHorizContinuityStateChange
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 12
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
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
        WordWrapRowCaptions = False
      end
    end
    object jvspPriorInfoVertContinuity: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Between_Layer_'
      Caption = 'jvspPriorInfoVertContinuity'
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 97
        Align = alTop
        TabOrder = 0
        object cbPriorInfoVertContinuity: TCheckBox
          Left = 6
          Top = 14
          Width = 395
          Height = 17
          Caption = 'Use between-layer continuity prior information'
          TabOrder = 0
        end
        object btnBetweenLayerPrior: TButton
          Left = 6
          Top = 37
          Width = 251
          Height = 54
          Caption = 
            'Use between-layer continuity prior information for all parameter' +
            's.'
          TabOrder = 1
          WordWrap = True
          OnClick = btnBetweenLayerPriorClick
        end
      end
      object rdgPriorInfoVertContinuity: TRbwDataGrid4
        Left = 0
        Top = 97
        Width = 562
        Height = 345
        Align = alClient
        ColCount = 4
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
        OnSetEditText = rdgPriorInfoVertContinuitySetEditText
        ExtendedAutoDistributeText = False
        AutoMultiEdit = False
        AutoDistributeText = False
        AutoIncreaseColCount = False
        AutoIncreaseRowCount = False
        SelectedRowOrColumnColor = clAqua
        UnselectableColor = clBtnFace
        OnBeforeDrawCell = rdgPriorInfoVertContinuityBeforeDrawCell
        OnStateChange = rdgPriorInfoVertContinuityStateChange
        ColorRangeSelection = False
        Columns = <
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustCaptionRowHeights = True
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
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
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
            ComboUsed = True
            Format = rcf4String
            LimitToList = True
            MaxLength = 12
            ParentButtonFont = False
            WordWrapCaptions = True
            WordWrapCells = False
            CaseSensitivePicklist = False
            CheckStyle = csCheck
            AutoAdjustColWidths = True
          end
          item
            AutoAdjustRowHeights = False
            AutoAdjustCaptionRowHeights = True
            ButtonCaption = '...'
            ButtonFont.Charset = DEFAULT_CHARSET
            ButtonFont.Color = clWindowText
            ButtonFont.Height = -11
            ButtonFont.Name = 'Tahoma'
            ButtonFont.Style = []
            ButtonUsed = False
            ButtonWidth = 20
            CheckMax = False
            CheckMin = True
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
        WordWrapRowCaptions = False
      end
    end
    object jvspPrediction1: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Prediction_Ana'
      Caption = 'jvspPrediction1'
      object lblPredMinMax: TLabel
        Left = 157
        Top = 19
        Width = 314
        Height = 18
        Caption = 'Goal of predictive analysis (NPREDMAXMIN)'
      end
      object lblTargetObjectiveFunction: TLabel
        Left = 159
        Top = 75
        Width = 212
        Height = 18
        Caption = 'Target objective function (PD0)'
      end
      object lblAcceptedObjectiveFunction: TLabel
        Left = 159
        Top = 99
        Width = 248
        Height = 18
        Caption = 'Acceptable objective function (PD1)'
      end
      object lblTestLambdaPhi: TLabel
        Left = 157
        Top = 130
        Width = 378
        Height = 18
        Caption = 'Objective function for altering testing procedures (PD2)'
      end
      object lblAbsoluteLamdaCriterion: TLabel
        Left = 157
        Top = 155
        Width = 350
        Height = 36
        Caption = 
          'Absolute prediction change to terminate Marquardt lambda testing' +
          ' (ABSPREDLAM)'
        WordWrap = True
      end
      object lblRelativeLamdaCriterion: TLabel
        Left = 157
        Top = 197
        Width = 349
        Height = 36
        Caption = 
          'Relative prediction change to terminate Marquardt lambda testing' +
          ' (RELPREDLAM)'
        WordWrap = True
      end
      object lblInitialLineSearchFactor: TLabel
        Left = 157
        Top = 239
        Width = 263
        Height = 18
        Caption = 'Initial line search factor (INITSCHFAC)'
      end
      object lblUpdateLineSearchFactor: TLabel
        Left = 157
        Top = 267
        Width = 303
        Height = 18
        Caption = 'Line search factor multiplier (MULSCHFAC)'
      end
      object lblLineSearchRuns: TLabel
        Left = 79
        Top = 295
        Width = 407
        Height = 18
        Caption = 'Maximum number of model runs in line search (NSEARCH)'
      end
      object lblAbsolutePredictionSwitch: TLabel
        Left = 157
        Top = 330
        Width = 349
        Height = 36
        Caption = 
          'Absolute prediction change at which to use central derivatives c' +
          'alculation (ABSPREDSWH) '
        WordWrap = True
      end
      object lblRelativePredictionSwitch: TLabel
        Left = 157
        Top = 372
        Width = 344
        Height = 36
        Caption = 
          'Relative prediction change at which to use central derivatives c' +
          'alculation (RELPREDSWH)'
        WordWrap = True
      end
      object comboPredMinMax: TComboBox
        Left = 6
        Top = 16
        Width = 145
        Height = 26
        Style = csDropDownList
        ItemIndex = 1
        TabOrder = 0
        Text = 'Maximize (+1)'
        Items.Strings = (
          'Minimize (-1)'
          'Maximize (+1)')
      end
      object cbPredictiveNoise: TCheckBox
        Left = 6
        Top = 48
        Width = 403
        Height = 17
        Caption = 'Take into account predictive noise (PREDNOISE)'
        TabOrder = 1
      end
      object rdeTargetObjectiveFunction: TRbwDataEntry
        Left = 8
        Top = 71
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        OnChange = rdeTargetObjectiveFunctionChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeAcceptedObjectiveFunction: TRbwDataEntry
        Left = 6
        Top = 99
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '0'
        OnChange = rdeAcceptedObjectiveFunctionChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeTestLambdaPhi: TRbwDataEntry
        Left = 6
        Top = 127
        Width = 145
        Height = 22
        TabOrder = 4
        Text = '0'
        OnChange = rdeTestLambdaPhiChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeAbsoluteLamdaCriterion: TRbwDataEntry
        Left = 6
        Top = 155
        Width = 145
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRelativeLamdaCriterion: TRbwDataEntry
        Left = 6
        Top = 197
        Width = 145
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeInitialLineSearchFactor: TRbwDataEntry
        Left = 6
        Top = 239
        Width = 145
        Height = 22
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeUpdateLineSearchFactor: TRbwDataEntry
        Left = 6
        Top = 267
        Width = 145
        Height = 22
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seLineSearchRuns: TJvSpinEdit
        Left = 6
        Top = 295
        Width = 67
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 9
      end
      object rdeAbsolutePredictionSwitch: TRbwDataEntry
        Left = 6
        Top = 327
        Width = 145
        Height = 22
        TabOrder = 10
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRelativePredictionSwitch: TRbwDataEntry
        Left = 6
        Top = 372
        Width = 145
        Height = 22
        TabOrder = 11
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspPrediction2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Prediction_An2'
      Caption = 'jvspPrediction2'
      object lblMaxNoPredictionImprovmentRuns: TLabel
        Left = 79
        Top = 18
        Width = 407
        Height = 36
        Caption = 
          'Iterations since prediction improved at which termination is tri' +
          'ggered (NPREDNORED) '
        WordWrap = True
      end
      object lblAbsoluteImprovementCriterion: TLabel
        Left = 157
        Top = 63
        Width = 319
        Height = 36
        Caption = 
          'Absolute prediction change at which to trigger termination (ABSP' +
          'REDSTP)'
        WordWrap = True
      end
      object lblRelativeImprovementCriterion: TLabel
        Left = 157
        Top = 105
        Width = 314
        Height = 36
        Caption = 
          'Relative prediction change at which to trigger termination (RELP' +
          'REDSTP)'
        WordWrap = True
      end
      object lblNumberOfPredictionsToCompare: TLabel
        Left = 79
        Top = 147
        Width = 474
        Height = 36
        Caption = 
          'Number of iterations over which ABSPREDSTP and RELPREDSTP apply ' +
          '(NPREDSTP)'
        WordWrap = True
      end
      object seMaxNoPredictionImprovmentRuns: TJvSpinEdit
        Left = 6
        Top = 15
        Width = 67
        Height = 26
        TabOrder = 0
      end
      object rdeAbsoluteImprovementCriterion: TRbwDataEntry
        Left = 6
        Top = 60
        Width = 145
        Height = 22
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRelativeImprovementCriterion: TRbwDataEntry
        Left = 6
        Top = 105
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seNumberOfPredictionsToCompare: TJvSpinEdit
        Left = 6
        Top = 144
        Width = 67
        Height = 26
        MaxValue = 2147483647.000000000000000000
        MinValue = 2.000000000000000000
        Value = 2.000000000000000000
        TabOrder = 3
      end
    end
    object jvspPareto1: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Pareto_Control'
      Caption = 'jvspPareto1'
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 562
        Height = 289
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblFinalIterationCount: TLabel
          Left = 79
          Top = 243
          Width = 388
          Height = 36
          Caption = 
            'Number of optimisation iterations with final weight factor (NUM_' +
            'ITER_FIN)'
          WordWrap = True
        end
        object lblFinalParetoWeight: TLabel
          Left = 157
          Top = 96
          Width = 391
          Height = 18
          Caption = 'Pareto group final weight factor (PARETO_WTFAC_FIN)'
          WordWrap = True
        end
        object lblInitialIterationCount: TLabel
          Left = 79
          Top = 156
          Width = 396
          Height = 36
          Caption = 
            'Number of optimisation iterations with initial weight factor (NU' +
            'M_ITER_START)'
          WordWrap = True
        end
        object lblInitialParetoWeight: TLabel
          Left = 157
          Top = 51
          Width = 227
          Height = 36
          Caption = 'Pareto group initial weight factor (PARETO_WTFAC_START)'
          WordWrap = True
        end
        object lblIntermediateIterationCount: TLabel
          Left = 79
          Top = 201
          Width = 455
          Height = 36
          Caption = 
            'Number of optimisation iterations with intermediate weight facto' +
            'rs (NUM_ITER_GEN)'
          WordWrap = True
        end
        object lblParetoGroup: TLabel
          Left = 157
          Top = 19
          Width = 354
          Height = 18
          Caption = 'Pareto observation group (PARETO_OBSGROUP)'
        end
        object lblParetoIncrements: TLabel
          Left = 79
          Top = 124
          Width = 349
          Height = 18
          Caption = 'Number of Pareto increments (NUM_WTFAC_INT)'
        end
        object comboParetoGroup: TComboBox
          Left = 6
          Top = 16
          Width = 145
          Height = 26
          Style = csDropDownList
          TabOrder = 0
          OnChange = comboParetoGroupChange
        end
        object rdeFinalParetoWeight: TRbwDataEntry
          Left = 6
          Top = 93
          Width = 145
          Height = 22
          TabOrder = 1
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object rdeInitialParetoWeight: TRbwDataEntry
          Left = 6
          Top = 48
          Width = 145
          Height = 22
          TabOrder = 2
          Text = '0'
          DataType = dtReal
          Max = 1.000000000000000000
          CheckMin = True
          ChangeDisabledColor = True
        end
        object seFinalIterationCount: TJvSpinEdit
          Left = 6
          Top = 243
          Width = 67
          Height = 26
          MaxValue = 2147483647.000000000000000000
          TabOrder = 3
        end
        object seInitialIterationCount: TJvSpinEdit
          Left = 6
          Top = 153
          Width = 67
          Height = 26
          CheckMinValue = True
          TabOrder = 4
        end
        object seIntermediateIterationCount: TJvSpinEdit
          Left = 6
          Top = 198
          Width = 67
          Height = 26
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 5
        end
        object seParetoIncrements: TJvSpinEdit
          Left = 6
          Top = 121
          Width = 67
          Height = 26
          MaxValue = 2147483647.000000000000000000
          MinValue = 1.000000000000000000
          Value = 1.000000000000000000
          TabOrder = 6
        end
      end
      object rdgObservationsToReport: TRbwDataGrid4
        Left = 0
        Top = 289
        Width = 562
        Height = 153
        Align = alClient
        ColCount = 2
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 1
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
            AutoAdjustCaptionRowHeights = False
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
            AutoAdjustCaptionRowHeights = True
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
          end>
        WordWrapRowCaptions = False
        ColWidths = (
          64
          119)
      end
    end
    object jsvpPareto2: TJvStandardPage
      Left = 0
      Top = 0
      Width = 562
      Height = 442
      HelpType = htKeyword
      HelpKeyword = 'PEST_Properties_Pareto_Alterna'
      Caption = 'jsvpPareto2'
      object lblObservationName: TLabel
        Left = 157
        Top = 42
        Width = 379
        Height = 18
        Caption = 'Observation used to halt Pareto analysis (OBS_TERM)'
      end
      object lblAltDirection: TLabel
        Left = 157
        Top = 74
        Width = 288
        Height = 36
        Caption = 'Condition requited to halt Pareto analysis (ABOVE_OR_BELOW)'
        WordWrap = True
      end
      object lblAltThreshold: TLabel
        Left = 157
        Top = 119
        Width = 403
        Height = 18
        Caption = 'Threshold for terminating Pareto analysis (OBS_THRESH)'
      end
      object lblAltIterations: TLabel
        Left = 71
        Top = 147
        Width = 446
        Height = 36
        Caption = 
          'Number of optimization iterations for terminating Pareto analysi' +
          's (ITER_THRESH)'
        WordWrap = True
      end
      object cbAltTerminationOption: TCheckBox
        Left = 6
        Top = 16
        Width = 539
        Height = 17
        Caption = 'Terminate Pareto analysis based on observation value (ALT_TERM)'
        TabOrder = 0
      end
      object comboObservationName: TComboBox
        Left = 6
        Top = 39
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 1
      end
      object comboAltDirection: TComboBox
        Left = 6
        Top = 71
        Width = 145
        Height = 26
        Style = csDropDownList
        TabOrder = 2
        Items.Strings = (
          'above'
          'below')
      end
      object rdeAltThreshold: TRbwDataEntry
        Left = 6
        Top = 116
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        ChangeDisabledColor = True
      end
      object seAltIterations: TJvSpinEdit
        Left = 6
        Top = 144
        Width = 59
        Height = 26
        TabOrder = 4
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 442
    Width = 760
    Height = 42
    Align = alBottom
    TabOrder = 2
    object btnHelp: TBitBtn
      Left = 494
      Top = 6
      Width = 83
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
      OnClick = btnHelpClick
    end
    object btnOK: TBitBtn
      Left = 583
      Top = 6
      Width = 83
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 672
      Top = 6
      Width = 83
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
  object dlgOpenCovarianceMatrixFile: TOpenDialog
    Filter = 'All Files|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 62
    Top = 128
  end
  object dlgOpenPilotPoints: TOpenDialog
    Filter = 
      'Shapefile (*.shp)|*.shp|Text files (*.txt,*.csv)|*.csv;*.txt|Tex' +
      't file (*.txt)|*.txt|CSV file (*.csv)|*.csv'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 38
    Top = 64
  end
end
