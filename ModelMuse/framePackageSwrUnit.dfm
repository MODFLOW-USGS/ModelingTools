inherited framePackageSwr: TframePackageSwr
  Width = 595
  Height = 514
  ExplicitWidth = 595
  ExplicitHeight = 514
  DesignSize = (
    595
    514)
  object splttrSwr: TJvNetscapeSplitter [2]
    Left = 146
    Top = 0
    Height = 514
    Align = alLeft
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
    ExplicitLeft = 344
    ExplicitTop = 32
    ExplicitHeight = 100
  end
  inherited memoComments: TMemo
    Width = 564
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    ExplicitWidth = 564
  end
  object jvplSwr: TJvPageList [4]
    Left = 156
    Top = 0
    Width = 439
    Height = 514
    ActivePage = jvspSpecificationMethod
    PropagateEnable = False
    Align = alClient
    object jvspComments: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Comments'
      Caption = 'jvspComments'
    end
    object jvspSolutionOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Solution_Options'
      Caption = 'jvspSolutionOptions'
      object lblScaling: TLabel
        Left = 6
        Top = 272
        Width = 38
        Height = 15
        Caption = 'Scaling'
      end
      object lblReordering: TLabel
        Left = 6
        Top = 320
        Width = 58
        Height = 15
        Caption = 'Reordering'
      end
      object lblNewton: TLabel
        Left = 6
        Top = 374
        Width = 103
        Height = 15
        Caption = 'Newton-Correction'
      end
      object cbSwrOnly: TCheckBox
        Left = 6
        Top = 3
        Width = 430
        Height = 17
        Caption = 'Only surface water routing (no groundwater) (ISWRONLY)'
        TabOrder = 0
      end
      object cbContinueNonConverge: TCheckBox
        Left = 6
        Top = 26
        Width = 393
        Height = 34
        Caption = 
          'Continue even if model does not converge (USE_NONCONVERGENCE_CON' +
          'TINUE)'
        TabOrder = 1
        WordWrap = True
      end
      object cbUpstreamWeighting: TCheckBox
        Left = 6
        Top = 66
        Width = 393
        Height = 34
        Caption = 
          'Use upstream weighting for diffusive wave (USE_UPSTREAM_WEIGHTIN' +
          'G)'
        TabOrder = 2
        WordWrap = True
      end
      object cbInexactNewton: TCheckBox
        Left = 6
        Top = 106
        Width = 419
        Height = 17
        Caption = 'Use inexact Newton (USE_INEXACT_NEWTON)'
        TabOrder = 3
      end
      object cbUseSteadyStateStorage: TCheckBox
        Left = 6
        Top = 140
        Width = 419
        Height = 31
        Caption = 'Use steady-state storage (USE_STEADYSTATE_STORAGE)'
        TabOrder = 4
        WordWrap = True
      end
      object cbUseLaggedStagesAndFlows: TCheckBox
        Left = 6
        Top = 177
        Width = 419
        Height = 34
        Caption = 'Use lagged stages and flows (USE_LAGGED_OPR_DATA)'
        TabOrder = 5
        WordWrap = True
      end
      object cbUseLinearDepthScaling: TCheckBox
        Left = 6
        Top = 225
        Width = 417
        Height = 17
        Caption = 'Use linear depth scaling (USE_LINEAR_DEPTH_SCALING)'
        TabOrder = 6
        WordWrap = True
      end
      object comboScaling: TJvImageComboBox
        Left = 6
        Top = 294
        Width = 411
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 411
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 1
        TabOrder = 7
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Diagonal scaling (USE_DIAGONAL_SCALING)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'L2Norm scaling (USE_L2NORM_SCALING)'
          end>
      end
      object comboReordering: TJvImageComboBox
        Left = 6
        Top = 339
        Width = 411
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 411
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 8
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'USE_RCMREORDERING'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'USE_RCMREORDERING_IF_IMPROVEMENT'
          end>
      end
      object comboNewton: TJvImageComboBox
        Left = 6
        Top = 396
        Width = 411
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 411
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 2
        TabOrder = 9
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Implicit (USE_IMPLICIT_NEWTON_CORRECTION)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Explicit (USE_EXPLICIT_NEWTON_CORRECTION)'
          end>
      end
    end
    object jvspTimeStepOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Time_Step_Options'
      Caption = 'jvspTimeStepOptions'
      object lblInitialTimeStepLength: TLabel
        Left = 157
        Top = 6
        Width = 156
        Height = 15
        Caption = 'Initial time step length (RTINI)'
      end
      object lblMinTimeStepLength: TLabel
        Left = 157
        Top = 34
        Width = 188
        Height = 15
        Caption = 'Minimum time step length (RTMIN)'
      end
      object lblMaxTimeStepLength: TLabel
        Left = 157
        Top = 62
        Width = 193
        Height = 15
        Caption = 'Maximum time step length (RTMAX)'
      end
      object lblTimeStepMultiplier: TLabel
        Left = 157
        Top = 90
        Width = 158
        Height = 15
        Caption = 'Time step multiplier (RTMULT)'
      end
      object lblTimeStepIncreaseFrequency: TLabel
        Left = 133
        Top = 118
        Width = 119
        Height = 45
        Caption = 'Frequency with which time step increases (NTMULT)'
        WordWrap = True
      end
      object lblMinGradientForDiffusiveFlow: TLabel
        Left = 157
        Top = 157
        Width = 103
        Height = 45
        Caption = 'Minimum gradient for diffusive flow (DMINGRAD)'
        WordWrap = True
      end
      object lblMinDepthForOutflow: TLabel
        Left = 157
        Top = 196
        Width = 221
        Height = 15
        Caption = 'Minimum depth for outflow (DMINDPTH)'
      end
      object lblMaxRainfallForStepAdjustment: TLabel
        Left = 157
        Top = 224
        Width = 97
        Height = 60
        Caption = 'Maximum rainfall for step adjustment (DMAXRAI)'
        WordWrap = True
      end
      object lblMaxStageChangePerStep: TLabel
        Left = 157
        Top = 268
        Width = 151
        Height = 30
        Caption = 'Maximum stage change per step (DMAXSTG)'
        WordWrap = True
      end
      object lblMaxInflowChange: TLabel
        Left = 157
        Top = 312
        Width = 133
        Height = 30
        Caption = 'Maximum inflow change per step (DMAXINF)'
        WordWrap = True
      end
      object rdeInitialTimeStepLength: TRbwDataEntry
        Left = 6
        Top = 3
        Width = 145
        Height = 22
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMinTimeStepLength: TRbwDataEntry
        Left = 6
        Top = 31
        Width = 145
        Height = 22
        TabOrder = 1
        Text = '0'
        OnChange = rdeMinTimeStepLengthChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxTimeStepLength: TRbwDataEntry
        Left = 6
        Top = 59
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        OnChange = rdeMaxTimeStepLengthChange
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeTimeStepMultiplier: TRbwDataEntry
        Left = 6
        Top = 87
        Width = 145
        Height = 22
        TabOrder = 3
        Text = '1'
        OnChange = rdeTimeStepMultiplierChange
        DataType = dtReal
        Max = 1.000000000000000000
        Min = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seTimeStepIncreaseFrequency: TJvSpinEdit
        Left = 6
        Top = 115
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 2.000000000000000000
        Value = 2.000000000000000000
        TabOrder = 4
      end
      object rdeMinGradientForDiffusiveFlow: TRbwDataEntry
        Left = 6
        Top = 157
        Width = 145
        Height = 22
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMinDepthForOutflow: TRbwDataEntry
        Left = 6
        Top = 193
        Width = 145
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxRainfallForStepAdjustment: TRbwDataEntry
        Left = 6
        Top = 221
        Width = 145
        Height = 22
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxStageChangePerStep: TRbwDataEntry
        Left = 6
        Top = 265
        Width = 145
        Height = 22
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxInflowChange: TRbwDataEntry
        Left = 6
        Top = 309
        Width = 145
        Height = 22
        TabOrder = 9
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object jvspSpecificationMethod: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Specification_Method'
      Caption = 'jvspSpecificationMethod'
      object grpSpecificationMethod: TGroupBox
        Left = 0
        Top = 0
        Width = 227
        Height = 514
        Align = alLeft
        Caption = 'Specification method'
        TabOrder = 0
        object rgRainfallSpecification: TRadioGroup
          Left = 3
          Top = 24
          Width = 221
          Height = 105
          Caption = 'Rainfall (IRDRAI)'
          Items.Strings = (
            'Specify by object (> 0)'
            'Specify by cell (< 0)')
          TabOrder = 0
          OnClick = rgRainfallSpecificationClick
        end
        object rgEvapSpecification: TRadioGroup
          Left = 3
          Top = 128
          Width = 221
          Height = 105
          Margins.Top = 0
          Caption = 'Evaporation (IRDEVP)'
          Items.Strings = (
            'Specify by object (> 0)'
            'Specify by cell (< 0)')
          TabOrder = 1
          OnClick = rgEvapSpecificationClick
        end
        object rgLateralInflowSpecification: TRadioGroup
          Left = 3
          Top = 239
          Width = 221
          Height = 105
          Margins.Top = 0
          Caption = 'Lateral inflow (IRDLIN)'
          Items.Strings = (
            'Specify by object (> 0)'
            'Specify by cell (< 0)')
          TabOrder = 2
          OnClick = rgLateralInflowSpecificationClick
        end
        object rgStageSpecification: TRadioGroup
          Left = 3
          Top = 344
          Width = 221
          Height = 105
          Margins.Top = 0
          Caption = 'Stage (IRDSTG)'
          Items.Strings = (
            'Specify by object (> 0)'
            'Specify by cell (< 0)')
          TabOrder = 3
          OnClick = rgStageSpecificationClick
        end
      end
      object grpAssignmentMethod: TGroupBox
        Left = 233
        Top = 0
        Width = 206
        Height = 514
        Align = alRight
        Caption = 'Assignment method'
        TabOrder = 1
        DesignSize = (
          206
          514)
        object rgRainAssignmentMethod: TRadioGroup
          Left = 3
          Top = 24
          Width = 200
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Rain'
          Enabled = False
          ItemIndex = 1
          Items.Strings = (
            'Objects overwrite values of previous objects'
            'Sum values of all objects')
          TabOrder = 0
          WordWrap = True
        end
        object rgEvapAssignmentMethod: TRadioGroup
          Left = 3
          Top = 128
          Width = 200
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Evaporation'
          Enabled = False
          ItemIndex = 1
          Items.Strings = (
            'Objects overwrite values of previous objects'
            'Sum values of all objects')
          TabOrder = 1
          WordWrap = True
        end
        object rgLateralInflowAssignmentMethod: TRadioGroup
          Left = 3
          Top = 239
          Width = 200
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Lateral inflow'
          Enabled = False
          ItemIndex = 1
          Items.Strings = (
            'Objects overwrite values of previous objects'
            'Sum values of all objects')
          TabOrder = 2
          WordWrap = True
        end
        object rgStageAssignmentMethod: TRadioGroup
          Left = 3
          Top = 344
          Width = 200
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Stage'
          Enabled = False
          ItemIndex = 1
          Items.Strings = (
            'Objects overwrite values of previous objects'
            'Sum values of all objects')
          TabOrder = 3
          WordWrap = True
        end
      end
    end
    object jvspPrintOptions: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Print_Flags'
      Caption = 'jvspPrintOptions'
      object lblPrintInflowsAndOutflows: TLabel
        Left = 157
        Top = 12
        Width = 227
        Height = 30
        Caption = 'Save stage, inflows, and outflows for reach groups (ISWRPRGF)'
        WordWrap = True
      end
      object lblPrintStage: TLabel
        Left = 157
        Top = 44
        Width = 119
        Height = 15
        Caption = 'Save stage (ISWRPSTG)'
      end
      object lblPrintReachExchangeAndProperties: TLabel
        Left = 157
        Top = 76
        Width = 163
        Height = 15
        Caption = 'Save reach values (ISWRPQAQ)'
      end
      object lblPrintReachLateralFlow: TLabel
        Left = 157
        Top = 108
        Width = 182
        Height = 15
        Caption = 'Save reach lateral flow (ISWRPQM)'
      end
      object lblPrintStructureFlow: TLabel
        Left = 157
        Top = 140
        Width = 164
        Height = 15
        Caption = 'Save structure flow (ISWRPSTR)'
      end
      object lblSaveSwrTimeStepLength: TLabel
        Left = 157
        Top = 218
        Width = 106
        Height = 45
        Caption = 'Save SWR time step length (SAVE_SWRDT)'
        WordWrap = True
      end
      object lblSaveRiver: TLabel
        Left = 6
        Top = 328
        Width = 100
        Height = 15
        Caption = 'Save River package'
      end
      object lblSaveObs: TLabel
        Left = 6
        Top = 382
        Width = 94
        Height = 15
        Caption = 'Save observations'
      end
      object lblSaveFrequency: TLabel
        Left = 157
        Top = 482
        Width = 105
        Height = 45
        Caption = 'Frequency with which data is saved (RTPRN)'
        WordWrap = True
      end
      object lblObsFormat: TLabel
        Left = 157
        Top = 439
        Width = 165
        Height = 15
        Caption = 'Observation format (IOPTUNIT)'
      end
      object comboPrintInflowsAndOutflows: TJvImageComboBox
        Left = 6
        Top = 9
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 0
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
      object comboPrintStage: TJvImageComboBox
        Left = 6
        Top = 41
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 1
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
      object comboPrintReachExchangeAndProperties: TJvImageComboBox
        Left = 6
        Top = 73
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 2
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
      object comboPrintReachLateralFlow: TJvImageComboBox
        Left = 6
        Top = 105
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 3
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
      object comboPrintStructureFlow: TJvImageComboBox
        Left = 6
        Top = 137
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 4
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
      object cbPrintMaxFroude: TCheckBox
        Left = 6
        Top = 169
        Width = 371
        Height = 17
        Caption = 'Save maximum Froude number (ISWRPFRN)'
        TabOrder = 5
      end
      object cbPrintSwrDataToScreen: TCheckBox
        Left = 6
        Top = 192
        Width = 430
        Height = 17
        Caption = 'Print SWR data to screen (PRINT_SWR_TO_SCREEN)'
        TabOrder = 6
      end
      object comboSaveSwrTimeStepLength: TJvImageComboBox
        Left = 6
        Top = 215
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 7
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
      object cbSaveConvergenceHistory: TCheckBox
        Left = 6
        Top = 295
        Width = 430
        Height = 33
        Caption = 'Save convergence history (SAVE_CONVERGENCE_HISTORY)'
        TabOrder = 9
        WordWrap = True
      end
      object comboSaveRiver: TJvImageComboBox
        Left = 6
        Top = 353
        Width = 419
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 419
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 10
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'SAVE_RIVER_PACKAGE'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'SAVE_RIVER_PACKAGE_ALL'
          end>
      end
      object comboSaveObs: TJvImageComboBox
        Left = 6
        Top = 407
        Width = 419
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 419
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 11
        OnChange = comboSaveObsChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'None (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'SAVE_SWROBSERVATIONS'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'SAVE_SWROBSERVATIONS_ALL'
          end>
      end
      object rdeSaveFrequency: TRbwDataEntry
        Left = 6
        Top = 479
        Width = 145
        Height = 22
        TabOrder = 13
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbSaveAverageSimulatedResults: TCheckBox
        Left = 6
        Top = 247
        Width = 419
        Height = 42
        Caption = 'Save average simulated results (SAVE_AVERAGE_RESULTS)'
        TabOrder = 8
        WordWrap = True
      end
      object comboObsFormat: TJvImageComboBox
        Left = 6
        Top = 436
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 145
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 12
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Text (>0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary (<0)'
          end>
      end
    end
    object jvspSolverMandatory: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Solver_Options_1'
      Caption = 'jvspSolverMandatory'
      object lblSolver: TLabel
        Left = 183
        Top = 6
        Width = 86
        Height = 15
        Caption = 'Solver (ISOLVER)'
      end
      object lblMaxOuterIterations: TLabel
        Left = 133
        Top = 39
        Width = 117
        Height = 45
        Caption = 'Maximum number of outer iterations (NOUTER)'
        WordWrap = True
      end
      object lblMaxInnerIterations: TLabel
        Left = 133
        Top = 77
        Width = 117
        Height = 45
        Caption = 'Maximum number of inner iterations (NINNER)'
        WordWrap = True
      end
      object lblMaxLineSearchIterations: TLabel
        Left = 133
        Top = 123
        Width = 117
        Height = 45
        Caption = 'Maximum number of line search iterations (IBT)'
        WordWrap = True
      end
      object lblStageTolerance: TLabel
        Left = 159
        Top = 168
        Width = 118
        Height = 15
        Caption = 'Stage tolerance (TOLS)'
      end
      object lblFlowToleranceOption: TLabel
        Left = 239
        Top = 197
        Width = 115
        Height = 15
        Caption = 'Flow tolerance option'
      end
      object lblFlowTolerance: TLabel
        Left = 157
        Top = 229
        Width = 115
        Height = 15
        Caption = 'Flow tolerance (TOLR)'
      end
      object lblExchangeToleranceOption: TLabel
        Left = 239
        Top = 257
        Width = 141
        Height = 15
        Caption = 'Exchange tolerance option'
      end
      object lblExchangeTolerance: TLabel
        Left = 157
        Top = 289
        Width = 142
        Height = 15
        Caption = 'Exchange tolerance (TOLA)'
      end
      object lblSteadyStateDampingFactor: TLabel
        Left = 157
        Top = 317
        Width = 205
        Height = 15
        Caption = 'Steady state damping factor (DAMPSS)'
      end
      object lblTransientDampingFactor: TLabel
        Left = 157
        Top = 345
        Width = 190
        Height = 15
        Caption = 'Transient damping factor (DAMPTR)'
      end
      object lblConvergencePrintoutInterval: TLabel
        Left = 133
        Top = 374
        Width = 210
        Height = 15
        Caption = 'Convergence printout interval (IPRSWR)'
      end
      object lblPrintConvergence: TLabel
        Left = 6
        Top = 403
        Width = 156
        Height = 15
        Caption = 'Print convergence (MUTSWR)'
      end
      object comboSolver: TJvImageComboBox
        Left = 6
        Top = 3
        Width = 171
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 171
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 0
        OnChange = comboSolverChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'LU decomposition (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'bi-CGSTAB (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'GMRES (3)'
          end>
      end
      object seMaxOuterIterations: TJvSpinEdit
        Left = 6
        Top = 35
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 1
      end
      object seMaxInnerIterations: TJvSpinEdit
        Left = 6
        Top = 73
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 2
      end
      object seMaxLineSearchIterations: TJvSpinEdit
        Left = 6
        Top = 119
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 3
        OnChange = seMaxLineSearchIterationsChange
      end
      object rdeStageTolerance: TRbwDataEntry
        Left = 6
        Top = 165
        Width = 145
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboFlowToleranceOption: TJvImageComboBox
        Left = 6
        Top = 194
        Width = 227
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 227
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 1
        TabOrder = 5
        OnChange = comboFlowToleranceOptionChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'none'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'USE_FRACTIONAL_TOLR'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'USE_L2NORM_TOLR'
          end>
      end
      object rdeFlowTolerance: TRbwDataEntry
        Left = 6
        Top = 226
        Width = 145
        Height = 22
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboExchangeToleranceOption: TJvImageComboBox
        Left = 6
        Top = 254
        Width = 227
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 227
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 2
        TabOrder = 7
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'none'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'USE_GLOBAL_TOLA'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'USE_ABSOLUTE_TOLA'
          end>
      end
      object rdeExchangeTolerance: TRbwDataEntry
        Left = 6
        Top = 286
        Width = 145
        Height = 22
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeSteadyStateDampingFactor: TRbwDataEntry
        Left = 6
        Top = 314
        Width = 145
        Height = 22
        TabOrder = 9
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeTransientDampingFactor: TRbwDataEntry
        Left = 6
        Top = 342
        Width = 145
        Height = 22
        TabOrder = 10
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seConvergencePrintoutInterval: TJvSpinEdit
        Left = 6
        Top = 370
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 11
      end
      object comboPrintConvergence: TJvImageComboBox
        Left = 6
        Top = 422
        Width = 299
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 299
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 12
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print max residual every time step (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print number of iterations (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print none (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print if convergence fails'
          end>
      end
    end
    object jvspSolverOptional: TJvStandardPage
      Left = 0
      Top = 0
      Width = 439
      Height = 514
      HelpType = htKeyword
      HelpKeyword = 'SWR_Solver_Options_2'
      Caption = 'jvspSolverOptional'
      object lblPreconditioner: TLabel
        Left = 231
        Top = 6
        Width = 107
        Height = 15
        Caption = 'Preconditioner (IPC)'
      end
      object lblMaxLevels: TLabel
        Left = 133
        Top = 39
        Width = 203
        Height = 15
        Caption = 'Maximum number of levels (NLEVELS)'
      end
      object lblDropThreshold: TLabel
        Left = 159
        Top = 68
        Width = 141
        Height = 15
        Caption = 'Drop threshold (DROPTOL)'
      end
      object lblPrintLineSearchInterval: TLabel
        Left = 133
        Top = 97
        Width = 145
        Height = 30
        Caption = 'Interval for printing line search information (IBTPRT)'
        WordWrap = True
      end
      object lblAlternativeFlowTolerance: TLabel
        Left = 159
        Top = 150
        Width = 180
        Height = 15
        Caption = 'Alternative flow tolerance (PTOLR)'
      end
      object comboPreconditioner: TJvImageComboBox
        Left = 6
        Top = 3
        Width = 219
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        DroppedWidth = 253
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 3
        TabOrder = 0
        OnChange = comboPreconditionerChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'No preconditioning (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Jacobi preconditioning (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'ILU(0) preconditioning (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'MILU(0) preconditioning (3)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'ILUT preconditioning (4)'
          end>
      end
      object seMaxLevels: TJvSpinEdit
        Left = 6
        Top = 35
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 1
      end
      object rdeDropThreshold: TRbwDataEntry
        Left = 6
        Top = 65
        Width = 145
        Height = 22
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object sePrintLineSearchInterval: TJvSpinEdit
        Left = 6
        Top = 93
        Width = 121
        Height = 21
        MaxValue = 2147483647.000000000000000000
        Value = 1.000000000000000000
        TabOrder = 3
      end
      object rdeAlternativeFlowTolerance: TRbwDataEntry
        Left = 6
        Top = 144
        Width = 145
        Height = 22
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
  end
  object tvpglstSwr: TJvPageListTreeView [5]
    Left = 0
    Top = 0
    Width = 146
    Height = 514
    PageDefault = 0
    PageList = jvplSwr
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 0
    Items.Links = {00000000}
  end
end
