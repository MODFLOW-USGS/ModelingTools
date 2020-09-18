inherited frmPEST: TfrmPEST
  Caption = 'PEST'
  ClientHeight = 273
  ClientWidth = 637
  ExplicitWidth = 653
  ExplicitHeight = 312
  PixelsPerInch = 96
  TextHeight = 18
  object tvPEST: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 231
    ShowButtons = True
    PageDefault = 0
    PageList = pgMain
    Align = alLeft
    Indent = 19
    TabOrder = 0
    Items.Links = {00000000}
    ExplicitTop = -6
  end
  object pgMain: TJvPageList
    Left = 121
    Top = 0
    Width = 516
    Height = 231
    ActivePage = jvspParameterAdjustmentControls
    PropagateEnable = False
    Align = alClient
    ExplicitWidth = 303
    ExplicitHeight = 184
    object jvspBasic: TJvStandardPage
      Left = 0
      Top = 0
      Width = 516
      Height = 231
      Caption = 'jvspBasic'
      ExplicitLeft = 6
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
      object lblPilotPointSpacing: TLabel
        Left = 176
        Top = 39
        Width = 128
        Height = 18
        Caption = 'Pilot point spacing'
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
      object cbShowPilotPoints: TCheckBox
        Left = 176
        Top = 16
        Width = 193
        Height = 17
        Caption = 'Show pilot points'
        TabOrder = 3
      end
      object rdePilotPointSpacing: TRbwDataEntry
        Left = 176
        Top = 63
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
    object jvspControlDataMode: TJvStandardPage
      Left = 0
      Top = 0
      Width = 516
      Height = 231
      Caption = 'jvspControlDataMode'
      ExplicitWidth = 0
      ExplicitHeight = 0
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
      Width = 516
      Height = 231
      Caption = 'jvspDimensions'
      ExplicitLeft = 6
      ExplicitWidth = 496
      ExplicitHeight = 184
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
      Width = 516
      Height = 231
      Caption = 'jvspInversionControls'
      ExplicitLeft = 6
      ExplicitWidth = 496
      ExplicitHeight = 307
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
      Width = 516
      Height = 231
      ExplicitLeft = 6
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
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 231
    Width = 637
    Height = 42
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 184
    ExplicitWidth = 424
    object btnHelp: TBitBtn
      Left = 350
      Top = 6
      Width = 83
      Height = 33
      Kind = bkHelp
      NumGlyphs = 2
      TabOrder = 0
    end
    object btnOK: TBitBtn
      Left = 439
      Top = 6
      Width = 83
      Height = 33
      Kind = bkOK
      NumGlyphs = 2
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 528
      Top = 6
      Width = 83
      Height = 33
      Kind = bkCancel
      NumGlyphs = 2
      TabOrder = 2
    end
  end
end
