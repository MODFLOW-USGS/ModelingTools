inherited framePackageNwt: TframePackageNwt
  Width = 457
  Height = 479
  ExplicitWidth = 457
  ExplicitHeight = 479
  DesignSize = (
    457
    479)
  inherited memoComments: TMemo
    Width = 426
    ExplicitWidth = 426
  end
  object pcNWT: TPageControl [3]
    Left = 0
    Top = 157
    Width = 457
    Height = 322
    ActivePage = TabChi_MD_Variables
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tabBasic: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Basic_Options_Tab_Newton_Solver'
      Caption = 'Basic Options'
      object lblSolverMethod: TLabel
        Left = 90
        Top = 131
        Width = 129
        Height = 15
        Caption = 'Matrix solver (LINMETH)'
      end
      object lblThicknessFactor: TLabel
        Left = 90
        Top = 89
        Width = 365
        Height = 15
        Caption = 
          'Portion of cell thickness used for coefficient adjustment (THICK' +
          'FACT)'
      end
      object lblMaxOuterIt: TLabel
        Left = 90
        Top = 62
        Width = 279
        Height = 15
        Caption = 'Maximum number of outer iterations (MAXITEROUT)'
      end
      object lblFluxTolerance: TLabel
        Left = 90
        Top = 34
        Width = 174
        Height = 15
        Caption = 'Flux tolerance (FLUXTOL) (L^3/T)'
      end
      object lblHeadTolerance: TLabel
        Left = 90
        Top = 6
        Width = 158
        Height = 15
        Caption = 'Head tolerance (HEADTOL) (L)'
      end
      object lblOptions: TLabel
        Left = 90
        Top = 234
        Width = 156
        Height = 15
        Caption = 'Model complexity (OPTIONS)'
      end
      object rdeHeadTolerance: TRbwDataEntry
        Left = 3
        Top = 3
        Width = 81
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeFluxTolerance: TRbwDataEntry
        Left = 3
        Top = 31
        Width = 81
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object spinMaxOuterIt: TJvSpinEdit
        Left = 3
        Top = 59
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 2
      end
      object rdeThicknessFactor: TRbwDataEntry
        Left = 3
        Top = 93
        Width = 81
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboSolverMethod: TJvImageComboBox
        Left = 3
        Top = 126
        Width = 81
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 4
        OnChange = comboSolverMethodChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'GMRES (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Chi MD (2)'
          end>
      end
      object cbPrintFlag: TCheckBox
        Left = 3
        Top = 167
        Width = 273
        Height = 17
        Caption = 'Print solver convergence information (IPRNWT)'
        Enabled = False
        TabOrder = 5
      end
      object cbCorrectForCellBottom: TCheckBox
        Left = 3
        Top = 190
        Width = 424
        Height = 35
        Caption = 
          'Correct groundwater head relative to the cell-bottom altitude wh' +
          'en the cell is surrounded by dry cells (IBOTAV)'
        Enabled = False
        TabOrder = 6
        WordWrap = True
      end
      object comboOptions: TJvImageComboBox
        Left = 3
        Top = 231
        Width = 81
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 7
        OnChange = comboOptionsChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Simple'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Moderate'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Complex'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Specified'
          end>
      end
      object cbContinue: TCheckBox
        Left = 3
        Top = 264
        Width = 435
        Height = 17
        Caption = 
          'Continue to execute model despite non-convergence (CONTINUE OPTI' +
          'ON)'
        TabOrder = 8
      end
    end
    object tabAdditional: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Additional_Options_Tab_Newton_'
      Caption = 'Additional Options'
      ImageIndex = 1
      object lblDbdTheta: TLabel
        Left = 87
        Top = 3
        Width = 277
        Height = 30
        Caption = 
          'Coefficient used to reduce the weight applied to the head change' +
          ' (DBDTHETA)'
        WordWrap = True
      end
      object lblDbdKappa: TLabel
        Left = 87
        Top = 31
        Width = 284
        Height = 30
        Caption = 
          'Coefficient used to increase the weight applied to the head chan' +
          'ge (DBDKAPPA)'
        WordWrap = True
      end
      object lblDbdGamma: TLabel
        Left = 87
        Top = 59
        Width = 299
        Height = 30
        Caption = 
          'Factor used to weight the head change for iterations n-1 and n (' +
          'DBDGAMMA)'
        WordWrap = True
      end
      object lblMomentumCoefficient: TLabel
        Left = 87
        Top = 94
        Width = 69
        Height = 45
        Caption = 'Momentum coefficient (MOMFACT) '
        WordWrap = True
      end
      object Label4: TLabel
        Left = 63
        Top = 143
        Width = 276
        Height = 45
        Caption = 
          'Maximum number of reductions (backtracks) in the head change bet' +
          'ween nonlinear iterations (MAXBACKITER) '
        WordWrap = True
      end
      object lblBackTol: TLabel
        Left = 87
        Top = 172
        Width = 308
        Height = 60
        Caption = 
          'The proportional decrease in the root-mean-squared error of the ' +
          'groundwater-flow equation used to determine if residual control ' +
          'is required at the end of a nonlinear iteration (BACKTOL)'
        WordWrap = True
      end
      object lblReductionFactor: TLabel
        Left = 87
        Top = 220
        Width = 246
        Height = 45
        Caption = 
          'Reduction factor used for residual control that reduces the head' +
          ' change between nonlinear iterations (BACKREDUCE)'
        WordWrap = True
      end
      object rdeDbdTheta: TRbwDataEntry
        Left = 3
        Top = 3
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 0
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeDbdKappa: TRbwDataEntry
        Left = 3
        Top = 35
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeDbdGamma: TRbwDataEntry
        Left = 3
        Top = 63
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 2
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMomentumCoefficient: TRbwDataEntry
        Left = 3
        Top = 91
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbUseResidualControl: TCheckBox
        Left = 3
        Top = 120
        Width = 253
        Height = 17
        Caption = 'Use residual control (BACKFLAG)'
        Enabled = False
        TabOrder = 4
      end
      object seMaxReductions: TJvSpinEdit
        Left = 3
        Top = 143
        Width = 54
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 5
      end
      object rdeBackTol: TRbwDataEntry
        Left = 3
        Top = 180
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeReductionFactor: TRbwDataEntry
        Left = 3
        Top = 217
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabGmresVariables: TTabSheet
      HelpType = htKeyword
      HelpKeyword = '_GMRES_Variables_Tab_Newton_So'
      Caption = 'GMRES Variables'
      ImageIndex = 2
      object lblMaxIterationsGmres: TLabel
        Left = 90
        Top = 6
        Width = 362
        Height = 15
        Caption = 
          'Maximum number of iterations for the linear solution (MAXITINNER' +
          ')'
      end
      object lblIluMethod: TLabel
        Left = 3
        Top = 30
        Width = 269
        Height = 15
        Caption = 'Method for incomplete factorization (ILUMETHOD)'
      end
      object lblFillLimit1: TLabel
        Left = 90
        Top = 78
        Width = 200
        Height = 15
        Caption = 'Fill limit for ILUMETHOD = 1 (LEVFILL)'
      end
      object lblFillLimit2: TLabel
        Left = 90
        Top = 105
        Width = 215
        Height = 15
        Caption = 'Level of fill for ILUMETHOD = 2 (LEVFILL)'
      end
      object lblTolerance: TLabel
        Left = 87
        Top = 132
        Width = 176
        Height = 30
        Caption = 'Tolerance for convergence of the linear solver (STOPTOL)'
        WordWrap = True
      end
      object lblRestarts: TLabel
        Left = 90
        Top = 160
        Width = 351
        Height = 15
        Caption = 'Number of iterations between restarts of the GMRES solver (MSDR)'
      end
      object seMaxIterationsGmres: TJvSpinEdit
        Left = 3
        Top = 3
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 0
      end
      object comboIluMethod: TJvImageComboBox
        Left = 3
        Top = 46
        Width = 262
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 262
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboIluMethodChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'ILU with drop tolerance and fill limit (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'ILU(k), Order k incomplete LU factorization (2)'
          end>
      end
      object seFillLimit1: TJvSpinEdit
        Left = 3
        Top = 75
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 2
      end
      object seFillLimit2: TJvSpinEdit
        Left = 3
        Top = 102
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 3
      end
      object rdeTolerance: TRbwDataEntry
        Left = 3
        Top = 129
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seRestarts: TJvSpinEdit
        Left = 3
        Top = 157
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 5
      end
    end
    object TabChi_MD_Variables: TTabSheet
      HelpType = htKeyword
      HelpKeyword = 'Chi_MD_Variables_Tab_Newton_So'
      Caption = 'Chi MD Variables'
      ImageIndex = 3
      object lblAccelMethod: TLabel
        Left = 183
        Top = 6
        Width = 147
        Height = 15
        Caption = 'Acceleration method (IACL)'
      end
      object lblOrderingScheme: TLabel
        Left = 184
        Top = 35
        Width = 239
        Height = 15
        Caption = 'Scheme of ordering the unknowns (NORDER)'
      end
      object lblFillLevel: TLabel
        Left = 90
        Top = 64
        Width = 266
        Height = 15
        Caption = 'Level of fill for incomplete LU factorization (LEVEL)'
      end
      object lblNumOrtho: TLabel
        Left = 90
        Top = 85
        Width = 196
        Height = 45
        Caption = 
          'Number of orthogonalization for the ORTHOMIN acceleration scheme' +
          ' (NORTH) '
        WordWrap = True
      end
      object lblResRedCrit: TLabel
        Left = 87
        Top = 141
        Width = 174
        Height = 30
        Caption = 'Residual reduction-convergence criterion (RRCTOLS)'
        WordWrap = True
      end
      object lblDropTolerance: TLabel
        Left = 87
        Top = 193
        Width = 131
        Height = 30
        Caption = 'Drop tolerance for preconditioning (EPSRN)'
        WordWrap = True
      end
      object lblHeadClosure: TLabel
        Left = 87
        Top = 221
        Width = 199
        Height = 30
        Caption = 'Head closure criteria for inner (linear) iterations (HCLOSEXMD)'
        WordWrap = True
      end
      object lblMaxIterChimd: TLabel
        Left = 87
        Top = 249
        Width = 359
        Height = 15
        Caption = 'Maximum number of iterations for the linear solution (MXITERXMD)'
      end
      object comboAccelMethod: TJvImageComboBox
        Left = 3
        Top = 3
        Width = 174
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 246
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 0
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Conjugate gradient (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'ORTHOMIN (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Bi-CGSTAB (2)'
          end>
      end
      object comboOrderingScheme: TJvImageComboBox
        Left = 3
        Top = 32
        Width = 174
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 174
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 2
        TabOrder = 1
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Original ordering (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'RCM ordering (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Minimum Degree ordering (2)'
          end>
      end
      object seFillLevel: TJvSpinEdit
        Left = 3
        Top = 61
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 2
      end
      object seNumOrtho: TJvSpinEdit
        Left = 3
        Top = 88
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 3
      end
      object cbApplyReducedPreconditioning: TCheckBox
        Left = 3
        Top = 115
        Width = 374
        Height = 17
        Caption = 'Apply reduced system preconditioning (IREDSYS)'
        Enabled = False
        TabOrder = 4
      end
      object rdeResRedCrit: TRbwDataEntry
        Left = 3
        Top = 138
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 5
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object cbUseDropTolerance: TCheckBox
        Left = 3
        Top = 167
        Width = 374
        Height = 17
        Caption = 'Use drop tolerance in the preconditioning (IDROPTOL)'
        Enabled = False
        TabOrder = 6
      end
      object rdeDropTolerance: TRbwDataEntry
        Left = 3
        Top = 190
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 7
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeHeadClosure: TRbwDataEntry
        Left = 3
        Top = 218
        Width = 78
        Height = 22
        Color = clBtnFace
        Enabled = False
        TabOrder = 8
        Text = '0'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seMaxIterChimd: TJvSpinEdit
        Left = 3
        Top = 246
        Width = 81
        Height = 24
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 9
      end
    end
  end
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = rdeHeadTolerance
      end
      item
        Control = rdeFluxTolerance
      end
      item
        Control = spinMaxOuterIt
      end
      item
        Control = rdeThicknessFactor
      end
      item
        Control = comboSolverMethod
      end
      item
        Control = cbPrintFlag
      end
      item
        Control = cbCorrectForCellBottom
      end
      item
        Control = comboOptions
      end
      item
        Control = rdeDbdTheta
      end
      item
        Control = rdeDbdKappa
      end
      item
        Control = rdeDbdGamma
      end
      item
        Control = rdeMomentumCoefficient
      end
      item
        Control = cbUseResidualControl
      end
      item
        Control = seMaxReductions
      end
      item
        Control = rdeBackTol
      end
      item
        Control = rdeReductionFactor
      end
      item
        Control = seMaxIterationsGmres
      end
      item
        Control = comboIluMethod
      end
      item
        Control = rdeTolerance
      end
      item
        Control = seRestarts
      end
      item
        Control = comboAccelMethod
      end
      item
        Control = comboOrderingScheme
      end
      item
        Control = seFillLevel
      end
      item
        Control = seNumOrtho
      end
      item
        Control = cbApplyReducedPreconditioning
      end
      item
        Control = rdeResRedCrit
      end
      item
        Control = cbUseDropTolerance
      end
      item
        Control = rdeDropTolerance
      end
      item
        Control = rdeHeadClosure
      end
      item
        Control = seMaxIterChimd
      end>
  end
end
