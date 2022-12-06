inherited framePackageSWI: TframePackageSWI
  Width = 445
  Height = 522
  ExplicitWidth = 445
  ExplicitHeight = 522
  DesignSize = (
    445
    522)
  inherited lblComments: TLabel
    Left = 17
    Top = 22
    ExplicitLeft = 17
    ExplicitTop = 22
  end
  inherited lblPackage: TLabel
    Top = 3
    ExplicitTop = 3
  end
  inherited memoComments: TMemo
    Top = 41
    Width = 414
    Height = 35
    ExplicitTop = 41
    ExplicitWidth = 414
    ExplicitHeight = 35
  end
  object pcSWI: TPageControl [3]
    Left = 0
    Top = 82
    Width = 445
    Height = 440
    ActivePage = tabBasic
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    ExplicitHeight = 416
    object tabBasic: TTabSheet
      Caption = 'Basic'
      object lblNumberOfSurfaces: TLabel
        Left = 139
        Top = 6
        Width = 143
        Height = 30
        Caption = 'Number of surfaces (NSRF)'#13#10
      end
      object lblDensityChoice: TLabel
        Left = 256
        Top = 33
        Width = 140
        Height = 15
        Caption = 'Density treatment (ISTRAT)'
      end
      object lblObservations: TLabel
        Left = 163
        Top = 85
        Width = 125
        Height = 15
        Caption = 'Observations (ISWIOBS)'
      end
      object lblToeslope: TLabel
        AlignWithMargins = True
        Left = 93
        Top = 146
        Width = 211
        Height = 15
        Margins.Top = 6
        Caption = 'Maximum slope of toe cells (TOESLOPE)'
      end
      object lblTipSlope: TLabel
        AlignWithMargins = True
        Left = 93
        Top = 174
        Width = 204
        Height = 15
        Margins.Top = 6
        Caption = 'Maximum slope of tip cells (TIPSLOPE)'
      end
      object lblAlpha: TLabel
        AlignWithMargins = True
        Left = 93
        Top = 198
        Width = 314
        Height = 15
        Margins.Top = 6
        Caption = 'Fraction of threshold used to move the tip and toe (ALPHA)'
      end
      object lblBeta: TLabel
        AlignWithMargins = True
        Left = 93
        Top = 230
        Width = 262
        Height = 15
        Margins.Top = 6
        Caption = 'Fraction of threshold used to move the toe (BETA)'
      end
      object lblMaxAdaptiveSteps: TLabel
        Left = 139
        Top = 273
        Width = 173
        Height = 45
        Caption = 
          'Maximum number of SWI2 time steps per MODFLOW time step (NADPTMX' +
          ') '
        WordWrap = True
      end
      object lblMinAdaptiveSteps: TLabel
        Left = 140
        Top = 315
        Width = 171
        Height = 45
        Caption = 
          'Minimum number of SWI2 time steps per MODFLOW time step (NADPTMN' +
          ') '
        WordWrap = True
      end
      object lblAdaptiveFactor: TLabel
        AlignWithMargins = True
        Left = 93
        Top = 353
        Width = 225
        Height = 30
        Margins.Top = 6
        Caption = 
          'Factor controlling number of SWI2 time steps per MODFLOW time st' +
          'ep (ADPTFCT) '
        WordWrap = True
      end
      object lblModflowPrecision: TLabel
        Left = 164
        Top = 117
        Width = 111
        Height = 15
        Caption = 'MODFLOW precision'
      end
      object comboObservations: TJvImageComboBox
        Left = 12
        Top = 82
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 3
        OnChange = comboObservationsChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'No observations (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'ASCII file for observations >0'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Binary file for observations <0'
          end>
      end
      object seNumberOfSurfaces: TJvSpinEdit
        Left = 12
        Top = 3
        Width = 121
        Height = 24
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 0
        OnChange = seNumberOfSurfacesChange
      end
      object cbSaveZeta: TCheckBox
        Left = 12
        Top = 59
        Width = 206
        Height = 17
        Caption = 'Save ZETA to file (ISWIZT)'
        Enabled = False
        TabOrder = 2
      end
      object comboDensityChoice: TJvImageComboBox
        Left = 12
        Top = 30
        Width = 238
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 238
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = 0
        TabOrder = 1
        OnChange = comboDensityChoiceChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Density varies linear between surfaces (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Density is constant between surfaces (1)'
          end>
      end
      object cbAdaptive: TCheckBox
        Left = 12
        Top = 255
        Width = 238
        Height = 17
        Caption = 'Use adaptive time stepping (ADAPTIVE)'
        Enabled = False
        TabOrder = 9
        OnClick = cbAdaptiveClick
      end
      object rdeToeslope: TRbwDataEntry
        AlignWithMargins = True
        Left = 12
        Top = 143
        Width = 74
        Height = 22
        Hint = 
          'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum change in head between two iterations is less than HCLOSE, ' +
          'the program will check the other criterion (RCLOSE).  If both cr' +
          'iteria are met, the program will go on to the next outer iterati' +
          'on.'
        HelpContext = 940
        Color = clBtnFace
        Enabled = False
        TabOrder = 5
        Text = '0.001'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeTipSlope: TRbwDataEntry
        AlignWithMargins = True
        Left = 12
        Top = 171
        Width = 74
        Height = 22
        Hint = 
          'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum change in head between two iterations is less than HCLOSE, ' +
          'the program will check the other criterion (RCLOSE).  If both cr' +
          'iteria are met, the program will go on to the next outer iterati' +
          'on.'
        HelpContext = 940
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '0.001'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeAlpha: TRbwDataEntry
        AlignWithMargins = True
        Left = 12
        Top = 199
        Width = 74
        Height = 22
        Hint = 
          'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum change in head between two iterations is less than HCLOSE, ' +
          'the program will check the other criterion (RCLOSE).  If both cr' +
          'iteria are met, the program will go on to the next outer iterati' +
          'on.'
        HelpContext = 940
        Color = clBtnFace
        Enabled = False
        TabOrder = 7
        Text = '0.001'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeBeta: TRbwDataEntry
        AlignWithMargins = True
        Left = 12
        Top = 227
        Width = 74
        Height = 22
        Hint = 
          'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum change in head between two iterations is less than HCLOSE, ' +
          'the program will check the other criterion (RCLOSE).  If both cr' +
          'iteria are met, the program will go on to the next outer iterati' +
          'on.'
        HelpContext = 940
        Color = clBtnFace
        Enabled = False
        TabOrder = 8
        Text = '0.001'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object seMaxAdaptiveSteps: TJvSpinEdit
        Left = 12
        Top = 278
        Width = 121
        Height = 24
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 10
      end
      object seMinAdaptiveSteps: TJvSpinEdit
        Left = 12
        Top = 317
        Width = 121
        Height = 24
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 11
      end
      object rdeAdaptiveFactor: TRbwDataEntry
        AlignWithMargins = True
        Left = 12
        Top = 357
        Width = 74
        Height = 22
        Hint = 
          'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum change in head between two iterations is less than HCLOSE, ' +
          'the program will check the other criterion (RCLOSE).  If both cr' +
          'iteria are met, the program will go on to the next outer iterati' +
          'on.'
        HelpContext = 940
        Color = clBtnFace
        Enabled = False
        TabOrder = 12
        Text = '0.001'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboModflowPrecision: TJvImageComboBox
        Left = 12
        Top = 114
        Width = 145
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 145
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemHeight = 19
        ItemIndex = -1
        TabOrder = 4
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Single'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Double'
          end>
      end
    end
    object tabSolver: TTabSheet
      Caption = 'Solver'
      ImageIndex = 1
      object lblSolver: TLabel
        Left = 163
        Top = 6
        Width = 130
        Height = 15
        Caption = 'Solver choice (NSOLVER)'
      end
      object lblPrintoutInterval: TLabel
        Left = 139
        Top = 32
        Width = 134
        Height = 15
        Caption = 'Printout interval (IPRSOL)'
      end
      object lblPCGPrintControl: TLabel
        AlignWithMargins = True
        Left = 304
        Top = 65
        Width = 142
        Height = 15
        Margins.Top = 7
        Caption = 'Printing control (MUTPCG)'
      end
      object lblMaxIterOuter: TLabel
        Left = 139
        Top = 92
        Width = 248
        Height = 15
        Caption = 'Maximum number of outer iterations (MXITER)'
      end
      object lblMaxIterInner: TLabel
        Left = 139
        Top = 119
        Width = 235
        Height = 15
        Caption = 'Maximum number of inner iterations (ITER1)'
      end
      object lblPCGMethod: TLabel
        AlignWithMargins = True
        Left = 272
        Top = 149
        Width = 228
        Height = 15
        Margins.Top = 7
        Caption = 'Matrix preconditioning method (NPCOND)'
      end
      object lblMaxZetaChange: TLabel
        AlignWithMargins = True
        Left = 139
        Top = 179
        Width = 182
        Height = 15
        Margins.Top = 6
        Caption = 'Max. abs. change in zeta (ZCLOSE)'
      end
      object lblMaxRes: TLabel
        AlignWithMargins = True
        Left = 139
        Top = 207
        Width = 147
        Height = 15
        Margins.Top = 6
        Caption = 'Max. abs. residual (RCLOSE)'
      end
      object lblRelax: TLabel
        AlignWithMargins = True
        Left = 139
        Top = 235
        Width = 157
        Height = 15
        Margins.Top = 6
        Caption = 'Relaxation parameter (RELAX)'
      end
      object lblEigenValue: TLabel
        AlignWithMargins = True
        Left = 272
        Top = 263
        Width = 242
        Height = 15
        Margins.Top = 7
        Caption = 'Upper bound of the max. eigenvalue (NBPOL)'
      end
      object lblDamp: TLabel
        AlignWithMargins = True
        Left = 139
        Top = 295
        Width = 128
        Height = 15
        Margins.Top = 6
        Caption = 'Damping factor (DAMP)'
      end
      object lblDampT: TLabel
        AlignWithMargins = True
        Left = 139
        Top = 323
        Width = 206
        Height = 15
        Margins.Top = 6
        Caption = 'Transient damping factor (DAMPPCGT)'
      end
      object comboSolver: TJvImageComboBox
        Left = 3
        Top = 3
        Width = 154
        Height = 25
        Style = csOwnerDrawVariable
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 154
        Enabled = False
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
            Text = 'DE4 (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'PCG (2)'
          end>
      end
      object sePrintoutInterval: TJvSpinEdit
        Left = 3
        Top = 32
        Width = 130
        Height = 24
        MaxValue = 2147483647.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 1
      end
      object comboPCGPrint: TJvImageComboBox
        AlignWithMargins = True
        Left = 3
        Top = 62
        Width = 295
        Height = 23
        Hint = 'MUTPCG controls the information that is to be printed.'
        HelpContext = 990
        Style = csDropDownList
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 295
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemIndex = -1
        TabOrder = 2
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Solver information (0)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Iteration only (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Suppress printing (2)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Print only if convergence fails (3)'
          end>
      end
      object seMaxIterOuter: TJvSpinEdit
        Left = 3
        Top = 89
        Width = 130
        Height = 24
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 3
      end
      object seMaxIterInner: TJvSpinEdit
        Left = 3
        Top = 116
        Width = 130
        Height = 24
        MaxValue = 2147483647.000000000000000000
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 4
      end
      object comboPCGPrecondMeth: TJvImageComboBox
        AlignWithMargins = True
        Left = 3
        Top = 146
        Width = 263
        Height = 23
        Hint = 
          'NPCOND = 2 is rarely used because it is generally slower than NP' +
          'COND = 1.'
        HelpContext = 930
        Style = csDropDownList
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 265
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemIndex = -1
        TabOrder = 5
        OnChange = comboPCGPrecondMethChange
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Modified incomplete Cholesky (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Polynomial (2)'
          end>
      end
      object rdeMaxZetaChange: TRbwDataEntry
        AlignWithMargins = True
        Left = 3
        Top = 176
        Width = 130
        Height = 22
        Hint = 
          'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum change in head between two iterations is less than HCLOSE, ' +
          'the program will check the other criterion (RCLOSE).  If both cr' +
          'iteria are met, the program will go on to the next outer iterati' +
          'on.'
        HelpContext = 940
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '0.001'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeMaxRes: TRbwDataEntry
        AlignWithMargins = True
        Left = 3
        Top = 204
        Width = 130
        Height = 22
        Hint = 
          'RCLOSE is one of two convergence criteria in PCG2.  When the max' +
          'imum absolute flow residual is less than RCLOSE, the program wil' +
          'l check HCLOSE{linkID=940}.  If both criteria are met, the progr' +
          'am will go on to the next outer iteration.'
        HelpContext = 950
        Color = clBtnFace
        Enabled = False
        TabOrder = 7
        Text = '1000'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeRelax: TRbwDataEntry
        AlignWithMargins = True
        Left = 3
        Top = 232
        Width = 130
        Height = 22
        Hint = 
          'The relaxation input value is usually set to 1.  However, if you' +
          ' are using the rewetting capability in the Block-Centered Flow a' +
          'nd Layer Property Flow Packages, you may wish to set it to 0.97 ' +
          'to 0.99 because this may prevent zero divide and non-diagonally ' +
          'dominant matrix errors.'
        HelpContext = 960
        Color = clBtnFace
        Enabled = False
        TabOrder = 8
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboEigenValue: TJvImageComboBox
        AlignWithMargins = True
        Left = 3
        Top = 260
        Width = 263
        Height = 23
        Hint = 
          'In many cases you can speed up execution time slightly by settin' +
          'g NBPOL=2.  The estimated value is usually close to 2 and the nu' +
          'mber of iterations required is relatively insensitive to the exa' +
          'ct value of the estimate.'
        HelpContext = 970
        Style = csDropDownList
        ButtonStyle = fsLighter
        Color = clBtnFace
        DroppedWidth = 265
        Enabled = False
        ImageHeight = 0
        ImageWidth = 0
        ItemIndex = -1
        TabOrder = 9
        Items = <
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Calculated (1)'
          end
          item
            Brush.Style = bsClear
            Indent = 0
            Text = 'Two (2)'
          end>
      end
      object rdeDamp: TRbwDataEntry
        AlignWithMargins = True
        Left = 3
        Top = 292
        Width = 130
        Height = 22
        Hint = 'Damping factor for reducing oscillation.'
        HelpContext = 1000
        Color = clBtnFace
        Enabled = False
        TabOrder = 10
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object rdeDampT: TRbwDataEntry
        AlignWithMargins = True
        Left = 3
        Top = 320
        Width = 130
        Height = 22
        Hint = 'Damping factor for reducing oscillation.'
        HelpContext = 1000
        Color = clBtnFace
        Enabled = False
        TabOrder = 11
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
    end
    object tabDensity: TTabSheet
      Caption = 'Dimensionless Density'
      ImageIndex = 2
      object rdgDensity: TRbwDataGrid4
        Left = 0
        Top = 0
        Width = 437
        Height = 409
        Align = alClient
        ColCount = 2
        Enabled = False
        FixedCols = 1
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
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
        ExplicitHeight = 401
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
        Control = seNumberOfSurfaces
      end
      item
        Control = comboDensityChoice
      end
      item
        Control = cbSaveZeta
      end
      item
        Control = comboObservations
      end
      item
        Control = rdeToeslope
      end
      item
        Control = rdeTipSlope
      end
      item
        Control = rdeAlpha
      end
      item
        Control = rdeBeta
      end
      item
        Control = cbAdaptive
      end
      item
        Control = comboSolver
      end
      item
        Control = sePrintoutInterval
      end
      item
        Control = comboPCGPrint
      end
      item
        Control = rdgDensity
      end>
  end
end
