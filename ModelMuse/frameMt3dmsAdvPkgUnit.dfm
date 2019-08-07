inherited frameMt3dmsAdvPkg: TframeMt3dmsAdvPkg
  Width = 601
  Height = 504
  ExplicitWidth = 601
  ExplicitHeight = 504
  DesignSize = (
    601
    504)
  inherited memoComments: TMemo
    Width = 570
    ExplicitWidth = 570
  end
  object pcAdvection: TPageControl [3]
    Left = 0
    Top = 168
    Width = 601
    Height = 336
    ActivePage = tabAdvection1
    Align = alBottom
    TabOrder = 2
    object tabAdvection1: TTabSheet
      Caption = 'Advection 1'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lbllMethod: TLabel
        Left = 3
        Top = 3
        Width = 175
        Height = 13
        Caption = 'Advection solution scheme (MIXELM)'
      end
      object lblParticleTracking: TLabel
        Left = 3
        Top = 33
        Width = 171
        Height = 13
        Caption = 'Particle tracking algorithm (ITRACK)'
      end
      object lbNumCellsParticle: TLabel
        Left = 3
        Top = 90
        Width = 398
        Height = 13
        Caption = 
          'Number of cells a particle can move in one time step, generally ' +
          '0.5 to 1.0 (PERCEL)'
      end
      object lblMaxParticlesCount: TLabel
        Left = 3
        Top = 112
        Width = 252
        Height = 13
        Caption = 'Maximum number of total moving particles (MXPART)'
      end
      object lblConcWeight: TLabel
        Left = 3
        Top = 146
        Width = 228
        Height = 13
        Caption = 'Concentration Weighting factor (0.5 to 1) (WD)'
      end
      object lblNegConcGrad: TLabel
        Left = 3
        Top = 170
        Width = 234
        Height = 13
        Caption = 'Size of negligible concentration gradient (DCEPS)'
      end
      object lblInitParticlesSmall: TLabel
        Left = 3
        Top = 202
        Width = 326
        Height = 13
        Caption = 
          'Initial particles per cell where concentration gradient < DCEPS ' +
          '(NPL)'
      end
      object lblInitParticlesLarge: TLabel
        Left = 3
        Top = 226
        Width = 328
        Height = 13
        Caption = 
          'Initial particles per cell where concentration gradient > DCEPS ' +
          '(NPH)'
      end
      object Label12: TLabel
        Left = 3
        Top = 60
        Width = 138
        Height = 13
        Caption = 'Weighting scheme (NADVFD)'
      end
      object comboAdvSolScheme: TComboBox
        Left = 285
        Top = 3
        Width = 305
        Height = 21
        Hint = 'Check the MT3DMS user manual for a comparison of these methods.'
        HelpContext = 3220
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 0
        Text = 'Third-order TVD (ULTIMATE) (-1)'
        OnChange = comboAdvSolSchemeChange
        Items.Strings = (
          'Third-order TVD (ULTIMATE) (-1)'
          'Standard finite difference method (0)'
          'Method of characterisitics MOC (1)'
          'Modified method of characterisitics MMOC (2)'
          'Hybrid MOC/MMOC (3)')
      end
      object comboParticleTrackingAlg: TComboBox
        Left = 285
        Top = 30
        Width = 305
        Height = 21
        Hint = 
          'The first order Euler particle tracking algorithm is faster but ' +
          'less accurate than the Runge-Kutta method especially in areas of' +
          ' converging or diverging flow such as sources and sinks.'
        HelpContext = 3230
        Style = csDropDownList
        Enabled = False
        ItemIndex = 2
        TabOrder = 1
        Text = 'Runge-Kutta only near sinks/sources (3)'
        Items.Strings = (
          'First-order Euler (1)'
          'Fourth-order Runge-Kutta (2)'
          'Runge-Kutta only near sinks/sources (3)')
      end
      object adeMaxParticleMovement: TRbwDataEntry
        Left = 501
        Top = 84
        Width = 89
        Height = 22
        Hint = 'Usually values of 0.5 to 1 are appropriate.'
        HelpContext = 3240
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        Text = '1'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object adeConcWeight: TRbwDataEntry
        Left = 501
        Top = 139
        Width = 89
        Height = 22
        Hint = 
          'WD is a concentration weighting factor between 0.5 and 1. 0.5 is' +
          ' usually a good choice. You can adjust it to achieve a better ma' +
          'ss balance. Advection becomes more dominant as WD increases.'
        HelpContext = 3260
        Color = clBtnFace
        Enabled = False
        TabOrder = 5
        Text = '0.5'
        DataType = dtReal
        Max = 1.000000000000000000
        Min = 0.500000000000000000
        CheckMax = True
        CheckMin = True
        ChangeDisabledColor = True
      end
      object adeNeglSize: TRbwDataEntry
        Left = 501
        Top = 167
        Width = 89
        Height = 22
        Hint = 
          'DCEPS affects how many particles are used in each cell. (See NPL' +
          ' and NPH).  A value abound 10^-5 is generally adequate.'
        HelpContext = 3270
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        Text = '1e-005'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object comboAdvWeightingScheme: TComboBox
        Left = 405
        Top = 57
        Width = 185
        Height = 21
        Hint = 
          'NADVFD determines which weighting schem should be used (central-' +
          'in-space, or upstream weighting) when the implicit finite differ' +
          'ence method is used.'
        HelpContext = 3300
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 2
        Text = 'upstream weighting (1)'
        Items.Strings = (
          'upstream weighting (1)'
          'central-in-space weighting (2)')
      end
      object spinMaxParticlesCount: TJvSpinEdit
        Left = 501
        Top = 112
        Width = 89
        Height = 21
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 4
      end
      object spinInitParticlesSmall: TJvSpinEdit
        Left = 501
        Top = 195
        Width = 89
        Height = 21
        CheckMinValue = True
        Enabled = False
        TabOrder = 7
      end
      object spinInitParticlesLarge: TJvSpinEdit
        Left = 501
        Top = 219
        Width = 89
        Height = 21
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 8
      end
    end
    object tabAdvection2: TTabSheet
      Caption = 'Advection 2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lblInitParticlePlacement: TLabel
        Left = 3
        Top = 3
        Width = 220
        Height = 13
        Caption = 'Initial placement of moving particles (NPLANE)'
      end
      object lblInitParticlePlanes: TLabel
        Left = 3
        Top = 36
        Width = 244
        Height = 13
        Caption = 'Number of planes of particles in each cell (NPLANE)'
      end
      object lblMinParticles: TLabel
        Left = 3
        Top = 55
        Width = 275
        Height = 13
        Caption = 'Minimum moving particles per cell (usually 0 to 4) (NPMIN)'
      end
      object lblMaxParticles: TLabel
        Left = 3
        Top = 89
        Width = 206
        Height = 13
        Caption = 'Maximum moving particles per cell (NPMAX)'
      end
      object lblSinkParticlePlacement: TLabel
        Left = 3
        Top = 108
        Width = 217
        Height = 13
        Caption = 'Initial placement of particles in sinks (NLSINK)'
      end
      object lblSinkParticlePlanes: TLabel
        Left = 3
        Top = 144
        Width = 262
        Height = 13
        Caption = 'Number of planes of particles in each sink cell (NLSINK)'
      end
      object lblSinkParticleN: TLabel
        Left = 3
        Top = 168
        Width = 274
        Height = 13
        Caption = 'Number of particles used to apprimate sink cells (NPSINK)'
      end
      object lblCritConcGrad: TLabel
        Left = 3
        Top = 192
        Width = 238
        Height = 13
        Caption = 'Critical relative concentration gradient (DCHMOC)'
      end
      object comboInitPartPlace: TComboBox
        Left = 408
        Top = 0
        Width = 89
        Height = 21
        Hint = 
          'The initial placement of moving particles can be either random o' +
          'r fixed. '
        HelpContext = 3320
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 0
        Text = 'Random (0)'
        OnChange = comboInitPartPlaceChange
        Items.Strings = (
          'Random (0)'
          'Fixed (1)')
      end
      object comboInitPartSinkChoice: TComboBox
        Left = 408
        Top = 108
        Width = 89
        Height = 21
        Hint = 
          'The initial placement of moving particles in sink cells can be e' +
          'ither random or fixed.'
        HelpContext = 3370
        Style = csDropDownList
        Enabled = False
        ItemIndex = 0
        TabOrder = 4
        Text = 'Random (0)'
        OnChange = comboInitPartSinkChoiceChange
        Items.Strings = (
          'Random (0)'
          'Fixed (1)')
      end
      object adeCritRelConcGrad: TRbwDataEntry
        Left = 408
        Top = 189
        Width = 89
        Height = 22
        Hint = 
          'DCHMOC is the critical relative concentration gradient for contr' +
          'olling the use of either the MOC or MMOC scheme in the hybrid MO' +
          'C/MMOC scheme.'
        HelpContext = 3400
        Color = clBtnFace
        Enabled = False
        TabOrder = 7
        Text = '0.01'
        DataType = dtReal
        Max = 1.000000000000000000
        CheckMin = True
        ChangeDisabledColor = True
      end
      object spinInitParticlePlanes: TJvSpinEdit
        Left = 408
        Top = 27
        Width = 89
        Height = 21
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 1
      end
      object spinMinParticles: TJvSpinEdit
        Left = 408
        Top = 54
        Width = 89
        Height = 21
        CheckMinValue = True
        Enabled = False
        TabOrder = 2
      end
      object spinMaxParticles: TJvSpinEdit
        Left = 408
        Top = 81
        Width = 89
        Height = 21
        CheckMinValue = True
        Enabled = False
        TabOrder = 3
      end
      object spinSinkParticlePlanes: TJvSpinEdit
        Left = 408
        Top = 135
        Width = 89
        Height = 21
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 5
      end
      object spinSinkParticleN: TJvSpinEdit
        Left = 408
        Top = 162
        Width = 89
        Height = 21
        CheckMaxValue = False
        MinValue = 1.000000000000000000
        Value = 1.000000000000000000
        Enabled = False
        TabOrder = 6
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
        Control = comboAdvSolScheme
      end
      item
        Control = comboAdvWeightingScheme
      end
      item
        Control = adeMaxParticleMovement
      end
      item
        Control = spinMaxParticlesCount
      end>
  end
end
