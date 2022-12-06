inherited framePackageSFR: TframePackageSFR
  Width = 614
  Height = 445
  ExplicitWidth = 614
  ExplicitHeight = 445
  DesignSize = (
    614
    445)
  object lblPrintStreams: TLabel [2]
    Left = 176
    Top = 144
    Width = 115
    Height = 15
    Caption = 'Print streams (ISTCB2)'
  end
  object lblStreamTolerance: TLabel [3]
    AlignWithMargins = True
    Left = 122
    Top = 246
    Width = 138
    Height = 15
    Caption = 'Tolerance (L^3/T) (DLEAK)'
  end
  object lblSfrTrailingWaveIncrements: TLabel [4]
    AlignWithMargins = True
    Left = 122
    Top = 270
    Width = 246
    Height = 15
    Caption = 'Number of trailing wave increments (NSTRAIL)'
  end
  object lblSfrMaxTrailingWaves: TLabel [5]
    AlignWithMargins = True
    Left = 122
    Top = 293
    Width = 252
    Height = 15
    Caption = 'Maximum number of trailing waves (NSFRSETS)'
  end
  object lblSfrMaxUnsatCells: TLabel [6]
    AlignWithMargins = True
    Left = 122
    Top = 316
    Width = 331
    Height = 15
    Caption = 'Maximum number of cells to define unsaturated zone (ISUZN) '
  end
  object lblNUMTIM: TLabel [7]
    AlignWithMargins = True
    Left = 122
    Top = 354
    Width = 349
    Height = 15
    Caption = 'Number of divisions per time step for kinematic waves (NUMTIM) '
  end
  object lblWeight: TLabel [8]
    AlignWithMargins = True
    Left = 122
    Top = 376
    Width = 342
    Height = 15
    Caption = 'Time weighting factor for the kinematic wave solution (WEIGHT) '
  end
  object lblFLWTOL: TLabel [9]
    AlignWithMargins = True
    Left = 122
    Top = 398
    Width = 312
    Height = 15
    Caption = 'Closure criterion for the kinematic wave solution (FLWTOL) '
  end
  object lblLossAdjustmentFactor: TLabel [10]
    Left = 399
    Top = 26
    Width = 140
    Height = 15
    Caption = 'Adjustment factor (Factor)'
  end
  inherited memoComments: TMemo
    Width = 583
    Height = 51
    TabOrder = 2
    ExplicitWidth = 583
    ExplicitHeight = 51
  end
  object cbSfrUnsatflow: TCheckBox95 [12]
    Left = 16
    Top = 117
    Width = 241
    Height = 23
    Hint = 
      'Model flow from the stream through the unsaturated zone to the w' +
      'ater table.'
    HelpContext = 621
    Alignment = taLeftJustify
    Caption = 'Unsaturated Flow (ISFROPT)'
    Enabled = False
    TabOrder = 3
    WordWrap = False
    OnClick = cbSfrUnsatflowClick
    AlignmentBtn = taLeftJustify
    LikePushButton = False
    VerticalAlignment = vaTop
  end
  object cbSfrLpfHydraulicCond: TCheckBox95 [13]
    Left = 263
    Top = 117
    Width = 330
    Height = 23
    Hint = 
      'Hydraulic conductivity in unsaturated zone comes from LPF packag' +
      'e'
    HelpContext = 622
    Alignment = taLeftJustify
    Caption = 'LPF hydraulic conductivites used (ISFROPT) '
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 4
    WordWrap = False
    AlignmentBtn = taLeftJustify
    LikePushButton = False
    VerticalAlignment = vaTop
  end
  object rgSfr2ISFROPT: TRadioGroup [14]
    Left = 16
    Top = 170
    Width = 583
    Height = 70
    Caption = 'Streambed properties (ISFROPT)'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Specify some streambed properties using segment endpoints'
      
        'Specify some streambed properties by reach (can'#39't inactivate str' +
        'eams)')
    TabOrder = 7
  end
  object comboPrintStreams: TComboBox [15]
    Left = 16
    Top = 143
    Width = 145
    Height = 23
    Style = csDropDownList
    Enabled = False
    TabOrder = 5
    Items.Strings = (
      'Don'#39't print flows'
      'Print flows in listing file'
      'Print flows in .sfr_out file')
  end
  object cbGage8: TCheckBox [16]
    Left = 16
    Top = 420
    Width = 415
    Height = 17
    Caption = 'Gage overall stream budget (OUTTYPE = 8)'
    Enabled = False
    TabOrder = 16
    OnClick = cbIRTFLGClick
  end
  object rdeDLEAK: TRbwDataEntry [17]
    Left = 16
    Top = 246
    Width = 100
    Height = 18
    Color = clBtnFace
    Enabled = False
    TabOrder = 8
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeNstrail: TRbwDataEntry [18]
    Left = 16
    Top = 270
    Width = 100
    Height = 17
    Color = clBtnFace
    Enabled = False
    TabOrder = 9
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeNsfrsets: TRbwDataEntry [19]
    Left = 16
    Top = 293
    Width = 100
    Height = 17
    Color = clBtnFace
    Enabled = False
    TabOrder = 10
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeIsuzn: TRbwDataEntry [20]
    Left = 16
    Top = 316
    Width = 100
    Height = 17
    Color = clBtnFace
    Enabled = False
    TabOrder = 11
    Text = '0'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbIRTFLG: TCheckBox [21]
    Left = 16
    Top = 331
    Width = 425
    Height = 17
    Caption = 
      'Use transient streamflow routing with kinematic-wave equation (I' +
      'RTFLG)'
    Enabled = False
    TabOrder = 12
    OnClick = cbIRTFLGClick
  end
  object rdeNUMTIM: TRbwDataEntry [22]
    Left = 16
    Top = 354
    Width = 100
    Height = 16
    Color = clBtnFace
    Enabled = False
    TabOrder = 13
    Text = '1'
    DataType = dtInteger
    Max = 1.000000000000000000
    Min = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeWeight: TRbwDataEntry [23]
    Left = 16
    Top = 376
    Width = 100
    Height = 16
    Color = clBtnFace
    Enabled = False
    TabOrder = 14
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    Min = 0.500000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdeFLWTOL: TRbwDataEntry [24]
    Left = 16
    Top = 398
    Width = 100
    Height = 16
    Color = clBtnFace
    Enabled = False
    TabOrder = 15
    Text = '0'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object cbUseGsflowFormat: TCheckBox [25]
    Left = 328
    Top = 144
    Width = 185
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Use GSFLOW format'
    Enabled = False
    TabOrder = 6
  end
  object cbSeepageLoss: TCheckBox [26]
    Left = 304
    Top = 3
    Width = 249
    Height = 17
    Caption = 'Adjust seepage loss (LOSSFACTOR)'
    Enabled = False
    TabOrder = 0
    OnClick = cbSeepageLossClick
  end
  object rdeLossAdjustmentFactor: TRbwDataEntry [27]
    Left = 304
    Top = 23
    Width = 89
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
  inherited rcSelectionController: TRbwController
    ControlList = <
      item
        Control = lblComments
      end
      item
        Control = memoComments
      end
      item
        Control = rgSfr2ISFROPT
      end
      item
        Control = cbSfrUnsatflow
      end
      item
        Control = comboPrintStreams
      end
      item
        Control = rgSfr2ISFROPT
      end
      item
        Control = cbIRTFLG
      end
      item
        Control = cbGage8
      end
      item
        Control = cbUseGsflowFormat
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
