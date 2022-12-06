inherited framePCG: TframePCG
  Width = 451
  Height = 304
  Align = alClient
  Anchors = [akLeft, akTop, akBottom]
  ExplicitWidth = 451
  ExplicitHeight = 304
  DesignSize = (
    451
    304)
  inherited lblComments: TLabel
    Top = 40
    ExplicitTop = 40
  end
  object lblPCGMaxOuter: TLabel [2]
    AlignWithMargins = True
    Left = 13
    Top = 138
    Width = 222
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Max. number of outer iterations (MXITER):'
  end
  object lblPCGMaxInner: TLabel [3]
    AlignWithMargins = True
    Left = 13
    Top = 166
    Width = 209
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Max. number of inner iterations (ITER1):'
  end
  object lblPCGMethod: TLabel [4]
    AlignWithMargins = True
    Left = 13
    Top = 194
    Width = 231
    Height = 15
    Margins.Top = 7
    Anchors = [akLeft, akBottom]
    Caption = 'Matrix preconditioning method (NPCOND):'
  end
  object lblPCGMaxChangeHead: TLabel [5]
    AlignWithMargins = True
    Left = 13
    Top = 314
    Width = 192
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Max. abs. change in head (HCLOSE):'
  end
  object lblPCGMaxResidual: TLabel [6]
    AlignWithMargins = True
    Left = 13
    Top = 342
    Width = 150
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Max. abs. residual (RCLOSE):'
  end
  object lblPCGRelaxation: TLabel [7]
    AlignWithMargins = True
    Left = 13
    Top = 370
    Width = 160
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Relaxation parameter (RELAX):'
  end
  object lblPCGMaxEigen: TLabel [8]
    AlignWithMargins = True
    Left = 13
    Top = 398
    Width = 245
    Height = 15
    Margins.Top = 7
    Anchors = [akLeft, akBottom]
    Caption = 'Upper bound of the max. eigenvalue (NBPOL):'
  end
  object lblPCGPrintInterval: TLabel [9]
    AlignWithMargins = True
    Left = 13
    Top = 425
    Width = 139
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Printout interval (IPRPCG):'
  end
  object lblPCGPrintControl: TLabel [10]
    AlignWithMargins = True
    Left = 13
    Top = 453
    Width = 145
    Height = 15
    Margins.Top = 7
    Anchors = [akLeft, akBottom]
    Caption = 'Printing control (MUTPCG):'
  end
  object lblPCGDampingFactor: TLabel [11]
    AlignWithMargins = True
    Left = 13
    Top = 480
    Width = 154
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Damping factor (DAMPPCG):'
  end
  object lblPCGDampPcgT: TLabel [12]
    AlignWithMargins = True
    Left = 13
    Top = 508
    Width = 209
    Height = 15
    Margins.Top = 6
    Anchors = [akLeft, akBottom]
    Caption = 'Transient damping factor (DAMPPCGT):'
  end
  inherited memoComments: TMemo
    Left = 13
    Top = 59
    Width = 583
    Height = 70
    ExplicitLeft = 13
    ExplicitTop = 59
    ExplicitWidth = 583
    ExplicitHeight = 70
  end
  object rdePCGMaxOuter: TRbwDataEntry [14]
    AlignWithMargins = True
    Left = 343
    Top = 135
    Width = 254
    Height = 22
    Hint = 
      'For linear problems, MXITER, should be 1 unless more than 50 inn' +
      'er iterations are required.  In that case MXITER could be as lar' +
      'ge as 10.  For nonlinear problems, MXITER may need to be larger ' +
      'but should usually be less than 100.'
    HelpContext = 910
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 1
    Text = '20'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdePCGMaxInner: TRbwDataEntry [15]
    AlignWithMargins = True
    Left = 343
    Top = 163
    Width = 254
    Height = 22
    Hint = 
      'Usually <=30 for linear problems; usually 3-10 for nonlinear pro' +
      'blems.'
    HelpContext = 920
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
    Text = '30'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboPCGPrecondMeth: TJvImageComboBox [16]
    AlignWithMargins = True
    Left = 343
    Top = 191
    Width = 254
    Height = 23
    Hint = 
      'NPCOND = 2 is rarely used because it is generally slower than NP' +
      'COND = 1.'
    HelpContext = 930
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 265
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemIndex = -1
    TabOrder = 3
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
  object rdePCGMaxHeadChange: TRbwDataEntry [17]
    AlignWithMargins = True
    Left = 343
    Top = 311
    Width = 254
    Height = 22
    Hint = 
      'HCLOSE is one of two convergence criteria in PCG2.  When the max' +
      'imum change in head between two iterations is less than HCLOSE, ' +
      'the program will check the other criterion (RCLOSE).  If both cr' +
      'iteria are met, the program will go on to the next outer iterati' +
      'on.'
    HelpContext = 940
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
    Text = '0.001'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdePCGMaxResChange: TRbwDataEntry [18]
    AlignWithMargins = True
    Left = 343
    Top = 339
    Width = 254
    Height = 22
    Hint = 
      'RCLOSE is one of two convergence criteria in PCG2.  When the max' +
      'imum absolute flow residual is less than RCLOSE, the program wil' +
      'l check HCLOSE{linkID=940}.  If both criteria are met, the progr' +
      'am will go on to the next outer iteration.'
    HelpContext = 950
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 6
    Text = '1000'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object rdePCGRelax: TRbwDataEntry [19]
    AlignWithMargins = True
    Left = 343
    Top = 367
    Width = 254
    Height = 22
    Hint = 
      'The relaxation input value is usually set to 1.  However, if you' +
      ' are using the rewetting capability in the Block-Centered Flow a' +
      'nd Layer Property Flow Packages, you may wish to set it to 0.97 ' +
      'to 0.99 because this may prevent zero divide and non-diagonally ' +
      'dominant matrix errors.'
    HelpContext = 960
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 7
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboPCGEigenValue: TJvImageComboBox [20]
    AlignWithMargins = True
    Left = 343
    Top = 395
    Width = 254
    Height = 23
    Hint = 
      'In many cases you can speed up execution time slightly by settin' +
      'g NBPOL=2.  The estimated value is usually close to 2 and the nu' +
      'mber of iterations required is relatively insensitive to the exa' +
      'ct value of the estimate.'
    HelpContext = 970
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 265
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemIndex = -1
    TabOrder = 8
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
  object rdePCGPrintInt: TRbwDataEntry [21]
    AlignWithMargins = True
    Left = 343
    Top = 422
    Width = 254
    Height = 22
    Hint = 
      'Information is printed for each iteration of a time step wheneve' +
      'r the time step is an even multiple of IPRPCG.  The printout als' +
      'o is generated at the end of each stress period.'
    HelpContext = 980
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 9
    Text = '1'
    DataType = dtInteger
    Max = 1.000000000000000000
    CheckMin = True
    ChangeDisabledColor = True
  end
  object comboPCGPrint: TJvImageComboBox [22]
    AlignWithMargins = True
    Left = 343
    Top = 450
    Width = 254
    Height = 23
    Hint = 'MUTPCG controls the information that is to be printed.'
    HelpContext = 990
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 265
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemIndex = -1
    TabOrder = 10
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
  object rdePCGDamp: TRbwDataEntry [23]
    AlignWithMargins = True
    Left = 343
    Top = 477
    Width = 254
    Height = 22
    Hint = 'Damping factor for reducing oscillation.'
    HelpContext = 1000
    Anchors = [akLeft, akBottom]
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
  object rdePCGDampPcgT: TRbwDataEntry [24]
    AlignWithMargins = True
    Left = 343
    Top = 505
    Width = 254
    Height = 22
    Hint = 'Damping factor for reducing oscillation.'
    HelpContext = 1000
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    Enabled = False
    TabOrder = 12
    Text = '1'
    DataType = dtReal
    Max = 1.000000000000000000
    CheckMax = True
    CheckMin = True
    ChangeDisabledColor = True
  end
  object gbIHCOFADD: TGroupBox [25]
    Left = 13
    Top = 213
    Width = 583
    Height = 84
    Caption = 'IHCOFADD'
    TabOrder = 4
    object rbIHCOFADD_0: TRadioButton
      Left = 3
      Top = 21
      Width = 510
      Height = 17
      Caption = 'Convert active cells to dry when surrounded by dry cells (0)'
      Enabled = False
      TabOrder = 0
    end
    object rbIHCOFADD_1: TRadioButton
      Left = 3
      Top = 44
      Width = 558
      Height = 37
      Caption = 
        'Convert active cells to dry when surrounded by dry cells AND hea' +
        'd-dependant and storage flow is zero (1)'
      Enabled = False
      TabOrder = 1
      WordWrap = True
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
        Control = rdePCGMaxOuter
      end
      item
        Control = rdePCGMaxInner
      end
      item
        Control = comboPCGPrecondMeth
      end
      item
        Control = rdePCGMaxHeadChange
      end
      item
        Control = rdePCGMaxResChange
      end
      item
        Control = rdePCGPrintInt
      end
      item
        Control = comboPCGPrint
      end
      item
        Control = rdePCGDamp
      end
      item
        Control = rdePCGDampPcgT
      end
      item
        Control = rbIHCOFADD_0
      end
      item
        Control = rbIHCOFADD_1
      end>
  end
end
