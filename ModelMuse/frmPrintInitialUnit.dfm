inherited frmPrintInitial: TfrmPrintInitial
  Left = 551
  Top = 363
  HelpType = htKeyword
  HelpKeyword = 'Print_Initial_Conditions_Dialog_Box'
  ActiveControl = cbBoundaryConditions
  Caption = 'PHAST Print Initial Conditions'
  ClientHeight = 334
  ClientWidth = 598
  ExplicitWidth = 610
  ExplicitHeight = 372
  PixelsPerInch = 120
  TextHeight = 18
  object cbBoundaryConditions: TCheckBox
    Left = 8
    Top = 8
    Width = 281
    Height = 30
    Caption = 'Boundary conditions (*.O.probdef)'
    TabOrder = 0
  end
  object cbComponents: TCheckBox
    Left = 8
    Top = 40
    Width = 281
    Height = 30
    Caption = 'Components (*.O.comps)'
    TabOrder = 1
  end
  object cbConductance: TCheckBox
    Left = 8
    Top = 72
    Width = 281
    Height = 30
    Caption = 'Conductance (*.O.kd)'
    TabOrder = 2
  end
  object cbEchoInput: TCheckBox
    Left = 8
    Top = 104
    Width = 281
    Height = 30
    Caption = 'Echo input (*.log)'
    TabOrder = 3
  end
  object cbFluidProperties: TCheckBox
    Left = 8
    Top = 136
    Width = 281
    Height = 30
    Caption = 'Fluid properties (*.O.probdef)'
    TabOrder = 4
  end
  object cbForceChemistryPrint: TCheckBox
    Left = 8
    Top = 168
    Width = 281
    Height = 30
    Caption = 'Force chemistry print (*.O.chem)'
    TabOrder = 5
  end
  object cbHDF_Chemistry: TCheckBox
    Left = 8
    Top = 200
    Width = 281
    Height = 30
    Caption = 'HDF chemistry (*.h5)'
    TabOrder = 6
  end
  object cbHDF_Heads: TCheckBox
    Left = 8
    Top = 232
    Width = 281
    Height = 30
    Caption = 'HDF heads (*.h5)'
    TabOrder = 7
  end
  object cbHDF_SteadyFlowVelocity: TCheckBox
    Left = 8
    Top = 264
    Width = 281
    Height = 30
    Caption = 'HDF steady flow velocity (*.h5)'
    TabOrder = 8
  end
  object cbHeads: TCheckBox
    Left = 8
    Top = 296
    Width = 281
    Height = 30
    Caption = 'Heads (*.O.head)'
    TabOrder = 9
  end
  object cbMediaProperties: TCheckBox
    Left = 296
    Top = 8
    Width = 297
    Height = 30
    Caption = 'Media properties (*.O.probdef)'
    TabOrder = 10
  end
  object cbSolutionMethod: TCheckBox
    Left = 296
    Top = 40
    Width = 297
    Height = 30
    Caption = 'Solution method (*.O.probdef)'
    TabOrder = 11
  end
  object cbSteadyFlowVelocities: TCheckBox
    Left = 296
    Top = 72
    Width = 297
    Height = 30
    Caption = 'Steady flow velocities (*.O.vel)'
    TabOrder = 12
  end
  object cbWells: TCheckBox
    Left = 296
    Top = 104
    Width = 297
    Height = 30
    Caption = 'Wells (*.O.wel)'
    TabOrder = 13
  end
  object cbXYZ_Chemistry: TCheckBox
    Left = 296
    Top = 136
    Width = 297
    Height = 30
    Caption = 'XYZ chemistry (*.xyz.chem)'
    TabOrder = 14
  end
  object cbXYZ_Components: TCheckBox
    Left = 296
    Top = 168
    Width = 297
    Height = 30
    Caption = 'XYZ components (*.xyz.comps)'
    TabOrder = 15
  end
  object cbXYZ_Heads: TCheckBox
    Left = 296
    Top = 200
    Width = 297
    Height = 30
    Caption = 'XYZ heads (*.xyz.head)'
    TabOrder = 16
  end
  object cbXYZ_SteadyFlowVelocities: TCheckBox
    Left = 296
    Top = 232
    Width = 297
    Height = 30
    Caption = 'XYZ steady flow velocities (*.xyz.vel)'
    TabOrder = 17
  end
  object cbXYZ_Wells: TCheckBox
    Left = 296
    Top = 264
    Width = 297
    Height = 30
    Caption = 'XYZ wells (*.xyz.wel)'
    TabOrder = 18
  end
  object btnOK: TBitBtn
    Left = 409
    Top = 296
    Width = 89
    Height = 33
    Kind = bkOK
    NumGlyphs = 2
    TabOrder = 19
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 505
    Top = 296
    Width = 89
    Height = 33
    Kind = bkCancel
    NumGlyphs = 2
    TabOrder = 20
  end
  object btnHelp: TBitBtn
    Left = 313
    Top = 296
    Width = 89
    Height = 33
    Kind = bkHelp
    NumGlyphs = 2
    TabOrder = 21
    OnClick = btnHelpClick
  end
end
