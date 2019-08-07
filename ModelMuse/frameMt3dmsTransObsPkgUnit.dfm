inherited frameMt3dmsTransObsPkg: TframeMt3dmsTransObsPkg
  Width = 496
  Height = 512
  ExplicitWidth = 496
  ExplicitHeight = 512
  DesignSize = (
    496
    512)
  inherited memoComments: TMemo
    Width = 465
    ExplicitWidth = 465
  end
  object cbSaveBinary: TCheckBox [3]
    Left = 16
    Top = 157
    Width = 373
    Height = 18
    Caption = 'Save simulated values to binary file (inSaveObs)'
    Enabled = False
    TabOrder = 1
  end
  object grpbxConcentrationObservations: TGroupBox [4]
    Left = 17
    Top = 180
    Width = 464
    Height = 181
    Caption = 'Concentration observations'
    TabOrder = 2
    object lblConcScaleFactor: TLabel
      Left = 3
      Top = 21
      Width = 170
      Height = 13
      Caption = 'Concentration scale factor (CScale)'
    end
    object lblSaveType: TLabel
      Left = 3
      Top = 65
      Width = 262
      Height = 13
      Caption = 'Concentration observations results to save (iOutCobs)'
    end
    object rdeConcScaleFactor: TRbwDataEntry
      Left = 3
      Top = 37
      Width = 57
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
    object comboSaveConcType: TJvImageComboBox
      Left = 3
      Top = 91
      Width = 262
      Height = 23
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 262
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 17
      ItemIndex = 1
      TabOrder = 1
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'concentrations'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'concentrations, residuals'
        end>
    end
    object cbLogTransform: TCheckBox
      Left = 3
      Top = 120
      Width = 217
      Height = 18
      Caption = 'Log transform (iConcLOG)'
      Enabled = False
      TabOrder = 2
    end
    object cbInterpolate: TCheckBox
      Left = 3
      Top = 144
      Width = 446
      Height = 18
      Caption = 'Interpolate concentrations (iConcINTP)'
      Enabled = False
      TabOrder = 3
    end
  end
  object grpbxMassFluxObservations: TGroupBox [5]
    Left = 20
    Top = 375
    Width = 461
    Height = 123
    Caption = 'Mass flux observations'
    TabOrder = 3
    object lblMassFluxScaleFactor: TLabel
      Left = 3
      Top = 19
      Width = 146
      Height = 13
      Caption = 'Mass flux scale factor (FScale)'
    end
    object lblSaveMassFluxType: TLabel
      Left = 3
      Top = 68
      Width = 235
      Height = 13
      Caption = 'Mass flux observations results to save (iOutFlux)'
    end
    object rdeMassFluxScaleFactor: TRbwDataEntry
      Left = 3
      Top = 40
      Width = 57
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
    object comboSaveMassFluxType: TJvImageComboBox
      Left = 3
      Top = 89
      Width = 235
      Height = 23
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      Color = clBtnFace
      DroppedWidth = 235
      Enabled = False
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 17
      ItemIndex = 0
      TabOrder = 1
      Items = <
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'mass fluxes'
        end
        item
          Brush.Style = bsClear
          Indent = 0
          Text = 'mass fluxes, residuals'
        end>
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
        Control = cbSaveBinary
      end
      item
        Control = rdeConcScaleFactor
      end
      item
        Control = comboSaveConcType
      end
      item
        Control = cbLogTransform
      end
      item
        Control = cbInterpolate
      end
      item
        Control = rdeMassFluxScaleFactor
      end
      item
        Control = comboSaveMassFluxType
      end>
  end
end
