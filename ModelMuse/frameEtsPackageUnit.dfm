inherited frameEtsPackage: TframeEtsPackage
  Height = 237
  ExplicitHeight = 237
  DesignSize = (
    477
    237)
  inherited pnLayerOption: TPanel
    Top = 156
    Height = 81
    ExplicitTop = 156
    ExplicitHeight = 81
    object lblSegments: TLabel [1]
      Left = 64
      Top = 28
      Width = 112
      Height = 15
      Caption = 'Number of segments'
      Enabled = False
    end
    inherited comboLayerOption: TComboBox
      ItemIndex = -1
      Text = ''
    end
    object seSegments: TJvSpinEdit
      Left = 17
      Top = 25
      Width = 41
      Height = 23
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Enabled = False
      TabOrder = 2
    end
    object cbUseMultiplierMODFLOW6: TCheckBox
      Left = 16
      Top = 54
      Width = 273
      Height = 17
      Caption = 'Use Multiplier (MODFLOW 6)'
      Enabled = False
      TabOrder = 3
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
        Control = cbTimeVaryingLayers
      end
      item
        Control = comboLayerOption
      end
      item
        Control = lblLayerOption
      end
      item
        Control = seSegments
      end
      item
        Control = lblSegments
      end
      item
        Control = cbUseMultiplierMODFLOW6
      end>
  end
end
