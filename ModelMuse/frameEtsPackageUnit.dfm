inherited frameEtsPackage: TframeEtsPackage
  Height = 226
  ExplicitHeight = 226
  DesignSize = (
    477
    226)
  inherited pnLayerOption: TPanel
    Top = 161
    Height = 65
    ExplicitTop = 161
    ExplicitHeight = 65
    object lblSegments: TLabel [1]
      Left = 64
      Top = 28
      Width = 99
      Height = 13
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
      Height = 21
      ButtonKind = bkClassic
      MaxValue = 100.000000000000000000
      MinValue = 1.000000000000000000
      Value = 1.000000000000000000
      Enabled = False
      TabOrder = 2
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
      end>
  end
end
