inherited framePackageTransientLayerChoice: TframePackageTransientLayerChoice
  inherited pnLayerOption: TPanel
    inherited lblLayerOption: TLabel
      Width = 73
      Caption = 'Location option'
      ExplicitWidth = 73
    end
    inherited comboLayerOption: TComboBox
      OnChange = comboLayerOptionChange
    end
    object cbTimeVaryingLayers: TCheckBox
      Left = 358
      Top = 5
      Width = 235
      Height = 17
      Caption = 'Time varying layers'
      Enabled = False
      TabOrder = 1
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
      end>
  end
end
