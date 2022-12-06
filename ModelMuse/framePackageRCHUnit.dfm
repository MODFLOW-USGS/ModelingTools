inherited framePackageRCH: TframePackageRCH
  Height = 228
  ExplicitHeight = 228
  DesignSize = (
    477
    228)
  inherited memoComments: TMemo
    Height = 51
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 51
  end
  inherited pnLayerOption: TPanel
    Top = 128
    Height = 100
    ExplicitTop = 128
    ExplicitHeight = 100
    DesignSize = (
      477
      100)
    object rgAssignmentMethod: TRadioGroup
      Left = 16
      Top = 30
      Width = 446
      Height = 59
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Recharge assignment method'
      Enabled = False
      ItemIndex = 1
      Items.Strings = (
        'Objects overwrite values of previous objects'
        'Sum values of all objects')
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
        Control = rgAssignmentMethod
      end>
  end
end
