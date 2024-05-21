inherited framePackageRCH: TframePackageRCH
  Width = 591
  Height = 248
  ExplicitWidth = 591
  ExplicitHeight = 248
  DesignSize = (
    591
    248)
  inherited memoComments: TMemo
    Width = 560
    Height = 66
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 560
    ExplicitHeight = 96
  end
  inherited pnLayerOption: TPanel
    Top = 134
    Width = 591
    Height = 114
    ExplicitTop = 135
    ExplicitWidth = 591
    ExplicitHeight = 114
    DesignSize = (
      591
      114)
    object rgAssignmentMethod: TRadioGroup
      Left = 16
      Top = 27
      Width = 560
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
    object cbUseMultiplierMODFLOW6: TCheckBox
      Left = 16
      Top = 92
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
        Control = rgAssignmentMethod
      end
      item
        Control = cbUseMultiplierMODFLOW6
      end>
  end
end
