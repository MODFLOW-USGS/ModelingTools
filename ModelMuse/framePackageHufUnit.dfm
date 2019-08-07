inherited framePackageHuf: TframePackageHuf
  Height = 275
  ExplicitHeight = 275
  DesignSize = (
    422
    275)
  object cbSaveHeads: TCheckBox [3]
    Left = 16
    Top = 157
    Width = 288
    Height = 17
    Caption = 'Save heads for hydrogeologic units (IOHUFHEADS)'
    Enabled = False
    TabOrder = 1
  end
  object cbSaveFlows: TCheckBox [4]
    Left = 16
    Top = 180
    Width = 285
    Height = 17
    Caption = 'Save flows for hydrogeologic units (IOHUFFLOWS)'
    Enabled = False
    TabOrder = 2
  end
  object rgElevationSurfaceChoice: TRadioGroup [5]
    Left = 16
    Top = 203
    Width = 273
    Height = 62
    Caption = 'Reference surface for calculating depth'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Model top'
      'Reference surface data set')
    TabOrder = 3
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
        Control = cbSaveHeads
      end
      item
        Control = cbSaveFlows
      end>
  end
end
