inherited framePackageTvs: TframePackageTvs
  Height = 256
  ExplicitHeight = 256
  DesignSize = (
    422
    256)
  inherited memoComments: TMemo
    Height = 139
    ExplicitHeight = 139
  end
  object cbEnableStorageChangeIntegration: TCheckBox [3]
    Left = 16
    Top = 208
    Width = 391
    Height = 45
    Caption = 
      'Enable storage change integration (Inverse of DISABLE_STORAGE_CH' +
      'ANGE_INTEGRATION)'
    Enabled = False
    TabOrder = 1
    WordWrap = True
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
        Control = cbEnableStorageChangeIntegration
      end>
  end
end
