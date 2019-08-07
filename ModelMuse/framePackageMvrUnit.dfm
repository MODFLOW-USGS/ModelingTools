inherited framePackageMvr: TframePackageMvr
  Height = 207
  ExplicitHeight = 207
  DesignSize = (
    422
    207)
  object cbSaveBudget: TCheckBox [3]
    Left = 16
    Top = 168
    Width = 353
    Height = 17
    Caption = 'Save MVR budget to a binary file'
    Enabled = False
    TabOrder = 1
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
        Control = cbSaveBudget
      end>
  end
end
