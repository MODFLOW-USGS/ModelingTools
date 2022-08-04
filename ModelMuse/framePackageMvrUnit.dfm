inherited framePackageMvr: TframePackageMvr
  Height = 218
  ExplicitHeight = 218
  DesignSize = (
    422
    218)
  object cbSaveBudget: TCheckBox [3]
    Left = 16
    Top = 168
    Width = 353
    Height = 17
    Caption = 'Save MVR budget to a binary file'
    Enabled = False
    TabOrder = 1
  end
  object chSaveCsv: TCheckBox [4]
    Left = 16
    Top = 191
    Width = 353
    Height = 17
    Caption = 'Save MVR budget to a CSV file'
    Enabled = False
    TabOrder = 2
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
