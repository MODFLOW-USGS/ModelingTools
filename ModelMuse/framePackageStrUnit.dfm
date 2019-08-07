inherited framePackageStr: TframePackageStr
  Height = 215
  ExplicitHeight = 215
  DesignSize = (
    422
    215)
  object cbCalculateStage: TCheckBox [3]
    Left = 16
    Top = 157
    Width = 273
    Height = 17
    Caption = 'Calculate stage (ICALC)'
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
        Control = cbCalculateStage
      end>
  end
end
