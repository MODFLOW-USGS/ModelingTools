inherited framePackageRip: TframePackageRip
  inherited memoComments: TMemo
    Height = 59
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitHeight = 59
  end
  object cbWritePlantGroupFlows: TCheckBox [3]
    Left = 16
    Top = 127
    Width = 273
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Write plant group flows (IRIPCB1)'
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
        Control = cbWritePlantGroupFlows
      end>
  end
end
