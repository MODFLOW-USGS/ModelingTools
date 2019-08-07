inherited framePackageGNC: TframePackageGNC
  Height = 234
  ExplicitHeight = 234
  DesignSize = (
    422
    234)
  object rgFormulation: TRadioGroup [3]
    Left = 16
    Top = 151
    Width = 391
    Height = 74
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Formulation'
    Enabled = False
    Items.Strings = (
      'Implicit'
      'Explicit')
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
        Control = rgFormulation
      end>
  end
end
