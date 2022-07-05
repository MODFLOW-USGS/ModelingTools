inherited frameGwtAdvPackage: TframeGwtAdvPackage
  Height = 287
  ExplicitHeight = 287
  DesignSize = (
    422
    287)
  object rgScheme: TRadioGroup [3]
    Left = 16
    Top = 157
    Width = 391
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Advection Scheme'
    Enabled = False
    ItemIndex = 0
    Items.Strings = (
      'Upstream'
      'Central (rarely used)'
      'Total Variation Diminishing (TVD)')
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
        Control = rgScheme
      end>
  end
end
