inherited frameGweCndPackage: TframeGweCndPackage
  Width = 525
  Height = 434
  ExplicitWidth = 525
  ExplicitHeight = 434
  DesignSize = (
    525
    434)
  inherited lblComments: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited lblPackage: TLabel
    StyleElements = [seFont, seClient, seBorder]
  end
  inherited memoComments: TMemo
    Width = 494
    StyleElements = [seFont, seClient, seBorder]
    ExplicitWidth = 494
  end
  object cbUseXT3D: TCheckBox [3]
    Left = 16
    Top = 168
    Width = 289
    Height = 17
    Caption = 'Use XT3D (inverse of XT3D_OFF)'
    Checked = True
    Enabled = False
    State = cbChecked
    TabOrder = 1
    OnClick = cbUseXT3DClick
  end
  object cbXT3D_RHS: TCheckBox [4]
    Left = 16
    Top = 191
    Width = 391
    Height = 17
    Caption = 'Some XT3D terms on Right Hand Side (XT3D_RHS)'
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
        Control = cbUseXT3D
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
