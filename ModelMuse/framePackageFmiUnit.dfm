inherited framePackageFmi: TframePackageFmi
  Width = 476
  Height = 279
  ExplicitWidth = 476
  ExplicitHeight = 279
  DesignSize = (
    476
    279)
  inherited memoComments: TMemo
    Width = 445
    ExplicitWidth = 445
  end
  object cbSeparate: TCheckBox [3]
    Left = 16
    Top = 168
    Width = 305
    Height = 17
    Caption = 'Use separate time discretization (FMI6)'
    Enabled = False
    TabOrder = 1
    OnClick = cbSeparateClick
  end
  object cbFlowImbalance: TCheckBox [4]
    Left = 16
    Top = 208
    Width = 417
    Height = 17
    Caption = 'Use flow imbalance correction (FLOW_IMBALANCE_CORRECTION)'
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
        Control = cbSeparate
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
