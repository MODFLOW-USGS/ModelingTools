inherited framePackageFmi: TframePackageFmi
  Width = 476
  Height = 386
  ExplicitWidth = 476
  ExplicitHeight = 386
  DesignSize = (
    476
    386)
  inherited memoComments: TMemo
    Width = 445
    ExplicitWidth = 445
  end
  object cbFlowImbalance: TCheckBox [3]
    Left = 16
    Top = 160
    Width = 445
    Height = 17
    Caption = 'Use flow imbalance correction (FLOW_IMBALANCE_CORRECTION)'
    Enabled = False
    TabOrder = 1
  end
  object rgSimulationChoice: TRadioGroup [4]
    Left = 16
    Top = 192
    Width = 445
    Height = 121
    Caption = 'Simulation choice (FMI6)'
    Enabled = False
    Items.Strings = (
      '(1) Flow and solute transport in the same simulation'
      
        '(2) Separate flow simulation and separate simulations for each c' +
        'hemical species')
    TabOrder = 2
    WordWrap = True
    OnClick = rcSelectionControllerEnabledChange
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
        Control = rgSimulationChoice
      end>
    OnEnabledChange = rcSelectionControllerEnabledChange
  end
end
