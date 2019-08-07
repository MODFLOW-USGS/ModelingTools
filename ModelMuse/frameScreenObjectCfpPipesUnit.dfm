inherited frameScreenObjectCfpPipes: TframeScreenObjectCfpPipes
  Width = 407
  Height = 415
  ExplicitWidth = 407
  ExplicitHeight = 415
  DesignSize = (
    407
    415)
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 407
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object btnDiameter: TButton
    Left = 309
    Top = 36
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 1
  end
  object btnTortuosity: TButton
    Left = 309
    Top = 85
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 3
  end
  object btnRoughnessHeight: TButton
    Left = 309
    Top = 135
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 5
  end
  object btnLowerCriticalR: TButton
    Left = 309
    Top = 185
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 7
  end
  object edDiameter: TLabeledEdit
    Left = 3
    Top = 39
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 105
    EditLabel.Height = 13
    EditLabel.Caption = 'Diameter (DIAMETER)'
    TabOrder = 2
    OnChange = edDiameterChange
  end
  object edTortuosity: TLabeledEdit
    Left = 3
    Top = 88
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 124
    EditLabel.Height = 13
    EditLabel.Caption = 'Tortuosity (TORTUOSITY)'
    TabOrder = 4
    OnChange = edTortuosityChange
  end
  object edRoughnessHeight: TLabeledEdit
    Left = 3
    Top = 138
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 141
    EditLabel.Height = 13
    EditLabel.Caption = 'Roughness height (RHEIGHT)'
    TabOrder = 6
    OnChange = edRoughnessHeightChange
  end
  object edLowerCriticalR: TLabeledEdit
    Left = 3
    Top = 188
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 324
    EditLabel.Height = 13
    EditLabel.Caption = 
      'Lower critical Reynolds number (turbulent to laminar) (LCRITREY_' +
      'P)'
    TabOrder = 8
    OnChange = edLowerCriticalRChange
  end
  object edHigherCriticalR: TLabeledEdit
    Left = 3
    Top = 237
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 327
    EditLabel.Height = 13
    EditLabel.Caption = 
      'Higher critical Reynolds number (laminar to turbulent) (TCRITREY' +
      '_P)'
    TabOrder = 10
    OnChange = edHigherCriticalRChange
  end
  object btnHigherCriticalR: TButton
    Left = 309
    Top = 234
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 9
  end
  object edConductancePermeability: TLabeledEdit
    Left = 3
    Top = 287
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 213
    EditLabel.Height = 13
    EditLabel.Caption = 'Conductance or permeability (K_EXCHANGE)'
    TabOrder = 12
    OnChange = edConductancePermeabilityChange
  end
  object btnConductancePermeability: TButton
    Left = 309
    Top = 284
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 11
  end
  object edElevation: TLabeledEdit
    Left = 3
    Top = 335
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 133
    EditLabel.Height = 13
    EditLabel.Caption = 'Pipe elevation (ELEVATION)'
    TabOrder = 14
    OnChange = edConductancePermeabilityChange
  end
  object btnElevation: TButton
    Left = 309
    Top = 332
    Width = 90
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Edit F()...'
    TabOrder = 13
  end
  object cbRecordPipes: TCheckBox
    Left = 3
    Top = 365
    Width = 401
    Height = 17
    Caption = 'Save conduit flow rates and Reynolds numbers (PIPE_NUMBERS)'
    TabOrder = 15
    OnClick = cbRecordPipesClick
  end
  object cbRecordNodes: TCheckBox
    Left = 3
    Top = 388
    Width = 366
    Height = 17
    Caption = 'Save flow and head values at nodes (NODE_NUMBERS)'
    TabOrder = 16
    OnClick = cbRecordNodesClick
  end
end
