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
    EditLabel.Width = 114
    EditLabel.Height = 15
    EditLabel.Caption = 'Diameter (DIAMETER)'
    TabOrder = 2
    Text = ''
    OnChange = edDiameterChange
  end
  object edTortuosity: TLabeledEdit
    Left = 3
    Top = 88
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 128
    EditLabel.Height = 15
    EditLabel.Caption = 'Tortuosity (TORTUOSITY)'
    TabOrder = 4
    Text = ''
    OnChange = edTortuosityChange
  end
  object edRoughnessHeight: TLabeledEdit
    Left = 3
    Top = 138
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 154
    EditLabel.Height = 15
    EditLabel.Caption = 'Roughness height (RHEIGHT)'
    TabOrder = 6
    Text = ''
    OnChange = edRoughnessHeightChange
  end
  object edLowerCriticalR: TLabeledEdit
    Left = 3
    Top = 188
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 356
    EditLabel.Height = 15
    EditLabel.Caption = 
      'Lower critical Reynolds number (turbulent to laminar) (LCRITREY_' +
      'P)'
    TabOrder = 8
    Text = ''
    OnChange = edLowerCriticalRChange
  end
  object edHigherCriticalR: TLabeledEdit
    Left = 3
    Top = 237
    Width = 300
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 359
    EditLabel.Height = 15
    EditLabel.Caption = 
      'Higher critical Reynolds number (laminar to turbulent) (TCRITREY' +
      '_P)'
    TabOrder = 10
    Text = ''
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
    EditLabel.Width = 238
    EditLabel.Height = 15
    EditLabel.Caption = 'Conductance or permeability (K_EXCHANGE)'
    TabOrder = 12
    Text = ''
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
    EditLabel.Width = 143
    EditLabel.Height = 15
    EditLabel.Caption = 'Pipe elevation (ELEVATION)'
    TabOrder = 14
    Text = ''
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
