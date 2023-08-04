inherited frameScreenObjectCfpPipes: TframeScreenObjectCfpPipes
  Width = 545
  Height = 467
  ExplicitWidth = 545
  ExplicitHeight = 467
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 545
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 407
  end
  object pcCfpPipes: TPageControl
    Left = 0
    Top = 17
    Width = 545
    Height = 450
    ActivePage = tabOutput
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 407
    ExplicitHeight = 448
    object tabProperties: TTabSheet
      Caption = 'Properties'
      DesignSize = (
        537
        420)
      object edDiameter: TLabeledEdit
        Left = 3
        Top = 23
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 114
        EditLabel.Height = 15
        EditLabel.Caption = 'Diameter (DIAMETER)'
        TabOrder = 0
        Text = ''
        OnChange = edDiameterChange
      end
      object btnDiameter: TButton
        Left = 447
        Top = 20
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 1
      end
      object edTortuosity: TLabeledEdit
        Left = 3
        Top = 72
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 128
        EditLabel.Height = 15
        EditLabel.Caption = 'Tortuosity (TORTUOSITY)'
        TabOrder = 2
        Text = ''
        OnChange = edTortuosityChange
      end
      object btnTortuosity: TButton
        Left = 447
        Top = 69
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 3
      end
      object edRoughnessHeight: TLabeledEdit
        Left = 3
        Top = 122
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 154
        EditLabel.Height = 15
        EditLabel.Caption = 'Roughness height (RHEIGHT)'
        TabOrder = 4
        Text = ''
        OnChange = edRoughnessHeightChange
      end
      object btnRoughnessHeight: TButton
        Left = 447
        Top = 119
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 5
      end
      object edLowerCriticalR: TLabeledEdit
        Left = 3
        Top = 172
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 356
        EditLabel.Height = 15
        EditLabel.Caption = 
          'Lower critical Reynolds number (turbulent to laminar) (LCRITREY_' +
          'P)'
        TabOrder = 6
        Text = ''
        OnChange = edLowerCriticalRChange
      end
      object btnLowerCriticalR: TButton
        Left = 447
        Top = 169
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 7
      end
      object edHigherCriticalR: TLabeledEdit
        Left = 3
        Top = 221
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 359
        EditLabel.Height = 15
        EditLabel.Caption = 
          'Higher critical Reynolds number (laminar to turbulent) (TCRITREY' +
          '_P)'
        TabOrder = 8
        Text = ''
        OnChange = edHigherCriticalRChange
      end
      object btnHigherCriticalR: TButton
        Left = 447
        Top = 218
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 9
      end
      object edConductancePermeability: TLabeledEdit
        Left = 3
        Top = 271
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 238
        EditLabel.Height = 15
        EditLabel.Caption = 'Conductance or permeability (K_EXCHANGE)'
        TabOrder = 10
        Text = ''
        OnChange = edConductancePermeabilityChange
      end
      object btnConductancePermeability: TButton
        Left = 447
        Top = 268
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 11
      end
      object edElevation: TLabeledEdit
        Left = 3
        Top = 319
        Width = 438
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 143
        EditLabel.Height = 15
        EditLabel.Caption = 'Pipe elevation (ELEVATION)'
        TabOrder = 12
        Text = ''
        OnChange = edConductancePermeabilityChange
      end
      object btnElevation: TButton
        Left = 447
        Top = 316
        Width = 90
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Edit F()...'
        TabOrder = 13
      end
    end
    object tabOutput: TTabSheet
      Caption = 'Output'
      ImageIndex = 1
      object cbRecordPipes: TCheckBox
        Left = 3
        Top = 3
        Width = 401
        Height = 17
        Caption = 'Save conduit flow rates and Reynolds numbers (PIPE_NUMBERS)'
        TabOrder = 0
        OnClick = cbRecordPipesClick
      end
      object cbRecordNodes: TCheckBox
        Left = 3
        Top = 26
        Width = 366
        Height = 17
        Caption = 'Save flow and head values at nodes (NODE_NUMBERS)'
        TabOrder = 1
        OnClick = cbRecordNodesClick
      end
      object cbRecordTimeSeriesPipes: TCheckBox
        Left = 3
        Top = 49
        Width = 478
        Height = 17
        Caption = 'Record all input and output terms for pipes (TSAT_TUBE_NUMBERS)'
        TabOrder = 2
        OnClick = cbRecordTimeSeriesPipesClick
      end
      object cbRecordTimeSeriesNodes: TCheckBox
        Left = 3
        Top = 72
        Width = 502
        Height = 17
        Caption = 'Record all input and output terms for nodes (TSAT_NTS)'
        TabOrder = 3
        OnClick = cbRecordTimeSeriesNodesClick
      end
    end
  end
end
