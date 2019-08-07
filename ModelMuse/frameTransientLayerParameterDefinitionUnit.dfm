inherited frameTransientLayerParameterDefinition: TframeTransientLayerParameterDefinition
  inherited dgParameters: TRbwDataGrid4
    Top = 41
    Height = 151
    ExplicitTop = 41
    ExplicitHeight = 151
  end
  object pnLayerOption: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lblLayerOption: TLabel
      Left = 16
      Top = 6
      Width = 75
      Height = 13
      Caption = 'Location Option'
    end
    object comboLayerOption: TComboBox
      Left = 207
      Top = 3
      Width = 145
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Top layer'
      Items.Strings = (
        'Top layer'
        'Specified layer'
        'Top active cell')
    end
    object cbTimeVaryingLayers: TCheckBox
      Left = 358
      Top = 7
      Width = 235
      Height = 17
      Caption = 'Time Varying Layers'
      Enabled = False
      TabOrder = 1
    end
  end
end
