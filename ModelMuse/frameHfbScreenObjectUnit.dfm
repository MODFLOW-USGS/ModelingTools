object frameHfbScreenObject: TframeHfbScreenObject
  Left = 0
  Top = 0
  Width = 522
  Height = 211
  Enabled = False
  TabOrder = 0
  TabStop = True
  object lblParameterName: TLabel
    Left = 8
    Top = 34
    Width = 87
    Height = 15
    Margins.Left = 8
    Caption = 'Parameter name'
    Enabled = False
  end
  object lblHydraulicConductivity: TLabel
    Left = 8
    Top = 63
    Width = 155
    Height = 15
    Margins.Left = 8
    Caption = 'Barrier hydraulic conductivity'
    Enabled = False
  end
  object lblBarrierThickness: TLabel
    Left = 8
    Top = 91
    Width = 86
    Height = 15
    Margins.Left = 8
    Caption = 'Barrier thickness'
    Enabled = False
  end
  object comboHfbParameters: TJvImageComboBox
    Left = 312
    Top = 31
    Width = 170
    Height = 25
    Margins.Right = 8
    Style = csOwnerDrawVariable
    ButtonStyle = fsLighter
    Color = clBtnFace
    DroppedWidth = 170
    Enabled = False
    ImageHeight = 0
    ImageWidth = 0
    ItemHeight = 19
    ItemIndex = -1
    TabOrder = 1
    OnChange = comboHfbParametersChange
    Items = <>
  end
  object rgAngleAdjustment: TRadioGroup
    Left = 8
    Top = 116
    Width = 450
    Height = 85
    Margins.Left = 8
    Margins.Right = 8
    Caption = 'Angle adjustment method'
    Enabled = False
    ItemIndex = 1
    Items.Strings = (
      'None'
      'Distribute conductivity among all sections'
      
        'Distribute conductivity among sections most nearly parallel to t' +
        'he grid')
    TabOrder = 6
  end
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlCaption'
    TabOrder = 0
  end
  object edHydraulicConductivity: TRbwEdit
    Left = 312
    Top = 60
    Width = 89
    Height = 23
    Color = clBtnFace
    Enabled = False
    TabOrder = 3
  end
  object edBarrierThickness: TRbwEdit
    Left = 312
    Top = 88
    Width = 89
    Height = 23
    Color = clBtnFace
    Enabled = False
    TabOrder = 5
  end
  object btnEditHfbHydraulicConductivityFormula: TButton
    Left = 407
    Top = 54
    Width = 75
    Height = 25
    Caption = 'Edit F()...'
    Enabled = False
    TabOrder = 2
  end
  object btnEditHfbThicknessyFormula: TButton
    Left = 407
    Top = 85
    Width = 75
    Height = 25
    Caption = 'Edit F()...'
    Enabled = False
    TabOrder = 4
  end
end
