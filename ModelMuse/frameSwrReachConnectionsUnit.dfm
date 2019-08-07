object frameSwrReachConnections: TframeSwrReachConnections
  Left = 0
  Top = 0
  Width = 431
  Height = 304
  TabOrder = 0
  object shpReachColor: TShape
    Left = 357
    Top = 3
    Width = 65
    Height = 25
  end
  object shpUnconnectedColor: TShape
    Left = 357
    Top = 33
    Width = 65
    Height = 25
  end
  object shpStructureColor: TShape
    Left = 357
    Top = 65
    Width = 65
    Height = 25
  end
  object cbReaches: TCheckBox
    Left = 3
    Top = 7
    Width = 126
    Height = 17
    Caption = 'Plot connections'
    TabOrder = 1
    OnClick = cbReachesClick
  end
  object btnReachColor: TButton
    Left = 151
    Top = 3
    Width = 200
    Height = 25
    Caption = 'Change connection color'
    TabOrder = 0
    OnClick = btnReachColorClick
  end
  object cbPlotUnconnected: TCheckBox
    Left = 3
    Top = 37
    Width = 126
    Height = 17
    Caption = 'Plot unconnected reaches'
    TabOrder = 3
    OnClick = cbPlotUnconnectedClick
  end
  object btnUnconnectedColor: TButton
    Left = 151
    Top = 33
    Width = 200
    Height = 25
    Caption = 'Change unconnected color'
    TabOrder = 2
    OnClick = btnUnconnectedColorClick
  end
  object rgItemsToPlot: TRadioGroup
    Left = 0
    Top = 105
    Width = 185
    Height = 88
    Caption = 'Items to plot'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'All objects'
      'Visible objects'
      'Selected objects')
    TabOrder = 6
  end
  object cbPlotStructures: TCheckBox
    Left = 3
    Top = 69
    Width = 126
    Height = 17
    Caption = 'Plot structures'
    TabOrder = 5
    OnClick = cbPlotStructuresClick
  end
  object btnStructureColor: TButton
    Left = 151
    Top = 65
    Width = 200
    Height = 25
    Caption = 'Change structure color'
    TabOrder = 4
    OnClick = btnStructureColorClick
  end
  object dlgLinkColor: TColorDialog
    Left = 248
    Top = 72
  end
end
