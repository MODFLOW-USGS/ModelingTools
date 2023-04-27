object frameDynamicScreenObjectsContainer: TframeDynamicScreenObjectsContainer
  Left = 0
  Top = 0
  Width = 640
  Height = 480
  TabOrder = 0
  object plTimeSeries: TJvPageList
    Left = 121
    Top = 0
    Width = 519
    Height = 439
    PropagateEnable = False
    Align = alClient
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 439
    Width = 640
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnAddGroup: TButton
      Left = 9
      Top = 6
      Width = 184
      Height = 25
      Caption = 'Add Time Series Group'
      TabOrder = 0
      OnClick = btnAddGroupClick
    end
    object btnDeleteGroup: TButton
      Left = 199
      Top = 6
      Width = 202
      Height = 25
      Caption = 'Delete Time Series Group'
      TabOrder = 1
      OnClick = btnDeleteGroupClick
    end
  end
  object tvTimeSeries: TJvPageListTreeView
    Left = 0
    Top = 0
    Width = 121
    Height = 439
    PageDefault = 0
    Align = alLeft
    HideSelection = False
    Indent = 19
    TabOrder = 2
    OnChange = tvTimeSeriesChange
    Items.Links = {00000000}
  end
end
