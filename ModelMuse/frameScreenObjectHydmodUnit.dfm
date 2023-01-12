object frameScreenObjectHydmod: TframeScreenObjectHydmod
  Left = 0
  Top = 0
  Width = 571
  Height = 503
  TabOrder = 0
  TabStop = True
  object lblHYDLBL: TLabel
    Left = 8
    Top = 40
    Width = 129
    Height = 13
    Caption = 'Hydrograph label (HYDLBL)'
  end
  object pnlCaption: TPanel
    Left = 0
    Top = 0
    Width = 571
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    Caption = 'pnlCaption'
    TabOrder = 0
  end
  object edHYDLBL: TRbwEdit
    Left = 8
    Top = 64
    Width = 208
    Height = 21
    TabOrder = 2
    OnExit = edHYDLBLExit
  end
  object rgINTYP: TRadioGroup
    Left = 222
    Top = 40
    Width = 231
    Height = 57
    Caption = 'Assignment method (INTYP)'
    ItemIndex = 0
    Items.Strings = (
      'Cell value'
      'Interpolated value')
    TabOrder = 1
  end
  object gbBasic: TGroupBox
    Left = 8
    Top = 103
    Width = 253
    Height = 110
    Caption = 'Basic package data'
    TabOrder = 3
    object clbBasic: TCheckListBox
      Left = 2
      Top = 15
      Width = 249
      Height = 93
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'Head (HD)'
        'Drawdown (DD)')
      TabOrder = 0
    end
  end
  object gbSubsidence: TGroupBox
    Left = 8
    Top = 219
    Width = 297
    Height = 230
    Caption = 'Subsidence package data'
    TabOrder = 5
    object lblLayerGroup: TLabel
      Left = 2
      Top = 96
      Width = 58
      Height = 13
      Caption = 'Layer group'
    end
    object lblNoDelayBed: TLabel
      Left = 2
      Top = 157
      Width = 64
      Height = 13
      Caption = 'No-delay bed'
    end
    object lblLayer: TLabel
      Left = 153
      Top = 96
      Width = 32
      Height = 13
      Caption = 'Layers'
    end
    object clbSub: TCheckListBox
      Left = 2
      Top = 15
      Width = 293
      Height = 66
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        'PreconsolidationHead (HC)'
        'Instantaneous compaction (CP)'
        'Instantaneous subsidence (SB)')
      TabOrder = 0
    end
    object comboLayerGroup: TJvImageComboBox
      Left = 2
      Top = 120
      Width = 145
      Height = 23
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 17
      ItemIndex = -1
      TabOrder = 1
      OnChange = comboLayerGroupChange
      Items = <>
    end
    object comboNoDelayBed: TJvImageComboBox
      Left = 2
      Top = 181
      Width = 145
      Height = 23
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 17
      ItemIndex = -1
      TabOrder = 3
      OnChange = comboNoDelayBedChange
      Items = <>
    end
    object clbLayer: TCheckListBox
      Left = 153
      Top = 120
      Width = 135
      Height = 81
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object gbSFR: TGroupBox
    Left = 267
    Top = 103
    Width = 301
    Height = 110
    Caption = 'SFR package data'
    TabOrder = 4
    object clbSFR: TCheckListBox
      Left = 2
      Top = 15
      Width = 297
      Height = 93
      OnClickCheck = clbSFRClickCheck
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        'Stream stage (ST)'
        'Streamflow into reach (SI)'
        'Streamflow out of reach (SO)'
        'Streamflow into or out of aquifer (SA)')
      TabOrder = 0
    end
  end
end
