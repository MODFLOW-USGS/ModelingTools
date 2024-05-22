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
    Width = 146
    Height = 15
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
    Height = 23
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
      Top = 17
      Width = 249
      Height = 91
      Align = alClient
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
      Width = 63
      Height = 15
      Caption = 'Layer group'
    end
    object lblNoDelayBed: TLabel
      Left = 2
      Top = 157
      Width = 72
      Height = 15
      Caption = 'No-delay bed'
    end
    object lblLayer: TLabel
      Left = 153
      Top = 96
      Width = 33
      Height = 15
      Caption = 'Layers'
    end
    object clbSub: TCheckListBox
      Left = 2
      Top = 17
      Width = 293
      Height = 66
      Align = alTop
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
      Height = 25
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 19
      ItemIndex = -1
      TabOrder = 1
      OnChange = comboLayerGroupChange
      Items = <>
    end
    object comboNoDelayBed: TJvImageComboBox
      Left = 2
      Top = 181
      Width = 145
      Height = 25
      Style = csOwnerDrawVariable
      ButtonStyle = fsLighter
      DroppedWidth = 145
      ImageHeight = 0
      ImageWidth = 0
      ItemHeight = 19
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
      Top = 17
      Width = 297
      Height = 91
      Align = alClient
      Items.Strings = (
        'Stream stage (ST)'
        'Streamflow into reach (SI)'
        'Streamflow out of reach (SO)'
        'Streamflow into or out of aquifer (SA)')
      TabOrder = 0
      OnClickCheck = clbSFRClickCheck
    end
  end
end
