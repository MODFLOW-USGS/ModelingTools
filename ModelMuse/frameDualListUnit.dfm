object frameDualList: TframeDualList
  Left = 0
  Top = 0
  Width = 388
  Height = 221
  TabOrder = 0
  TabStop = True
  object SrcLabel: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'Available Parameters:'
  end
  object DstLabel: TLabel
    Left = 205
    Top = 3
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'Selected Parameters:'
  end
  object IncludeBtn: TSpeedButton
    Left = 175
    Top = 30
    Width = 24
    Height = 24
    Caption = '>'
    OnClick = IncludeBtnClick
  end
  object IncAllBtn: TSpeedButton
    Left = 175
    Top = 60
    Width = 24
    Height = 24
    Caption = '>>'
    OnClick = IncAllBtnClick
  end
  object ExcludeBtn: TSpeedButton
    Left = 175
    Top = 90
    Width = 24
    Height = 24
    Caption = '<'
    Enabled = False
    OnClick = ExcludeBtnClick
  end
  object ExAllBtn: TSpeedButton
    Left = 175
    Top = 120
    Width = 24
    Height = 24
    Caption = '<<'
    Enabled = False
    OnClick = ExAllBtnClick
  end
  object SrcList: TListBox
    Left = 8
    Top = 24
    Width = 161
    Height = 185
    ItemHeight = 13
    Items.Strings = (
      'Item1'
      'Item2'
      'Item3'
      'Item4'
      'Item5')
    MultiSelect = True
    Sorted = True
    TabOrder = 0
  end
  object DstList: TListBox
    Left = 205
    Top = 25
    Width = 164
    Height = 185
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
end
