inherited frmModelVersionSelect: TfrmModelVersionSelect
  Width = 297
  Height = 158
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  ActiveControl = rgChoice
  Caption = 'Select model version'
  PixelsPerInch = 96
  object rgChoice: TRadioGroup
    Left = 8
    Top = 8
    Width = 281
    Height = 105
    Items.Strings = (
      'Release candidate 2'
      'Release candidate 5')
    Caption = 'Model version'
    ItemIndex = 1
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 208
    Top = 120
    Width = 83
    Height = 33
    ParentColor = True
    TabOrder = 1
    Kind = bkClose
  end
end
