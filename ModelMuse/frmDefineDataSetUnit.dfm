inherited frmDefineDataSet: TfrmDefineDataSet
  Width = 313
  Height = 274
  VertScrollBar.Range = 0
  HorzScrollBar.Range = 0
  Caption = 'Define Data Set'
  Position = poMainFormCenter
  PixelsPerInch = 120
  object rgDataType: TRadioGroup
    Left = 16
    Top = 48
    Width = 137
    Height = 177
    Items.Strings = (
      'Real'
      'Integer'
      'Boolean'
      'String')
    Caption = 'Data Type'
    ItemIndex = 0
    TabOrder = 0
  end
  object rgOrientation: TRadioGroup
    Left = 160
    Top = 48
    Width = 129
    Height = 177
    Items.Strings = (
      '2D Top'
      '2D Front'
      '2D Side'
      '3D')
    Caption = 'Orientation'
    ItemIndex = 0
    TabOrder = 1
  end
  object edName: TEdit
    Left = 80
    Top = 16
    Width = 209
    Height = 29
    TabOrder = 2
  end
  object lblName: TLabel
    Left = 16
    Top = 20
    Width = 52
    Height = 21
    Caption = 'Name:'
  end
  object BitBtn1: TBitBtn
    Left = 104
    Top = 232
    Width = 91
    Height = 33
    ParentColor = True
    TabOrder = 4
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 200
    Top = 232
    Width = 91
    Height = 33
    ParentColor = True
    TabOrder = 5
    Kind = bkCancel
  end
end
