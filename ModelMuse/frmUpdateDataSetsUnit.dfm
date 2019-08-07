inherited frmUpdateDataSets: TfrmUpdateDataSets
  Caption = 'Update Data Sets'
  ClientHeight = 189
  ClientWidth = 423
  Font.Height = -21
  ExplicitWidth = 441
  ExplicitHeight = 234
  PixelsPerInch = 96
  TextHeight = 24
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 407
    Height = 72
    Caption = 
      'One or more of the result data sets already exist.  How do you w' +
      'ant this situation to be handled?'
    WordWrap = True
  end
  object btnUpdate: TButton
    Left = 8
    Top = 83
    Width = 194
    Height = 78
    Caption = 'Update the existing data sets with new values'
    ModalResult = 1
    TabOrder = 0
    WordWrap = True
  end
  object btnCreate: TButton
    Left = 221
    Top = 83
    Width = 194
    Height = 78
    Caption = 'Create new data sets'
    ModalResult = 2
    TabOrder = 1
    WordWrap = True
  end
end
