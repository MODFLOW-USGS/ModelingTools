object frmSaveArchive: TfrmSaveArchive
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'frmSaveArchive'
  ClientHeight = 57
  ClientWidth = 218
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object cbSaveArchive: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Create archive'
    Checked = True
    State = cbChecked
    TabOrder = 0
  end
  object cbSaveDataSetValues: TCheckBox
    Left = 8
    Top = 31
    Width = 153
    Height = 17
    Caption = 'Save data set values'
    TabOrder = 1
  end
end
