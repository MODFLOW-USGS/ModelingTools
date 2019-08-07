object FormRenameGroup: TFormRenameGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Rename Group'
  ClientHeight = 115
  ClientWidth = 304
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object lblOldGroup: TLabel
    Left = 142
    Top = 18
    Width = 66
    Height = 16
    Caption = 'lblOldGroup'
  end
  object Label1: TLabel
    Left = 30
    Top = 18
    Width = 99
    Height = 16
    Caption = 'Old Group Name:'
  end
  object Label2: TLabel
    Left = 30
    Top = 41
    Width = 105
    Height = 16
    Caption = 'New Group Name:'
  end
  object edtNewGroup: TEdit
    Left = 140
    Top = 38
    Width = 117
    Height = 24
    TabOrder = 0
    Text = 'edtNewGroup'
  end
  object btnCancel: TBitBtn
    Left = 157
    Top = 76
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 76
    Top = 76
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
  end
end
