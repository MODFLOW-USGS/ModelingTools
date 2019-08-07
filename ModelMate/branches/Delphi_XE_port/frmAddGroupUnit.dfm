object frmAddGroup: TfrmAddGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Parameter Group'
  ClientHeight = 103
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object lblName: TLabel
    Left = 20
    Top = 23
    Width = 118
    Height = 16
    Caption = 'Name of new group:'
  end
  object edtGPName: TEdit
    Left = 140
    Top = 21
    Width = 151
    Height = 24
    MaxLength = 12
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 162
    Top = 60
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 81
    Top = 60
    Width = 75
    Height = 25
    TabOrder = 1
    OnClick = btnOKClick
    Kind = bkOK
  end
end
