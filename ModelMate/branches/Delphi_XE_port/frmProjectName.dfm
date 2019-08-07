object frmProjName: TfrmProjName
  Left = 0
  Top = 0
  Caption = 'Project Name and Title'
  ClientHeight = 170
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    422
    170)
  PixelsPerInch = 96
  TextHeight = 16
  object lblName: TLabel
    Left = 16
    Top = 56
    Width = 82
    Height = 16
    Caption = 'Project Name:'
  end
  object lblTitle: TLabel
    Left = 16
    Top = 96
    Width = 74
    Height = 16
    Caption = 'Project Title:'
  end
  object edtName: TEdit
    Left = 104
    Top = 55
    Width = 297
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edtTitle: TEdit
    Left = 104
    Top = 95
    Width = 297
    Height = 24
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 217
    Top = 129
    Width = 75
    Height = 25
    TabOrder = 4
    Kind = bkCancel
  end
  object btnOK: TBitBtn
    Left = 136
    Top = 129
    Width = 75
    Height = 25
    TabOrder = 3
    OnClick = btnOKClick
    Kind = bkOK
  end
  object ckbxNameLikeFile: TCheckBox
    Left = 16
    Top = 16
    Width = 385
    Height = 17
    Caption = 'Keep Project Name Same As Project (.mtc) File Name'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = ckbxNameLikeFileClick
  end
end
