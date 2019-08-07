object frmProjName: TfrmProjName
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Project Name and Title'
  ClientHeight = 204
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    504
    204)
  PixelsPerInch = 120
  TextHeight = 18
  object lblName: TLabel
    Left = 37
    Top = 68
    Width = 99
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taRightJustify
    Caption = 'Project Name:'
  end
  object lblTitle: TLabel
    Left = 51
    Top = 116
    Width = 85
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taRightJustify
    Caption = 'Project Title:'
  end
  object edtName: TEdit
    Left = 144
    Top = 65
    Width = 335
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object edtTitle: TEdit
    Left = 144
    Top = 113
    Width = 335
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object btnCancel: TBitBtn
    Left = 258
    Top = 165
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object btnOK: TBitBtn
    Left = 161
    Top = 165
    Width = 89
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkOK
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 3
    OnClick = btnOKClick
  end
  object ckbxNameLikeFile: TCheckBox
    Left = 19
    Top = 19
    Width = 457
    Height = 20
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Keep Project Name Same As Project (.mtc) File Name'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = ckbxNameLikeFileClick
  end
end
