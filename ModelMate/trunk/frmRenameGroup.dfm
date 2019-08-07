object FormRenameGroup: TFormRenameGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Rename Group'
  ClientHeight = 137
  ClientWidth = 361
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object lblOldGroup: TLabel
    Left = 169
    Top = 21
    Width = 82
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'lblOldGroup'
  end
  object Label1: TLabel
    Left = 37
    Top = 21
    Width = 121
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taRightJustify
    Caption = 'Old Group Name:'
  end
  object Label2: TLabel
    Left = 30
    Top = 47
    Width = 128
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taRightJustify
    Caption = 'New Group Name:'
  end
  object edtNewGroup: TEdit
    Left = 166
    Top = 45
    Width = 139
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    TabOrder = 0
    Text = 'edtNewGroup'
  end
  object btnCancel: TBitBtn
    Left = 186
    Top = 90
    Width = 90
    Height = 30
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DoubleBuffered = True
    Kind = bkCancel
    NumGlyphs = 2
    ParentDoubleBuffered = False
    TabOrder = 2
  end
  object btnOK: TBitBtn
    Left = 90
    Top = 90
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
    TabOrder = 1
    OnClick = btnOKClick
  end
end
