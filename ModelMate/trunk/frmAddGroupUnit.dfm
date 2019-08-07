object frmAddGroup: TfrmAddGroup
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Add Parameter Group'
  ClientHeight = 122
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object lblName: TLabel
    Left = 39
    Top = 28
    Width = 139
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Alignment = taRightJustify
    Caption = 'Name of new group:'
  end
  object edtGPName: TEdit
    Left = 183
    Top = 25
    Width = 180
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MaxLength = 12
    TabOrder = 0
  end
  object btnCancel: TBitBtn
    Left = 192
    Top = 71
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
    TabOrder = 2
  end
  object btnOK: TBitBtn
    Left = 96
    Top = 71
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
