object FormSelectDirectory: TFormSelectDirectory
  Left = 0
  Top = 0
  Caption = 'FormSelectDirectory'
  ClientHeight = 138
  ClientWidth = 508
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
    508
    138)
  PixelsPerInch = 96
  TextHeight = 16
  object lblMain: TLabel
    AlignWithMargins = True
    Left = 6
    Top = 12
    Width = 496
    Height = 33
    Margins.Left = 6
    Margins.Top = 12
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alTop
    AutoSize = False
    Caption = 'lblMain'
    WordWrap = True
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 735
  end
  object DirEdit1: TJvDirectoryEdit
    Left = 6
    Top = 58
    Width = 492
    Height = 24
    DialogKind = dkWin32
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'DirEdit1'
  end
  object btnCancel: TButton
    Left = 266
    Top = 94
    Width = 80
    Height = 30
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 180
    Top = 94
    Width = 80
    Height = 30
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
  end
end
