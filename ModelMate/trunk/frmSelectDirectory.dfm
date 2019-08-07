object FormSelectDirectory: TFormSelectDirectory
  Left = 0
  Top = 0
  Caption = 'FormSelectDirectory'
  ClientHeight = 164
  ClientWidth = 603
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
    603
    164)
  PixelsPerInch = 120
  TextHeight = 18
  object lblMain: TLabel
    AlignWithMargins = True
    Left = 7
    Top = 14
    Width = 589
    Height = 49
    Margins.Left = 7
    Margins.Top = 14
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alTop
    AutoSize = False
    Caption = 'lblMain'
    WordWrap = True
  end
  object DirEdit1: TJvDirectoryEdit
    Left = 7
    Top = 74
    Width = 584
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    DialogKind = dkWin32
    ButtonWidth = 25
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'DirEdit1'
  end
  object btnCancel: TButton
    Left = 316
    Top = 116
    Width = 95
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object btnOK: TButton
    Left = 214
    Top = 116
    Width = 95
    Height = 35
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    TabOrder = 2
    OnClick = btnOKClick
  end
end
