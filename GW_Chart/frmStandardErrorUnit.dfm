object frmStandardError: TfrmStandardError
  Left = 284
  Top = 116
  Width = 207
  Height = 158
  Caption = 'Standard Error'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 16
  object lblStandardErrorOfRegression: TLabel
    Left = 8
    Top = 8
    Width = 167
    Height = 16
    Caption = 'Standard Error of Regression'
  end
  object adeStandardError: TArgusDataEntry
    Left = 8
    Top = 32
    Width = 185
    Height = 22
    ItemHeight = 16
    TabOrder = 0
    Text = '0'
    DataType = dtReal
    Max = 1
    ChangeDisabledColor = True
  end
  object btnRead: TButton
    Left = 8
    Top = 64
    Width = 185
    Height = 25
    Caption = 'Read from "global" or "list" file'
    TabOrder = 1
    OnClick = btnReadClick
  end
  object BitBtn1: TBitBtn
    Left = 40
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 2
    Kind = bkCancel
  end
  object BitBtn2: TBitBtn
    Left = 120
    Top = 96
    Width = 75
    Height = 25
    TabOrder = 3
    Kind = bkOK
  end
  object OpenDialog1: TOpenDialog
    Filter = 'global or list files|*.list;*.lst;*.glo;*.lsg|All Files|*.*'
    Title = 'Select "global" or "list" file'
    Left = 16
    Top = 88
  end
end
