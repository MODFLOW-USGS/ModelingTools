inherited frmFarmFileType: TfrmFarmFileType
  Left = 616
  Top = 288
  Width = 375
  Height = 157
  Caption = 'Farm File Type'
  PixelsPerInch = 96
  TextHeight = 13
  object rgFarmFileType: TRadioGroup [0]
    Left = 0
    Top = 0
    Width = 359
    Height = 78
    Align = alClient
    Caption = 'Farm File Type'
    Items.Strings = (
      'Farm Budget file (FB_DETAILS.OUT, FB_COMPACT.OUT)'
      'Farm Demand-Supply file (FDS.OUT)')
    TabOrder = 0
  end
  object pnl1: TPanel [1]
    Left = 0
    Top = 78
    Width = 359
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btnOK: TBitBtn
      Left = 272
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
  end
  inherited RBW_SetWindowState1: TRBW_SetWindowState
    Owner = Owner
  end
end
