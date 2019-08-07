object frameScreenObjectFootprintWell: TframeScreenObjectFootprintWell
  Left = 0
  Top = 0
  Width = 492
  Height = 240
  TabOrder = 0
  object lblPumpingRate: TLabel
    Left = 8
    Top = 63
    Width = 141
    Height = 16
    Margins.Left = 8
    Caption = 'Withdrawal rate (L^3/T)'
    Enabled = False
  end
  object cbUseFootprintWell: TCheckBox
    Left = 8
    Top = 16
    Width = 177
    Height = 17
    AllowGrayed = True
    Caption = 'Use footprint well'
    TabOrder = 0
    OnClick = cbUseFootprintWellClick
  end
  object edPumpingRate: TRbwEdit
    Left = 176
    Top = 60
    Width = 225
    Height = 24
    Color = clBtnFace
    Enabled = False
    TabOrder = 2
  end
  object btnPumpingRate: TButton
    Left = 407
    Top = 58
    Width = 75
    Height = 25
    Caption = 'Edit F()...'
    Enabled = False
    TabOrder = 1
  end
end
