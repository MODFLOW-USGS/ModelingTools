inherited frmNewVideos: TfrmNewVideos
  Caption = 'New ModelMuse Videos'
  ExplicitWidth = 438
  ExplicitHeight = 272
  TextHeight = 18
  object lblNewVideo: TLabel
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 418
    Height = 18
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Alignment = taCenter
    Caption = 'One or more new ModelMuse videos are available.'
    ExplicitWidth = 351
  end
  object memoNewVideos: TMemo
    Left = 0
    Top = 26
    Width = 426
    Height = 157
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 422
    ExplicitHeight = 156
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 183
    Width = 426
    Height = 51
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 182
    ExplicitWidth = 422
    object btnVideoPage: TButton
      Left = 16
      Top = 10
      Width = 257
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Click here for ModelMuse Videos'
      TabOrder = 0
      OnClick = btnVideoPageClick
    end
    object btnClose: TBitBtn
      Left = 312
      Top = 8
      Width = 94
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Kind = bkClose
      NumGlyphs = 2
      TabOrder = 1
    end
  end
end
